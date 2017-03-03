;;; function-args.el --- C++ completion for GNU Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2013-2014  Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/function-args
;; Version: 0.5.1
;; Package-Requires: ((swiper "0.2.0"))

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This extension provides several commands that are useful for c++-mode:
;;
;; * `fa-show' -- show an overlay hint with current function arguments.
;; * `fa-jump' -- jump to definition of current element of `fa-show'.
;; * `moo-complete' -- a c++-specific version of `semantic-ia-complete-symbol'.
;; * `moo-propose-virtual' -- in class declaration, list all virtual
;;   methods that the current class can override.
;; * `moo-propose-override' -- similar to `moo-propose-virtual', but lists all
;;   inherited methods instead.
;; * `moo-jump-local' -- jump to a tag defined in current buffer.


;;; Code:
;; ——— Requires ————————————————————————————————————————————————————————————————
(require 'cl-lib)
(require 'cc-cmds)
(require 'etags)
(eval-when-compile
  (require 'cl))
(require 'semantic/ia)
(require 'semantic/db-find)
(require 'semantic-directory)
(defvar ivy-last)
(declare-function ivy-read "ext:ivy")
(declare-function ivy-state-window "ext:ivy")
(declare-function helm "ext:helm")
(declare-function helm-build-sync-source "ext:helm-source")
(declare-function aya-expand "ext:auto-yasnippet")

;; ——— Customization ———————————————————————————————————————————————————————————
(defgroup function-args nil
  "C++ function completion."
  :group 'completion
  :prefix "fa-")

(defgroup function-args-faces nil
  "Font-lock faces for `function-args'."
  :prefix "fa-"
  :group 'function-args)

(defcustom fa-hint-position-below nil
  "Non-nil means hint will be shown below point (instead of above)."
  :type 'boolean)

(defcustom fa-max-one-line-width 60
  "Max hint size that can be displayed on one line."
  :type 'integer)

(defcustom moo-select-method 'ivy
  "Method to select a candidate from a list of strings."
  :type '(choice
          (const :tag "Ivy" ivy)
          (const :tag "Helm" helm)
          (const :tag "Helm fuzzy" helm-fuzzy)
          (const :tag "Plain" display-completion-list)))

(defcustom fa-insert-method 'name
  "How to insert a function completed with `moo-complete'."
  :type '(choice
          (const :tag "Name only" name)
          (const :tag "Name and parens" name-and-parens)
          (const :tag "Name, space, parens" name-space-parens)
          (const :tag "Name and parens and hint" name-and-parens-and-hint)))

(defconst fa-hint-dark-color "gray20")

(defface fa-face-hint
    `((((class color) (background light))
       :inherit 'default :background "#fff3bc")
      (((class color) (background dark))
       :background ,fa-hint-dark-color))
  "Basic hint face."
  :group 'function-args-faces)

(defface fa-face-hint-bold
  '((t (:inherit fa-face-hint :bold t)))
  "Basic hint face with bold font. Bold is used to signify the current element."
  :group 'function-args-faces)

(defface fa-face-type
    `((((class color) (background light))
       :inherit 'font-lock-type-face :background "#fff3bc")
      (((class color) (background dark))
       :inherit 'font-lock-type-face :background ,fa-hint-dark-color))
  "Face for displaying types."
  :group 'function-args-faces)

(defface fa-face-type-bold
    '((t (:inherit 'fa-face-type :bold t)))
  "Face for displaying types. Bold is used to signify the current element"
  :group 'function-args-faces)

(defface fa-face-semi
    '((((class color) (background light))
       :inherit fa-face-hint-bold
       :foreground "#2a00ff")
      (((class color) (background dark))
       :inherit fa-face-hint-bold
       :foreground "white"))
  "Face for displaying separators."
  :group 'function-args-faces)

(defface fa-face-type-definition
    `((((class color) (background light))
      :inherit font-lock-type-face :background "#CECEFF")
     (((class color) (background dark))
      :inherit font-lock-type-face :background ,fa-hint-dark-color))
  "Face for type definitions."
  :group 'function-args-faces)

(defface fa-face-type-compound
  '((t (:inherit font-lock-type-face)))
  "Face for compound types."
  :group 'function-args-faces)

(defconst fa-paren-open (propertize "(" 'face 'fa-face-semi)
  "String to open argument list.")

(defconst fa-paren-close (propertize ") : " 'face 'fa-face-semi)
  "String to close argument list.")

(defconst fa-comma (propertize "," 'face 'fa-face-semi)
  "String to join arguments.")

(defcustom moo-do-includes t
  "When t, `moo-jump-local' will list includes as well."
  :type 'boolean
  :group 'function-args)

;; ——— Minor mode ——————————————————————————————————————————————————————————————
(defvar function-args-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode function-args-mode
    "Minor mode for C++ code completion bindings.

\\{function-args-mode-map}"
  :keymap function-args-mode-map
  :group 'function-args
  :lighter " FA"
  (if function-args-mode
      (semantic-mode 1)))

(defvar fa-idx nil
  "Current function arguments variant.")

(defvar fa-lst nil
  "Current function arguments variants.")

(defvar fa-arg 0
  "Current function argument.")

(defun fa-idx-cycle-down ()
  "Cycle `fa-idx' down and redisplay function arguments."
  (interactive)
  (setq fa-idx
        (mod (+ fa-idx 1)
             (length fa-lst)))
  (fa-update-arg))

(defun fa-idx-cycle-up ()
  "Cycle `fa-idx' up and redisplay function arguments."
  (interactive)
  (setq fa-idx
        (mod (+ fa-idx -1)
             (length fa-lst)))
  (fa-update-arg))

(let ((map function-args-mode-map))
  (define-key map (kbd "M-o") 'moo-complete)
  (define-key map (kbd "M-i") 'fa-show)
  (define-key map (kbd "M-n") 'fa-idx-cycle-down)
  (define-key map (kbd "M-h") 'fa-idx-cycle-up)
  (define-key map (kbd "M-u") 'fa-abort)
  (define-key map (kbd "M-j") 'fa-jump-maybe)
  (define-key map (kbd "C-M-j") 'moo-jump-directory)
  (define-key map (kbd "C-M-k") 'moo-jump-local))

(defvar fa-overlay nil
  "Hint overlay instance.")

(defun fa-jump-maybe ()
  "Jump to definition if `fa-show' overlay is active.
Otherwise, call `c-indent-new-comment-line' that's usually bound to \"M-j\"."
  (interactive)
  (if fa-overlay
      (fa-jump)
    (c-indent-new-comment-line)))

(defun turn-on-function-args-mode ()
  (function-args-mode 1))

;; ——— Setup ———————————————————————————————————————————————————————————————————
;;;###autoload
(defun fa-config-default ()
  "Set up default key bindings."
  (add-hook 'c++-mode-hook 'turn-on-function-args-mode)
  (add-hook 'c-mode-hook 'turn-on-function-args-mode))

;; ——— Internal variables ——————————————————————————————————————————————————————
(defvar fa-hint-pos nil
  "Point position where the hint should be (re-) displayed.")

(defvar fa-beg-pos nil
  "Position of ( after `fa-start-tracking' was invoked.")

(defvar fa-end-pos nil
  "Position of ) after `fa-start-tracking' was invoked.")

(defvar fa-superclasses (make-hash-table :test 'equal)
  "Stores superclasses tags.")

(defcustom fa-delay 2
  "Number of seconds to delay before calling `fa-show'.")

(defvar fa-timer nil
  "Timer for calling `fa-show' after idling for `fa-delay' seconds.")

(defvar fa-last-pos 1
  "Last position of call to `fa-show'.")

(defun fa-show-wrapper ()
  "Wrap around `fa-show'."
  (when (memq major-mode '(c-mode c++-mode))
    (unless (= (point) fa-last-pos)
      (ignore-errors (fa-show)
                     (setq fa-last-pos (point))))))

(defun fa-auto ()
  "Toggle automatic calls to `fa-show'."
  (interactive)
  (if fa-timer
      (progn
        (cancel-timer fa-timer)
        (setq fa-timer nil))
    (setq fa-timer (run-with-idle-timer fa-delay t #'fa-show-wrapper))))

;; ——— Interactive functions ———————————————————————————————————————————————————
(defun fa-show ()
  "Display the arguments of the closest function."
  (interactive)
  (save-excursion
    (fa-do-position)
    (setq fa-lst (fa-calculate))
    (setq fa-hint-pos (point))
    (setq fa-idx 0))
  (if (eq (length fa-lst) 0)
      (message "nothing found")
    (fa-update-arg)
    (fa-start-tracking)))

(defun fa-abort ()
  "Stop tracking the cursor and remove the overlay."
  (interactive)
  (if (overlayp fa-overlay)
      (progn
        (delete-overlay fa-overlay)
        (setq fa-overlay nil)
        (remove-hook 'after-change-functions
                     'fa-after-change)
        (remove-hook 'before-change-functions
                     'fa-before-change))
    (fa-update-arg)))

(defun fa-jump ()
  "Jump to current function of `fa-arg'."
  (interactive)
  (when (overlayp fa-overlay)
    (fa-abort)
    (ring-insert find-tag-marker-ring (point-marker))
    (let ((tag (nth 2 (car (nth fa-idx fa-lst)))))
      (let ((fname (or (car tag)
                       (save-excursion
                         (fa-do-position)
                         (backward-sexp)
                         (fa-backward-char-skip<>)
                         (moo-get-filename)))))
        (switch-to-buffer (find-file-noselect fname))
        (goto-char
         (cdr tag))))))

(defun moo-complete--candidates (&optional only-class)
  (let ((symbol (moo-ctxt-current-symbol))
        prefix candidates)
    (cond
      ((= (length symbol) 2)
       ;; either var.prefix or var->prefix
       (setq prefix (cadr symbol))
       (setq candidates (moo-complete-candidates-2 prefix (car symbol))))
      ((= (length symbol) 1)
       (setq prefix (car symbol))
       (setq candidates (moo-complete-candidates-1 prefix))
       (when only-class
         (setq candidates
               (moo-filter-tag-by-class 'variable candidates))))
      ((= (length symbol) 3)
       (setq prefix (caddr symbol))
       (setq candidates
             (or (moo-ttype->tmembers
                  (moo-complete-type-member
                   (car (moo-complete-candidates-2 (cadr symbol) (car symbol)))))
                 (semantic-analyze-possible-completions
                  (semantic-analyze-current-context (point)))))))
    (cons prefix candidates)))

(defun moo-complete (arg)
  "Complete current C++ symbol at point.
When ARG is not nil offer only variables as candidates."
  (interactive "P")
  (cl-destructuring-bind (prefix . candidates) (moo-complete--candidates arg)
    (if candidates
        (moo-handle-completion
         prefix
         (cl-delete-duplicates candidates :test #'moo-tag=))
      (semantic-ia-complete-symbol (point)))))

(defun moo-completion-at-point ()
  (let ((bnd (semantic-ctxt-current-symbol-and-bounds))
        beg end)
    (if bnd
        (progn
          (setq beg (caar (cddr bnd)))
          (setq end (cdar (cddr bnd))))
      (setq beg (point))
      (setq end (point)))
    (list
     beg end (mapcar #'semantic-tag-name
                     (cdr (moo-complete--candidates))))))

;; ——— Predicates ——————————————————————————————————————————————————————————————
(defmacro fa-and (&rest predicates)
  "Return a lambda that combines PREDICATES with `and'."
  `(lambda (x) (and ,@(mapcar (lambda (y) (list y 'x))
                              predicates))))

(defun fa-char-upcasep (c)
  "Return t if C is upper case."
  (eq c (upcase c)))

(defun moo-virtualp (tag)
  "Return t if TAG is a virtual function tag."
  (and
   (or
    (member "virtual"
            (semantic-tag-get-attribute
             tag :typemodifiers))
    (semantic-tag-get-attribute
     tag :pure-virtual-flag))
   ;; don't want distructors
   (not (semantic-tag-get-attribute
         tag :destructor-flag))))

(defun moo-typedefp (tag)
  "Return string definition of TAG if it's a typedef."
  (car (semantic-tag-get-attribute tag :typedef)))

(defun moo-namespacep (tag)
  "Return t if TAG is a namespace tag."
  (let ((attr (semantic-tag-get-attribute tag :type)))
    (and (stringp attr)
         (string= attr "namespace"))))

(defun moo-functionp (tag)
  "Return t if TAG is a function tag."
  (semantic-tag-of-class-p tag 'function))

(defun moo-variablep (tag)
  "Return t if TAG is a variable tag."
  (semantic-tag-of-class-p tag 'variable))

(defun moo-typep (tag)
  "Return t if TAG is a type tag."
  (semantic-tag-of-class-p tag 'type))

(defun moo-includep (tag)
  "Return t if TAG is an include tag."
  (semantic-tag-of-class-p tag 'include))

(defun moo-usingp (tag)
  "Return t if TAG is a using tag."
  (semantic-tag-of-class-p tag 'using))

(defun moo-constructorp (tag)
  "Return t if TAG is a constructor tag."
  (semantic-tag-get-attribute tag :constructor-flag))

(defun moo-prototype-flag-p (tag)
  "Return t if TAG is a has a prototype-flag."
  (semantic-tag-get-attribute tag :prototype-flag))

(defun moo-enump (tag)
  "Return t if TAG is an enum tag."
  (and (moo-typep tag)
       (equal "enum" (semantic-tag-get-attribute tag :type))))

;; ——— Comparers ———————————————————————————————————————————————————————————————
(defun fa-test-with (pred x1 x2)
  "Return (equal (PRED X1) (PRED X2))."
  (equal (funcall pred x1)
         (funcall pred x2)))

(defun moo-variable= (v1 v2)
  "Return t if variable tags V1 and V2 are equivalent."
  (and (moo-variablep v1)
       (moo-variablep v2)
       (fa-test-with #'car v1 v2)
       (fa-test-with (lambda (x) (semantic-tag-get-attribute x :reference)) v1 v2)
       (fa-test-with (lambda (x) (semantic-tag-get-attribute x :constant-flag)) v1 v2)
       (fa-test-with (lambda (x) (semantic-tag-get-attribute x :type)) v1 v2)))

(defun moo-function= (f1 f2)
  "Return t if function tags F1 and F2 are equivalent."
  (and (moo-functionp f1)
       (moo-functionp f2)
       (string= (car f1) (car f2))
       (fa-test-with (lambda (x) (semantic-tag-get-attribute x :typemodifiers)) f1 f2)
       (fa-test-with (lambda (x) (semantic-tag-get-attribute x :type)) f1 f2)
       (let ((a1 (semantic-tag-get-attribute f1 :arguments))
             (a2 (semantic-tag-get-attribute f2 :arguments)))
         (and (= (length a1) (length a2))
              (cl-every #'identity
                        (cl-mapcar #'moo-variable= a1 a2))))))

(defun moo-tag= (x1 x2)
  "Return t if tags X1 and X2 are equivalent."
  (cond ((moo-functionp x1)
         (moo-function= x1 x2))
        ((moo-variablep x1)
         (moo-variable= x1 x2))
        (t
         (equal (car x1) (car x2)))))

(defun moo-tag-pos= (tag1 tag2)
  "Return t if positions of TAG1 and TAG2 are equal."
  (and (fa-test-with #'moo-tget-beginning-position tag1 tag2)
       (let ((fname1 (moo-tget-filename tag1))
             (fname2 (moo-tget-filename tag2)))
         ;; normally all tags should have fname, but some don't
         (or (null fname1)
             (null fname2)
             (equal fname1 fname2)))))

;; ——— Tag getters —————————————————————————————————————————————————————————————
(defun moo-tget-filename (tag)
  "Get TAG file name."
  (or (semantic--tag-get-property tag :filename)
      (and (overlayp (car (last tag)))
           (buffer-file-name
            (overlay-buffer (car (last tag)))))))

(defun moo-tget-beginning-position (tag)
  "Get TAG beginning position."
  (let ((x (car (last tag))))
    (cond ((overlayp x)
           (overlay-start x))
          ((arrayp x)
           (aref x 0))
          (t 0))))

(defun moo-tget-constructors (tag)
  "Assuming TAG is a type tag, return its constructors."
  (ignore-errors
    (setq tag (moo-dereference-typedef tag))
    (let ((enump (moo-tget-enum-members tag)))
      (cond
        ;; enum
        (enump
         `(( ;; name
            ,(semantic-tag-name tag)
             ;; class
            function
             ;; attributes
            (:arguments
             ((,(mapconcat
                 #'car
                 enump
                 " | ")
                variable (:type ,(semantic-tag-name tag))))
             :type
             "enum")
             ;; properties
            ,(semantic-tag-properties tag)
             ;; overlay
            ,(semantic-tag-overlay tag))))
        ;; else
        (t
         (filter #'moo-constructorp
                 (moo-get-member-functions tag)))))))

(defun moo-tget-enum-members (tag)
  "Return members of enum TAG."
  (when (moo-enump tag)
    (semantic-tag-get-attribute tag :members)))

(defun moo-tget-scope (tag)
  "Return scope part of TAG."
  (caadr (cl-find-if (lambda (x) (and (listp x) (eq (car x) 'scope))) tag)))

;; ——— Tag setters —————————————————————————————————————————————————————————————
(defun moo-tput-filename (tag filename)
  "Set TAG's :filename property to FILENAME."
  (semantic--tag-put-property tag :filename filename))

(defun moo-tput-filename-to-types (types-list filename)
  "Set :filename property for members of types on TYPES-LIST to FILENAME."
  (mapcar
   (lambda (type)
     (semantic-tag-put-attribute
      type :members
      (mapcar (lambda (tag) (moo-tput-filename tag filename))
              (semantic-tag-get-attribute type :members))))
   types-list))

(defcustom fa-do-comments nil
  "When non-nil, try to add the declaration comment to the overlay.")

(defun fa-looking-back (x)
  "Forward to `looking-back' X.
Avoid byte compiler warnings."
  (looking-back x (line-beginning-position)))

(defun fa--in-comment-p ()
  "Test if point is inside a comment."
  (save-excursion
    (unless (eolp)
      (forward-char 1))
    (nth 4 (syntax-ppss))))

(defun fa--bounds-comment ()
  (comment-beginning)
  (comment-normalize-vars)
  (beginning-of-line)
  (let ((cs (comment-search-forward (line-end-position) t)))
    (when cs
      (goto-char cs)
      (skip-syntax-backward " ")
      (setq cs (point))
      (comment-forward)
      (cons cs (point)))))

(defun fa-get-comment (x)
  "Try to extract the declaration comment from X.
X is an element of `fa-lst'."
  (let* ((file-and-pos (nth 2 (car x)))
         (file (car file-and-pos))
         (pos (cdr file-and-pos)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char pos)
        (cond ((progn
                 (skip-chars-backward " \t\n")
                 (move-beginning-of-line 1)
                 (forward-char 1)
                 (fa--in-comment-p))
               (let ((bnd (fa--bounds-comment)))
                 (buffer-substring
                  (car bnd)
                  (cdr bnd)))))))))

;; ——— Pretty priting ——————————————————————————————————————————————————————————
(defun fa-fancy-string (wspace)
  "Return the string that corresponds to (nth fa-idx fa-lst).
WSPACE is the padding."
  (if (< wspace 0)
      (setq wspace 0))
  (let* ((lst (nth fa-idx fa-lst))
         (n-string
          (if (> (length fa-lst) 1)
              (format "[%d of %d] " (+ fa-idx 1) (length fa-lst))
            ""))
         (padding-length (- wspace (+ 1 (length n-string))))
         (padder (when (> padding-length 0) (make-string padding-length ?\ )))
         (str-width (+ (apply #'+ (mapcar (lambda (x) (+ (length (car x))
                                                         (length (cdr x))))
                                          (cdr lst)))
                       (length (caar lst))
                       (length (cadar lst))
                       7))
         (glue (if (> (+ wspace str-width)
                      (min fa-max-one-line-width (frame-width)))
                   ;; each arg on its own line
                   (concat fa-comma "\n" (make-string (1+ wspace) ?\ ))
                 fa-comma))
         (args (mapcar #'fa-fancy-argument
                       (cdr lst)))
         (args-current-cdr (nthcdr fa-arg args))
         comment)
    (when args-current-cdr
      (setcar args-current-cdr
              (fa-fancy-argument (nth fa-arg (cdr lst)) t)))
    (concat
     (if (or (null fa-do-comments)
             (null (setq comment (fa-get-comment lst))))
         ""
       (concat
        (mapconcat (lambda (x)
                     (concat " " padder x))
                   (split-string comment "\n")
                   "\n")
        "\n"))
     padder
     (propertize n-string 'face 'fa-face-hint-bold)
     " "
     fa-paren-open
     (and args (mapconcat 'identity args glue))
     fa-paren-close
     ;; template
     (and (caar lst) (propertize (caar lst) 'face 'fa-face-hint))
     ;; name
     (and (cadar lst) (propertize (cadar lst) 'face 'fa-face-type)))))

(defun fa-fancy-argument (cel &optional bold)
  "Return string representation for CEL.
CEL is (TYPE . NAME).
Select bold faces when BOLD is t."
  (if (= (length (cdr cel)) 0)
      (propertize (car cel) 'face
                  (if bold 'fa-face-type-bold 'fa-face-type))
    (concat
     (propertize (car cel) 'face
                 (if bold 'fa-face-type-bold 'fa-face-type))
     (propertize " " 'face 'fa-face-hint)
     (propertize (cdr cel) 'face
                 (if bold 'fa-face-hint-bold 'fa-face-hint)))))

(defun fa-tfunction->fal (tag &optional output-string)
  "Return function argument list structure for TAG.
It has the structure: (template type (file . position) arguments)."
  (unless (eq (semantic-tag-class tag) 'function)
    (error "Not a function"))
  (let ((filename (moo-tget-filename tag))
        (position (moo-tget-beginning-position tag))
        (name (semantic-tag-name tag))
        (r (semantic-tag-attributes tag))
        template-p type-p arguments-p constant-flag-p
        methodconst-flag-p typemodifiers-p constructor-flag-p
        pointer-p item)
    (while r
      (setq item (pop r))
      (case item
        (:template (setq template-p (pop r)))
        (:type (setq type-p (pop r)))
        (:arguments (setq arguments-p (pop r)))
        (:constant-flag (setq constant-flag-p (pop r)))
        (:methodconst-flag (setq methodconst-flag-p (pop r)))
        (:typemodifiers (setq typemodifiers-p (pop r)))
        (:constructor-flag (setq constructor-flag-p (pop r)))
        (:pointer (setq pointer-p (pop r)))
        ((:prototype-flag
          :parent
          :operator-flag
          :destructor-flag
          :pure-virtual-flag
          :throws
          :filename
          :depth
          :template-specifier
          :truefile
          :file)
         (pop r))
        (t (error "Unknown token %s" item))))
    (let ((argument-conses (mapcar
                            #'fa-variable->cons
                            (mapcar
                             (lambda (x) (unless (stringp (car x)) (setcar x "")) x)
                             arguments-p)))
          (template-part
           (and template-p
                (concat "template " (fa-ttemplate-specifier->str template-p) " ")))
          (return-type
           (concat (if constant-flag-p
                       "const "
                     "")
                   (if type-p
                       (propertize (fa-type->str type-p) 'face 'font-lock-type-face)
                     "?")
                   (if pointer-p " *" ""))))
      (if (null output-string)
          (cons
           ;; name and type part
           (list template-part
                 (if constructor-flag-p
                     name
                   return-type)
                 (cons filename position))
           ;; arguments part
           argument-conses)
        ;; ——— output a string instead —————————————————————————————————————————————
        (concat
         template-part
         (and typemodifiers-p
              (propertize
               (concat (mapconcat #'identity typemodifiers-p " ") " ")
               'face
               'font-lock-keyword-face))
         (if constructor-flag-p
             ""
           (if (string-match "\\*$" return-type)
               return-type
             (concat return-type " ")))
         (propertize name 'face 'font-lock-function-name-face)
         " ("
         (mapconcat (lambda (x) (concat (car x) " " (cdr x)))
                    argument-conses
                    ", ")
         ")"
         (if methodconst-flag-p (propertize " const" 'face 'font-lock-keyword-face) "")
         ";")))))

(defun fa-throw-unless-eq (x v)
  "Return t if X equals V.
Raise an error otherwise."
  (unless (eq x v)
    (error "Expected %s got %s" v x)))

(defun fa-variable->cons (tag)
  "Return (TYPE . NAME) for variable TAG.
TYPE and NAME are strings."
  (let ((name (pop tag))
        (r (progn (pop tag) (pop tag)))
        item constant-flag-p type-p
        reference-p pointer-p dereference-p default-value-p)
    (while r
      (setq item (pop r))
      (case item
        (:constant-flag (setq constant-flag-p t) (fa-throw-unless-eq (pop r) t))
        (:type (setq type-p (pop r)))
        (:reference (setq reference-p t) (fa-throw-unless-eq (pop r) 1))
        (:pointer (setq pointer-p (pop r)))
        (:dereference (setq dereference-p (pop r)))
        (:default-value (setq default-value-p (pop r)))
        ((:typemodifiers
          :function-pointer
          :arguments)
         (pop r))
        (t (error "Unknown token %s" item))))
    (cons (concat (and constant-flag-p (propertize "const " 'face 'font-lock-keyword-face))
                  (propertize (fa-type->str type-p) 'face 'font-lock-type-face))
          (concat (and reference-p "&")
                  (and pointer-p "*")
                  ;; pretty up std:: identifiers
                  (replace-regexp-in-string "^_+" "" name)
                  (and dereference-p "[]")
                  (and default-value-p (format " = %s"
                                               default-value-p))))))

(defun fa-type->str (tag)
  "Return string representation of type TAG."
  (if (stringp tag)
      tag
    (let ((name (semantic-tag-name tag))
          (template (semantic-tag-get-attribute tag :template-specifier)))
      (if template
          (let ((inside (mapconcat #'fa-type->str template ",")))
            (format
             (if (string-match ">" inside)
                 "%s<%s >"
               "%s<%s>")
             name inside))
        name))))

(defun fa-ttemplate-specifier->str (tag)
  (and tag
       (concat "<"
               (mapconcat
                (lambda (x) (replace-regexp-in-string "^_+" "" (car x)))
                tag
                ",")
               ">")))

(defun moo-tag->str (tag)
  (let* ((class (semantic-tag-class tag))
         (depth (or (semantic-tag-get-attribute tag :depth) 0))
         (str (ignore-errors
                (cl-case class
                  (function
                   (fa-tfunction->fal tag t))
                  (variable
                   (let ((type (semantic-tag-type tag))
                         (face 'font-lock-type-face)
                         pointer-depth)
                     (cond ((consp type)
                            (setq type (car type))
                            (setq face 'fa-face-type-compound))
                           ((null type)
                            (setq type "#define")
                            (setq face 'font-lock-preprocessor-face)))
                     (format "%s%s%s %s"
                             (if (and (semantic-tag-get-attribute tag :constant-flag)
                                      (not (equal type "#define")))
                                 (propertize "const " 'face 'font-lock-keyword-face)
                               "")
                             (propertize type 'face face)
                             (if (setq pointer-depth (semantic-tag-get-attribute tag :pointer))
                                 (make-string pointer-depth ?*)
                               "")
                             (propertize (car tag) 'face 'font-lock-variable-name-face))))
                  (type
                   (propertize (car tag) 'face
                               'fa-face-type-definition))
                  (label)
                  (include
                   (format "%s <%s>"
                           (propertize "#include" 'face 'font-lock-preprocessor-face)
                           (car tag)))
                  (using
                   (format "%s %s"
                           (propertize "using" 'face 'font-lock-keyword-face)
                           (car tag)))
                  (t (error "Unknown tag class: %s" class))))))
    (when str
      (concat
       (if depth
           (make-string (* depth 2) ?\ )
         "")
       str))))

;; ——— Misc non-pure ———————————————————————————————————————————————————————————
(defun moo-propose-virtual ()
  "Call `moo-propose' for virtual functions."
  (interactive)
  (moo-propose (fa-and moo-functionp moo-virtualp)))

(defun moo-propose-override ()
  "Call `moo-propose' for all functions."
  (interactive)
  (moo-propose #'moo-functionp))

(defun moo-propose-variables ()
  "Call `moo-propose' for all variables."
  (interactive)
  (moo-propose #'moo-variablep))

(defun moo-format-tag-line (str file)
  (let ((width (window-width))
        (col (max 70 (- (window-width) 20))))
    (when (or (null file)
              (not (file-exists-p file)))
      (error "Bad tag: %s" str))
    (when (> (length str) col)
      (setq str
            (concat (substring str 0 (- col 4))
                    " ...")))
    (format (format "%%s%% %ds" (- width
                                   (length str)))
            str
            (file-name-nondirectory file))))

(defvar moo-jump-local-cache (make-hash-table :test 'equal))

(defvar moo-jump-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-j")
      (lambda ()
        (interactive)
        (ivy-exit-with-action
         (lambda (_)
           (moo-jump-directory nil ivy-text)))))
    (define-key map (kbd "C-M-k")
      (lambda ()
        (interactive)
        (ivy-call)
        (let ((buf (with-ivy-window (current-buffer))))
          (ivy-exit-with-action
           `(lambda (_)
              (switch-to-buffer ,buf)
              (moo-jump-local ivy-text))))))
    map))

(defun moo-jump-local (&optional initial-input)
  "Jump to a tag in the current file."
  (interactive)
  (ivy-read "tag: "
            (delq nil
                  (mapcar
                   (lambda (x)
                     (let ((s (moo-tag->str x)))
                       (when s
                         (cons s x))))
                   (moo-flatten-namepaces
                    (semantic-fetch-tags))))
            :initial-input initial-input
            :action #'moo-action-jump
            :keymap moo-jump-keymap
            :preselect (car (semantic-current-tag))))

(defun moo-jump-directory (arg &optional initial-input)
  "Select a tag to jump to from tags defined in current directory.
When ARG is non-nil, regenerate tags."
  (interactive "P")
  (let* ((file-list (cl-remove-if
                     (lambda (x)
                       (string-match "^\\.#" x))
                     (append (file-expand-wildcards "*.cc")
                             (file-expand-wildcards "*.c")
                             (file-expand-wildcards "*.hh")
                             (file-expand-wildcards "*.h")
                             (file-expand-wildcards "*.hpp"))))
         (ready-tags
          (or (and (null arg) (gethash file-list moo-jump-local-cache))
              (let ((tags (sd-fetch-tags file-list)))
                (when (memq major-mode '(c++-mode c-mode))
                  (setq tags
                        (delq nil
                              (mapcar
                               (lambda (x)
                                 (let ((s (moo-tag->str x)))
                                   (when s
                                     (cons
                                      (moo-format-tag-line
                                       s (semantic-tag-get-attribute x :truefile))
                                      x))))
                               (moo-flatten-namepaces tags)))))
                (puthash file-list tags moo-jump-local-cache)
                tags)))
         (preselect (car (semantic-current-tag)))
         (preselect (and preselect
                         (if (memq moo-select-method '(helm helm-fuzzy))
                             (regexp-quote preselect)
                           preselect))))
    (moo-select-candidate
     ready-tags
     #'moo-action-jump
     preselect
     initial-input)))

(defun moo-reset-superclasses-cache ()
  "Reset `fa-superclasses'."
  (interactive)
  (setq fa-superclasses (make-hash-table :test 'equal)))

(defun fa-do-position ()
  "Position the cursor at the `(', which is logically closest."
  (cond
    ((looking-at "("))
    ((fa-looking-back "(")
     (backward-char))
    ((fa-looking-back "^\\([^(\n]*\\)([^(\n]*")
     (re-search-backward "("))
    ((looking-at "[^\n]*([^\n]*$")
     (re-search-forward "(")
     (backward-char))
    (t
     (up-list)
     (backward-list)))
  (unless (fa-looking-back "^[ \t]*")
    (while (fa-looking-back " ")
      (backward-char)))
  (point))

(defun fa-start-tracking ()
  "Call `fa-after-change' after each change to buffer."
  (setq fa-beg-pos (save-excursion (re-search-backward "(" nil t) (point)))
  (setq fa-end-pos (save-excursion (re-search-forward ")" nil t) (- (point) 1)))
  (add-hook 'after-change-functions 'fa-after-change))

(defun fa-update-arg (&optional arg)
  "Update `fa-arg' if it needs to be updated."
  (let ((argn (semantic-ctxt-current-argument)))
    (unless (and
             (not
              (cond ((numberp argn)
                     (when (and (>= argn 1)
                                (< argn (length (nth fa-idx fa-lst))))
                       (if (eq fa-arg (1- argn))
                           nil
                         (setq fa-arg (1- argn)))))
                    ((null argn)
                     (setq fa-arg 0)
                     nil)
                    (t
                     (fa-abort))))
             arg)
      (fa-do-show))))

(defun fa-do-show ()
  "Show function arguments hint."
  (save-excursion
    (goto-char fa-hint-pos)
    (let ((str (fa-fancy-string (- (point) (line-beginning-position)))))
      (setq str
            (if fa-hint-position-below
                (progn
                  (forward-line)
                  (concat str "\n" (make-string 1 (char-after))))
              (forward-line -1)
              (end-of-line)
              (concat "\n" str (make-string 1 (char-after)))))
      (font-lock-unfontify-region (point) (+ (point) 1))
      (if fa-overlay
          (progn
            (move-overlay fa-overlay (point) (+ (point) 1))
            (overlay-put fa-overlay 'invisible nil))
        (setq fa-overlay (make-overlay (point) (+ (point) 1)))
        (overlay-put fa-overlay 'priority 9999))
      (overlay-put fa-overlay 'display str)
      (overlay-put fa-overlay 'after-string ""))))

(defvar ac-prefix-overlay)

(defun fa-after-change (beg end len)
  ;; abort if out of range
  (if (or (< beg fa-beg-pos)
          (> beg fa-end-pos))
      ;; work around for when auto-complete-mode is active
      (unless (and ;; (bound-and-true-p auto-complete-mode)
               ac-prefix-overlay
               ;; (>= (- end beg) 1)
               )
        (fa-abort))
    (cond ((eq len 0)                   ; insertion
           (cl-incf fa-end-pos (- end beg)))
          ((eq beg end)                 ; deletion
           (decf fa-end-pos len)))
    (fa-update-arg t)))

(defun fa-backward-char-skip<> (&optional arg)
  "Move point backward until [A-Za-z_0-9] is encountered.
Skips anything between matching <...>.
Reverse direction when ARG is not nil."
  (let ((dir (if arg -1 1))
        (char-inc (if arg ?< ?>))
        (char-dec (if arg ?> ?<)))
    (backward-char dir)
    ;; TODO: look into `c-backward-<>-arglist'
    (while (not (looking-at "[A-Za-z_0-9]"))
      (if (eq (char-after) char-inc)
          (let ((n 1)
                (bound (- (point) 400)))
            (while (and (> n 0) (> (point) bound))
              (backward-char dir)
              (cond
                ((= (char-after) char-inc)
                 (cl-incf n))
                ((= (char-after) char-dec)
                 (cl-decf n)))))
        (backward-char dir)))))

(defun moo-erase-string (str)
  "Ensure `looking-back' STR and erase it.
`case-fold-search' is set to nil."
  (let ((case-fold-search nil))
    (if (fa-looking-back str)
        (delete-region (match-beginning 0) (match-end 0))
      (error "Can't erase %s" str))))

(defun moo-handle-completion (prefix candidates &optional params)
  "Select tag that starting with PREFIX from CANDIDATES.
PARAMS are passed further to `moo-action-insert'."
  (cond
    ((null candidates)
     (message "there is no completions, only Zuul"))
    ;; either one candidate or multiple with same name:
    ((or (= 1 (length candidates))
         (cl-reduce (lambda (x1 x2) (and x1 (string= (car x1) (car x2)) x1)) candidates))
     (moo-action-insert (car candidates) params prefix))
    ;; multiple candidates with different names
    (t
     (moo-select-candidate
      (delq nil
            (mapcar 'moo-tag->cons candidates))
      (lambda (x) (moo-action-insert x params prefix))))))

(defun moo-tag->cons (tag)
  "Return for TAG a cons (TAG . STR).
STR is the result of `moo-tag->str' on TAG,
NAME is the TAG name."
  (let ((tag-name (moo-tag->str tag)))
    (when tag-name
      (cons tag-name tag))))

(defun moo-action-insert (candidate formatter &optional prefix)
  "Insert tag CANDIDATE.
When PREFIX is not nil, erase it before inserting."
  (when prefix
    (moo-erase-string prefix))
  (cond ((eq formatter 'full-tag)
         (insert (moo-tag->str candidate)))
        ((moo-functionp candidate)
         (insert (semantic-tag-name candidate))
         (cl-case fa-insert-method
           (name)
           (name-space-parens
            (insert " ()")
            (backward-char 1))
           (name-and-parens
            (insert "()")
            (backward-char 1))
           (name-and-parens-and-hint
            (insert "()")
            (backward-char 1)
            (semantic-fetch-tags)
            (fa-show))))
        ((stringp candidate)
         (insert candidate))
        ((and (consp candidate)
              (stringp (car candidate)))
         (insert (car candidate)))
        (t
         (error "Unexpected"))))

(defvar ivy-height)

(defun moo-select-candidate (candidates action &optional preselect initial-input)
  (cond ((eq moo-select-method 'display-completion-list)
         (with-output-to-temp-buffer "*moo-jump*"
           (display-completion-list
            (all-completions "" candidates))))

        ((eq moo-select-method 'ivy)
         (require 'ivy)
         (let ((ivy-height 15))
           (ivy-read "tag: " candidates
                     :preselect preselect
                     :action action
                     :require-match t
                     :sort nil
                     :keymap moo-jump-keymap
                     :initial-input initial-input
                     :caller 'moo-select-candidate)))

        ((prog1 (eq moo-select-method 'helm)
           (require 'helm)
           (require 'helm-help)
           (require 'helm-source))
         (helm :sources
               `((name . "semantic tags")
                 (candidates . ,candidates)
                 (action . ,action))
               :preselect preselect
               :buffer "*moo-jump*"))

        ((eq moo-select-method 'helm-fuzzy)
         (helm :sources
               (helm-build-sync-source "semantic tags"
                 :candidates candidates
                 :fuzzy-match t
                 :action action)
               :preselect preselect
               :buffer "*moo-jump*"))
        (t
         (error "Bad `moo-select-method': %S" moo-select-method))))

(when (version< emacs-version "25.1")
  (eval-after-load 'etags
    '(add-to-list 'byte-compile-not-obsolete-vars 'find-tag-marker-ring)))

(defun moo-action-jump (tag)
  (setq tag (cdr tag))
  (with-selected-window (cl-case moo-select-method
                          (ivy
                           (ivy-state-window ivy-last))
                          ((helm helm-fuzzy)
                           (selected-window))
                          (t
                           (selected-window)))
    (if (semantic-tag-p tag)
        (progn
          (let ((file-name (semantic-tag-get-attribute tag :truefile))
                (ov (semantic-tag-overlay tag)))
            (ring-insert
             find-tag-marker-ring (point-marker))
            (when file-name
              (find-file file-name))
            (goto-char (if (overlayp ov)
                           (overlay-start ov)
                         (aref ov 0)))))
      (error "Unexpected tag: %S" tag))))

(defun moo-propose (pred)
  "Display a list of current class members that satisfy PRED."
  (let ((stype (moo-c++-class-name)))
    (when stype
      (let ((ttype (moo-tag-at-point stype #'moo-typep)))
        (when ttype
          (let ((members (filter pred
                                 (moo-ttype->tmembers ttype))))
            (setq members (sort members (lambda (a b) (string< (car a) (car b)))))
            (moo-handle-completion "" members 'full-tag)))))))

;; ——— Internals ———————————————————————————————————————————————————————————————
(defalias 'filter 'cl-remove-if-not)

(defun moo-tag-at-point (str &optional predicate)
  "Find a tag with name STR that's visible near point.
Optional PREDICATE is used to improve uniqueness of returned tag."
  (let ((class-name (moo-c++-class-name)))
    (moo-tag-at-point-generic
     str
     `(lambda (x)
        (and (not (semantic-tag-get-attribute x :prototype))
             ,(if predicate `(,predicate x) t)
             (or (not (moo-variablep x))
                 (equal ,class-name
                        (save-excursion
                          (goto-char (moo-tget-beginning-position x))
                          (moo-c++-class-name)))))))))

(defun moo-type-tag-at-point (str)
  (moo-tag-at-point-generic
   str
   (lambda (x) (and (not (semantic-tag-get-attribute x :prototype))
                    (moo-typep x)))))

(defun moo-tag-at-point-generic (str predicate)
  "Find a tag near point with name STR that satisfies PREDICATE."
  (let* ((matches (moo-desperately-find-sname str))
         (filtered-matches (filter predicate matches)))
    (cond
      ;; fall back to semantic
      ((null filtered-matches)
       (save-excursion
         (search-backward str)
         (semantic-analyze-interesting-tag
          (semantic-analyze-current-context (point)))))
      ((eq 1 (length filtered-matches))
       (car filtered-matches))
      ((cl-every #'moo-namespacep matches)
       `(,str type
              (:type
               namespace
               :members
               (,@(apply #'append
                         (mapcar #'moo-ttype->tmembers matches))))))
      ((cl-every #'moo-typep matches)
       `(,str type
              (:members
               (,@(apply #'append
                         (mapcar #'moo-ttype->tmembers matches))))))
      ((cl-every #'moo-variablep matches)
       (car matches))
      (t
       (error "Multiple definitions for %s" str)))))

(defun moo-complete-candidates-2 (prefix var-name)
  (let* ((var-used-as-pointer-p (fa-looking-back "->\\(?:[A-Za-z][A-Za-z_0-9]*\\)?"))
         (var-used-as-classvar-p (fa-looking-back "\\.\\(?:[A-Za-z][A-Za-z_0-9]*\\)?"))
         (var-tag (if (fa-looking-back (concat "::" prefix))
                      (save-excursion
                        (re-search-backward prefix)
                        (backward-char 2)
                        (fa-backward-char-skip<>)
                        (moo-ctxt-type))
                    ;; TODO: maybe just call `moo-ctxt-type' here
                    (moo-tag-at-point var-name
                                      (when var-used-as-classvar-p
                                        'moo-variablep))))
         (var-pointer-p (semantic-tag-get-attribute var-tag :pointer))
         (tmembers (moo-ttype->tmembers
                    (cond
                      (var-used-as-classvar-p
                       ;; semantic may think it's a function
                       (let ((tag-type (moo-complete-type-member var-tag)))
                         (when (eq tag-type t)
                           (setq var-tag (moo-ctxt-type))
                           (setq tag-type (semantic-tag-get-attribute var-tag :type)))
                         (if (moo-ttype->tmembers tag-type)
                             tag-type
                           ;; this works sometimes
                           (moo-sname->tag var-name))))
                      ;; Type::member
                      ((fa-looking-back "::\\(?:[A-Za-z][A-Za-z_0-9]*\\)?")
                       (if (moo-functionp var-tag)
                           (moo-sname->tag var-name)
                         var-tag))
                      (var-used-as-pointer-p
                       ;; is it a usual pointer or a smart pointer?
                       (if var-pointer-p
                           (moo-complete-type-member var-tag)
                         (let ((type-template (semantic-tag-get-attribute
                                               (semantic-tag-get-attribute var-tag :type)
                                               :template-specifier)))
                           ;; assume that the first template parameter is the relevant one
                           ;; (normally, there should be only one anyway)
                           (moo-stype->tag (caar type-template)))))
                      ;; otherwise just get its type
                      (t
                       (cond ((moo-typep var-tag)
                              var-tag)
                             ((moo-variablep var-tag)
                              (moo-tvar->ttype var-tag))
                             (t (error "Unexpected")))))))
         (pred (cond
                 ((= (length prefix) 0)
                  #'identity)
                 ;; wildcard
                 ((eq ?_ (aref prefix 0))
                  `(lambda(x) (cl-search ,(substring prefix 1) (downcase (car x)))))
                 ;; case-sensitive
                 ((fa-char-upcasep (aref prefix 0))
                  (lambda(x) (eq 0 (cl-search prefix (car x)))))
                 ;; case-insensitive
                 (t
                  `(lambda(x) (eq 0 (cl-search ,(downcase prefix) (downcase (car x)))))))))
    (filter pred tmembers)))

(defun moo-complete-candidates-1 (prefix)
  (let ((candidates-1 (semantic-analyze-possible-completions
                       (semantic-analyze-current-context (point))))
        (candidates-2 (and (featurep 'semantic/db)
                           (semanticdb-minor-mode-p)
                           (ignore-errors
                             (semanticdb-fast-strip-find-results
                              (semanticdb-deep-find-tags-for-completion prefix))))))
    (cl-delete-duplicates (append candidates-1 candidates-2) :test #'moo-tag=)))

;; this is similar to stype->tag
;; I should refactor this
(defun moo-complete-type-member (var-tag)
  (when var-tag
    (let ((type-name (semantic-tag-get-attribute var-tag :type)))
      (cond
        ;; this happens sometimes
        ((equal type-name "class")
         var-tag)
        ;; this as well
        ((or (equal type-name "namespace") (eq type-name 'namespace))
         (moo-sname->tag (car var-tag)))
        (t
         (when (listp type-name)
           (setq type-name (car type-name)))
         (or (moo-stype->tag type-name)
             (moo-type-tag-at-point type-name)))))))

(defun moo-ctxt-current-symbol ()
  (or (semantic-ctxt-current-symbol)
      (save-excursion
        (fa-backward-char-skip<>)
        (semantic-ctxt-current-symbol))))

(defun fa-calculate ()
  "Return current function (or functions in case of overloading) in the form:
 ((name-1 arg-1 arg-2 ...) (name-2 arg-1 arg2 ...) ...)."
  (let* ((function (moo-ctxt-current-symbol))
         (result
          (save-excursion
            (cond
              ;; semantic didn't recognize anything
              ;; try a class temp initialization
              ((= 0 (length function))
               (fa-backward-char-skip<>)
               (moo-tget-constructors (moo-ctxt-type)))
              ;; semantic gave just a list with one string - a variable name
              ((= 1 (length function))
               (search-backward (car function))
               (let ((ctxt-type (moo-ctxt-type)))
                 (cond
                   ;; happens sometimes
                   ((stringp ctxt-type)
                    (if (fa-looking-back "\\()[ \n\t]*:[^;()]*\\)\\|,[^;()]*")
                        (moo-tget-constructors (moo-sname->tag (car function)))
                      (fa-backward-char-skip<>)
                      (moo-tget-constructors (moo-ctxt-type))))
                   ((and (semantic-tag-p ctxt-type)
                         (cond
                           ;; variable init inside constructor
                           ((and (moo-variablep ctxt-type)
                                 (fa-looking-back ":[^;]*"))
                            (or (filter #'moo-constructorp
                                        (apply #'append
                                               (delq nil
                                                     (mapcar
                                                      #'moo-ttype->tmembers
                                                      (moo-desperately-find-sname
                                                       (car (semantic-tag-type ctxt-type)))))))
                                (moo-tget-constructors
                                 (moo-sname->tag (car function)))))
                           ;; parent class init inside constructor
                           ;; or constructor as part of expression
                           ((moo-typep ctxt-type)
                            (or (moo-tget-constructors ctxt-type)
                                (moo-tget-constructors
                                 (moo-tvar->ttype (car (moo-desperately-find-sname (car function)))))
                                (moo-tget-constructors
                                 (moo-tag-at-point (car ctxt-type)))))
                           ;; global function call
                           ((moo-functionp ctxt-type)
                            (if (moo-prototype-flag-p ctxt-type)
                                (list ctxt-type)
                              ;; should remove duplicates here
                              (append (list ctxt-type)
                                      (moo-desperately-find-sname (car function))))))))
                   ;; global function invocation
                   ((fa-looking-back "\\(:?}\\|else\\|;\\|{\\|\\(:?//.*\\)\\)[ \t\n]*")
                    (cl-mapcan #'fa-process-tag-according-to-class
                               (moo-desperately-find-sname (car function))))
                   ;; try to match a variable with a constructor declaration:
                   ;; move to the type
                   (t
                    (fa-backward-char-skip<>)
                    (let* ((ctxt-type (moo-ctxt-type)))
                      (moo-tget-constructors (moo-dereference-typedef ctxt-type)))))))
              ((= 2 (length function))
               (re-search-backward ".\\(?:\\.\\|->\\|::\\)")
               (cond
                 ;; array or map of objects or operator []
                 ;; function can be called with either . or -> or operator ()
                 ((looking-at "]")
                  (search-forward "(")
                  (backward-char 2)
                  (moo-complete-candidates-2 (cadr function) (car function)))

                 ((looking-at ">")
                  (forward-char)
                  (fa-backward-char-skip<>)))

               (let* ((ctxt-type (moo-ctxt-type))
                      (ctype (semantic-tag-get-attribute ctxt-type :type)))
                 (fa-backward-char-skip<> -1)
                 (cond
                   ((fa-looking-back "::")
                    (cl-delete-duplicates
                     (append
                      (fa-process (cadr function)
                                  ctxt-type)
                      (cl-mapcan
                       `(lambda (tag)
                          (filter (lambda (tag) (eq (cadr tag) 'function))
                                  (moo-filter-tag-by-name ,(cadr function) (moo-ttype->tmembers tag))))
                       (moo-desperately-find-sname (car function))))
                     :test #'moo-tag-pos=))
                   ;; smart pointer?
                   ((and (fa-looking-back "->") (not (semantic-tag-get-attribute ctxt-type :pointer)))
                    (let* ((type (semantic-tag-get-attribute ctxt-type :type))
                           (type-template
                            (semantic-tag-get-attribute (if (equal type "class") ctxt-type type)
                                                        :template-specifier)))
                      (fa-process (cadr function)
                                  (moo-stype->tag (caar type-template)))))
                   ;; rest
                   (t
                    ;; get variable's type
                    (when (moo-variablep ctxt-type)
                      (setq ctxt-type (moo-stype->tag (car ctype))))
                    (fa-process (cadr function) ctxt-type))))))))
         (result (cl-remove-duplicates result :test #'moo-function=)))
    (or (mapcar #'fa-tfunction->fal result)
        ;; fall back to semantic
        (let ((fntag (semantic-analyze-find-tag-sequence
                      function (semantic-calculate-scope (point)))))
          (unless (stringp (setq fntag (car (last fntag))))
            (list (fa-tfunction->fal fntag)))))))

(defun fa-process-tag-according-to-class (tag)
  "Coerse TAG to list of functions with same name."
  (cond ((moo-functionp tag)
         (list tag))
        ((moo-typep tag)
         (moo-tget-constructors tag))
        ((moo-variablep tag)
         nil)
        (t nil)))

(defun fa-process (str ttype)
  "Get all functions with name STR from TTYPE.
This includes the constructors of types with name STR."
  (let ( ;; TODO: this fails for namespaces such as std::
        (filename (moo-tget-filename ttype)))
    (mapcar (lambda (tag) (moo-tput-filename tag filename))
            (let ((candidates (moo-filter-tag-by-name
                               str
                               (moo-ttype->tmembers ttype))))
              (append
               (moo-filter-tag-by-class 'function candidates)
               (apply #'append
                      (mapcar
                       #'moo-tget-constructors
                       (moo-filter-tag-by-class 'type candidates))))))))

(defun moo-filter-tag-by-name (sname members)
  (filter (lambda (tag) (string= (car tag) sname))
          members))

(defun moo-filter-tag-by-class (class members)
  (filter (lambda (tag) (semantic-tag-of-class-p tag class))
          members))

(defun moo-ctxt-type ()
  (save-excursion
    (when (moo-variable-definition-p)
      (fa-backward-char-skip<>))
    (let ((ctxt (semantic-analyze-current-context (point))))
      (if (null ctxt)
          (error "Nothing under cursor")
        ;; (setq ctxt (car (oref ctxt prefix)))
        (setq ctxt (car (reverse (oref ctxt prefix))))
        (ignore-errors
          (cond ((stringp ctxt)
                 (or (moo-tag-at-point ctxt) ctxt))
                ;; check if variable constructor initialization is mistaken
                ;; for function prototype definition:
                ;; ((and (moo-functionp ctxt)
                ;;       (moo-prototype-flag-p ctxt)
                ;;       (let ((arg1 (caar (semantic-tag-get-attribute ctxt :arguments))))
                ;;         (and arg1 (stringp arg1) (string= arg1 ""))))
                ;;  (or (ignore-errors
                ;;        (moo-tag-at-point (car (semantic-tag-get-attribute ctxt :type))))
                ;;      (semantic-tag-get-attribute ctxt :type)))
                (t
                 ctxt)))))))

(defun moo-stype->tag (str)
  (let ((candidates
         (or (ignore-errors
               (semantic-analyze-find-tag-sequence
                (list str) (semantic-calculate-scope (point)) 'prefixtypes 'unfindable))
             (filter
              (lambda (x) (and (moo-typep x) (semantic-tag-get-attribute x :members)))
              (moo-desperately-find-sname str)))))
    (cond ((= 0 (length candidates)))
          ((= 1 (length candidates))
           (car candidates))
          (t (error "`moo-stype->tag': too many candidates")))))

(defun moo-get-member-functions (ttype)
  (cond
    ((moo-typep ttype)
     (filter #'moo-functionp
             (moo-ttype->tmembers ttype)))
    ((moo-variablep ttype)
     (moo-get-member-functions
      (moo-stype->tag
       (car (semantic-tag-get-attribute ttype :type)))))))

(defun moo-dereference-typedef (tag)
  "When TAG is a typedef, dereference it.
Returns TAG if it's not a typedef."
  (let ((typedef-p (moo-typedefp tag)))
    (if (null typedef-p)
        tag
      (let ((defs (filter `(lambda (x) (and (eq (cadr x) 'type)
                                            (string= (car x) ,typedef-p)))
                          (semantic-tag-get-attribute
                           (moo-tget-scope tag) :members))))
        (case (length defs)
          (1 (car defs))
          (0 (cons (car tag) (cdr typedef-p)))
          (t (error "Typedef has multiple definitions")))))))

(defun moo-navigate-members (tag)
  (let ((typedef (semantic-tag-get-attribute tag :typedef)))
    (when typedef
      (setq tag
            (or
             (moo-sname->tag (car typedef))
             typedef)))
    (semantic-tag-get-attribute tag :members)))

(defun moo-ttype->tmembers (ttype)
  (let* ((own-members
          (cl-delete-if (lambda (tag) (and (stringp (car tag))
                                           (or (string= (car tag) "public")
                                               (string= (car tag) "private"))))
                        (moo-navigate-members ttype)))
         (own-members
          (apply #'append
                 own-members
                 ;; members of enums join the containing type members
                 (mapcar #'moo-tget-enum-members own-members)))
         (inherited-members
          (ignore-errors
            (mapcar (lambda (parent-tag)
                      ;; parent-tag's only useful part is the name
                      (let ((parent-name (car parent-tag)))
                        (setq parent-tag
                              (or (gethash parent-name fa-superclasses)
                                  (puthash parent-name (moo-stype->tag parent-name) fa-superclasses)))
                        (when (eq parent-tag t)
                          (setq parent-tag nil)))
                      ;; don't inherit constructors
                      (cl-delete-if #'moo-constructorp
                                    (moo-ttype->tmembers parent-tag)))
                    (semantic-tag-get-attribute ttype :superclasses))))
         (cands (apply #'append (cons own-members inherited-members))))
    (cl-remove-duplicates cands :test #'moo-function=)))

(defun moo-sname->tag (str-name)
  (let ((var-tag (semantic-analyze-select-best-tag
                  (semantic-scope-find str-name nil))))
    (or (and var-tag
             (ignore-errors
               (semantic-analyze-tag-type
                var-tag
                (semantic-calculate-scope (point)))))
        (car (moo-desperately-find-sname str-name)))))

(defun moo-tvar->ttype (var-tag)
  (let ((type-attr (semantic-tag-get-attribute var-tag :type)))
    (when (consp type-attr)
      (let* ((var-stype (car type-attr))
             (type-tag (moo-stype->tag var-stype)))
        (moo-dereference-typedef type-tag)))))

(defun moo-get-tag-by-name (sname tlist)
  (cl-mapcan
   (lambda (tag)
     (if (string= (car tag) sname)
         (list tag)
       (and (or (moo-namespacep tag) (moo-typep tag))
            (moo-get-tag-by-name
             sname
             (semantic-tag-get-attribute tag :members)))))
   tlist))

(defun moo-desperately-find-sname (stag)
  (let* ((file-tags (semantic-fetch-tags))
         (own-tags (moo-get-tag-by-name stag file-tags))
         (include-tags (filter (lambda (tag) (moo-includep tag))
                               file-tags))
         (include-filenames (delq nil (mapcar #'semantic-dependency-tag-file include-tags))))
    (apply #'append
           (apply #'append
                  (mapcar (lambda (filename)
                            (moo-tput-filename-to-types
                             (moo-find-sname-in-tags
                              stag
                              (semantic-file-tag-table filename))
                             filename))
                          include-filenames))
           (list own-tags))))

(defun moo-namespace-reduce (func tags)
  "Reduce with two-argument function FUNC the forest TAGS."
  (cl-labels ((namespace-reduce
                  (func tags out depth)
                (dolist (tag tags)
                  (cond ((and (not moo-do-includes)
                              (or (moo-includep tag) (moo-usingp tag)))
                         ;; skip
                         )

                        ((or (moo-namespacep tag) (moo-typep tag))
                         (setq out
                               (namespace-reduce
                                func
                                (let ((truefile (semantic-tag-get-attribute tag :truefile)))
                                  (mapcar (lambda (x)
                                            (semantic-tag-put-attribute x :truefile truefile))
                                          (semantic-tag-get-attribute tag :members)))
                                (funcall func out tag depth)
                                (1+ depth))))

                        (t (setq out (funcall func out tag depth)))))
                out))
    (nreverse (namespace-reduce func tags nil 0))))

(defun moo-find-sname-in-tags (stag tags)
  "Find tags named STAG in forest TAGS."
  (moo-namespace-reduce
   (lambda (x y _depth) (if (string= (car y) stag) (push y x) x))
   tags))

(defun moo-flatten-namepaces (tags)
  "Traverse the namespace forest TAGS and return the leafs."
  (moo-namespace-reduce
   (lambda (x y depth)
     (push (semantic-tag-put-attribute y :depth depth) x))
   tags))

(defun moo-c++-class-name ()
  "Return current class name."
  (car (moo-c++-class-name-and-template)))

(defun moo-c++-class-template ()
  "Return the template of current class."
  (cdr (moo-c++-class-name-and-template)))

(defvar moo-c++-braces-table
  (let ((table (make-char-table 'syntax-table nil)))
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)
    table))

(defun moo-c++-match-constructor-impl ()
  (save-excursion
    (let ((pt (point)))
      (when (re-search-backward "template\\s-+<\\([^>]*\\)>[\n\t ]*\\([A-Za-z_0-9]+\\)\\(<[^>]*>\\)?::\\2" nil t)
        (unless (string-match "{" (buffer-substring-no-properties (point) pt))
          (cons (match-string-no-properties 2)
                (match-string-no-properties 1)))))))

(defun moo-c++-class-name-and-template ()
  "Return currrent class name and template as a cons."
  (or (moo-c++-match-constructor-impl)
      (ignore-errors
        (save-excursion
          (let (name template defun-start)
            ;; step out of the current block
            (with-syntax-table moo-c++-braces-table
              (up-list)
              (backward-list))
            ;; TODO take care of nested classes
            (if (fa-looking-back
                 "\\(?:class\\|struct\\) \\([A-Za-z][A-Z_a-z0-9]*\\)[: \t\n]+[^{;]*?")
                (progn
                  (goto-char (match-beginning 0))
                  (setq name (match-string-no-properties 1))
                  ;; try to match the template as well
                  (when (fa-looking-back ">[\n \t]*")
                    (let ((end (progn (goto-char (match-beginning 0)) (point)))
                          (beg (ignore-errors (forward-char) (backward-list) (point))))
                      (when end
                        (setq template (buffer-substring-no-properties (1+ beg) end))))))
              ;; we're not in class, but in a function
              (beginning-of-defun)
              (setq defun-start (point))
              (when (looking-at "template +<")
                (goto-char (1- (match-end 0)))
                (setq template (substring (moo-list-at-point) 1 -1))
                (forward-list))
              (re-search-forward " \\([A-Za-z][A-Z_a-z0-9]*\\)\\(\\(?:<[^>]*>\\)?\\)::")
              (setq name (match-string-no-properties 1))
              ;; check if there's a mess up
              (when (re-search-backward "{" defun-start t)
                (setq name nil)))
            (cons name template))))))

(defun moo-list-at-point ()
  "Return any list at point.
At least what the syntax thinks is a list."
  (forward-list)
  (let ((end (point)))
    (backward-list)
    (buffer-substring-no-properties (point) end)))

(defconst moo-c++-var-name-regex "[a-zA-Z_][a-zA-Z0-9_]*")

(defun moo-variable-definition-p ()
  "Return t if \"Type |var()\"."
  (save-excursion
    (back-to-indentation)
    (let ((str (buffer-substring-no-properties
                (point)
                (line-end-position))))
      (catch 'br
        (while (setq str (moo-unprefix-qualifier str))
          (when (string-match "^ +" str)
            (throw 'br
              (string-match
               (format "\\s-+%s("
                       moo-c++-var-name-regex)
               str))))))))

(defun moo-unprefix-qualifier (str)
  "Return STR without Type<...>:: qualifier."
  (let ((out ""))
    (when (string-match moo-c++-var-name-regex str)
      (setq out (substring str (match-end 0))))
    (cond ((string-match "::" out)
           (substring out 2))
          ((string-match "<" out)
           (moo-unprefix-template out)))))

(defun moo-unprefix-template (str)
  "Return STR without <...> prefix."
  (if (= ?< (aref str 0))
      (let ((N (length str))
            (n 1)
            (i 0))
        (catch 'br
          (while (and (> n 0) (< (incf i) N))
            (cl-case (aref str i)
              (?< (incf n))
              (?> (when (<= (decf n) 0)
                    (throw 'br (substring str (incf i)))))))
          (error "Unbalanced string: %s" str)))
    str))

(defun moo-get-filename ()
  "Get filename of tag at point."
  (let* ((ctxt (semantic-analyze-current-context))
         (pf (and ctxt (reverse (oref ctxt prefix))))
         (first (car pf)))
    (and (or (semantic-tag-with-position-p first)
             (semantic-tag-get-attribute first :line))
         (semantic-tag-file-name first))))

(defvar aya-current)

(defun moo-doxygen ()
  "Generate a doxygen yasnippet and expand it with `aya-expand'.
The point should be on the top-level function name."
  (interactive)
  (move-beginning-of-line nil)
  (let ((tag (semantic-current-tag)))
    (unless (semantic-tag-of-class-p tag 'function)
      (error "Expected function, got %S" tag))
    (let* ((attrs (semantic-tag-attributes tag))
           (args (plist-get attrs :arguments))
           (ord 1))
      (setq aya-current
            (format
             "/**
* $1
*
%s
* @return $%d
*/
"
             (mapconcat
              (lambda (x)
                (format "* @param %s $%d"
                        (car x) (incf ord)))
              args
              "\n")
             (incf ord)))
      (aya-expand))))

(provide 'function-args)

;;; Local Variables:
;;; outline-regexp: ";; ———"
;;; End:

;;; function-args.el ends here
