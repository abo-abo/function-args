;;; function-args.el --- C++ completion for GNU Emacs

;; Copyright (C) 2013  Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/function-args
;; Version: 0.4

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


(require 'cl-lib)
(eval-when-compile
  (require 'cl)
  (defvar c++-mode-map)
  (defvar ac-menu)
  (defvar auto-complete-mode))
(require 'semantic/ia)
(require 'semantic/db-find)
(semantic-mode 1)

;; ——— Setup —————————————————————————————————————————————————————————————————————————
(defun fa-config-default ()
  (add-hook
   'c++-mode-hook
   (lambda()
     (define-key c++-mode-map (kbd "M-o") 'moo-complete)
     (define-key c++-mode-map (kbd "M-i") 'fa-show)
     (define-key c++-mode-map (kbd "M-n") (fa-idx-cycle 1))
     (define-key c++-mode-map (kbd "M-h") (fa-idx-cycle -1))
     (define-key c++-mode-map (kbd "M-u") 'fa-abort)
     (define-key c++-mode-map (kbd "M-j") `(lambda()(interactive)
                                             (if fa-overlay
                                                 (fa-jump)
                                               (,(key-binding (kbd "M-j")))))))))

;; ——— Customization —————————————————————————————————————————————————————————————————
(defgroup function-args nil
  "C++ function completion."
  :group 'completion
  :prefix "fa-")

(defgroup function-args-faces nil
  "Font-lock faces for `function-args'."
  :group 'function-args
  :prefix "fa-")

(defcustom fa-hint-position-below nil
  "Non-nil means hint will be shown below point (instead of above)."
  :type 'boolean
  :group 'function-args)

(defcustom fa-max-one-line-width 60
  "Max hint size that can be displayed on one line."
  :type 'integer
  :group 'function-args)

(defcustom moo-select-method 'helm
  "Method to select a candidate from a list of strings."
  :type '(choice
          (const :tag "Helm" helm)
          (const :tag "Plain" display-completion-list))
  :group 'function-args)

(defface fa-face-hint
  '((t (:background "#fff3bc" :foreground "black")))
  "Basic hint face."
  :group 'function-args-faces)

(defface fa-face-hint-bold
  '((t (:background "#fff3bc" :bold t) ))
  "Basic hint face with bold font. Bold is used to signify the current element."
  :group 'function-args-faces)

(defface fa-face-type
  '((t (:inherit 'font-lock-type-face :background "#fff3bc") ))
  "Face for displaying types."
  :group 'function-args-faces)

(defface fa-face-type-bold
  '((t (:inherit 'font-lock-type-face :background "#fff3bc" :bold t) ))
  "Face for displaying types. Bold is used to signify the current element"
  :group 'function-args-faces)

(defface fa-face-semi
  '((t (:foreground "#2a00ff" :background "#fff3bc")))
  "Face for displaying separators."
  :group 'function-args-faces)

(defconst fa-paren-open (propertize "(" 'face 'fa-face-semi)
  "String to open argument list.")

(defconst fa-paren-close (propertize ") : " 'face 'fa-face-semi)
  "String to close argument list.")

(defconst fa-comma (propertize "," 'face 'fa-face-semi)
  "String to join arguments")

;; ——— Internal variables ————————————————————————————————————————————————————————————
(defvar fa-overlay nil
  "Hint overlay instance.")

(defvar fa-hint-pos nil
  "Point position where the hint should be (re-) displayed.")

(defvar fa-beg-pos nil
  "Position of ( after `fa-start-tracking' was invoked.")

(defvar fa-end-pos nil
  "Position of ) after `fa-start-tracking' was invoked.")

(defvar fa-lst nil
  "Current function arguments variants.")

(defvar fa-arg 0
  "Current function argument.")

(defvar fa-idx nil
  "Current function arguments variant.")

(defvar fa-superclasses (make-hash-table :test 'equal)
  "Stores superclasses tags.")

;; ——— Interactive functions —————————————————————————————————————————————————————————
(defun fa-show ()
  "Display the arguments of the closest function."
  (interactive)
  (fa-do-position)
  (setq fa-lst (fa-calculate))
  (if (eq (length fa-lst) 0)
      (message "nothing found")
    (forward-char)
    (setq fa-idx 0)
    (setq fa-hint-pos (point))
    (fa-update-arg)
    (fa-do-show)
    (fa-start-tracking)))

(defmacro fa-idx-cycle (arg)
  "Cycle `fa-idx' by ARG and redisplay function arguments."
  `(lambda ()
     (interactive)
     (setq fa-idx
        (mod (+ fa-idx ,arg)
             (length fa-lst)))
     (fa-update-arg)
     (fa-do-show)))

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
    (fa-update-arg)
    (fa-do-show)))

(defun fa-jump ()
  "Jump to current function of `fa-arg'"
  (interactive)
  (when (overlayp fa-overlay)
    (fa-abort)
    (push-mark (point) t)
    (let ((tag (nth 2 (car (nth fa-idx fa-lst)))))
      (switch-to-buffer
       (find-file-noselect
        (car tag)))
      (goto-char
       (cdr tag)))))

(defun moo-complete-candidates-2 (prefix var-name)
  (let* ((var-used-as-pointer-p (looking-back "->\\(?:[A-Za-z][A-Za-z_0-9]*\\)?"))
         (var-used-as-classvar-p (looking-back "\\.\\(?:[A-Za-z][A-Za-z_0-9]*\\)?"))
         (var-tag (moo-tag-at-point var-name))
         (var-pointer-p (semantic-tag-get-attribute var-tag :pointer))
         (tmembers (moo-ttype->tmembers
                    (cond
                      (var-used-as-classvar-p
                       (or
                        ;; semantic may think it's a function
                        (moo-complete-type-member var-tag)
                        ;; this works sometimes
                        (moo-sname->tag var-name)))
                      ;; Type::member
                      ((looking-back "::\\(?:[A-Za-z][A-Za-z_0-9]*\\)?")
                       (if (semantic-tag-of-class-p var-tag 'function)
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
                       (cond ((semantic-tag-of-class-p var-tag 'type)
                              var-tag)
                             ((semantic-tag-of-class-p var-tag 'variable)
                              (moo-tvar->ttype var-tag))
                             (t (error "unexpected")))))))
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
    (append candidates-1 candidates-2)))


(defun moo-complete (arg)
  "Complete current C++ symbol at point."
  (interactive "P")
  (let ((symbol (moo-ctxt-current-symbol))
        prefix candidates)
    (if (cond
          ;; ———  ————————————————————————————————————————————————————————————————————
          ((= (length symbol) 2)
           ;; either var.prefix or var->prefix
           (setq prefix (cadr symbol))
           (setq candidates (moo-complete-candidates-2 prefix (car symbol))))
          ;; ———  ————————————————————————————————————————————————————————————————————
          ((= (length symbol) 1)
           (setq prefix (car symbol))
           (setq candidates (moo-complete-candidates-1 prefix))
           (if arg
               (setq candidates
                     (moo-filter-tag-by-class 'variable candidates))
             t)))
        (moo-handle-completion
         prefix
         (cl-delete-duplicates candidates :test #'moo-tag=))
      ;; ———  ————————————————————————————————————————————————————————————————————————
      (semantic-ia-complete-symbol (point)))))

(defun moo-propose-virtual (arg)
  "Call `moo-propose' for virtual functions."
  (interactive "P")
  (when arg
    (setq fa-superclasses (make-hash-table :test 'equal)))
  (moo-propose (fa-and moo-functionp moo-virtualp)))

(defun moo-propose-override (arg)
  "Call `moo-propose' for all functions."
  (interactive "P")
  (when arg
    (setq fa-superclasses (make-hash-table :test 'equal)))
  (moo-propose #'moo-functionp))

(defun moo-propose-variables (arg)
  "Call `moo-propose' for all variables."
  (interactive "P")
  (when arg
    (setq fa-superclasses (make-hash-table :test 'equal)))
  (moo-propose #'moo-variablep))

(defun moo-jump-local ()
  "Select a tag to jump to from tags defined in current buffer."
  (interactive)
  (let ((tags (semantic-fetch-tags)))
    (moo-select-candidate
     (if (eq major-mode 'c++-mode)
         (mapcar
          (lambda(x)(cons x (moo-tag->str x)))
          (moo-flatten-namepaces tags))
       tags)
     #'moo-action-jump)))

;; ——— Predicates ————————————————————————————————————————————————————————————————————
(defmacro fa-and (&rest predicates)
  "Return a lambda that combines the predicates with an and"
  `(lambda(x)(and ,@(mapcar (lambda(y)(list y 'x))
                       predicates))))

(defun fa-char-upcasep (c)
  (eq c (upcase c)))

(defun moo-virtualp (function-tag)
  (and
   (or
    (member "virtual"
            (semantic-tag-get-attribute
             function-tag :typemodifiers))
    (semantic-tag-get-attribute
     function-tag :pure-virtual-flag))
   ;; don't want distructors
   (not (semantic-tag-get-attribute
         function-tag :destructor-flag))))

(defun moo-typedefp (tag)
  (semantic-tag-get-attribute tag :typedef))

(defun moo-namespacep (tag)
  (let ((attr (semantic-tag-get-attribute tag :type)))
    (and (stringp attr)
         (string= attr "namespace"))))

(defun moo-functionp (tag)
  (semantic-tag-of-class-p tag 'function))

(defun moo-variablep (tag)
  (semantic-tag-of-class-p tag 'variable))

(defun moo-typep (tag)
  (semantic-tag-of-class-p tag 'type))

(defun moo-includep (tag)
  (semantic-tag-of-class-p tag 'include))

(defun moo-usingp (tag)
  (semantic-tag-of-class-p tag 'using))

(defun moo-constructorp (tag)
  (semantic-tag-get-attribute tag :constructor-flag))

;; ——— Comparers —————————————————————————————————————————————————————————————————————
(defun test-with (pred x1 x2)
  "Return (equal (PRED X1) (PRED X2))"
  (equal (funcall pred x1)
         (funcall pred x2)))

(defun moo-variable= (v1 v2)
  "Return t if variable tags V1 and V2 are equivalent."
  (and (moo-variablep v1)
       (moo-variablep v2)
       (test-with #'car v1 v2)
       (test-with (lambda(x)(semantic-tag-get-attribute x :reference)) v1 v2)
       (test-with (lambda(x)(semantic-tag-get-attribute x :constant-flag)) v1 v2)
       (test-with (lambda(x)(semantic-tag-get-attribute x :type)) v1 v2)))

(defun moo-function= (f1 f2)
  "Return t if function tags F1 and F2 are equivalent."
  (and (moo-functionp f1)
       (moo-functionp f2)
       (string= (car f1) (car f2))
       (equal (semantic-tag-get-attribute f1 :typemodifiers)
              (semantic-tag-get-attribute f2 :typemodifiers))
       (equal (semantic-tag-get-attribute f1 :type)
              (semantic-tag-get-attribute f2 :type))
       (cl-every #'identity
                 (cl-mapcar #'moo-variable=
                            (semantic-tag-get-attribute f1 :arguments)
                            (semantic-tag-get-attribute f2 :arguments)))))

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
  (and (equal (moo-tget-beginning-position tag1)
              (moo-tget-beginning-position tag2))
       (let ((fname1 (moo-tget-filename tag1))
             (fname2 (moo-tget-filename tag2)))
         ;; normally all tags should have fname, but some don't
         (or (null fname1)
             (null fname2)
             (equal fname1 fname2)))))

;; ——— Tag getters ———————————————————————————————————————————————————————————————————
(defun moo-tget-filename (tag)
  (or (semantic--tag-get-property tag :filename)
      (and (overlayp (car (last tag)))
           (buffer-file-name
            (overlay-buffer (car (last tag)))))))

(defun moo-tget-beginning-position (tag)
  (let ((x (car (last tag))))
    (cond ((overlayp x)
           (overlay-start x))
          ((arrayp x)
           (aref x 0))
          (t 0))))

(defun moo-tget-end-position (tag)
  (let ((x (car (last tag))))
    (cond ((overlayp x)
           (overlay-end x))
          ((arrayp x)
           (aref x 1))
          (t 0))))

(defun moo-tget-constructors (type)
  (ignore-errors
  (setq type (moo-dereference-typedef type))
  (let ((enump (moo-tget-enum-members type)))
    (cond
     ;; enum
     (enump
      `((
         ;; name
         ,(semantic-tag-name type)
         ;; class
         function
         ;; attributes
         (:arguments
          ((,(mapconcat
              #'car
              enump
              " | ")
            variable (:type ,(semantic-tag-name type))))
          :type
          "enum")
         ;; properties
         ,(semantic-tag-properties type)
         ;; overlay
         ,(semantic-tag-overlay type))))
     ;; else
     (t
      (filter #'moo-constructorp
              (moo-get-member-functions type)))))))

(defun moo-tget-enum-members (tag)
  (let ((stag (and (semantic-tag-of-class-p tag 'type)
                   (semantic-tag-get-attribute tag :type))))
    (and (stringp stag)
         (string= stag "enum")
         (semantic-tag-get-attribute tag :members))))

(defun moo-tget-superclasses (ttype)
  (semantic-tag-get-attribute ttype :superclasses))

;; ——— Tag setters ———————————————————————————————————————————————————————————————————
(defun moo-tput-filename (tag filename)
  (semantic--tag-put-property tag :filename filename))

(defun moo-tput-filename-to-types (types-list filename)
  (mapcar
   (lambda(type)
     (semantic-tag-put-attribute
      type :members
      (mapcar (lambda(tag) (moo-tput-filename tag filename))
              (semantic-tag-get-attribute type :members))))
   types-list))

;; ——— Pretty priting ————————————————————————————————————————————————————————————————
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
         (str-width (+ (apply #'+ (mapcar (lambda (x) (+ (length (car x))
                                                    (length (cdr x))))
                                          (cdr lst)))
                       (length (caar lst))
                       (length (cadar lst))
                       7))
         (glue (if (> (+ wspace str-width)
                      (min fa-max-one-line-width (frame-width)))
                   ;; each arg on its own line
                   (concat fa-comma "\n" (make-string wspace ? ))
                 fa-comma))
         (args (mapcar #'fa-fancy-argument
                         (cdr lst)))
         (args-current-cdr (nthcdr fa-arg args)))
    (when args-current-cdr
      (setcar args-current-cdr
              (fa-fancy-argument (nth fa-arg (cdr lst)) t)))
    (concat
     (when (> padding-length 0) (make-string padding-length ? ))
     (propertize n-string 'face 'fa-face-hint-bold)
     fa-paren-open
     (and args (mapconcat 'identity args glue))
     fa-paren-close
     ;; template
     (and (caar lst) (propertize (caar lst) 'face 'fa-face-hint))
     ;; name
     (and (cadar lst) (propertize (cadar lst) 'face 'fa-face-type)))))

(defun fa-fancy-argument (cell &optional bold)
  (concat
   (propertize (car cell) 'face
               (if bold 'fa-face-type-bold 'fa-face-type))
   (propertize " " 'face 'fa-face-hint)
   (propertize (cdr cell) 'face
               (if bold 'fa-face-hint-bold 'fa-face-hint))))

(defun fa-tfunction->fal (sm &optional output-string)
  (let ((filename (moo-tget-filename sm))
        (position (moo-tget-beginning-position sm))
        (name (pop sm))
        (name-e (pop sm)))
    (if (eq name-e 'type)
        (propertize name 'face 'font-lock-type-face)
      (if (not (eq name-e 'function))
          (error "not a function")
        (let ((r (pop sm))
              template-p
              type-p
              arguments-p
              constant-flag-p
              prototype-flag-p
              typemodifiers-p
              pointer-p
              constructor-flag-p
              operator-flag-p
              parent-p
              pointer-p
              destructor-flag-p
              pure-virtual-flag-p
              template-specifier-p
              filename-p
              throws-p
              item)
          (while r
            (setq item (pop r))
            (case item
              (:template
               (setq template-p (pop r)))
              (:type
               (setq type-p (pop r)))
              (:arguments
               (setq arguments-p (pop r)))
              (:prototype-flag
               (setq prototype-flag-p (pop r)))
              (:constant-flag
               (setq constant-flag-p (pop r)))
              (:typemodifiers           ;what's this?
               (setq typemodifiers-p (pop r)))
              (:constructor-flag
               (setq constructor-flag-p (pop r)))
              (:parent
               (setq parent-p (pop r)))
              (:operator-flag
               (setq operator-flag-p (pop r)))
              (:destructor-flag
               (setq destructor-flag-p (pop r)))
              (:pointer
               (setq pointer-p (pop r)))
              (:pure-virtual-flag
               (setq pure-virtual-flag-p (pop r)))
              (:template-specifier
               (setq template-specifier-p (pop r)))
              (:throws
               (setq throws-p (pop r)))
              (:filename
               (setq filename-p (pop r)))
              (t (error (concat "fa-tfunction->fal unknown token" (prin1-to-string item))))))
          (let ((argument-conses (mapcar
                                  #'fa-tvar->cons
                                  (mapcar
                                   (lambda (x) (if (string= (car x) "") (setcar x "")) x)
                                   arguments-p))))
            (if (null output-string)
                (cons
                 ;; name and type part
                 (list (and template-p
                            (concat "template " (fa-ttemplate-specifier->str template-p) " "))
                       (if constructor-flag-p
                           name
                         (if type-p
                             (fa-ttype->str type-p)
                           "?"))
                       (cons filename
                             position))
                 ;; arguments part
                 argument-conses)
              ;; ——— output a string instead —————————————————————————————————————————————
              (concat
               (and template-p (concat "template " (fa-ttemplate-specifier->str template-p) " "))
               (and typemodifiers-p (concat (mapconcat #'identity typemodifiers-p " ") " "))
               (if constructor-flag-p
                   ""
                 (if type-p
                     (fa-ttype->str type-p)
                   "?"))
               " " (propertize name 'face 'font-lock-function-name-face)
               "("
               (mapconcat (lambda (x) (concat (car x) " " (cdr x)))
                          argument-conses
                          ", ")
               ");"))))))))

(defun fa-tvar->cons (sm)
  (let ((name (pop sm))
        (name-e (pop sm)))
    (let ((r (pop sm))
          (s "")
          item
          reference-p
          constant-p
          typemodifiers-p
          pointer-p
          type-p
          dereference-p
          default-value-p
          function-pointer-p
          arguments-p)
        (while r
          (setq item (pop r))
          (case item
            (:reference
             (setq reference-p t)
             (unless (eq (pop r) 1) (error "expected 1")))
            (:constant-flag
             (setq constant-p t)
             (unless (eq (pop r) t) (error "expected t")))
            (:type (setq type-p (pop r)))
            (:typemodifiers
             (setq typemodifiers-p (pop r)))
            (:pointer
             (setq pointer-p (pop r)))
            (:dereference
             (setq dereference-p (pop r)))
            (:default-value
             (message (setq default-value-p (pop r))))
            ;; ——— function pointer specific —————————————————————————————————————————
            (:function-pointer
             (setq function-pointer-p (pop r)))
            (:arguments
             (setq arguments-p (pop r)))
            (t (error (concat "unknown token" (prin1-to-string item))))))
        (cons (concat (and constant-p "const ")
                      (fa-ttype->str type-p))
              (concat (and reference-p "&")
                      (and pointer-p "*")
                      ;; pretty up std:: identifiers
                      (replace-regexp-in-string "^_+" "" name)
                      (and dereference-p "[]"))))))

(defun fa-ttype->str (sm)
  (if (stringp sm)
      sm
    (let ((name (pop sm))
          (name-e (pop sm)))
      (if (not (eq name-e 'type))
          (error "not a type")
        (let ((rst (pop sm))
              item
              template-specifier-p)
          (while rst
            (setq item (pop rst))
            (case item
              (:template-specifier
               (setq template-specifier-p (pop rst)))
              (t (pop rst))))
          (concat name (fa-ttemplate-specifier->str
                        template-specifier-p)))))))

(defun fa-ttemplate-specifier->str (sm)
  (and sm (concat "<"
                  (mapconcat (lambda(x)(replace-regexp-in-string "^_+" "" (car x))) sm ",")
                  ">")))

(defun moo-tag->cons (tag)
  "Return for TAG a cons (STR . NAME).
STR is the result of `moo-tag->str' on TAG,
NAME is the TAG name."
  (cons (car tag) (moo-tag->str tag)))

(defun moo-tag->str (tag)
  (or (ignore-errors (fa-tfunction->fal tag t))
      (car tag)))

;; ——— Misc non-pure —————————————————————————————————————————————————————————————————
(defun fa-do-position ()
  "Position the cursor at the `(', which is logically closest"
  (cond
    ((looking-at "("))
    ((looking-back "(")
     (backward-char))
    ((looking-back "^\\([^(\n]*\\)([^(\n]*")
     (re-search-backward "("))
    ((looking-at "[^\n]*([^\n]*$")
     (re-search-forward "(")
     (backward-char))
    (t
     (error "not inside function")))
  (unless (looking-back "^[ \t]*")
    (while (looking-back " ")
      (backward-char)))
  (point))

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

(defun fa-start-tracking ()
  (let ((beg (save-excursion (re-search-backward "(" nil t) (point)))
        (end (save-excursion (re-search-forward ")" nil t) (- (point) 1))))
    (setq fa-beg-pos beg)
    (setq fa-end-pos end)
    (add-hook 'after-change-functions
              'fa-after-change)))

(defun fa-update-arg ()
  "Update `fa-arg' if it needs to be updated.
Return non-nil if it was updated."
  (let ((argn (semantic-ctxt-current-argument)))
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
           (fa-abort)))))

(defun fa-after-change (beg end len)
  (let ((info (format "b=%d, e=%d l=%d; fb=%d, fe=%d, point= %d" beg end len fa-beg-pos fa-end-pos (point))))
    (if (or (< beg fa-beg-pos)
            (> beg fa-end-pos))       ; out of range, abort
        ;; work around for when auto-complete-mode is active
        (unless (and (fboundp 'auto-complete-mode) auto-complete-mode
                     (or (and (featurep 'auto-complete) ac-menu)
                         (> (- end beg) 1)))
          (fa-abort))
      (cond
       ((eq len 0)                      ; insertion
        (cl-incf fa-end-pos (- end beg)))
       ((eq beg end)                    ; deletion
        (decf fa-end-pos len)))
      (when (fa-update-arg)
      (fa-do-show)))))

(defun erase-string (str)
  "Ensure `looking-back' STR and erase it.
`case-fold-search' is set to nil."
  (let ((case-fold-search nil))
    (if (looking-back str)
        (delete-region (match-beginning 0) (match-end 0))
      (error "Can't erase %s." str))))

(defun fa-backward-char-skip<> (&optional arg)
  "Moves point backward until [A-Za-z_0-9] is encountered.
Skips anything between matching <...>"
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

(defun moo-handle-completion (prefix candidates &optional formatter)
  "Select tag that starts with PREFIX from CANDIDATES.
FORMATTER is used to convert tag to string.
The default FORMATTER is `moo-tag->cons'."
  (cond
   ((null candidates)
    (message "there is no completions, only Zuul"))
   ;; either one candidate or multiple with same name:
   ((or (= 1 (length candidates))
        (cl-reduce (lambda (x1 x2) (and x1 (string= (car x1) (car x2)) x1)) candidates))
    (moo-action-insert
     (funcall (or formatter #'car) (car candidates))
     prefix))
   ;; multiple candidates with different names
   (t
    (let* ((completion-ignore-case (string= prefix (downcase prefix)))
           (tc (try-completion (or prefix "") candidates)))
      (if (and (stringp tc) (not (string= tc (or prefix ""))))
          (moo-action-insert tc prefix)
        (moo-select-candidate
         (mapcar (or formatter #'moo-tag->cons)
                 candidates)
         (lambda(x)(moo-action-insert x prefix))))))))

(defun moo-select-candidate (candidates action &optional name)
  (unless name
    (setq name "Candidates"))
  (case moo-select-method
    (helm
     (require 'helm)
     (helm :sources `((name . ,name)
                      (candidates . ,(mapcar
                                      (lambda(x)
                                        (if (listp x)
                                            (if (stringp (cdr x))
                                                (cons (cdr x) (car x))
                                              (cons (car x) x))
                                          x)) candidates))
                      (action . ,action))))
    (display-completion-list
     (with-output-to-temp-buffer "*Completions*"
       (display-completion-list candidates)))))

(defun moo-action-insert (candidate &optional prefix)
  (when prefix
    (erase-string prefix))
  (insert candidate))

(defun moo-action-jump (tag)
  (when (semantic-tag-p tag)
    (push-mark)
    (semantic-go-to-tag tag)
    (switch-to-buffer (current-buffer))))

(defun moo-propose (pred)
  "Display a list of current class members that satisfy PRED."
  (let ((stype (c++-get-class-name)))
    (when stype
      (let ((ttype (moo-tag-at-point stype #'moo-typep)))
        (when ttype
          (let ((members (filter pred
                                 (moo-ttype->tmembers ttype))))
            (setq members (sort members (lambda (a b) (string< (car a) (car b)))))
            (moo-handle-completion "" members #'moo-tag->str)))))))

;; ——— Internals —————————————————————————————————————————————————————————————————————
(defalias 'filter 'cl-remove-if-not)

(defun moo-tag-at-point (str &optional predicate)
  "Find a tag with name STR that's visible near point.
Optional PREDICATE is used to improve uniqueness of returned tag."
  (let* ((matches (moo-desperately-find-sname str))
         (class-name (c++-get-class-name))
         ;; try to filter based on class
         (filtered-matches
          (filter (lambda(x)
                    (and (not (semantic-tag-get-attribute x :prototype))
                         (if predicate (funcall predicate x) t)
                         (or (not (semantic-tag-of-class-p x 'variable))
                             (equal class-name
                                    (save-excursion
                                      (goto-char (moo-tget-beginning-position x))
                                      (c++-get-class-name))))))
                  matches)))
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
      (t
       (error "multiple definitions for %s" str)))))

(defun moo-type-tag-at-point (str)
  (let* ((matches (moo-desperately-find-sname str))
         (filtered-matches
          (filter (lambda(x)
                    (and (not (semantic-tag-get-attribute x :prototype))
                         (semantic-tag-of-class-p x 'type)))
                  matches)))
    (cond
      ;; fall back to semantic
      ((null filtered-matches)
       (save-excursion
         (search-backward str)
         (semantic-analyze-interesting-tag
          (semantic-analyze-current-context (point)))))
      ((eq 1 (length filtered-matches))
       (car filtered-matches))
      (t
       (error "multiple definitions for %s" str)))))

;; this is similar to stype->tag
;; I should refactor this
(defun moo-complete-type-member (var-tag)
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
           (moo-type-tag-at-point type-name))))))

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
                   (if (looking-back "\\()[ \n\t]*:[^;()]*\\)\\|,[^;()]*")
                       (moo-tget-constructors (moo-sname->tag (car function)))
                     (fa-backward-char-skip<>)
                     (moo-tget-constructors (moo-ctxt-type))))
                  ((and (semantic-tag-p ctxt-type)
                   (cond
                    ;; variable init inside constructor
                    ((and (semantic-tag-of-class-p ctxt-type 'variable)
                               (looking-back ":[^;]*"))
                         (moo-tget-constructors (moo-sname->tag (car function))))
                    ;; parent class init inside constructor
                    ;; or constructor as part of expression
                    ((semantic-tag-of-class-p ctxt-type 'type)
                     (or (moo-tget-constructors ctxt-type)
                         (moo-tget-constructors
                          (moo-tvar->ttype (car (moo-desperately-find-sname (car function)))))
                         (moo-tget-constructors
                          (moo-tag-at-point (car ctxt-type)))))
                    ;; global function call
                    ((semantic-tag-of-class-p ctxt-type 'function)
                     (let ((prototype-flag-p
                            (semantic-tag-get-attribute ctxt-type :prototype-flag))
                           (tag-end (moo-tget-end-position ctxt-type)))
                       (if (and prototype-flag-p
                               (and tag-end (< (point) tag-end)))
                           (or (progn
                                 (fa-backward-char-skip<>)
                                 (moo-tget-constructors (moo-ctxt-type)))
                               (list ctxt-type))
                         ;; should remove duplicates here
                         (append (list ctxt-type)
                                 (moo-desperately-find-sname (car function)))))))))
                  ;; global function invocation
                  ((looking-back "\\(:?}\\|else\\|;\\|{\\|\\(:?//.*\\)\\)[ \t\n]*")
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
                  (forward-char)
                  (backward-list)
                  (error "[]-> or []. not implemented yet")
                  ;; do something
                  )
                 ((looking-at ">")
                  (forward-char)
                  (fa-backward-char-skip<>)))

               (let* ((ctxt-type (moo-ctxt-type))
                      (ctype (semantic-tag-get-attribute ctxt-type :type)))
                 (fa-backward-char-skip<> -1)
                 (cond
                  ((looking-back "::")
                   (cl-delete-duplicates
                    (append             ; a lot of time both are the same
                     (fa-process (cadr function)
                                 ctxt-type)
                     (cl-mapcan
                      `(lambda (tag)
                         (filter (lambda (tag) (eq (cadr tag) 'function))
                                 (moo-filter-tag-by-name ,(cadr function) (moo-ttype->tmembers tag))))
                      (moo-desperately-find-sname (car function))))
                    :test #'moo-tag-pos=))
                  ;; smart pointer?
                  ((and (looking-back "->") (not (semantic-tag-get-attribute ctxt-type :pointer)))
                   (let* ((type (semantic-tag-get-attribute ctxt-type :type))
                          (type-template
                           (semantic-tag-get-attribute (if (equal type "class") ctxt-type type)
                                                       :template-specifier)))
                     (fa-process (cadr function)
                                    (moo-stype->tag (caar type-template)))))
                  ;; rest
                  (t
                   ;; get variable's type
                   (when (semantic-tag-of-class-p ctxt-type 'variable)
                     (setq ctxt-type (moo-stype->tag (car ctype))))
                   (fa-process (cadr function) ctxt-type)))))))))
    (or (mapcar #'fa-tfunction->fal result)
        ;; fall back to semantic
        (let ((fntag (semantic-analyze-find-tag-sequence
                      function (semantic-calculate-scope (point)))))
          (and fntag
               (not (stringp (car (last fntag))))
               (setq fntag (car (last fntag)))
               (list (fa-tfunction->fal fntag)))))))

(defun fa-process-tag-according-to-class (tag)
  "TTAGS is a list of tags with the same name.
Reduce them to functions only"
  (cond ((semantic-tag-of-class-p tag 'function)
         (list tag))
        ((semantic-tag-of-class-p tag 'type)
         (moo-tget-constructors
          ;; (moo-dereference-typedef tag scope
          tag))
        ((semantic-tag-of-class-p tag 'variable)
         nil)
        (t nil)))

(defun fa-process (str ttype)
  "Get all functions of TTYPE with name STR.
This includes the constructors of types with name STR."
  (let (
        ;; TODO: this fails for namespaces such as std::
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
  (let ((ctxt (semantic-analyze-current-context (point))))
    (if (null ctxt)
        (error "nothing under cursor")
      (setq ctxt (car (oref ctxt prefix)))
      (ignore-errors
        (cond ((stringp ctxt)
               (or (moo-tag-at-point ctxt) ctxt))
              ;; check if variable constructor initialization is mistaken
              ;; for function prototype definition:
              ((and (eq (nth 1 ctxt) 'function)
                    (semantic-tag-get-attribute ctxt :prototype-flag)
                    (let ((arg1 (caar (semantic-tag-get-attribute ctxt :arguments))))
                      (and arg1 (stringp arg1) (string= arg1 ""))))
               (moo-tag-at-point (car (semantic-tag-get-attribute ctxt :type))))
              (t
               ctxt))))))

(defun moo-stype->tag (str)
  (or
   (ignore-errors
     (let ((scope (semantic-calculate-scope (point))))
       ;; TODO: check (= (length retval) 1)
       (car
        (catch 'unfindable
          (semantic-analyze-find-tag-sequence
           (list str) scope 'prefixtypes 'unfindable)))))
   (let ((candidates (filter
                      (lambda(x)
                        (and (moo-typep x) (semantic-tag-get-attribute x :members)))
                      (moo-desperately-find-sname str))))
     (cond ((= 0 (length candidates)))

           ((= 1 (length candidates))
            (car candidates))

           (t (error "`moo-stype->tag': too many candidates"))))))

(defun moo-get-member-functions (ttype)
  (cond
   ((semantic-tag-of-class-p ttype 'type)
    (filter (lambda(tag) (semantic-tag-of-class-p tag 'function))
            (moo-ttype->tmembers ttype)))
   ((semantic-tag-of-class-p ttype 'variable)
    (moo-get-member-functions
     (moo-stype->tag
      (car (semantic-tag-get-attribute ttype :type)))))))

(defun moo-navigate-members (tag)
  (let ((typedef (semantic-tag-get-attribute tag :typedef)))
    (if typedef
        (moo-sname->tag (car typedef))
      (semantic-tag-get-attribute tag :members))))

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
                          (setq parent-tag)))
                      ;; don't inherit constructors
                      (cl-delete-if #'moo-constructorp
                                    (moo-ttype->tmembers parent-tag)))
                    (moo-tget-superclasses ttype))))
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

(defun moo-dereference-typedef (tag)
  "if tag is a typedef, search for it in scope."
  (let ((typedef-p (moo-typedefp tag))
        (scope (moo-tag-get-scope tag))
        defs)
    (if (null typedef-p)
        tag
      (setq defs (filter `(lambda (x) (and (eq (cadr x) 'type)
                                      (string= (car x) ,(car typedef-p))))
                         (semantic-tag-get-attribute scope :members)))
      (case (length defs)
        (1 (car defs))
        (0 (cons (car tag) (cdr typedef-p)))
        (t (error "typedef has multiple definitions"))))))

(defun moo-tvar->ttype (var-tag)
  (let* ((var-name (car var-tag))
         (var-stype (car (semantic-tag-get-attribute var-tag :type)))
         (type-tag (moo-stype->tag var-stype)))
    (if (moo-typedefp type-tag)
        (moo-dereference-typedef type-tag)
      type-tag)))

(defun moo-tag-get-scope (tag)
  (caadr (cl-find-if (lambda (x) (and (listp x) (eq (car x) 'scope))) tag)))

(defun moo-get-tag-by-name (sname tlist)
  (cl-mapcan
   (lambda (tag)
     (if (string= (car tag) sname)
         (list tag)
       (and (or (moo-namespacep tag) (semantic-tag-of-class-p tag 'type))
            (moo-get-tag-by-name
             sname
             (semantic-tag-get-attribute tag :members)))))
   tlist))

(defun moo-desperately-find-sname (stag)
  (let* ((file-tags (semantic-fetch-tags))
         (own-tags (moo-get-tag-by-name stag file-tags))
         (include-tags (filter (lambda (tag) (semantic-tag-of-class-p tag 'include))
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
  "Traverse the forest TAGS, reducing with two-argument function FUNC."
  (cl-labels ((namespace-reduce
               (func tags out)
               (dolist (tag tags)
                 (cond ((or (moo-includep tag) (moo-usingp tag))
                        ;; skip
                        )

                       ((moo-namespacep tag)
                        (setq out
                              (namespace-reduce
                                  func
                                  (semantic-tag-get-attribute tag :members)
                                  (funcall func out tag))))

                       (t (setq out (funcall func out tag)))))
               out))
    (nreverse (namespace-reduce func tags nil))))

(defun moo-find-sname-in-tags (stag tags)
  "Find tags named STAG in forest TAGS."
  (moo-namespace-reduce
   (lambda(x y) (if (string= (car y) stag) (push y x) x))
   tags))

(defun moo-flatten-namepaces (tags)
  "Traverse the namespace forest TAGS and return the leafs."
  (moo-namespace-reduce (lambda(x y) (push y x)) tags))

(defun c++-get-class-name ()
  (car (c++-get-class-name-and-template)))

(defun c++-get-class-template ()
  (cdr (c++-get-class-name-and-template)))

(defvar c++-braces-table
  (let ((table (make-char-table 'syntax-table nil)))
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)
    table))

(defun c++-get-class-name-and-template ()
  (ignore-errors
    (save-excursion
      (let (name template)
        ;; step out of the current block
        (with-syntax-table c++-braces-table
          (up-list)
          (backward-list))
        ;; TODO take care of nested classes
        (if (looking-back
             "\\(?:class\\|struct\\) \\([A-Za-z][A-Z_a-z0-9]*\\)[: \t\n]+[^{;]*?")
            (progn
              (goto-char (match-beginning 0))
              (setq name (match-string-no-properties 1))
              ;; try to match the template as well
              (when (looking-back ">[\n \t]*")
                (let ((end (progn (goto-char (match-beginning 0)) (point)))
                      (beg (ignore-errors (forward-char)(backward-list)(point))))
                  (when end
                    (setq template (buffer-substring-no-properties (1+ beg) end))))))
          ;; we're not in class, but in a function
          (beginning-of-defun)
          (when (looking-at "template +<")
            (goto-char (1- (match-end 0)))
            (setq template (substring (moo-list-at-point) 1 -1))
            (forward-list))
          (re-search-forward " \\([A-Za-z][A-Z_a-z0-9]*\\)\\(\\(?:<[^>]*>\\)?\\)::")
          (setq name (match-string-no-properties 1)))
        (cons name template)))))

(defun moo-list-at-point ()
  "Return any list at point. At least what the syntax
thinks is a list."
  (let ((beg (point))
        end)
    (forward-list)
    (setq end (point))
    (backward-list)
    ;; maybe do something with beg and (point)
    (setq beg (point))
    (buffer-substring-no-properties beg end)))

(provide 'function-args)
;;; function-args.el ends here
