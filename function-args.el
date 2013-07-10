;;; function-args.el --- C++ completion for GNU Emacs

;; Copyright (C) 2013  Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/function-args
;; Version: 0.1

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
;; This extension provides three main commands that are useful for c++-mode:
;;
;; * `fa-show' -- show an overlay hint with current function arguments.
;; * `fa-jump' -- jump to definition of current element of `fa-show'.
;; * `moo-complete' -- a c++-specific version of `semantic-ia-complete-symbol'.


(require 'cl-lib)
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

(defcustom fa-hint-position-below nil
  "Non-nil means hint will be shown below point (instead of above)."
  :type 'boolean
  :group 'function-args)

(defcustom fa-max-one-line-width 60
  "Max hint size that can be displayed on one line."
  :type 'integer
  :group 'function-args)

(defface fa-face-hint
  '((t (:background "#fff3bc" :foreground "black")))
  "Basic hint face."
  :group 'font-lock-highlighting-faces)

(defface fa-face-hint-bold
  '((t (:background "#fff3bc" :bold t) ))
  "Basic hint face with bold font. Bold is used to signify the current element."
  :group 'font-lock-highlighting-faces)

(defface fa-face-type
  '((t (:inherit 'font-lock-type-face :background "#fff3bc") ))
  "Face for displaying types."
  :group 'font-lock-highlighting-faces)

(defface fa-face-type-bold
  '((t (:inherit 'font-lock-type-face :background "#fff3bc" :bold t) ))
  "Face for displaying types. Bold is used to signify the current element"
  :group 'font-lock-highlighting-faces)

(defface fa-face-semi
  '((t (:foreground "#2a00ff" :background "#fff3bc")))
  "Face for displaying separators."
  :group 'font-lock-highlighting-faces)

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
(defun fa-show (point)
  "Display the arguments of the closest function."
  (interactive "P")
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
  (while (looking-back " ")
    (backward-char))
  (setq fa-lst (fa-calculate (point)))
  (if (eq (length fa-lst) 0)
      (message "nothing found")
    (forward-char)
    (setq fa-idx 0)
    (fa-update-arg)
    (setq fa-hint-pos (point))
    (fa-do-show)
    (fa-start-tracking point)))

(defmacro fa-idx-cycle (arg)
  "Cycle `fa-idx' by ARG and update the hint."
  `(lambda ()
     (interactive)
     (setq fa-idx
        (mod (+ fa-idx ,arg)
             (length fa-lst)))
     (fa-update-arg)
     (fa-do-show)))

(defun fa-abort (&optional msg)
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

(defun moo-complete (&optional pos)
  "Complete current C++ symbol at POS."
  (interactive "d")
  (when (semantic-active-p)
    (let ((symbol (semantic-ctxt-current-symbol)))
      (cond
       ;; ———  ———————————————————————————————————————————————————————————————————————
       ((= (length symbol) 2)
        ;; either var.mem or var->mem
        (let ((var-name (car symbol))
              (mem-name (cadr symbol))
              (var-used-as-pointer-p (looking-back "->\\(?:[A-Za-z][A-Za-z_0-9]*\\)?"))
              (var-used-as-classvar-p (looking-back "\\.\\(?:[A-Za-z][A-Za-z_0-9]*\\)?")))
          (let* ((var-tag (save-excursion
                            (search-backward var-name)
                            (semantic-analyze-interesting-tag
                             (semantic-analyze-current-context (point)))))
                 (var-pointer-p (semantic-tag-get-attribute var-tag :pointer))
                 (tmembers (moo-ttype->tmembers
                            (cond
                             (var-used-as-classvar-p
                              (or
                               ;; semantic may think it's a function
                               (let ((type-name (semantic-tag-get-attribute var-tag :type)))
                                 (if (equal type-name "class")
                                     ;; this happens sometimes
                                     var-tag
                                   (moo-stype->tag (car type-name))))
                               ;; this works sometimes
                               (moo-sname->tag var-name)))
                             ;; Type::member
                             ((looking-back "::\\(?:[A-Za-z][A-Za-z_0-9]*\\)?")
                              (if (semantic-tag-of-class-p var-tag 'function)
                                  (moo-sname->tag var-name)
                                var-tag))
                             ;; is it a smart pointer?
                             ((and var-used-as-pointer-p (not var-pointer-p))
                              (let ((type-template (semantic-tag-get-attribute
                                                    (semantic-tag-get-attribute var-tag :type)
                                                    :template-specifier)))
                                ;; assume that the first template parameter is the relevant one
                                ;; (normally, there should be only one anyway)
                                (moo-stype->tag (caar type-template))))
                             ;; otherwise just get its type
                             (t
                              (cond ((semantic-tag-of-class-p var-tag 'type)
                                     var-tag)
                                    ((semantic-tag-of-class-p var-tag 'variable)
                                     (moo-tvar->ttype var-tag))
                                    (t (error "unexpected")))))))
                 (candidates (delete-dups
                              (filter (lambda(x) (eq 0 (cl-search mem-name (car x))))
                                      tmembers))))
            (moo-handle-completion mem-name candidates))))
       ;; ———  ————————————————————————————————————————————————————————————————————————
       ((= (length symbol) 1)
        (let* ((sym-name (car symbol))
               (candidates-1 (semantic-analyze-possible-completions
                              (semantic-analyze-current-context pos)))
               (candidates-2 (and (featurep 'semantic/db)
                                  (semanticdb-minor-mode-p)
                                  (semanticdb-fast-strip-find-results
                                   (semanticdb-deep-find-tags-for-completion sym-name))))
               (candidates 
                (append 
                 candidates-1 
                 (filter (lambda (tag) (semantic-tag-of-class-p tag 'type))
                         (cl-delete-duplicates 
                          candidates-2
                          :test (lambda (t1 t2) (equal (car t1) (car t2))))))))
          (moo-handle-completion sym-name candidates)))
       ;; ———  ———————————————————————————————————————————————————————————————————————
       (t
        (semantic-ia-complete-symbol (or pos (point))))))))

;; ——— Internals —————————————————————————————————————————————————————————————————————
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

(defun fa-start-tracking (point)
  (interactive "P")
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
           (fa-abort
            (concat
             "bad semantic-ctxt-current-argument: "
             (prin1-to-string argn)))))))

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

(defun moo-tag-get-filename (tag)
  (or (semantic--tag-get-property tag :filename)
      (and (overlayp (car (last tag)))
           (buffer-file-name
            (overlay-buffer (car (last tag)))))))

(defun moo-tag-put-filename (tag filename)
  (semantic--tag-put-property tag :filename filename))

(defun moo-tag-get-position (tag)
  (let ((x (car (last tag))))
    (cond ((overlayp x)
           (overlay-start x))
          ((arrayp x)
           (aref x 0))
          (t 0))))

(defun moo-tags-same-pos-p (tag1 tag2)
  (and (equal (moo-tag-get-position tag1)
              (moo-tag-get-position tag2))
       (let ((fname1 (moo-tag-get-filename tag1))
             (fname2 (moo-tag-get-filename tag2)))
         ;; normally all tags should have fname, but some don't
         (or (null fname1)
             (null fname2)
             (equal fname1 fname2)))))

(defun moo-tag-put-filename-to-types (types-list filename)
  (mapcar
   (lambda(type)
     (semantic-tag-put-attribute
      type :members
      (mapcar (lambda(tag) (moo-tag-put-filename tag filename))
              (semantic-tag-get-attribute type :members))))
   types-list))

(defun fa-calculate (pos)
  "Return current function (or functions in case of overloading) in the form:
 ((name-1 arg-1 arg-2 ...) (name-2 arg-1 arg2 ...) ...)."
  (let* ((function (semantic-ctxt-current-symbol))
         (result
           (save-excursion
             (cond
              ;; semantic didn't recognize anything
              ;; try a class temp initialization
              ((= 0 (length function))
               (mp-backward-char-skip<>)
               (moo-get-constructors (moo-ctxt-type)))
              ;; semantic gave just a list with one string - a variable name
              ((= 1 (length function))
               (search-backward (car function))
               (let ((ctxt-type (moo-ctxt-type)))
                 (cond
                  ;; variable init inside constructor
                  ((and (semantic-tag-p ctxt-type)
                        (semantic-tag-of-class-p ctxt-type 'variable)
                        (looking-back ":[^;]*"))
                   (moo-get-constructors (moo-sname->tag (car function))))
                  ;; parent class init inside constructor
                  ((and (semantic-tag-p ctxt-type)
                        (semantic-tag-of-class-p ctxt-type 'type)
                        (looking-back ":[^;]*"))
                    (moo-get-constructors ctxt-type))
                  ;; global function invocation
                  ((looking-back "\\(:?}\\|else\\|;\\|{\\|\\(:?//.*\\)\\)[ \t\n]*")
                   (cl-mapcan #'fa-process-tag-according-to-class
                           (moo-desperately-find-sname (car function))))
                  ;; constructor as part of expression
                  ((semantic-tag-of-class-p ctxt-type 'type)
                   (moo-get-constructors ctxt-type))
                  ;; try to match a variable with a constructor declaration:
                  ;; move to the type
                  (t
                   (mp-backward-char-skip<>)
                   (let* ((ctxt-type (moo-ctxt-type))
                          (scope (moo-tag-get-scope ctxt-type)))
                     (moo-get-constructors (moo-dereference-typedef ctxt-type scope)))))))
              ((= 2 (length function))
               (re-search-backward ".\\(?:\\.\\|->\\|::\\)")
               (when (looking-at ">")
                 (forward-char)
                 (mp-backward-char-skip<>))
               (let* ((ctxt-type (moo-ctxt-type))
                      (ctype (semantic-tag-get-attribute ctxt-type :type)))
                 (mp-backward-char-skip<> -1)
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
                    :test #'moo-tags-same-pos-p))
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
                      function (semantic-calculate-scope pos))))
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
         (moo-get-constructors
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
        (filename (moo-tag-get-filename ttype)))
    (mapcar (lambda (tag) (moo-tag-put-filename tag filename))
            (moo-filter-tag-by-class 'function
                                     (moo-filter-tag-by-name
                                      str
                                      (moo-ttype->tmembers ttype))))))

(defun moo-filter-tag-by-name (sname members)
  (filter (lambda (tag) (string= (car tag) sname))
            members))

(defun moo-filter-tag-by-class (class members)
  (filter (lambda (tag) (semantic-tag-of-class-p tag class))
            members))

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

(defun fa-tfunction->fal (sm)
  (let ((filename (moo-tag-get-filename sm))
        (position (moo-tag-get-position sm))
        (name (pop sm))
        (name-e (pop sm)))
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
            (:typemodifiers             ;what's this?
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
         (mapcar #'fa-tvar->cons
                 (mapcar (lambda (x) (if (string= (car x) "") (setcar x "")) x) arguments-p)))))))

(defun fa-tvar->cons (sm)
  (let ((name (pop sm))
        (name-e (pop sm)))
    (if (not (eq name-e 'variable))
        (error "not a variable")
      (let ((r (pop sm))
            (s "")
            item
            reference-p
            constant-p
            typemodifiers-p
            pointer-p
            type-p
            dereference-p)
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
            (t (error (concat "unknown token" (prin1-to-string item))))))
        (cons (concat (and constant-p "const ")
                      (fa-ttype->str type-p))
              (concat (and reference-p "&")
                      (and pointer-p "*")
                      ;; pretty up std:: identifiers
                      (replace-regexp-in-string "^_+" "" name)
                      (and dereference-p "[]")))))))

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

(defun moo-ctxt-type ()
  (let ((ctxt (semantic-analyze-current-context (point))))
    (if (null ctxt)
        (error "nothing under cursor")
      (setq ctxt (car (oref ctxt prefix)))
      (cond ((stringp ctxt)
             ctxt)
            ;; check if variable constructor initialization is mistaken
            ;; for function prototype definition:
            ((and (eq (nth 1 ctxt) 'function)
               (semantic-tag-get-attribute ctxt :prototype-flag)
               (let ((arg1 (caar (semantic-tag-get-attribute ctxt :arguments))))
                 (and arg1 (stringp arg1) (string= arg1 ""))))
             (semantic-tag-get-attribute ctxt :type))
            (t
             ctxt)))))

(defun moo-get-constructors (type)
  (let ((enump (moo-tenum->tmembers type)))
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
      (filter #'moo-tag-constructor-p
              (moo-get-member-functions type))))))

(defun moo-tag-constructor-p (tag)
  (semantic-tag-get-attribute tag :constructor-flag))

(defun moo-stype->tag (str)
  (unwind-protect
      (let (retval)
        (let* ((position (point))
               (scope (semantic-calculate-scope position)))
          (condition-case ex
              (setq retval (catch 'unfindable
                             (semantic-analyze-find-tag-sequence
                              str scope 'prefixtypes 'unfindable)))
            ('error nil))
          ;; TODO: check (= (length retval) 1)
          (car retval)))))

(defun moo-tenum->tmembers (tag)
  (let ((stag (and (semantic-tag-of-class-p tag 'type)
                   (semantic-tag-get-attribute tag :type))))
    (and (stringp stag)
         (string= stag "enum")
         (semantic-tag-get-attribute tag :members))))

(defun moo-get-member-functions (ttype)
  (and (semantic-tag-of-class-p ttype 'type)
       (filter (lambda(tag) (semantic-tag-of-class-p tag 'function))
               (moo-ttype->tmembers ttype))))

(defun moo-ttype->tmembers (ttype)
  (apply #'append
         (cons
          ;; own
          (let ((own-members
                 (cl-delete-if (lambda (tag) (and (stringp (car tag)) (string= (car tag) "public")))
                  (semantic-tag-get-attribute ttype :members))))
            (apply #'append
                   own-members
                   ;; members of enums join the containing type members
                   (mapcar #'moo-tenum->tmembers own-members)))
          ;; inherited
          (mapcar (lambda (parent-tag)
                    ;; parent-tag's only useful name is the name
                    (let ((parent-name (car parent-tag)))
                      (setq parent-tag
                            (or (gethash parent-name fa-superclasses)
                                (puthash parent-name (moo-stype->tag parent-name) fa-superclasses))))
                    ;; don't inherit constructors
                    (cl-delete-if #'moo-tag-constructor-p
                               (moo-ttype->tmembers parent-tag)))
                  (moo-ttype->tsuperclasses ttype)))))

(defun moo-ttype->tsuperclasses (ttype)
  (semantic-tag-get-attribute ttype :superclasses))

(defun moo-handle-completion (prefix candidates)
  (cond
   ((null candidates)
    (message "there is no completions, only Zuul"))
   ;; either one candidate or multiple with same name:
   ((or (= 1 (length candidates))
        (cl-reduce (lambda (x1 x2) (and x1 (string= (car x1) (car x2)) x1)) candidates))
    ;; TODO: add bounds to this
    (while (not (looking-back prefix))
      (forward-sexp))
    (let ((str (caar candidates))
          (case-fold-search nil))
      (if (looking-back (substring str 0 (length prefix)))
          (insert (substring str (length prefix)))
        (backward-kill-word 1)
        (insert str)))
    (when (eq (cadar candidates) 'function)
      (insert "()")
      (backward-char)))
   ;; multiple candidates with different names
   (t
    (let* ((completion-ignore-case (string= prefix (downcase prefix)))
           (tc (try-completion (or prefix "") candidates)))
      (if (and (stringp tc) (not (string= tc (or prefix ""))))
          (progn
            (unless (string= prefix "")
              (backward-kill-sexp))
            (insert tc))
        (with-output-to-temp-buffer "*Completions*"
          (display-completion-list
           (mapcar semantic-ia-completion-format-tag-function candidates))))))))

(defun moo-sname->tag (str-name)
  (let* ((scope (semantic-calculate-scope (point)))
         (var-tag (semantic-analyze-select-best-tag
                   (semantic-scope-find str-name nil )))
         (type-tag (semantic-analyze-tag-type var-tag scope)))
    type-tag))

(defun moo-dereference-typedef (tag scope)
  "if tag is a typedef, search for it in scope."
  (let ((typedef-p (semantic-tag-get-attribute tag :typedef))
        defs)
    (if (null typedef-p)
        tag
      (setq defs (filter `(lambda (x) (and (eq (cadr x) 'type)
                                      (string= (car x) ,(car typedef-p))))
                         (semantic-tag-get-attribute scope :members)))
      (if (eq (length defs) 1)
          (car defs)
        (error "typedef has multiple definitions")))))

(defun moo-tvar->ttype (var-tag)
  (let* ((var-name (car var-tag))
         (var-stype (car (semantic-tag-get-attribute var-tag :type)))
         (type-tag (moo-stype->tag var-stype))
         (type-typedef-p (semantic-tag-get-attribute type-tag :typedef)))
    (if type-typedef-p
        (moo-sname->tag var-name)
      type-tag)))

(defun filter (pred lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall pred x) x)) lst)))

(defun moo-tag-get-scope (tag)
  (caadr (cl-find-if (lambda (x) (and (listp x) (eq (car x) 'scope))) tag)))

(defun moo-desperately-find-sname (stag)
  (let* ((file-tags (semantic-fetch-tags))
         (own-tags (moo-filter-tag-by-name stag file-tags))
         (include-tags (filter (lambda (tag) (semantic-tag-of-class-p tag 'include))
                               file-tags))
         (include-filenames (delq nil (mapcar #'semantic-dependency-tag-file include-tags))))
    (apply #'append 
           (apply #'append
                  (mapcar (lambda (filename)
                            (moo-tag-put-filename-to-types
                             (filter (lambda (tag) (string= (car tag) stag))
                                     (semantic-file-tag-table filename))
                             filename))
                          include-filenames))
           (list own-tags))))

(defmacro mp-backward-char-skip<> (&optional arg)
  "Moves point backward until [A-Za-z_0-9] is encountered.
Skips anything between matching <...>"
  (let ((dir (if arg -1 1))
        (char-inc (if arg ?< ?>))
        (char-dec (if arg ?> ?<)))
    `(progn
       (backward-char ,dir)
       ;; TODO: look into `c-backward-<>-arglist'
       (while (not (looking-at "[A-Za-z_0-9]"))
         (if (eq (char-after) ,char-inc)
             (let ((n 1)
                   (bound (- (point) 400)))
               (while (and (> n 0) (> (point) bound))
                 (backward-char ,dir)
                 (case (char-after)
                   (,char-inc (cl-incf n))
                   (,char-dec (cl-decf n)))))
           (backward-char ,dir))))))

(provide 'function-args)
;;; function-args.el ends here
