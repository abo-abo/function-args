;;; function-args.el --- C++ completion for GNU Emacs

;; Copyright (C) 2013  Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; Version: 0.1

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
;; This extension provides two main commands that are useful for c++-mode:
;; 
;; * `fa-show' -- show an overlay hint with current function arguments
;; * `moo-complete' -- this is essentially a c++-specific version
;;   of `semantic-ia-complete-symbol'.


(require 'semantic/ia)
(require 'semantic/db-find)
(semantic-mode 1)

;; ——— Setup —————————————————————————————————————————————————————————————————————————
(add-hook
 'c++-mode-hook
 (lambda()
   (define-key c++-mode-map (kbd "M-o") 'moo-complete)
   (define-key c++-mode-map (kbd "M-i") 'fa-show)
   (define-key c++-mode-map (kbd "M-n") (fa-idx-cycle 1))
   (define-key c++-mode-map (kbd "M-h") (fa-idx-cycle -1))
   (define-key c++-mode-map (kbd "M-u") 'fa-abort)))

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

;; ——— Internal variables ————————————————————————————————————————————————————————————
(defvar fa-overlay nil
  "Hint overlay instance.")

(defvar *fa-hint-pos* nil
  "Point position where the hint should be (re-) displayed.")

(defvar *fa-beg-pos* nil
  "Position of ( after `fa-start-tracking' was invoked.")

(defvar *fa-end-pos* nil
  "Position of ) after `fa-start-tracking' was invoked.")

(defvar *fa-lst* nil 
  "Current function arguments variants.")

(defvar *fa-arg* nil
  "Current function argument.")

(defvar *fa-idx* nil
  "Current function arguments variant.")

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
  (setq *fa-lst* (fa-calculate (point)))
  (if (eq (length *fa-lst*) 0)
      (message "nothing found")
    (forward-char)
    (setq *fa-idx* 0)
    (fa-update-arg)
    (setq *fa-hint-pos* (point))
    (fa-do-show)
    (fa-start-tracking point)))

(defmacro fa-idx-cycle (arg)
  "Cycle `*fa-idx*' by ARG and update the hint."
  `(lambda ()
     (interactive)
     (setq *fa-idx*
        (mod (+ *fa-idx* ,arg)
             (length *fa-lst*)))
     (fa-update-arg)
     (fa-do-show)))

(defun fa-abort (&optional msg)
  "Stop tracking the cursor and remove the overlay."
  (interactive)
  (remove-hook 'after-change-functions
               'fa-after-change)
  (remove-hook 'before-change-functions
               'fa-before-change)
  (when (overlayp fa-overlay)
    (delete-overlay fa-overlay)))

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
              tmembers)
          (let* ((var-tag (save-excursion
                            (search-backward var-name)
                            (semantic-analyze-interesting-tag
                             (semantic-analyze-current-context (point)))))
                 (var-pointer-p (semantic-tag-get-attribute var-tag :pointer)))
            (cond
             ;; Type::member
             ((looking-back "::\\(?:[A-Za-z][A-Za-z_0-9]*\\)?")
              (if (eq (nth 1 var-tag) 'function)
                  (setq var-tag (moo-sname->tag var-name)))
              (setq tmembers (moo-ttype->tmembers var-tag)))
             ;; is it a smart pointer?
             ((and var-used-as-pointer-p (not var-pointer-p))
              (let ((type-template (semantic-tag-get-attribute
                                    (semantic-tag-get-attribute var-tag :type) :template-specifier)))
                ;; assume that the first template parameter is the relevant one
                ;; (normally, there should be only one anyway)
                (setq tmembers (moo-stype->tmembers (caar type-template)))))
             ;; otherwise just get its type
             (t
              (setq tmembers (moo-tvar->tmembers var-tag))))
            (let ((candidates
                   (delete-dups
                    (filter (lambda(x) (eq 0 (cl-search mem-name (car x))))
                            tmembers))))
              (moo-handle-completion mem-name candidates)))))
       ;; ———  ————————————————————————————————————————————————————————————————————————
       ((= (length symbol) 1)
        (let* ((sym-name (car symbol))
               (candidates
                (or (semantic-analyze-possible-completions
                     (semantic-analyze-current-context pos))
                (and (featurep 'semantic/db)
                     (semanticdb-minor-mode-p)
                     (semanticdb-fast-strip-find-results
                      (semanticdb-deep-find-tags-for-completion sym-name))))))
          (moo-handle-completion sym-name candidates)))
       ;; ———  ———————————————————————————————————————————————————————————————————————
       (t
        (semantic-ia-complete-symbol (or pos (point))))))))

;; ——— Internals —————————————————————————————————————————————————————————————————————

(defun fa-do-show (&optional arg)
  "Show function arguments hint.
ARG determines if the hint above/below the point
preference `fa-hint-position-below' is inverted."
  (interactive "p")
  (save-excursion
    (goto-char *fa-hint-pos*)
    (save-excursion
      (let ((str (fancy-string (- (point) (line-beginning-position)))))
        (setq str
              (if (eq fa-hint-position-below (or (null arg) (eq arg 1)))
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
        (overlay-put fa-overlay 'after-string "")))))

(defun fa-start-tracking (point)
  (interactive "P")
  (let ((beg (save-excursion (re-search-backward "(" nil t) (point)))
        (end (save-excursion (re-search-forward ")" nil t) (- (point) 1))))
    (setq *fa-beg-pos* beg)
    (setq *fa-end-pos* end)
    (add-hook 'after-change-functions
              'fa-after-change)))

(defun fa-update-arg ()
  "Update `*fa-arg*' if it needs to be updated.
Return non-nil if it was updated."
  (let ((argn (semantic-ctxt-current-argument)))
    (cond ((numberp argn)
           (when (and (>= argn 1)
                      (< argn (length (nth *fa-idx* *fa-lst*))))
             (if (eq *fa-arg* (1- argn))
                 nil
               (setq *fa-arg* (1- argn)))))
          ((null argn)
           (setq *fa-arg* 0)
           nil)
          (t
           (fa-abort
            (concat
             "bad semantic-ctxt-current-argument: "
             (prin1-to-string argn)))))))

(defun fa-after-change (beg end len)
  (let ((info (format "b=%d, e=%d l=%d; fb=%d, fe=%d, point= %d" beg end len *fa-beg-pos* *fa-end-pos* (point))))
    (if (or (< beg *fa-beg-pos*)
            (> beg *fa-end-pos*))       ; out of range, abort
        ;; work around for when auto-complete-mode is active
        (unless (and (fboundp 'auto-complete-mode) auto-complete-mode
                     (or (and (featurep 'auto-complete) ac-menu)
                         (> (- end beg) 1)))
          (fa-abort))
      (cond      
       ((eq len 0)                      ; insertion
        (incf *fa-end-pos* (- end beg)))
       ((eq beg end)                    ; deletion
        (decf *fa-end-pos* len)))
      (when (fa-update-arg)
      (fa-do-show)))))

(defun fa-calculate (pos)
  "Return current function (or functions in case of overloading) in the form:
 ((name-1 arg-1 arg-2 ...) (name-2 arg-1 arg2 ...) ...)."
  (let* ((function (semantic-ctxt-current-symbol))
         (result
          (cond
           ;; semantic didn't recognize anything
           ;; try a class temp initialization
           ((= 0 (length function))
            (save-excursion
              (mp-backward-char-skip<>)
              (mapcar #'sm->fal (moo-get-constructors
                                 (moo-ctxt-type)))))
           ;; semantic gave just a list with one string - a variable name
           ((= 1 (length function))
            (save-excursion
              (search-backward (car function))
              (let ((ctxt-type (moo-ctxt-type)))
                (cond
                 ;; variable init inside constructor
                 ((and (semantic-tag-p ctxt-type)
                       (semantic-tag-of-class-p ctxt-type 'variable)
                       (looking-back ":[^;]*"))
                  (mapcar #'sm->fal
                          (moo-get-constructors
                           (moo-sname->tag (car function)))))
                 ;; global function invocation
                 ((looking-back "\\(:?;\\|{\\|\\(:?//.*\\)\\)[ \t\n]*")
                  (list (sm->fal ctxt-type)))
                 ;; try to match a variable with a constructor declaration:
                 ;; move to the type
                 (t
                  (mp-backward-char-skip<>)
                  (let* ((ctxt-type (moo-ctxt-type))
                         (scope (moo-get-tag-scope ctxt-type)))
                    (mapcar #'sm->fal 
                            (moo-get-constructors
                             (moo-dereference-typedef ctxt-type scope)))))))))
           ((= 2 (length function))
            (save-excursion
              (search-backward (car function))
              (let* ((ctxt-type (moo-ctxt-type))
                     (ctype (semantic-tag-get-attribute ctxt-type :type)))
                ;; check for A::b
                (if (or (and (stringp ctype) (string= ctype "namespace"))
                        (save-excursion (search-forward (car function))
                                        (looking-at "::")))
                    (or
                     (fa-process ctxt-type
                                 (cadr function))
                     (mapcar #'sm->fal
                             (cl-mapcan
                              `(lambda (tag)
                                 (filter (lambda (tag) (eq (cadr tag) 'function))
                                         (moo-filter-members tag ,(cadr function))))
                              (moo-desperately-find-sname (car function)))))
                  (mapcar #'sm->fal
                          (filter `(lambda(f) (string= (car f) ,(cadr function)))
                                  (if (listp ctype) ; this is a check for A.B or A::B
                                      (moo-stype->tmembers (car ctype))
                                    (moo-get-member-functions ctxt-type)))))))))))
    (or result
        (let ((fntag (semantic-analyze-find-tag-sequence
                      function (semantic-calculate-scope pos))))
          (and fntag
               (not (stringp (car (last fntag))))
               (setq fntag (car (last fntag)))
               (setq result (list (sm->fal fntag))))))))

(defun fa-process (ttype str)
  "Get all functions of TTYPE with name STR.
This includes the constructors of types with name STR."
  (let ((members (moo-filter-members ttype str)))
    (mapcar #'sm->fal
            (apply #'append
                   (mapcar
                    (lambda (tag1)
                      (cond ((semantic-tag-of-class-p tag1 'function)
                             (list tag1))
                            ((semantic-tag-of-class-p tag1 'type)
                             (moo-get-constructors
                              (moo-dereference-typedef tag1 ttype)))
                            ((semantic-tag-of-class-p tag1 'variable)
                             nil)       ; skip
                            (t
                             (error
                              (concat "fa-process "
                                      (prin1-to-string tag1)
                                      (prin1-to-string ttype)
                                      str)))))
                    members)))))

(defconst fa-paren-open  (propertize "(" 'face 'fa-face-semi))
(defconst fa-paren-close (propertize ") : " 'face 'fa-face-semi))
(defconst fa-comma (propertize "," 'face 'fa-face-semi))

(defun fancy-string (wspace)
  "Return the string that corresponds to (nth *fa-idx* *fa-lst*).
WSPACE is the padding."
  (if (< wspace 0)
      (setq wspace 0))
  (let* ((lst (nth *fa-idx* *fa-lst*))
         (n-string
          (if (> (length *fa-lst*) 1)
              (format "[%d of %d] " (+ *fa-idx* 1) (length *fa-lst*))
            ""))
         (padding-length (- wspace (+ 1 (length n-string))))
         (str-width (apply #'+ (mapcar (lambda (x) (+ (length (car x))
                                                 (length (cdr x))))
                                       lst)))
         glue
         args)
    (setq glue (if (> str-width fa-max-one-line-width)
                   ;; each arg on its own line
                   (concat
                    fa-comma
                    "\n"
                    (make-string wspace ? ))
                 fa-comma))
    (when (> (length lst) 1)
      (setq args (mapcar #'fancy-argument
                         (cdr lst)))
      (setcar (nthcdr *fa-arg* args)
              (fancy-argument (nth *fa-arg* (cdr lst)) t)))
    (concat
     (when (> padding-length 0) (make-string padding-length ? ))
     (propertize n-string 'face 'fa-face-hint-bold)
     fa-paren-open
     (and args (mapconcat 'identity args glue))
     fa-paren-close
     ;; template
     (and (caar lst) (propertize (caar lst) 'face 'fa-face-hint))
     ;; name
     (and (cdar lst) (propertize (cdar lst) 'face 'fa-face-type)))))

(defun fancy-argument (cell &optional bold)
  (concat
   (propertize (car cell) 'face
               (if bold 'fa-face-type-bold 'fa-face-type))
   (propertize " " 'face 'fa-face-hint)
   (propertize (cdr cell) 'face
               (if bold 'fa-face-hint-bold 'fa-face-hint))))

(defun sm->fal (sm)
  (let ((name (pop sm))
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
            (t (error (concat "sm->fal unknown token" (prin1-to-string item))))))
        (cons
         ;; name and type part
         (cons (and template-p
                    (concat "template " (sm->template-specifier template-p) " "))
               (if constructor-flag-p
                   name
                 (if type-p
                     (sm->type type-p)
                   "?")))
         ;; arguments part
         (mapcar #'sm->var
                 (mapcar (lambda (x) (if (string= (car x) "") (setcar x "")) x) arguments-p)))))))

(defun sm->var (sm)
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
                      (sm->type type-p))
              (concat (and reference-p "&")
                      (and pointer-p "*")
                      ;; pretty up std:: identifiers
                      (replace-regexp-in-string "^_+" "" name)
                      (and dereference-p "[]")))))))

(defun sm->type (sm)
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
          (concat name (sm->template-specifier
                        template-specifier-p)))))))

(defun sm->template-specifier (sm)
  (and sm
       (concat "<"
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

(defun moo-get-member-functions (type)
  (and (semantic-tag-of-class-p type 'type)
       (let ((members (semantic-tag-get-attribute type :members)))
         (filter (lambda(tag) (semantic-tag-of-class-p tag 'function))
                 members))))

(defun moo-get-constructors (type)
  (let ((enump (moo-tenum->tmembers type)))
    (cond
     ;; enum
     (enump
      `((,(semantic-tag-name type)
        function
        (:arguments
         ((,(mapconcat
             #'car
             enump
             " | ")
           variable (:type ,(semantic-tag-name type))))
         :type
         "enum"))))
     ;; else
     (t 
      (filter (lambda(tag) (semantic-tag-get-attribute tag :constructor-flag))
              (moo-get-member-functions type))))))

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
          retval))))

(defun moo-tenum->tmembers (tag)
  (let ((stag (and (semantic-tag-of-class-p tag 'type)
                   (semantic-tag-get-attribute tag :type))))
    (and (stringp stag)
         (string= stag "enum")
         (semantic-tag-get-attribute tag :members))))

(defun moo-ttype->tmembers (tag-type)
  (apply #'append
         (cons
          ;; own
          (let ((own-members (semantic-tag-get-attribute
                              tag-type
                              :members)))
            (apply #'append
                   own-members
                   ;; members of enums join the containing type members
                   (mapcar #'moo-tenum->tmembers own-members)))
          ;; inherited
          (mapcar #'moo-ttype->tmembers 
                  (moo-ttype->tsuperclasses tag-type)))))

(defun moo-ttype->tsuperclasses (tag-type)
  (semantic-tag-get-attribute tag-type :superclasses))

(defun moo-stype->tmembers (str-type)
  (apply #'append
         (cons
          ;; own
          (semantic-tag-get-attribute
           (car (moo-stype->tag str-type))
           :members)
          ;; inherited
          (mapcar #'identity
                  (mapcar #'moo-stype->tmembers 
                          (moo-stype->ssuperclasses str-type))))))

(defun moo-stype->ssuperclasses (str-type)
  (mapcar #'car
          (semantic-tag-get-attribute (car (moo-stype->tag str-type)) :superclasses)))

(defun moo-handle-completion (prefix candidates &optional formatter)
  (cond ((null candidates)
         (message "there is no completions, only Zuul"))
        
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
        (t
         (let* ((completion-ignore-case (string= prefix (downcase prefix)))
               (tc (try-completion (or prefix "") candidates)))
           (if (and (stringp tc) (not (string= tc (or prefix ""))))
               (progn
                 (backward-kill-sexp)
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

(defun moo-tvar->tmembers (var-tag)
  (let* ((var-name (car var-tag))
         (var-pointer-p (semantic-tag-get-attribute var-tag :pointer))
         (var-stype (car (semantic-tag-get-attribute var-tag :type)))
         (type-tag (car (moo-stype->tag var-stype)))
         (type-typedef-p (semantic-tag-get-attribute type-tag :typedef)))
    (if type-typedef-p
        (moo-ttype->tmembers (moo-sname->tag var-name))
      (moo-stype->tmembers var-stype))))

(defun filter (pred lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall pred x) x)) lst)))

(defun moo-get-tag-scope (tag)
  (caadr (cl-find-if (lambda (x) (and (listp x) (eq (car x) 'scope))) tag)))

(defun moo-desperately-find-sname (stag)
  (let ((file-tags (semantic-fetch-tags)))
    (apply #'append
        (delete-dups
         (mapcar (lambda (tag)
                   (filter (lambda (tag1) (string= (car tag1) stag))
                           (semantic-file-tag-table (semantic-dependency-tag-file tag))))
                 (filter (lambda (tag) (semantic-tag-of-class-p tag 'include))
                         file-tags))))))

(defun moo-filter-members (tag sname)
  (let ((members (semantic-tag-get-attribute tag :members)))
    (filter (lambda (tag) (string= (car tag) sname))
            members)))

(defun mp-backward-char-skip<> ()
  "Moves point backward until [A-Za-z_0-9] is encountered.
Skips anything between matching <...>"
  (backward-char)
  (while (not (looking-at "[A-Za-z_0-9]"))
            (if (eq (char-after) ?>)
                (let ((n 1)
                      (bound (- (point) 400)))
                  (while (and (> n 0) (> (point) bound))
                    (backward-char)
                    (case (char-after)
                      (?> (incf n))
                      (?< (decf n)))))
              (backward-char))))

(provide 'function-args)
