;;; semantic-directory.el --- Get a cached list of semantic tags for many files  -*- lexical-binding: t -*-

;; Copyright (C) 2015 Oleh Krehel

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

;;; Code:

(require 'semantic)
(require 'semantic/db)

(defvar sd-db (make-hash-table :test 'equal)
  "An alist of file to a pretty list of tags.")

(cl-defstruct sd-dbfile
  file
  tags
  modtime
  plain-tags)

(defun sd-file-fresh-p (actual-time stored-time)
  "Return t when ACTUAL-TIME isn't much larger than STORED-TIME."
  (and stored-time
       (< (time-to-seconds
           (time-subtract
            actual-time
            stored-time))
          1.0)))

(defvar sd-force-reparse nil
  "When non-nil, ignore that tags are up-to-date and parse anyway.")

(defun sd-fetch-this-file-tags (&optional file)
  "Fetch tags for FILE."
  (setq file (or file (buffer-file-name)))
  (semantic-new-buffer-fcn)
  (let ((tags (semantic-parse-region (point-min) (point-max))))
    tags))

(defun sd-fetch-tags (file-list)
  "Get a list of tags for FILE-LIST."
  (let ((file-count (length file-list))
        (i 0)
        res dbfile db-to-save)
    (dolist (file file-list)
      (message "Parsing ... (%d/%d)" (cl-incf i) file-count)
      (let ((file-modtime (nth 5 (file-attributes file 'integer)))
            (exfile (expand-file-name file)))
        (unless (and (null sd-force-reparse)
                     (setq dbfile
                           (gethash exfile sd-db))
                     (sd-file-fresh-p
                      file-modtime
                      (sd-dbfile-modtime dbfile))
                     (sd-dbfile-tags dbfile))
          (let ((table (semanticdb-create-table-for-file (expand-file-name file))))
            (if (null table)
                (error "Couldn't open semanticdb for file: %S" file)
              (let ((db (car table))
                    (table (cdr table))
                    tags)
                (unless (and (null sd-force-reparse)
                             (sd-file-fresh-p
                              file-modtime
                              (oref table lastmodtime))
                             (setq tags
                                   (ignore-errors
                                     (oref table tags)))
                             (semantic-tag-overlay (car-safe tags))
                             (not (eq (cadr (car-safe tags)) 'code)))
                  (let ((buf (get-file-buffer file)))
                    (with-current-buffer (or buf (find-file-noselect file))
                      (semantic-new-buffer-fcn)
                      (semantic-mode 1)
                      (oset table tags
                            (let ((semantic-parse-tree-state 'needs-update))
                              (sd-fetch-this-file-tags file)))
                      (oset table lastmodtime
                            (current-time))
                      (semanticdb-set-dirty table)
                      (cl-pushnew db db-to-save)
                      (unless buf
                        (kill-buffer)))))
                (puthash
                 exfile
                 (setq dbfile
                       (make-sd-dbfile
                        :file file
                        :modtime (oref table lastmodtime)
                        :tags (mapcar
                               (lambda (x)
                                 (semantic-tag-put-attribute x :truefile exfile))
                               (oref table tags))
                        :plain-tags (oref table tags)))
                 sd-db)))))
        (setq res (append (sd-dbfile-tags dbfile) res))))
    (dolist (db db-to-save)
      (semanticdb-save-db db))
    res))

(provide 'semantic-directory)

;;; semantic-directory.el ends here
