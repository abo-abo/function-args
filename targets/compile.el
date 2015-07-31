(add-to-list 'load-path default-directory)
(mapc #'byte-compile-file '("function-args.el" "semantic-directory.el"))

