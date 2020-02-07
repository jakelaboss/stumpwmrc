

;; Loads All Necessary Libraries and Other Dependencies:
;; Such as the my common-lisp folder



(defun nil let not or

(in-package :stumpwm)

(load "/home/vagabond/quicklisp/setup.lisp")
;; (load "/home/vagabond/quicklisp/dists/quicklisp/software/closer-mop-20170830-git/closer-mop.asd")
;; (load "/home/vagabond/quicklisp/dists/quicklisp/software/inferior-shell-20160929-git/inferior-shell.asd")
;; (load "/home/vagabond/quicklisp/dists/quicklisp/software/usocket-0.7.0.1/usocket.asd")
;; (ql:update-all-dists)

;; Sometimes theres some junk in the config
(asdf:clear-configuration)

(ql:quickload "sb-rotate-byte")
(asdf:oos 'asdf:load-op 'sb-rotate-byte)

(ql:quickload '(
                "closer-mop"
                "inferior-shell"
                "usocket"
                "sb-rotate-byte"
                "ironclad"
                "osicat"
                ;; "sxql"
                "alexandria"
                ;; "stumpwm"
                ;; "ironclad"
                ;; "mito"
                ;; "trivial-ssh"
                "cl-fad"
                "zpng"
                "cl-ppcre"
                "swank"
                "swank-client"
                ;; "hunchentoot"
                ))

;; Clear out the gc
(sb-ext:gc)

(mapcar #'load-module '(
                        "stumptray"
                        "clipboard-history"
                        ;; "swm-gaps"
                        "cpu"
                        "mem"
                        ))




;; (load "~/common-lisp/core.lisp")
;; (defvar al/cl-directory
;;   (directory-namestring
;;    (truename (merge-pathnames (user-homedir-pathname)
;;                               "common-lisp")))
;;   "A directory with initially loaded files.")

;; (defun al/cl-load (filename)
;;   "Load a file FILENAME (without extension) from `cl/init-directory'."
;;   (let ((file (merge-pathnames (stumpwm:concat filename ".lisp")
;;                                al/cl-directory)))
;;     (if (probe-file file)
;;         (load file)
;;       (format *error-output* "File '~a' doesn't exist." file))))

;; (al/cl-load "libraries/core/functions")
