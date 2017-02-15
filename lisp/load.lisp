;;------------------------------------------------------------------------------------------------------------------------ ;;

;; Loads All Necessary Libraries and Other Dependencies:
;; Such as the my common-lisp folder

;;------------------------------------------------------------------------------------------------------------------------ ;;

(dolist (m '(
             "inferior-shell"
             "usocket"
             "sxql"
             "alexandria"
             "ironclad"
             "mito"
             "trivial-ssh"
             "cl-fad"
             "cl-ppcre"
             "hunchentoot"))
  (progn
    (ql:quickload m)))


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
