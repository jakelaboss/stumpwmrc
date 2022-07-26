(in-package :stumpwm)
(use-package :neuron.code)

(add-hook *selection-notify-hook* 'clipboard-history::save-clipboard-history)

(defparameter clipboard-history::*clipboard-poll-timeout* 30)

(import '(clipboard-history::*clipboard-poll-timeout*
          clipboard-history::*clipboard-timer*
          clipboard-history:start-clipboard-manager
          clipboard-history::poll-selection
          clipboard-history::*clipboard-history*
          clipboard-history::*clipboard-poll-timeout*))

(defun eval-current-selection ()
  (let ((timeout *clipboard-poll-timeout*))
    (when (setf *clipboard-poll-timeout* 0)
      (if (null *clipboard-timer*)
          (start-clipboard-manager))
      (progn
        (map nil #'poll-selection '(:clipboard :primary))
        (let ((result (eval (read-from-string (print (car *clipboard-history*))))))
          (setf *clipboard-poll-timeout* timeout)
          result)))))

(defun get-current-selection ()
  (let ((timeout *clipboard-poll-timeout*))
    (when (setf *clipboard-poll-timeout* 0)
      (if (null *clipboard-timer*)
          (start-clipboard-manager))
      (progn
        (map nil #'poll-selection '(:clipboard :primary))
        (let ((result (car *clipboard-history*)))
          (setf *clipboard-poll-timeout* timeout)
          result)))))

(defcommand eval-from-clipboard () ()
  (unwind-protect (format nil "~a" (unwind-protect (eval-current-selection)))))

(defvar last-generated-response (stmx:tvar nil))

(defmacro fork-gpt (&rest body)
  `(let ((future (lparallel:future ,@body)))
     (lparallel:future
       (stmx:atomic
        (time (loop until (lparallel:fulfilledp future)
               do (sleep .1)
               finally (setf (stmx:$ last-generated-response)
                             (lparallel:force future))))))))

(defun generate-from-current-selection ()
  (let ((timeout *clipboard-poll-timeout*))
    (if (null *clipboard-timer*)
        (start-clipboard-manager))
    (when (setf *clipboard-poll-timeout* 0)
      (progn
        (map nil #'poll-selection '(:clipboard :primary))
        (fork-gpt (neuron.code::generate-text (car *clipboard-history*)))
        (setf *clipboard-poll-timeout* timeout)))))

(defun output-last-generation ()
  (window-send-string (stmx:$ last-generated-response)))

(defun get-last-generation ()
  (stmx:$ last-generated-response))


;; (generate-from-current-selection)
;; (read-from-string
;;  (concat "(quote" (get-last-generation) ")"))

;; to safely turn this into a piece of data we need to do a few things
;; ensure read safety

(defun safe-replace (str)
  (let ((items '( (#\# . "") (#\. ""))) (result str))
    (dolist (x items)
      (setf result (cl-ppcre:regex-replace-all (car x) result (cdr x))))
    result))

;; ensure parentheses matching
(defun paren-matcher (str)
  (labels ((create (b l paren)
             (concatenate 'string (loop for x from 1 to (- b l)
                                   collect paren))))
    (let* ((result str)
           (begin (count #\( result))
           (end (count #\) result)))
      (cond ((= begin end))
            ((> begin end)
             (concat result (create begin end #\))))
            ((> end begin)
             (concat result (create end begin #\()))))))

;; (eval (read-from-string (concat "(list"
;;                            (paren-matcher (safe-replace (get-last-generation))) ")")))

;; (read-from-string (paren-matcher (safe-replace (get-last-generation))))

;; ;; add quotes

;; (quote (this is a test))

;; (safe-re (get-last-generation))
;; (get-current-selection)
;; ()

;; (generate-from-current-selection ())
;; (start-clipboard-manager)
;; (output-last-generation)
;; (print (stmx:$ last-generated-response))
;; (print (stmx:$ last-generated-response))

;; (mod 55 0)


;; (defmacro! with-p (syms o!box &rest body)
;;   `(symbol-macrolet
;;        (,@(mapcar #`(,a1 (get-p ,g!box ',a1))
;; 		 :$unboxed-vars))

;; (p (with-p,(funcall,box :pget-methods),,box
;; 		 :$unboxed-methods))

;; (vbox `(with-p,(funcall,box :unbox),,box
;; 		     :$unboxed-vars)
;;       (pbox `(with-p,(funcall,box :unbox),,box
;; 			         :$unboxed-methods)))
;; ,@body))

;; (p-get-vars)   ; ALLVARS
;; (p-get-methods) ; ALLMETHODS

;; (define-macro (with-exact-p (box expr &optional (constructor :test #'string=))
;;                 &body body)
;;     `(define ,(if (but (define vars
;;                           (if,@(apply-lambda-list body) :fail t))
;;                       (box,expr))
;;                  ,expr)
;;        ,constructor))

;; (p-box expr &body body) ; basic box with test

;; (p-box-p expr &body body) ; basic box with test

;; (p-box-list (box expr &body body) &body body) ; list box with test

;; ;;; <p> -> <list<p>>; :list makes it a list

;; (p-list expr &body body) ; basic list with test

;; (p-list-p expr &body body) ; basic list with test

;; (p-list-list (
