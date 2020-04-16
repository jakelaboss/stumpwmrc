;; ; -*- Mode: Lisp; Mode: StumpWM; -*-
;; ; .stumpwmrc
(in-package :stumpwm)

(defparameter *startup-message* "Welcome Vagabond")
;; (setq *startup-mode-line* t)

; --- process management ----------------------------------------
(defun ps-exists (ps)
  (let ((f "ps -ef | grep ~S | grep -v -e grep -e stumpish | wc -l"))
    (< 0 (parse-integer (run-shell-command (format nil f ps) t)))))

(defun start-uniq-command-ps (command &key options (background t))
  (unless (ps-exists command)
    (run-shell-command
     (concat command " " options " " (when background "&")))))

(defun kill-ps-command ()
  (format nil "kill -TERM `ps -ef | grep ~S | grep -v grep | awk '{print $2}'`"
          command))

(defun kill-ps (command)
  (run-shell-command (kill-ps-command command)))

(defcommand ps () ()
  (when-let ((network (car (select-from-menu (current-screen)
                                             (mapcar (lambda (g) (list g))
                                                     (cl-ppcre:split "\\n"
                                                                     (sudo-run "netctl list")))
                                             "All running processes"))))
    (sudo-run (format nil "netctl switch-to ~a" network))))

(defcommand pkill () (:rest)
  (when-let ((process (car (select-from-menu (current-screen)
                                             (remove nil
                                                     (mapcar (lambda (x)
                                                               (cdr (cl-ppcre:split "\\d+:\\d+:\\d+ " x)))
                                                             (cl-ppcre:split "\\n"
                                                                             (sudo-run "ps -al"))))
                                             "Choose a process to kill:"))))
    (sudo-run (format nil "pkill ~a" process))))


; --- sudo command definitions  --------------------------------

;; 1,228,800,000,000

(ql:quickload :ironclad)

(defun get-cipher (key)
  (ironclad:make-cipher :threefish1024
                        :mode :ecb
                        :key (ironclad:ascii-string-to-byte-array key)))

(defun encrypt (plaintext key)
  (let ((cipher (get-cipher key))
      (msg (ironclad:ascii-string-to-byte-array plaintext)))
    (ironclad:encrypt-in-place cipher msg)
    (ironclad:octets-to-integer msg)))

;;Example for encrypt
;; (with-open-file (s "filename" :direction :output :if-exists :supersede :if-does-not-exist :create)
;;   (write (encrypt "thisisanexamplepassword" "passwordkey") :stream s))

(defun decrypt (ciphertext-int key)
  (let ((cipher (get-cipher key))
      (msg (ironclad:integer-to-octets ciphertext-int)))
    (ironclad:decrypt-in-place cipher msg)
    (coerce (mapcar #'code-char (coerce msg 'list)) 'string)))

;; Example decrypt from file
;; (with-open-file (s "filename")
;;   (decrypt  (read s) "passwordkey"))

(defvar *salt-length* (length (decrypt *salt* *lisp-key*)))

(defun sudo-password () (remove-from-end (decrypt *lisp-password* *lisp-key*) *salt-length*))

(defun root-password () (remove-from-end (decrypt *root-password* *lisp-key*) *salt-length*))

(defun check-password (pass)
  (equalp *lisp-password*
          (encrypt (concat pass (decrypt *salt* *lisp-key*)) *lisp-key*)))

(defun sudo-run (command)
  (if (stringp command)
      (inferior-shell:run/ss (format nil
                                     "echo \"~a\" | sudo -S sh -c '~a'"
                                     (sudo-password)
                                     command))
      (error "Command not of type: String")))

(defun sudo-command (command)
  (if (stringp command)
      (inferior-shell:run/ss (format nil
                                     "echo \"~a\" | sudo -S sh -c '~a'"
                                     (sudo-password)
                                     command))
      (error "Command not of type: String")))

;; Stumpwm password type
(define-stumpwm-type :password (input prompt)
  (let ((history *input-history*)
        (arg (argument-pop input))
        (fn (symbol-function 'draw-input-bucket)))
    (unless arg
      (unwind-protect
           (setf (symbol-function 'draw-input-bucket)
                 (lambda (screen prompt input &optional errorp)
                   (let ((i (copy-structure input)))
                     (setf (input-line-string i)
                           (make-string (length (input-line-string i))
                                        :initial-element #\*))
                     (funcall fn screen prompt i)))
                 arg (read-one-line (current-screen) prompt))
        (setf (symbol-function 'draw-input-bucket) fn
              *input-history* history))
      arg)))

(defmacro define-sudo-command (name command &key output)
  (let ((cmd (gensym)))
    `(defcommand ,name (password) ((:password "sudo password: "))
       (let ((,cmd (concat "echo '" password "' | sudo -S " ,command)))
         ,(if output
              `(run-prog-collect-output *shell-program* "-c" ,cmd)
              `(run-prog *shell-program* :args (list "-c" ,cmd) :wait nil))))))

(defmacro define-su-command (name user command &key output)
  (let ((cmd (gensym)))
    `(defcommand ,name (password) ((:password "user password: "))
       (let ((,cmd (concat "echo '" password "' | su -c '" ,command "'" ,user)))
         ,(if output
              `(run-prog-collect-output *shell-program* "-c" ,cmd)
              `(run-prog *shell-program* :args (list "-c" ,cmd) :wait nil))))))

(defcommand send-sudo-password () ()
  (window-send-string (concat (sudo-password) (string #\Newline))))


(defcommand send-root-password () ()
  (window-send-string (concat (root-password) (string #\Newline))))
