;;-------~---~----------~----------~----;;
;; Record Your Own Macros ;;
;;-------~---~----------~----------~----;;

(in-package :stumpwm)

;; so one possiblity is to just remap the top-map, along with every other
;; key, to a macro-map, where anything that isn't top-map bound get's entered into
;; a sequence
(defvar *keybindings-recording* '() "List of bindings to record/replay.")
(defvar *keybindings-sequence* '() "List of bindings to record/replay.")
(defvar *keybindings-commands* '() "List of bindings to record/replay.")

;; (defcommand push-key (x) ((:string x))
;;   (send-fake-key (current-window) (parse-key x)))

(defcommand push-key (x) ((:string x))
  (window-send-string x))

(defcommand push-meta-key (key) ((:string key))
  (send-fake-key (current-window) (parse-key key)))

(defcommand push-space () ()
  (send-fake-key (current-window) (make-key :keysym 32)))

(defcommand push-keycode (code) ((:number code))
  (send-fake-key (current-window) (make-key :keysym code)))

(defun play-command (command)
  "Given a command, send it to the current window."
  (cond ((stringp command) (eval-command command))
        (t (send-fake-key (current-window) command))))

(defun play-commands (commands)
  "Given a list of commands, send them to the current window."
  (mapcar 'play-command commands))

(defun key-recorder-fn (key key-seq cmd)
  "Add the key to the bindings recording."
  (if (stringp cmd)
      (push cmd *keybindings-commands*)))

(defcommand reset-macro () ()
  (setf *keybindings-commands* nil))

(defun start-macros ()
  "Start the key bindings recording."
  (reset-macro)
  (add-hook *key-press-hook* 'key-recorder-fn))

;; Stop macro is not always called
(defun stop-macros ()
  "Start the key bindings recording"
  (if *keybindings-commands*
      (progn
        (message "stopping macro definition")
        ;; stop recording
        (remove-hook *key-press-hook* 'key-recorder-fn)
        ;; remove last call
        (pop *keybindings-commands*)
        ;; Check and remove any replay-macro
        (setf *keybindings-commands* (remove "replay-macros" *keybindings-commands*  :test 'equal)))
      (message "No macro defined...")))

(defcommand replay-macros () ()
  "Start the bindings recording"
  (if (or (member 'key-recorder-fn *key-press-hook* :test 'equal) (member "replay-macros" *keybindings-commands* :test 'equal))
      (message "Macro being defined or bad")
      (if *keybindings-commands*
          (progn (message "Replay macros...")
                 (play-commands (reverse *keybindings-commands*)))
          (message "No macro defined..."))))

(defun check-file-for-keys (file start)
  (multiple-value-bind (s end) (cl-ppcre:scan "#x.+" file :start start)
    (if s (cons (cl-ppcre:scan-to-strings "#x.+" file :start start)
                (check-file-for-keys file end))
        nil)))

;; Because the *key-press-hook* only activates for the current keymap
;; we define a keymap that has coverage for virtually all keys, as well as
;; commands to
(defmacro define-interactive-keymap
    (name (&key on-enter on-exit abort-if (exit-on '((kbd "RET")
                                                     (kbd "ESC")
                                                     (kbd "C-g"))))
     &body key-bindings)
  "Declare an interactive keymap mode. This can be used for developing
interactive modes or command trees, such as @command{iresize}.

The NAME argument follows the same convention as in @command{defcommand}.

ON-ENTER and ON-EXIT are optional functions to run before and after the
interactive keymap mode, respectively. If ABORT-IF is defined, the interactive
keymap will only be activated if calling ABORT-IF returns true.

KEY-BINDINGS is a list of the following form: ((KEY COMMAND) (KEY COMMAND) ...)

Each element in KEY-BINDINGS declares a command inside the interactive keymap.
Be aware that these commands won't require a prefix to run."
  (let* ((command (if (listp name) (car name) name))
         (exit-command (format nil "EXIT-~A" command))
         (keymap (gensym "m")))
    (multiple-value-bind (key-bindings decls docstring)
        (parse-body key-bindings :documentation t)
      `(let ((,keymap (make-sparse-keymap)))
         ,@(loop for keyb in key-bindings
                 collect `(define-key ,keymap ,@keyb))
         ,@(loop for keyb in exit-on
                 collect `(define-key ,keymap ,keyb ,exit-command))

         (defcommand ,name () ()
           ,@decls
           ,(or docstring
                (format nil "Starts interactive command \"~A\"" command))
           ,@(when abort-if `((when (funcall ,abort-if)
                                (return-from ,command))))

           ,@(when on-enter `((funcall ,on-enter)))
           (enter-interactive-keymap ,keymap (quote ,command)))

         (defcommand ,(intern exit-command) () ()
           ,@(when on-exit `((funcall ,on-exit)))
           (exit-interactive-keymap (quote ,command)))))))


(defmacro def-interactive-keymap ()
  `(define-interactive-keymap (macro-keymap tile-group) (:on-enter #'start-macros :on-exit #'stop-macros :exit-on nil)
     ;; Obscure keycodes like backspace
     ,@(mapcar #'(lambda (x)
                 (let ((key (keysym-name->keysym
                             (subseq (reverse (subseq (reverse (cl-ppcre:scan-to-strings "\".+\"" x)) 1)) 1))))
                   `((make-key :keysym ,key) ,(format nil "push-keycode ~a" key))))
               (check-file-for-keys
                (read-file-into-string "/home/vagabond/quicklisp/dists/quicklisp/software/stumpwm-20181018-git/keysyms.lisp")
                0))

     ;; letters on the keyboard
     ,@(mapcan #'(lambda (x) `(((kbd ,(format nil "~a" x)) ,(format nil "push-key ~a" x))
                           ((kbd ,(format nil "C-~a" x)) ,(format nil "push-meta-key \"C-~a\"" x))
                          ((kbd ,(format nil "M-~a" x)) ,(format nil "push-meta-key \"M-~a\"" x))))

               (loop for x across "aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ123456789'!@#$%^&*[]-+\\(){}|\"" collect x))

     ;; space gets it's own command
     ((make-key :keysym 32) "push-space")
     ((kbd "ESC") "push-keycode 65307")
     ((kbd "C-Tab") "push-meta-key \"C-Tab\"")

     ;; Use the top-map commands too
     ,@(mapcar #'(lambda (x)
                   `((kbd ,(if #1=(key-super (binding-key x))
                               (format nil "s-~a"
                                       #2=(keysym->keysym-name (key-keysym (binding-key x))))
                               #2#))
                     ,(binding-command x)))
               (kmap-bindings *top-map*))))

(def-interactive-keymap)

(defcommand keyboard-interactive-reset () ()
  (def-interactive-keymap))

(defun save-macro-command (filename)
  (sosei:pwrite* filename *keybindings-commands*))

;; (save-macro-command "group-two")
;; (sosei:pread* "group-two")

;; (defun this ()
;;   (define-key map key-seq )
;;   )

;; (define-interactive-keymap (save-macro-definition ))

(defcommand end-macro-def () ()
  (if (member 'key-recorder-fn *key-press-hook* :test 'equal)
      (progn (remove-hook *key-press-hook* 'key-recorder-fn)
             (pop *keybindings-commands*)
             (removef *keybindings-commands* "replay-macros"  :test 'equal)))
  (exit-interactive-keymap 'macro-keymap))

;; Key definitions
(define-key *top-map* (kbd "s-(") "macro-keymap")
(define-key *top-map* (kbd "s-)") "end-macro-def")
(define-key *top-map* (kbd "s-'")  "replay-macros")

(def-interactive-keymap)

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Number arguments
;;------------------------------------------------------------------------------------------------------------------------ ;;
(defvar *dotimes-commands* '() "List of bindings to exec number-of times.")

(defun number-times-record-fn (key key-seq cmd)
  "Add the key to the bindings recording."
  (declare (ignorable key key-seq))
  (if (stringp cmd)
      (push cmd *dotimes-commands*)))

(defcommand reset-dotimes () ()
  (setf *dotimes-commands* nil))

(defun number-times (number)
  (bt:make-thread #'(lambda ()
                      (reset-dotimes)
                      (add-hook *key-press-hook* 'number-times-record-fn)
                      (loop until *dotimes-commands* do (sleep .001))
                      (progn (dotimes (x number)
                               (eval-command (car (reverse *dotimes-commands*))))
                             (remove-hook *key-press-hook* 'number-times-record-fn)))))


(define-key *top-map* (kbd "s-2") "eval (stumpwm::number-times 2)")
(define-key *top-map*  (kbd "s-3") "eval (stumpwm::number-times 3)")
(define-key *top-map*  (kbd "s-4") "eval (stumpwm::number-times 4)")
(define-key *top-map*  (kbd "s-5") "eval (stumpwm::number-times 5)")
(define-key *top-map*  (kbd "s-6") "eval (stumpwm::number-times 6)")
(define-key *top-map*  (kbd "s-7") "eval (stumpwm::number-times 7)")
(define-key *top-map*  (kbd "s-8") "eval (stumpwm::number-times 8)")
(define-key *top-map* (kbd "s-9") "eval (stumpwm::number-times 9)")


(def-interactive-keymap)
