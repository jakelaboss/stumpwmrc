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


  ;; (defcommand push-key (x) ((:string x))
  ;;   (send-fake-key (current-window) (parse-key x)))

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
                ;; Old config
                ;; (read-file-into-string "/home/jake/quicklisp/dists/quicklisp/software/stumpwm-20181018-git/keysyms.lisp")
                (read-file-into-string "/home/jake/Downloads/stumpwm/keysyms.lisp")
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


#| TODO

1. create interactive keymap to save macro commands
1a. Keymap should allow for nesting if there is a keymap already defined.
It should also check if a sequence is already taken.

2. Create system to save a restore macros? This should probably be done with sosei but can likely just be done by writing to the file system

3. Restore macros on write
|#

(defun store-command (filename)
  (let ((file (cat "~/common-lisp/stumpwmrc/storage/lisp/" filename)))
    (if (directory file) (error "command already exists")
        (with-open-file (s file :direction :output)
          (write *keybindings-commands* :stream s)))))

(defcommand restore-command (filename) ((:string filename))
  (play-commands
   (reverse (read-from-string
             (read-file-into-string
              (cat "~/common-lisp/stumpwmrc/storage/lisp/" filename))))))

;; TODO memoize restore-command
;; (memoize restore-command)

(ql:quickload :fare-memoization)
(use-package :fare-memoization)

(memoize 'restore-command)

(defun define-macro-command (map key-seq command-name)
  (store-command command-name)
  (define-key map key-seq (format nil "restore-command ~a" command-name)))

(defun detect-sequence (key-seq map)
  (find key-seq
        (kmap-bindings map)
        :test 'equalp
        :key (lambda (x)
               (binding-key x))))

;; So if the detected sequence command is also a map then ask go to the next item?
;; If we don't have multiple
;; Support two different inputs
;; 1. "s-m o" ;; can be a keymap and then an input
;; 2. "s-l" ;; or just a standard input/key-sequence

;; Let's make sure we check if it's not either of those,
;; or if it's something already bound
;; or if it's something that just shouldn't be reasonably bound
(defcommand save-current-macro (key-seq name)
    ((:string "Input key-seq: ") (:string "What is this command called?: "))
  (labels ((parse-key-seqs (seq)
             (mapcar #'(lambda (x) (car (parse-key-seq x)))
                     (cl-ppcre:split " " seq))))
    (let* ((key-seq (print (parse-key-seqs key-seq)))
           (first-seq (detect-sequence (car key-seq) *top-map*))
           (binding (if first-seq
                        (eval (binding-command first-seq)))))
      ;; Okay so what am I checking first
      ;; if the first seq is a binding and has 2 parts cont
      ;; if the first seq is a binding but has only one part we should error
      ;; if the first-seq is not a binding but there are two parts we should error
      ;; if the first seq is not a binding and has only one part we are good
      (cond ((and (cdr key-seq) binding)
             (if (kmap-p binding)
                 ;; So this is to detect if our new sequence already has a binding
                 ;; this is where we actually define the key
                 (if (detect-sequence (cadr key-seq) binding)
                     (define-macro-command binding (cadr key-seq) name)
                     (define-macro-command binding (cadr key-seq) name))))
            ((and (null (cdr key-seq)) (null binding))
             ;; Let's check this for sanity
             ;; does the key-seq have resonable meta-tags?
             (if (key-mods-p (car key-seq))
                 (define-macro-command *top-map* (car key-seq) (print name))))))))

(define-key *top-map* (kbd "s-\\") "save-current-macro")

(defcommand end-macro-def () ()
  (if (member 'key-recorder-fn *key-press-hook* :test 'equal)
      (progn
        (remove-hook *key-press-hook* 'key-recorder-fn)
        (pop *keybindings-commands*)
        (removef *keybindings-commands* "replay-macros"  :test 'equal)))
  (exit-interactive-keymap 'macro-keymap))

;; Key definitions
(define-key *top-map* (kbd "s-(") "macro-keymap")
(define-key *top-map* (kbd "s-)") "end-macro-def")
(define-key *top-map* (kbd "s-'")  "replay-macros")


(defcommand keyboard-interactive-reset () ()
  (define-key *top-map* (kbd "s-(") "macro-keymap")
  (define-key *top-map* (kbd "s-)") "end-macro-def")
  (define-key *top-map* (kbd "s-'")  "replay-macros")
  (def-interactive-keymap))

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
