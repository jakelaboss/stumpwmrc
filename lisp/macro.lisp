;;-------~---~----------~----------~----;;
;; Record Your Own Macros ;;
;;-------~---~----------~----------~----;;

(defvar *keybindings-recording* '() "List of bindings to record/replay.")

(defun play-command (command)
  "Given a command, send it to the current window."
  (cond ((stringp command) (eval-command command))
        (t                 (send-fake-key (current-window) command))))

(defun play-commands (&rest commands)
  "Given a list of commands, send them to the current window."
  (mapcar 'play-command commands))

(defun key-recorder-fn (key key-seq cmd)
  "Add the key to the bindings recording."
  (push key *keybindings-recording*))

(defcommand start-macros () ()
  "Start the key bindings recording."
  (if *keybindings-recording*
      (message "Already defining keyboard macro")
    (add-hook *key-press-hook* 'key-recorder-fn)))

(defcommand stop-macros () ()
  "Start the key bindings recording"
  (if *keybindings-recording*
      (progn
        ;; stop recording
        (remove-hook *key-press-hook* 'key-recorder-fn)
        ;; remove last call
        (pop *keybindings-recording*))
    (message "No macro defined...")))

(defcommand replay-macros () ()
  "Start the bindings recording"
  (message "Replay macros...")
  (if *keybindings-recording*
      (play-commands *keybindings-recording*)
    (message "No macro defined...")))

(defvar *key-macro-start*  (kbd "s-[") "Binding to start the key bindings recording.")
(defvar *key-macro-stop*   (kbd "s-]") "Binding to stop the key bindings recording.")
(defvar *key-macro-replay* (kbd "s-'") "Binding to replay the key bindings recording")

;; (setf *key-macro-start*  (kbd "("))
;; (setf *key-macro-stop*   (kbd ")"))
;; (setf *key-macro-replay* (kbd "M-e"))

;; (define-key *root-map* *key-macro-start*  "start-macros")
;; (define-key *root-map* *key-macro-stop*   "stop-macros")
;; (define-key *root-map* *key-macro-replay* "replay-macros")
;; (define-key *top-map* (kbd "s-[")  "start-macros")
;; (define-key *top-map* (kbd "s-]")  "stop-macros")
;; (define-key *top-map* (kbd "s-'")  "replay-macros")
