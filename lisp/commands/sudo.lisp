;; ; -*- Mode: Lisp; Mode: StumpWM; -*-
;; ; .stumpwmrc
;; (in-package :stumpwm)

;; (setq *startup-message* nil)
;; (setq *startup-mode-line* t)

;; (loop for file in '("battery" "notifications")
;;       do (load (make-pathname :name file :type "lisp" :directory
;;                               '(:relative ".stumpwm.d" "contrib"))))

; --- process management ----------------------------------------
(defun ps-exists (ps)
  (let ((f "ps -ef | grep ~S | grep -v -e grep -e stumpish | wc -l"))
    (< 0 (parse-integer (run-shell-command (format nil f ps) t)))))

(defun start-uniq-command-ps (command &key options (background t))
  (unless (ps-exists command)
    (run-shell-command
     (concat command " " options " " (when background "&")))))

(defun kill-ps-command (command)
  (format nil "kill -TERM `ps -ef | grep ~S | grep -v grep | awk '{print $2}'`"
          command))

(defun kill-ps (command)
  (run-shell-command (kill-ps-command command)))

;; ; --- stumpwm command definitions -------------------------------
;; (defmacro def-run-or-raise-command (cmd prop)
;;   (let ((cmd-str (string-downcase (symbol-name cmd))))
;;     `(defcommand ,cmd () ()
;;        (run-or-raise ,cmd-str ,prop))))

;; (def-run-or-raise-command gnome-terminal '(:class "Gnome-terminal"))
;; ;(def-run-or-raise-command mlterm '(:class "mlterm"))
;; ;(def-run-or-raise-command urxvt '(:class "URxvt"))
;; (def-run-or-raise-command firefox '(:class "Firefox"))
;; (def-run-or-raise-command soffice '(:class "soffice"))
;; (def-run-or-raise-command virtualbox '(:class "VirtualBox"))

;; (defcommand ps-kill (ps) ((:rest "Process to kill: "))
;;   (kill-ps ps))

;; (defcommand emacs () ()
;;   (run-or-raise "emacs" '(:class "Emacs")))

;; (defcommand nm-applet () ()
;;   (start-uniq-command-ps "nm-applet" :options "--sm-disable"))

;; (defcommand show-battery () ()
;;   (echo-string (current-screen) (run-shell-command "acpi" t)))

;; (defcommand uptime () ()
;;   (echo-string (current-screen) (run-shell-command "uptime" t)))

;; (defcommand gnome-panel () ()
;;   (if (ps-exists "gnome-panel")
;;     (progn (kill-ps "gnome-panel")
;;            (sleep 0.3)
;;            (when *startup-mode-line*
;;              (run-shell-command "stumpish mode-line")))
;;     (progn (when (head-mode-line (current-head))
;;              (mode-line))
;;            (run-shell-command "gnome-panel"))))

;; ; --- run commands  ---------------------------------------
;; (run-shell-command "xsetroot -solid black")
;; (run-shell-command "/opt/atokx3/sample/iiimf_status_hide")
;; (nm-applet)
;; (start-uniq-command-ps "gnome-settings-daemon")
;; (start-uniq-command-ps "gnome-power-manager")
;; (start-uniq-command-ps "gnome-valume-manager")
;; (start-uniq-command-ps "update-notifier")
;; (run-shell-command "~/src/backup/alert.sh")
;; (emacs)

; --- sudo command definitions  --------------------------------
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

(define-sudo-command eon
  (concat "pon em"
          (when (ps-exists "nm-applet")
            (concat "&& sudo " (kill-ps-command "nm-applet"))))
  :output t)

