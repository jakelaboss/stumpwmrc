;;-------~---~----------~----------~----
;; Mappings ;;
;;-------~---~----------~----------~----
(in-package :stumpwm)

;; Prefix Key ;;
(set-prefix-key (kbd "s-x"))


;; Interactive Colon ;;
(defcommand colon1 (&optional (initial "")) (:rest)
  (let ((cmd (read-one-line (current-screen) ": " :initial-input initial)))
    (when cmd
      (eval-command cmd t))))

;; Interactive Commands ;;
;; (define-key *top-map* (kbd "s-b")"colon1 sudo pia -a Brazil")
;; Document Reader
(define-key *root-map* (kbd "z") "exec zatura /~/documents/books/lisp/")
;; Browse somewhere
(define-key *root-map* (kbd "b") "colon1 exec qutebrowser http://www.")
;; Ssh somewhere
(define-key *root-map* (kbd "s-s") "colon1 exec xterm -e ssh ")
;; Lock screen
(define-key *root-map* (kbd "C-l") "exec xlock")

;; Pulse Audio Config ;;
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "exec /usr/bin/pulseaudio-ctl up")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "exec /usr/bin/pulseaudio-ctl down")
(define-key *top-map* (kbd "XF86AudioMute") "exec /usr/bin/pulseaudio-ctl mute")

;; Web jump Macro
(defmacro make-web-jump (name prefix)
  `(defcommand ,(intern name) (search) ((:rest ,(concatenate 'string name " search: ")))
     (cond (search
            (substitute #\+ #\Space search)
            (run-shell-command (concatenate 'string ,prefix search))))))

(make-web-jump "google" "firefox-developer-edition --new-tab https://www.google.com/search?q=")
(make-web-jump "stackoverflow" "firefox-developer-edition --new-tab https://www.stackoverflow.com/find?q=")

(load-module "clipboard-history")

(defun poll-selection (&optional (selection :primary))
  (xlib:convert-selection selection
                          :utf8_string
                          (stumpwm::screen-input-window
                           (stumpwm:current-screen))
                          :stumpwm-selection))

(defun string-maxlen (s maxlen)
  (let ((s1 (subseq s 0 (min maxlen (length s)))))
    (if (string-equal s1 s)
        s1
        (stumpwm:concat s1 " ..."))))

(defun replace-special-characters (s)
  (cl-ppcre:regex-replace-all #\Newline (cl-ppcre:regex-replace-all " +" s "+") "+"))

(defcommand google-from-clipboard () ()
  (let ((query (inferior-shell:run/s "xclip -o")))
    (|google|(string-maxlen (replace-special-characters query) 100))))

(defcommand eval-from-clipboard () ()
  (let ((query (inferior-shell:run/s "xclip -o")))
    (message "~a" (eval (read-from-string query)))))

(defcommand eval-to-clipboard () ()
  (restart-case
      (let* ((query (inferior-shell:run/s "xclip -o"))
             (result (unwind-protect (eval (read-from-string query)))))
        (inferior-shell:run/s (format nil "echo \"~a\" | xclip -i -selection \"clipboard\"" result))
        (message "~a" result))
    (lambda () nil)))

;; (xlib:convert-selection (poll-selection :primary))
;; (clipboard-history::basic-get-x-selection :primary)
;; (clipboard-history:start-clipboard-manager)
;; (print clipboard-history::*clipboard-history*)
(defun toggle-mouse-focus ()
  (cond ((equal *mouse-focus-policy* :sloppy)
         (setf *mouse-focus-policy* :click)
         (message "click"))
        ((equal *mouse-focus-policy* :click)
         (setf *mouse-focus-policy* :sloppy)
         (message "sloppy"))
        (t (message "State not recognized"))))

(defcommand toggle-mouse-policy () ()
  (toggle-mouse-focus))

(setf *mouse-focus-policy* :sloppy) ;; :click, :ignore, :sloppy

(defun flat-list (l)
  "Function that 'flatten a list."
  (cond ((null l) nil)
        #+sbcl((eq (type-of l) 'sb-impl::comma)
               (flat-list (sb-impl::comma-expr l)))
        ((atom l) (list l))
        (t (append (flat-list (first l))
                   (flat-list (rest l))))))
