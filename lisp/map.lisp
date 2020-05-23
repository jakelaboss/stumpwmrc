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
  `(defcommand ,(intern name) (sevagabond) ((:rest ,(concatenate 'string name " sevagabond: ")))
    (substitute #\+ #\Space sevagabond)
    (run-shell-command (concatenate 'string ,prefix sevagabond))))

(make-web-jump "google" "qutebrowser http://www.google.fr/sevagabond?q=")
(make-web-jump "imdb" "qutebrowser http://www.imdb.com/find?q=")
(make-web-jump "stackoverflow" "qutebrowser https://www.stackoverflow.com/find?q=")

(define-key *root-map* (kbd "C-f") "stackoverflow")
(define-key *root-map* (kbd "C-s") "google")
(define-key *root-map* (kbd "i") "imdb")

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
;; Sudo Commands ;;


;; Postgres Commands ;;
;; (define-sudo-command pg-start "pg_ctl start -D /var/lib/postgres/data")
;; (define-sudo-command  pg-start "su postgres -c 'pg_ctl start -D /usr/local/pgsql/data -l serverlog'"


;; (defun vpn (conf)
;;   (define-sudo-command vpn (concatenate 'string "openvpn /etc/openvpn/" (concatenate 'string conf ".conf"))))

;; (vpn "Brazil")


(defun flat-list (l)
  "Function that 'flatten a list."
  (cond ((null l) nil)
        #+sbcl((eq (type-of l) 'sb-impl::comma)
               (flat-list (sb-impl::comma-expr l)))
        ((atom l) (list l))
        (t (append (flat-list (first l))
                   (flat-list (rest l))))))
