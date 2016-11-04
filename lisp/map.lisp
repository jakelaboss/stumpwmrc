;;-------~---~----------~----------~----
;; Mappings ;;
;;-------~---~----------~----------~----

;; Prefix Key ;;
(set-prefix-key (kbd "C-z"))


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
(define-key *root-map* (kbd "b") "colon1 exec google-chrome-stable http://www.")
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
    (substitute #\+ #\Space search)
    (run-shell-command (concatenate 'string ,prefix search))))

(make-web-jump "google" "google-chrome-stable http://www.google.fr/search?q=")
(make-web-jump "imdb" "google-chrome-stable http://www.imdb.com/find?q=")
(make-web-jump "stackoverflow" "google-chrome-stable https://www.stackoverflow.com/find?q=")

(define-key *root-map* (kbd "C-f") "stackoverflow")
(define-key *root-map* (kbd "C-s") "google")
(define-key *root-map* (kbd "i") "imdb")
