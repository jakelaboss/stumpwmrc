;; ;;-------~---~----------~----------~----
;; ;; Evil Mode for Stump using Windows Key ;;
;; ;;-------~---~----------~----------~----

;; Movement Mapping ;;
(define-key *top-map* (kbd "s-j") "move-focus down")
(define-key *top-map* (kbd "s-h") "move-focus left")
(define-key *top-map* (kbd "s-k") "move-focus up")
(define-key *top-map* (kbd "s-l") "move-focus right")

(define-key *top-map* (kbd "s-J") "move-window down")
(define-key *top-map* (kbd "s-H") "move-window left")
(define-key *top-map* (kbd "s-K") "move-window up")
(define-key *top-map* (kbd "s-L") "move-window right")

(define-key *root-map* (kbd "J") "move-window down")
(define-key *root-map* (kbd "H") "move-window left")
(define-key *root-map* (kbd "K") "move-window up")
(define-key *root-map* (kbd "L") "move-window right")

;; Splits WIndows and Frames
(define-key *top-map* (kbd "s-v") "hsplit")
(define-key *top-map* (kbd "s-s") "vsplit")
(define-key *top-map* (kbd "s-r") "remove")
(define-key *top-map* (kbd "s-w") "windows")
(define-key *top-map* (kbd "s-f") "windowlist")
(define-key *top-map* (kbd "s-q") "kill")
(define-key *root-map* (kbd "q") "kill")
(define-key *top-map* (kbd "s--")"fclear")
(define-key *top-map* (kbd "s-n") "pull-hidden-next")
(define-key *top-map* (kbd "s-p") "pull-hidden-previous")

;; Mouse Commands ;;
(define-key *top-map* (kbd "s-b")"banish")

;; Group Configuration ;;
;; (define-key *top-map* (kbd "s-N") "gnext")
(define-key *root-map* (kbd "N") "gnext")
;; (define-key *top-map* (kbd "s-m") "fullscreen")

;; Eval Commands ;;
(define-key *top-map* (kbd "s-;") "colon")
(define-key *top-map* (kbd "s-:") "eval")
