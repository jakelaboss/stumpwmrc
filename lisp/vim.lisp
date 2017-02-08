;; ;;-------~---~----------~----------~----
;; ;; Evil Mode for Stump using Windows Key ;;
;; ;;-------~---~----------~----------~----

;; Movement Mapping ;;
(stumpwm:define-key *top-map* (stumpwm:kbd "s-j") "move-focus down")
(stumpwm:define-key *top-map* (stumpwm:kbd "s-h") "move-focus left")
(stumpwm:define-key *top-map* (stumpwm:kbd "s-k") "move-focus up")
(stumpwm:define-key *top-map* (stumpwm:kbd "s-l") "move-focus right")

(stumpwm:define-key *top-map* (stumpwm:kbd "s-J") "move-window down")
(stumpwm:define-key *top-map* (stumpwm:kbd "s-H") "move-window left")
(stumpwm:define-key *top-map* (stumpwm:kbd "s-K") "move-window up")
(stumpwm:define-key *top-map* (stumpwm:kbd "s-L") "move-window right")

(stumpwm:define-key *root-map* (stumpwm:kbd "J") "move-window down")
(stumpwm:define-key *root-map* (stumpwm:kbd "H") "move-window left")
(stumpwm:define-key *root-map* (stumpwm:kbd "K") "move-window up")
(stumpwm:define-key *root-map* (stumpwm:kbd "L") "move-window right")

;; Splits WIndows and Frames
(stumpwm:define-key *top-map* (stumpwm:kbd "s-v") "hsplit")
(stumpwm:define-key *top-map* (stumpwm:kbd "s-s") "vsplit")
(stumpwm:define-key *top-map* (stumpwm:kbd "s-r") "remove")
(stumpwm:define-key *root-map* (stumpwm:kbd "q") "kill")
(stumpwm:define-key *top-map* (stumpwm:kbd "s--")"fclear")
(stumpwm:define-key *top-map* (stumpwm:kbd "s-n") "pull-hidden-next")
(stumpwm:define-key *top-map* (stumpwm:kbd "s-p") "pull-hidden-previous")

;; Mouse Commands ;;
(stumpwm:define-key *top-map* (stumpwm:kbd "s-b")"banish")

;; Group Configuration ;;
;; (define-key *top-map* (kbd "s-N") "gnext")
(stumpwm:define-key *root-map* (stumpwm:kbd "N") "gnext")
;; (define-key *top-map* (kbd "s-m") "fullscreen")

;; Eval Commands ;;
(stumpwm:define-key *top-map* (stumpwm:kbd "s-;") "colon")
(stumpwm:define-key *top-map* (stumpwm:kbd "s-:") "eval")

;; (uiop:run-program "sudo mount -t ntfs-3g /dev/sd?n /where/to/mnt)
