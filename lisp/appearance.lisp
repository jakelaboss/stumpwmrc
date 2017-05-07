;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Appearance ::
;;------------------------------------------------------------------------------------------------------------------------ ;;

;; Wallpaper Sets ;;

(defvar *wallpapers-4k* "/home/vagabond/downloads/wallpapers/4k")
(defvar *mountains* (concatenate 'string *wallpapers-4k* "/mountains/"))
(defvar *galaxy* (concatenate 'string *wallpapers-4k* "/galaxy/"))
(defvar *abstract* (concatenate 'string *wallpapers-4k* "/abstract"))
(defvar *green* (concatenate 'string *wallpapers-4k* "/green/"))

(setf current-set '(stumpwm:run-shell-command (concatenate 'string "feh --bg-scale "
                                               *green* "7162209cbe40aeeba705870210e5eb7d.jpg " ; Forest
                                               ;; *green* "BM2vtfz.jpg " ; Blue
                                               *green* "RePIDAe.jpg " ; Pond
                                               *green* "hnBvq40.jpg " ; Cascades
                                               )))

(print current-set)
(eval current-set)

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Font ::
(in-package :stumpwm)

(set-font "xft:DejaVu Sans")

;; Message and Input Bar ::
(setf *timeout-wait* 3)
(setf *mode-line-timeout* 1)
(setf *message-window-gravity* :top-right)
(setf *suppress-frame-indicator* T)


;; Colors ;;
(defparameter *foreground-color* "darkcyan")
(stumpwm:set-focus-color *foreground-color*)

;;------------------------------------------------------------------------------------------------------------------------ ;;

;; Mode-Line ;;

(defun get-network-state ()
  (let* ((ip-link "ip link show wlp3s0")
         (net-state (subseq (run-shell-command ip-link t) 69 71)))
    (if (equal net-state "UP")
        (concatenate 'string "Wireless State: " net-state)
        (concatenate 'string "Wireless State: " "Down"))))

(setf *screen-mode-line-format* (list "%h |"
                                      "%g |"
                                       '(#.:eval  (subseq (stumpwm:run-shell-command "acpi -b" t) 11 47)) " | "
                                       '(#.:eval  (get-network-state)) " | "
                                       "%W |"
                                       '(#.:eval (stumpwm:run-shell-command "date" t))
                                        ))



 ;;------------------------------------------------------------------------------------------------------------------------ ;;




