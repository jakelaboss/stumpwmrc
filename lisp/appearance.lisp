;;-------~---~----------~----------~----
;; Appearance ::
;;---~----~-~-------------~---------~---

;; Wallpaper Sets ;;

(defvar *wallpapers-4k* "/home/arch/Downloads/wallpapers/4k/4k")
(defvar *mountains* (concatenate 'string *wallpapers-4k* "/mountains"))
(defvar *galaxy* (concatenate 'string *wallpapers-4k* "/galaxy"))
;; (setf mountains "/home/arch/Downloads/wallpapers/4k/4k/mountains")

;; Purple
(stumpwm:run-shell-command (concatenate 'string "feh --bg-scale " *mountains* "/EBSB15k.jpg " *mountains* "/Kd6MY6P.jpg  " *galaxy* "/9lcJsaP.jpg  " *mountains* "/oH1wlYb.jpg"))

                   ;; (run-shell-command (concatenate 'string "feh --bg-scale /home/arch/Downloads/wallpapers/4k/4k/mountains/EBSB15k.jpg  /home/arch/Downloads/wallpapers/4k/4k/mountains/Kd6MY6P.jpg  /home/arch/Downloads/wallpapers/4k/4k/galaxy/9lcJsaP.jpg  /home/arch/Downloads/wallpapers/4k/4k/mountains/oH1wlYb.jpg")
;; Black and White
;; (run-shell-command "feh --bg-scale /home/arch/Downloads/wallpapers/4k/4k/mountains/EBSB15k.jpg  /home/arch/Downloads/wallpapers/4k/4k/mountains/Kd6MY6P.jpg  /home/arch/Downloads/wallpapers/4k/4k/galaxy/9lcJsaP.jpg  /home/arch/Downloads/wallpapers/4k/4k/mountains/oH1wlYb.jpg")

;; Font ::
(set-font "xft:DejaVu Sans")


;; Message and Input Bar ::
(setf *timeout-wait* 3)
(setf *mode-line-timeout* 1)
(setf *message-window-gravity* :top-right)
(setf *suppress-frame-indicator* T)


;; Colors ;;
(defparameter *foreground-color* "darkcyan")
(stumpwm:set-focus-color *foreground-color*)

