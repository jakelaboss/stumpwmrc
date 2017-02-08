;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Appearance ::
;;------------------------------------------------------------------------------------------------------------------------ ;;

;; Wallpaper Sets ;;

(defvar *wallpapers-4k* "/home/arch/downloads/wallpapers/4k")
(defvar *mountains* (concatenate 'string *wallpapers-4k* "/mountains"))
(defvar *galaxy* (concatenate 'string *wallpapers-4k* "/galaxy"))
(defvar *abstract* (concatenate 'string *wallpapers-4k* "/abstract"))
(defvar *green* (concatenate 'string *wallpapers-4k* "/green"))

;; (defvar current-set)
;; Purple
;; (defun set-wallpaper-set (var-name &optional)
;;   (setf current-set var-name))


(setf current-set '(stumpwm:run-shell-command (concatenate 'string "feh --bg-scale "
                                               *green* "/RePIDAe.jpg "  ;left
                                               *green* "/k8nyfek.jpg " ;right
                                               *green* "/BM2vtfz.jpg " ;top
                                               *green* "/7162209cbe40aeeba705870210e5eb7d.jpg " ;center
                                               )))
(eval current-set)


;;------------------------------------------------------------------------------------------------------------------------ ;;
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


;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Mode-Line ;;
(setf *screen-mode-line-format* (list "%h |"
                                      "%g |"
                                      "%W |"
                                      "%u |"
                                      '(:eval (stumpwm:run-shell-command "date" t))))

;;------------------------------------------------------------------------------------------------------------------------ ;;

(defcommand gaps () ()
  "Toggle the padding of tiled windows"
  (setf *useless-gaps-on* (null *useless-gaps-on*))

  ;; Following is pseudo code to use hooks
  ;; to do something like change border colors or size
  ;; (if *useless-gaps-on*
  ;;     (run-hook 'frame-gap-on)
  ;;     (run-hook 'frame-gap-off))
  (reset-all-windows))


