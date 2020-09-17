;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Appearance ::
;;------------------------------------------------------------------------------------------------------------------------ ;;

;; Wallpaper Sets ;;

(in-package :stumpwm)


(defun flat-list (l)
  "Function that 'flatten a list."
  (cond ((null l) nil)
        #+sbcl((eq (type-of l) 'sb-impl::comma)
               (flat-list (sb-impl::comma-expr l)))
        ((atom l) (list l))
        (t (append (flat-list (first l))
                   (flat-list (rest l))))))

(defun check-emacs ()
  (if (cl-ppcre:scan "emacs"
                     (inferior-shell:run/s "ps aux"))
      t nil))

(defparameter *wallpapers-4k* "/home/vagabond/libraries/wallpapers/4k/")
(defparameter *wallpapers-desktop* "/home/vagabond/libraries/wallpapers/desktop/")
(defparameter *mountains* (concatenate 'string *wallpapers-4k* "mountains/"))
(defparameter *galaxy* (concatenate 'string *wallpapers-4k* "galaxy/"))
(defparameter *abstract* (concatenate 'string *wallpapers-4k* "abstract"))
(defparameter *green* (concatenate 'string *wallpapers-4k* "green/"))
(defparameter *orange* (concatenate 'string *wallpapers-4k* "orange/"))
(defparameter *neo* (concat *wallpapers-desktop* "abstract/"))


(setf current-set '(stumpwm:run-shell-command (concatenate 'string "feh --bg-scale "
                                               ;; *green* "BM2vtfz.jpg " ; Blue
                                               *green* "7162209cbe40aeeba705870210e5eb7d.jpg " ; Forest
                                               *green* "RePIDAe.jpg " ; Pond
                                               *green* "hnBvq40.jpg " ; Cascades
                                               )))

(defmacro set-wallpaper (pic-list)
  `(stumpwm:run-shell-command
    (concatenate 'string "feh --bg-fill "
                 ,@(mapcan #'(lambda (x) (if (stringp x) (list x "\" ")
                                             (list "\"" x)))
                           pic-list))))

(defun get-monitor-count ()
  (parse-integer (subseq (inferior-shell:run/s "xrandr --listmonitors") 10 11)))

(defcommand set-to-green () ()
  (defparameter *mode-line-foreground-color* "darkcyan")
  (stumpwm:set-focus-color *mode-line-foreground-color*)
  (if (> 4 (get-monitor-count))
      (print (set-wallpaper (*green* "RePIDAe.jpg" ; Pond
                               *green* "hnBvq40.jpg" ; Cascades
                               *green* "7162209cbe40aeeba705870210e5eb7d.jpg"))) ; Forest
      (set-wallpaper (*green* "7162209cbe40aeeba705870210e5eb7d.jpg" ; Forest
                              *wallpapers-desktop* "green/mountains/29 - zibIfl0.jpg" ; moutains
                              *green* "RePIDAe.jpg" ; Pond
                              *green* "hnBvq40.jpg")))) ; Cascades
;; (if (check-emacs)
;;     (set-emacs-theme 'green)))

(defcommand set-to-orange () ()
  (defparameter *mode-line-foreground-color* "darkorange")
  (defparameter *mode-line-background-color* "Gray10")
  (stumpwm:set-focus-color "darkorange")
  (set-wallpaper (*orange* "pQsq8QC.jpg" ; Galaxy
                           *orange* "05zJa4T.jpg" ; Coast
                           *orange* "p6qa5fP.jpg" ; Sparks
                           *orange* "vkTJUKA.jpg" ; Space
                           ;; *green* "hnBvq40.jpg " ; Cascades
                           )))
;; (if (check-emacs)
;;     (set-emacs-theme 'darktooth)))

(defcommand set-to-purple-mountains () ()
  (defparameter *mode-line-foreground-color* "darkcyan")
  (defparameter *mode-line-background-color* "Gray20")
  (stumpwm:set-focus-color "darkblue")
  (set-wallpaper (
                  *mountains* "Kd6MY6P.jpg"
                  *mountains* "EBSB15k.jpg"
                  *mountains* "ChfasKD.jpg")))
;; (if (check-emacs)
;; (set-emacs-theme 'cherry-blossom)))
;; (set-emacs-theme 'cherry-blossom))

(defcommand set-to-neo () ()
  (defparameter *mode-line-foreground-color* "purple")
  (stumpwm:set-focus-color *mode-line-foreground-color*)
  (set-wallpaper (
                  *neo* "neo/18 - o9wM9yF.jpg" ;; city
                        *neo* "62 - VYyoVxi.jpg" ;; galaxy
                        *neo* "17 - X95BVbg.jpg" ;; koan sound
                        *neo* "neo/21 - SJkkVGI.png"  ;; beach
                        )))

(defcommand set-to-mountains () ()
  (defparameter *mode-line-foreground-color* "darkcyan")
  (stumpwm:set-focus-color *mode-line-foreground-color*)
  (set-wallpaper (
                  *wallpapers-desktop* "green/mountains/cphIgT0.jpg"
                  *wallpapers-desktop* "green/mountains/hnBvq40.jpg"
                  *wallpapers-desktop* "green/mountains/Y2gvKrI.jpg"
                  *wallpapers-desktop* "green/mountains/5MJjfZC.jpg"
                  )))

(defcommand set-to-streams () ()
  (defparameter *mode-line-foreground-color* "darkcyan")
  (stumpwm:set-focus-color *mode-line-foreground-color*)
  (set-wallpaper (*wallpapers-desktop* "water/cK25Jhp.jpg"
                                       *wallpapers-desktop* "water/PxQhp6B.jpg"
                                       *wallpapers-desktop* "water/qLRqQJX.jpg"
                                       *wallpapers-desktop* "water/Y5L3Mpc.jpg"
                                       )))

(defcommand set-to-grass () ()
  (defparameter *mode-line-foreground-color* "darkcyan")
  (stumpwm:set-focus-color *mode-line-foreground-color*)
  (set-wallpaper (
                  *wallpapers-desktop* "green/grass/qlgQyNU.jpg"
                  *wallpapers-desktop* "green/grass/ivan-ovsyannikov-img3.jpg"
                  *wallpapers-desktop* "green/grass/185459.jpg"
                  *wallpapers-desktop* "green/grass/9nalIJG.jpg"
                  )))

(defcommand set-to-garden () ()
  (defparameter *mode-line-foreground-color* "darkcyan")
  (stumpwm:set-focus-color *mode-line-foreground-color*)
  (set-wallpaper (
                  *wallpapers-desktop* "green/garden/7W4gNBN.jpg"
                  *wallpapers-desktop* "green/garden/9nalIJG.jpg"
                  *wallpapers-desktop* "green/garden/47ad9357ff8a1e2d57af264422750d0a.jpg"
                  *wallpapers-desktop* "green/garden/landscapes_makoto_shinkai_the_garden_of_words_1366x768_54834.jpg"
                  )))


;; (directory (concat *mountains* "*.jpg"))

;; (defcommand set-wallpaper-list () (:rest)
;;   (when-let ((x (select-from-menu (current-screen)
;;                                   (orange green))))
;;     (intern ()))
;;     (set-to)

;; (set-to-green)
;; (set-to-orange)
(set-to-mountains)

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Frame Configuration
(defvar *appearance-bindings*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "r") "set-to-orange")
    m ; NOTE: this is important
    ))

(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-u") '*appearance-bindings*)

;; (print current-set)
;; (eval current-set)

;; (defun compton (&optional (opacity t) (active-opacity nil))
;;   (let ((x (cl-ppcre:scan-to-strings "compton" (inferior-shell:run/s "ps -A")))
;;     (y `(cond ((null ,active-opacity)
;;               (stumpwm:run-shell-command "compton -CGb -i 0.8 --no-fading-destroyed-argb --no-fading-openclose --active-opacity 0.9 --vsync opengl --refresh-rate 60")
;;               (if (null ,opacity)
;;                   (stumpwm:run-shell-command "compton -CGb --config /dev/null --no-fading-destroyed-argb --no-fading-openclose --vsync opengl --refresh-rate 60")
;;                   (stumpwm:run-shell-command "compton -CGb -i 0.8 --no-fading-destroyed-argb --no-fading-openclose --vsync opengl --refresh-rate 60"))))))
;;     (if x
;;         (progn
;;           (run-shell-command "pkill compton")
;;           (eval y))
;;         (eval y))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))


;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Screenshots

(defun take-screenshot ()
  (run-shell-command "scrot -s 'Desktop/%Y-%m-%d-%s.png'"))

(defcommand screenshot () ()
  (take-screenshot))

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Compton
(defun start-compton (&optional (active-transparancy nil) (passive-transparancy nil))
  (stumpwm:run-shell-command "pkill compton")
  (sleep .5)
  (if active-transparancy (stumpwm:run-shell-command
                           (concat "compton -CGb -i 0.8 --no-fading-destroyed-argb --no-fading-openclose --active-opacity 0.9 "
                                   "--unredir-if-possible --vsync opengl-oml --glx-no-stencil --xrender-sync-fence "
                                   "--glx-no-rebind-pixmap -menu-opacity=0.8 --inactive-opacity-override -e .8"))
      (if passive-transparancy
          (stumpwm:run-shell-command
           (concat "compton -CGb -i 0.8 --no-fading-destroyed-argb --no-fading-openclose --unredir-if-possible"
                   "--vsync opengl-oml --glx-no-stencil --xrender-sync-fence --glx-swap-method undefined "
                   "--glx-no-rebind-pixmap -menu-opacity=0.8 --inactive-opacity-override -e .8"))
          (stumpwm:run-shell-command
           (concat "compton -CGb --no-fading-destroyed-argb --no-fading-openclose --unredir-if-possible "
                   "--vsync opengl-oml --glx-no-stencil --xrender-sync-fence --glx-swap-method undefined "
                   "--glx-no-rebind-pixmap")))))

(defcommand compton (active-op passive-op) ((:y-or-n "Active transparancy on?: ") (:y-or-n "Passive transparrancy on?: "))
  (start-compton active-op passive-op))


;;------------------------------------------------------------------------------------------------------------------------ ;;

;; Font ::
;; (in-package :stumpwm)
(stumpwm:load-module "ttf-fonts")
;; (set-font (make-instance 'xft:font :family "Anonymous Pro" :subfamily "Regular" :size 13))
;; (set-font (make-instance 'xft:font :family "Noto Mono" :subfamily "Regular" :size 13))

;; Error: Keep Noto Fonts package below 2019-02
(set-font (make-instance 'xft:font :family "Noto Sans Med" :subfamily "Regular" :size 12))


;; (set-font "-misc-dejavu sans condensed-medium-o-semicondensed--0-0-0-0-p-0-ascii-0")
;; (load-module "ttf-fonts")
;; (set-font "Noto Sans Regular")
;; (set-font )
;; (set-font "9x15bold")
;; (set-font "Noto Sans Display")
;; (set-font "-monotype-noto sans med-medium-r-normal--0-0-0-0-p-0-iso10646-1")
;; (set-font "-monotype-noto sans med-medium-r-normal--0-0-0-0-p-0-iso8859")
;; (font-exists-p "Noto Sans Display")
;; NotoSansDisplay-Regular.ttf: "Noto Sans Display" "Regular"
;; (mapc (lambda (x) (format t "Family: ~a  Subfamilies: ~{~a, ~}~%" x (clx-truetype:get-font-subfamilies x)) ) (clx-truetype:get-font-families)))
;; (set-font (make-instance 'xft:font :family "Noto Sans Med" :subfamily "Regular" :size 12))
;; (clx-truetype:get-font-families)
;; (set-font "/usr/share/fonts/noto/NotoSans-Regular.ttc")
;; (font-exists-p "-monotype-noto sans med-medium-r-normal--0-0-0-0-p-0-iso10646-1")
;; (+default-font-name+)
;; (font-exists-p "9x15")
;; (xlib:font-p "Noto Sans Regular")

;; (font-exists-p  "NotoSans-Regular")
;; (ql:quickload :clx-truetype)
;; (clx-truetype:*font-dirs*)
;; (clx-truetype:get-font-families)
;; (clx-truetype:cache-fonts)
;; (print (clx-truetype:get-font-families))

;; (xlib:font-p (setf *font* (make-instance 'clx-truetype:font :family "Noto Sans Med" :subfamily "Regular" :size 16)))
;; (setf *font* (make-instance 'xft:font :family "Noto Sans Med" :subfamily "Regular" :size 16)))
;; (xlib:open-font stumpwm:*display* "Noto Sans")
;; (xft:font)

;; (clx-truetype:get-font-subfamilies "Noto Sans Med")
;;  "Noto Sans Regular")
;; (open-font xlib:display )

;; Message and Input Bar ::
(setf *timeout-wait* 3)
(setf *mode-line-timeout* 10)

(turn-on-mode-line-timer)

(setf *message-window-gravity* :top-right)
(setf *suppress-frame-indicator* T)


;; Colors ;;
;; *colors*
;; ;; (defparameter *foreground-color* "darkcyan")
;; (defparameter *foreground-color* "darkmagenta")
;; (defparameter *foreground-color* "darkblue")
;; (stumpwm:set-focus-color *foreground-color*)
;; (defparameter *mode-line-foreground-color* "magenta")
;; (defparameter *mode-line-foreground-color* "darkcyan")
;; (defparameter *mode-line-foreground-color* "red")

;;------------------------------------------------------------------------------------------------------------------------ ;;

;; Mode-Line ;;

(setf *mode-line-position* :top)
(setf *mode-line-pad-x* 12)
(setf *mode-line-pad-y* 2)
(defvar network-state nil)

;; (mode-line-loop get-network-state ()
;;   (let* ((ip-link "ip link show wlp3s0")
;;          (net-state (subseq (run-shell-command "ip link show wlp3s0" t) 69 71)))
;;     (if (equal net-state "UP")
;;         (concatenate 'string "Wireless State: "
;;                      (setf network-state net-state))
;;         (concatenate 'string "Wireless State: "
;;                      (setf network-state "Down")))))

;; (defun get-network-state (&optional (status nil))
;;   (let ((enabled status))
;;     (sb-thread:make-thread
;;      (loop while enabled
;;            do (progn
;;                 (let* ((ip-link "ip link show wlp3s0")
;;                        (net-state (subseq (run-shell-command ip-link t) 69 71)))
;;                   (if (equal net-state "UP")
;;                       (concatenate 'string "Wireless State: "
;;                                    (setf network-state net-state))
;;                       (concatenate 'string "Wireless State: "
;;                                    (setf network-state "Down"))))
;;                 (sleep 10))))))

;; (get-network-state)

(defun get-backlight-percent ()
  (let* ((x
           ;; (parse-integer (stumpwm:run-shell-command "cat /sys/class/backlight/intel_backlight/brightness" t)))
           (parse-integer (read-file-into-string "/sys/class/backlight/intel_backlight/brightness")))
         (percent (floor (* x 100) *max-brightness*)))
    (format nil "~a%" percent)))

(defun mode-time ()
  (let ((*day-names*
        '("Monday" "Tuesday" "Wednesday"
          "Thursday" "Friday" "Saturday"
          "Sunday"))
      (night-day)
      (*month-names*
        '("January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December")))
    (multiple-value-bind
          (second minute hour date month year day-of-week dst-p tz)
        (get-decoded-time)
                                        ; " ~2,'0d:~2,'0d:~2,'0d of ~a, ~d/~2,'0d/~d (GMT~@d)"
      (format nil "~a ~a ~a\, ~2,'0d:~2,'0d:~2,'0d ~a"
              (nth day-of-week *day-names*)
              (nth (- month 1) *month-names*)
              date
              (if (<= 12 hour)
                  (progn  (setf night-day "pm") (- hour 12))
                  (progn (setf night-day "am") hour))
              minute
              second
              night-day))))

(defun mode-power ()
  (cons
   (car (cl-ppcre:split #\Newline (read-file-into-string "/sys/class/power_supply/BAT0/status")))
   (car (cl-ppcre:split #\Newline (read-file-into-string "/sys/class/power_supply/BAT0/capacity")))))

(defun cpu-temp ()
  (with-open-file (s "/sys/class/hwmon/hwmon0/temp1_input")
    (setf *cpu-temp* (format nil "~a" (float (/ (read s) 1000))))))

(defvar *mode-loop* t)

(defun mode-loop ()
  (bt:make-thread
   #'(lambda ()
       (unwind-protect
            (loop while *mode-loop*
                  do (setf *screen-mode-line-format*
                           (list " | "
                              (unwind-protect (mode-time))
                              " | Battery: "
                              (unwind-protect (car (mode-power))) ", "
                              (unwind-protect (cdr (mode-power))) "%"
                              " | Brightness: "
                              (unwind-protect (get-backlight-percent))
                              ;; "| Network Status: "
                              ;; '(#.:eval (unwind-protect (net-status "wlo1")))
                              ;; "| | %c %f "
                              " | Temp: "
                              (unwind-protect (cpu-temp))
                              " | Group: %g | "
                              ;; "%W | "
                              ))
                     (sleep 10))))
   :name "Mode Loop"))

(defvar *mode-thread* (mode-loop))

 ;;------------------------------------------------------------------------------------------------------------------------ ;;
