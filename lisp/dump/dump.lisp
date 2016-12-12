;; -*- stumpwm -*-

;; ========================== Loading dependency libraries

(load "~/quicklisp/setup.lisp")

;; ========================== Debugging properties (uncomment first)

;; (ql:quickload :swank)
;; (ql:quickload :clx)
;; (ql:quickload :clx-truetype)
;; (ql:quickload :cl-ppcre)
;; (ql:quickload :cl-xembed)
;; (ql:quickload :stumpwm)

;; (defun swank ()
;;   (setf *top-level-error-action* :break)
;;   (swank:create-server :port 4005
;;                        :style swank:*communication-style*
;;                        :dont-close t)
;;   (echo-string (current-screen)
;;                "Starting swank. M-x slime-connect RET RET, then (in-package stumpwm)."))

;;(swank)

;; ========================== Beginning setup of stumpwm

(use-package :ql-clj)
(set-prefix-key (kbd "C-;"))

(setf *contrib-dir* "~/repo/perso/stumpwm/contrib/")

;; load a few modules
;; (mapcar #'load-module '("cpu"
;;                         "mem"
;;                         "battery-portable"
;;                         "net"
;;                         "wifi"
;;                         "disk"))

;; up the debug level (see ~/.xsession-errors)
;; (setf stumpwm::*debug-level* 0)

;; set the font for the message bar and input bar
;; (set-font "-*-FreeMono-medium-r-*-*-6*")
;; (set-font "-adobe-helvetica-medium-r-normal--1*")
;; (set-font "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-7")
;; (set-font "-xos4-terminus-medium-r-normal--0-0-72-72-c-0-iso8859-7")
;; (set-font "-*-dina-medium-r-normal-*-*-*-*-*-*-*-*-*")
;; (set-font "-lispm-fixed-medium-r-normal-*-13-*-*-*-*-*-*-*")
;; (set-font "-artwiz-smoothansi-medium-r-normal--13-130-75-75-m-60-iso8859-1")
(set-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1")

(set-normal-gravity :bottom)
(setf *message-window-gravity* :bottom-left)
(setf *input-window-gravity* :bottom-left)

(setf *frame-number-map* "abcdefghijklmnopqrst")

(setf *window-format* "%m%n%s nm=%50t cl=%c id=%i")

;;; Window Appearance
;; The width in pixels given to the borders of regular windows.
(setf *normal-border-width* 0)
;; The width in pixels given to the borders of windows with maxsize or ratio hints.
(setf *maxsize-border-width* 0)
;; The width in pixels given to the borders of transient or pop-up windows.
(setf *transient-border-width* 0)

;;(run-commands "restore-from-file ~/.stumpwm.screendump")

;;Set the mouse focus policy to ignore the mouse
(setf *mouse-focus-policy* :click) ;; :click, :ignore, :sloppy

(defun key-seq-msg (key key-seq cmd)
  "Show a message with current incomplete key sequence."
  (declare (ignore key))
  (or (eq *top-map* *resize-map*)
      (stringp cmd)
      (let ((*message-window-gravity* :bottom-right))
        (message "~A" (print-key-seq (reverse key-seq))))))

(add-hook *key-press-hook* 'key-seq-msg)

;; ========================== setup

;; xenity - shell command with output inside a zenity popup window
(zenity-category
 ((ssh-add-list         "List the keys the user-agent holds"      "ssh-add -l"           "K")
  (cat-etc-environment  "Display the content of /etc/environment" "cat /etc/environment" "E")
  (cat-etc-hosts        "Display the content of /etc/hosts"       "cat /etc/hosts"       "H")
  (sbin-ifconfig        "/sbin/ifconfig"                          "/sbin/ifconfig"       "I")
  (acpi-cmd             "Display battery"                         "/usr/bin/acpi -b"     "B")))

;; run-or-raise application based on the class name of the window + deactivating the touchpad
(ror-tpd-off-category
 ((terminal             "terminal" "gnome-terminal --hide-menubar -x tmux -2" "Gnome-terminal" "x")
  (myemacs              "emacs"    "~/bin/emacs/emacs.sh"                     "Emacs"          "e")
  (conkeror             "conkeror" "conkeror"                                 "Conkeror"       "c")))

;; run-or-raise application based on the class name of the window + activating the touchpad
(ror-tpd-on-category
 ((firefox              "firefox"                 "firefox"                                                 "Firefox"                     "f")
;;(gnome-control-center "gnome-control-center"    "gnome-control-center"                                    "Gnome-Control-Center"        ",")
;;(control-center       "cinnamon-control-center" "cinnamon-control-center"                                 "Cinnamon-control-Center"     ",")
  (settings             "cinnamon-settings"       "cinnamon-settings"                                       "Cinnamon-settings.py"        ",")
  (video-player         "totem"                   "totem"                                                   "Totem"                       ".")
  (evince               "evince"                  "evince"                                                  "Evince"                      "C-e")
  (eog                  "eog"                     "eog"                                                     "Eog"                         "C-i")
  (pinta                "pinta"                   "pinta"                                                   "Pinta"                       "d")
  (gimp                 "gimp"                    "gimp"                                                    "Gimp"                        "I")
  (audacious            "audacious"               "/usr/bin/audacious"                                      "Audacious"                   "C-a")
  (jconsole             "jconsole"                "/usr/bin/jconsole"                                       "sun-tools-jconsole-JConsole" "C-j")
  (jvisualvm            "visualvm"                "~/applications/visualvm/bin/visualvm"                    "java-lang-Thread"            "j")
  (arduino              "arduino ide"             "arduino"                                                 "processing-appBase"          "C-c")
;;(intellij-idea        "intellij-idea"           "~/bin/ide/idea.sh"                                       "jetbrains-idea-ce"           "j")
;;(eclipse              "eclipse ide"             "~/bin/ide/eclipse.sh"                                    "Eclipse"                     "i")
  ))

;; run-or-raise application based on the instance of the window + activating the touchpad
(ror-id-tpd-on-category
 ((xephyr-gnome         "xephyr - gnome"  "~/bin/xephyr/xephyr.sh gnome"         "Xephyr"                 "C-x")
  (xephyr-xmonad        "xephyr - xmonad" "~/bin/xephyr/xephyr.sh xmonad"        "Xephyr"                 "C-X")
  (wireshark            "wireshark"        "gksudo wireshark"                     "wireshark"              "C-w")
  ;; (nautilus             "nautilus"         "nautilus"                             "nautilus"               "n")
  (nemo                 "nemo"             "nemo"                                 "nemo"                   "n")
  (thunar               "thunar"           "thunar"                               "thunar"                 "N")
  (yEd                  "yed"              "~/bin/app/yed.sh"                     "sun-awt-X11-XFramePeer" "y")
  (filezilla            "filezilla"        "/usr/bin/filezilla"                   "filezilla"              "C-f")
  (virtualbox           "virtualbox"       "virtualbox"                           "Qt-subapplication"      "C-v")
  (unetbootin           "unetbootin"       "unetbootin"                           "unetbootin"             "u")
  (transmission         "transmission-gtk" "/usr/bin/transmission-gtk"            "transmission-gtk"       "/")
  (gparted              "gparted"          "gksudo /usr/sbin/gparted"             "gpartedbin"             "G")
  (file-progress        "file progress"    ""                                     "file_progress"          "F")
  (xosview              "xosview"          "xosview"                              "xosview"                "X")
  (baobab               "baobab"           "baobab"                               "baobab"                 "b")
  (gitk                 "gitk"             "gitk"                                 "gitk"                   "z")
  (fbreader             "fbreader"         "fbreader"                             "fbreader"               "C-f")
  (lighttable           "LightTable"       "~/applications/LightTable/LightTable" "ltbin"                  "C")
  (tux-guitar           "TuxGuitar"        "/usr/bin/tuxguitar"                   "TuxGuitar"              "M-t")
  (skype                "skype"            "/usr/bin/skype"                       "skype"                  "C-c")))

;; run shell command - Simply execute shell command
(run-shell-command-category
 ((suspend              "Suspend"                              "gksudo pm-suspend"                                                                   "C-S")
  (hibernate            "Hibernate"                            "gksudo pm-hibernate"                                                                 "C-H")
  (ssh-add-identities   "Load ssh identities in ssh-agent"     "~/bin/ssh/ssh-add.sh"                                                                "A")
  (switch-to-proxy      "Switch to proxy."                     "gksudo ~/bin/proxy/proxy.sh on && ~/bin/wifi/nm-applet.sh stop"                      "p")
  (switch-to-no-proxy   "Switch to no proxy."                  "gksudo ~/bin/proxy/proxy.sh off && ~/bin/wifi/nm-applet.sh stop"                     "P")
  (dec-brightness       "Brightness decrement"                 "~/bin/brightness/dec-brightness.sh 5"                                                "C-b")
  (inc-brightness       "Brightness increment"                 "~/bin/brightness/inc-brightness.sh 5"                                                "C-f")
  (min-brightness       "Minimum screen brightness"            "~/bin/brightness/min-brightness.sh"                                                  "m")
  (half-brightness      "Half screen brightness"               "~/bin/brightness/half-brightness.sh"                                                 "C-M")
  (max-brightness       "Maximum screen brightness"            "~/bin/brightness/max-brightness.sh"                                                  "M")
  (inc-sound            "Increase sound"                       "exec amixer set Master 5%+"                                                          "M-f")
  (dec-sound            "Decrease sound"                       "exec amixer set Master 5%-"                                                          "M-b")
  (toggle-sound         "Toggle sound"                         "exec amixer set Master toggle"                                                       "M-m")
  (wifi-off             "wifi off"                             "~/bin/wifi/wifi-off.sh"                                                              "C-o")
  (wifi-on              "wifi on"                              "~/bin/wifi/wifi-on.sh"                                                               "O")
  (lock-session         "Lock the session"                     "~/bin/session/lock.sh"                                                               "C-M-l")
  (evince-haskell-prog  "read functional approach in haskell"  "evince ~/books/haskell/algorithms-a-functional-programming-haskell-approach.pdf"      "'")))

;; shell command - Simply execute shell command and possibly output the result in the screen
(shell-command-category
 ((top              "Display a single 'top' frame."             "top -b -n 1 -c -d 1"                                                                                              "^")
  (screenshot       "Take a screenshot for the current window." "/usr/bin/scrot -u $HOME/Pictures/screenshot_$(date +%F_%H-%M-%S).png"                                             "C-s")
  (screenshot-mouse "Take a screenshot with mouse selection."   "~/bin/touchpad/toggle-touchpad-manual.sh 1; /usr/bin/scrot -s $HOME/Pictures/screenshot_$(date +%F_%H-%M-%S).png" "M-s")))

;; ========================== simple binding

(define-key *root-map* (kbd "o") "fselect")

(define-key *root-map* (kbd "M-r") "exec")
(define-key *root-map* (kbd "Q")   "loadrc")
(define-key *root-map* (kbd "M-q") "quit")

(define-key *root-map* (kbd "C-u") "move-window up")
(define-key *root-map* (kbd "C-r") "move-window right")
(define-key *root-map* (kbd "C-d") "move-window down")
(define-key *root-map* (kbd "C-l") "move-window left")

(define-key *root-map* (kbd "-") "vsplit")
(define-key *root-map* (kbd "|") "hsplit")

(define-key *root-map* (kbd "M-x") "colon")

(define-key *input-map* (kbd "C-h")   'input-delete-backward-char)
(define-key *input-map* (kbd "C-M-h") 'input-backward-kill-word)
(define-key *input-map* (kbd "C-j")   'input-submit)
(define-key *input-map* (kbd "C-m")   'input-submit)
(define-key *input-map* (kbd "C-i")   'input-complete-forward)

(define-key *input-map* (kbd "M-p")   'input-history-back)
(define-key *input-map* (kbd "M-n")   'input-history-forward)

(defcommand scroll-forward-browser () ()
  "Scroll forward in firefox"
  (play-commands "firefox" (kbd "C-v") "myemacs"))
(define-key *root-map* (kbd "C-M-]") "scroll-forward-browser")

(defcommand scroll-backward-browser () ()
  "Scroll backward in firefox"
  (play-commands "firefox" (kbd "M-v") "myemacs"))
(define-key *root-map* (kbd "C-M-[") "scroll-backward-browser")

;; ========================== Help

(setf *help-map*
      (let ((m (make-sparse-keymap)))
        (define-key m (kbd "m") "man")
        (define-key m (kbd "i") "info")
        (define-key m (kbd "v") "describe-variable")
        (define-key m (kbd "f") "describe-function")
        (define-key m (kbd "k") "describe-key")
        (define-key m (kbd "w") "where-is")
        m))

;; gimp

(define-frame-preference "gimp"
  (1 t t :create "gimp-dump" :class "Gimp" :title nil :role nil)
  (0 t t :create "gimp-dump" :class "Gimp" :title nil :role "gimp-toolbox")
  (2 t t :create "gimp-dump" :class "Gimp" :title nil :role "gimp-dock")
  (1 t t :create "gimp-dump" :class "Gimp" :title nil :role "gimp-image-window"))

;; courtesy of Peter Seibel
;; (defmacro with-gensyms ((&rest names) &body body)
;;   `(let ,(loop for n in names collect `(,n (gensym)))
;;      ,@body))

;; (defmacro program-with-layout (name &key (command (string-downcase (string name)))
;;                                     (props `'(:class ,(string-capitalize command))))
;;   (with-gensyms (s w h files-path layout rules)
;;                 `(defcommand ,name () ()
;;                    (let* ((,s (current-screen))
;;                           (,w (prin1-to-string (screen-width ,s)))
;;                           (,h (prin1-to-string (screen-height ,s)))
;;                           (,files-path "/home/tony/.gimp")
;;                           (,layout (concat ,files-path ,command "-layout-" ,w "x" ,h))
;;                           (,rules (concat ,files-path ,command "-rules")))
;;                      (gnew ,command)
;;                      (restore-from-file ,layout)
;;                      (restore-window-placement-rules ,rules)
;;                      (run-or-raise ,command ,props)
;;                      (place-existing-windows)))))

;; (program-with-layout gimp)

(defun hostname () (run-shell-command "/bin/hostname" t))

;; ========================== cursor - https://github.com/stumpwm/stumpwm/wiki/GrabbedPointer

(setf *grab-pointer-foreground* (xlib:make-color :red 0.1 :green 0.25 :blue 0.5))
(setf *grab-pointer-background* (lookup-color (current-screen) "DeepSkyBlue"))
(setf *grab-pointer-character* 88)
(setf *grab-pointer-character-mask* 88)
(setf *grab-pointer-font* "fixed")
