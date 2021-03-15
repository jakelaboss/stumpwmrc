;; Global Variables
(in-package :stumpwm)
(ql:quickload '(:hyperluminal-mem :osicat :lparallel :sosei :s-base64))

(defvar *pg-data* "/var/lib/postgres/data")
(defvar *useless-gaps-size* 10)
(defvar *useless-gaps-on* nil)
(defvar *max-brightness* (read (open "/sys/class/backlight/intel_backlight/max_brightness")))

(setf sosei::*sosei-dir* "/home/vagabond/common-lisp/libraries/linux/stumpwm/storage/")

(sb-posix::chdir "/home/vagabond/")

;; Mode Line Variables
(setf *mode-line-timeout* 1)

;; Golden Ratio Variables
(defvar *golden-ratio-resize-increment* 150)

(defvar *resize-increment* 15
  "Number of pixels to increment by when interactively resizing frames.")
(defvar *golden-ratio-toplevel* nil)

;; (export stumpwm::password)
(defvar *stumpwm-storage* "/home/vagabond/common-lisp/libraries/linux/stumpwm/storage/")

(defun store-password (name password)
  (sosei:pwrite* name (encrypt (concat password (decrypt *salt* *lisp-key*)) *lisp-key*)))

;; to _actually_ unlock the *lisp-key* requires entering the master password
(defparameter *lisp-key* (sosei:pread* "keys/key"))
(defparameter *github-token* (sosei:pread* "keys/github-token"))
(defparameter *salt* (sosei:pread* "keys/salt"))
(defparameter *lisp-password* (sosei:pread* "keys/password"))
(defparameter *root-password* (sosei:pread* "keys/root"))
(defparameter *postgres-password* (sosei:pread* "keys/postgres"))


(defparameter *transient-border-width* 1)
(defparameter *window-border-style* :thin)
