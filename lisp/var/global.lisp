;; Global Variables
(in-package :stumpwm)
;; (ql:quickload '(:hyperluminal-mem :lparallel :sosei))

;; (defvar *pg-data* "/var/lib/postgres/data")
(defvar *useless-gaps-size* 10)
(defvar *useless-gaps-on* nil)
(defvar *max-brightness* (read (open "/sys/class/backlight/intel_backlight/max_brightness")))
(defvar *stumpwm-storage* "~/common-lisp/stumpwmrc/storage/")

;; (setf sosei::*sosei-dir* "/home/jake/common-lisp/libraries/linux/stumpwm/storage/")

;; Mode Line Variables
(setf *mode-line-timeout* 1)

;; Golden Ratio Variables
(defvar *golden-ratio-resize-increment* 150)

(defvar *resize-increment* 15
  "Number of pixels to increment by when interactively resizing frames.")

(defvar *golden-ratio-toplevel* nil)

;; (export stumpwm::password)

(defun open-key-file (filename)
  (read-file-into-string (concat *stumpwm-storage* "keys/" filename)))

(defparameter *lisp-key* (open-key-file "lisp-key"))

(defvar *salt-length* 21)

(defparameter *lisp-password* (open-key-file "password"))
(defparameter *root-password* (open-key-file "password"))

(defmacro defhash (name)
  `(defparameter ,name (make-hash-table :test 'equal)))

(defhash workspace-hash)
