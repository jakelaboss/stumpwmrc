
;;; Global Variables
(in-package :stumpwm)

(defvar *pg-data* "/var/lib/postgres/data")
(defvar *useless-gaps-size* 10)
(defvar *useless-gaps-on* nil)
(defvar *max-brightness* (read (open "/sys/class/backlight/intel_backlight/max_brightness")))

;; Mode Line Variables
(setf *mode-line-timeout* 1)

;; Golden Ratio Variables
(defvar *golden-ratio-resize-increment* 150)

(defvar *resize-increment* 15
  "Number of pixels to increment by when interactively resizing frames.")

(defvar *golden-ratio-toplevel* nil)

;; (export stumpwm::password)
(defvar *lisp-password* "lisphacker")

(defvar *transient-border-width* 2)
