
;;; Global Variables

(defvar *pg-data* "/var/lib/postgres/data")
(defvar *useless-gaps-size* 20)
(defvar *useless-gaps-on* nil)

;; Mode Line Variables
(setf *mode-line-timeout* 1)

;; Golden Ratio Variables
(defvar *golden-ratio-resize-increment* 100)

(defvar *resize-increment* 10
  "Number of pixels to increment by when interactively resizing frames.")
