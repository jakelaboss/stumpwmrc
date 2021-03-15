(in-package :stumpwm)
(ql:quickload :clifford)
(ql:quickload :cl-noaa)

(use-package :local-time)
(use-package :cl-noaa)
(use-package :clifford)
(use-package :cl-voice)

(change-voice "voice_cmu_us_fem_cg")
;; (change-voice "voice_cmu_us_awb_cg")

(let ((mic t))
  (defun toggle-mic ()
    (run/s "amixer set Capture toggle")
    (if mic (setf mic nil) (setf mic t))
    (if mic "mic is on" "mic is off")))

(defcommand mic-toggle () ()
  (toggle-mic))

;; (activate-topmap)
;; (hash-table-alist clifford::*atlas-hash*)
;; (ensure-clifford)
;; (define-rooms)
;; (stop-clifford)
;; (active-atlas-name)
;; (atlas-map *speech-map*)
;; (activate-atlas (gethash 'clifford::*music* clifford::*atlas-hash*))
;; (reset-results)
;; (read-results)


;; let's discuss creating a language for defining commands
;; we can do simple sentences like this, but adding things like OR, could be a big help

 ;; --- Greetings ----------------------------------------

;; facial recognition?
(defpattern *speech-map*  ((hi hello hey greeting) (clifford quivered 1)) ()
  (Hi Jake))

(defpattern *speech-map* (("I'm" 0) (good great)) ()
  (Thats good to hear))

(defpattern *speech-map* (how are you doing) ()
  ("I'm" great "today." Thanks for "asking."))

(defpattern *speech-map* ((what 1) (time 1) is)
            (let* ((ts (local-time:now))
                   (h (local-time:timestamp-hour ts))
                   (m (local-time:timestamp-minute ts)))
              (cl-voice:output (format nil "The Time is ~a:~a"
                                       (if (> h 12) (- h 12) h)
                                       (if (< m 10) (format nil "0~a" m) m)))))

(defpattern *speech-map* (("let's" let) (chat check))
            (activate-atlas clifford::chatlas)
  (okay))

;; --- Lights ----------------------------------------

;; living room
(defpattern *speech-map* ((living 2) room (on 0))
            (stumpwm::turn-living-room-on))

(defpattern *speech-map* ((living 2) room (off 0))
            (stumpwm::turn-living-room-off))

(defpattern *speech-map* (living room down)
            (stumpwm::turn-living-room-down))

(defpattern *speech-map* (living room (up pop))
            (stumpwm::turn-living-room-up))

(defpattern *speech-map* (living room (green forest))
            (set-state-of-room *living-room* forest-values))

(defpattern *speech-map* (living room (orange sunset))
            (set-state-of-room *living-room* sunset-values))

(defpattern *speech-map* (living room twilight)
            (set-state-of-room *living-room* twilight-values))

(defpattern *speech-map* (living room (reading 1))
            (set-state-of-room *living-room* reading-values))


;; bedroom
(defpattern *speech-map* ((bedroom bedrooms) on)
            (stumpwm::turn-bedroom-on))

(defpattern *speech-map* ((bedroom bedrooms) off)
            (stumpwm::turn-bedroom-off))

(defpattern *speech-map* ((bedroom bedrooms) down)
            (stumpwm::turn-bedroom-down))

(defpattern *speech-map* ((bedroom bedrooms) (up pop))
            (stumpwm::turn-bedroom-up))

(defpattern *speech-map* ((bedroom bedrooms) (green forest))
            (set-state-of-room *bedroom* forest-values))

(defpattern *speech-map* ((bedroom bedrooms) (orange sunset))
            (set-state-of-room *bedroom* sunset-values))

(defpattern *speech-map* ((bedroom bedrooms) twilight)
            (set-state-of-room *bedroom* twilight-values))

(defpattern *speech-map* ((bedroom bedrooms) nebula)
            (set-state-of-room *bedroom* nebula-values))

(defpattern *speech-map* ((bedroom bedrooms) (reading 1))
            (set-state-of-room *bedroom* reading-values))

(defpattern *speech-map* ((list ) (light lights) (sets set)) ()
  (current light presets forest sunset twilight "nebula," and reading))

;; light layer
(defatlas *light-state*)

(defpattern *speech-map* ((light lights 1) (state 0))
            (activate-atlas *light-state*)
  ("Okay." Entering light "layer."))

(defpattern *light-state* ((max 1))
            (mapcar #'(lambda (x) (stumpwm::turn-light-up x 255))
                    (stumpwm::hash-table-keys stumpwm::*living-room*)))

(defpattern *light-state* ((up pop 1))
  (stumpwm::turn-living-room-up))

(defpattern *light-state* ((down 1))
            (stumpwm::turn-living-room-down))

(defpattern *light-state* ((off of 1))
            (stumpwm::turn-living-room-off))

(defpattern *light-state* ((on bond 1))
            (stumpwm::turn-living-room-on))

(defpattern *light-state* ((okay ok exit leave 0))
            (Leaving light "layer.")
  (activate-atlas *speech-map*))


;; light sync
(defpattern *speech-map* ((lights light) (sync sink) (on))
            (stumpwm::light-sync-on)
  (lights will now sync with the screen))

;; battery
(defun format-timestamp (ts)
  (subseq (format-timestring nil ts) 0 10))

(defpattern *speech-map* (("what's" what) (weather 0))
            (let ((day (format-timestamp (now)))
                (day? (if (< (timestamp-hour (now)) 17) t)) ;; we'll do 6'clock
                (wd (forecast-date (gethash "Tonight" *weather-hash*))))
              ;; we take the day and check if it it's night time or not
              ;;if it's the right day we can continue
              (unless (equal day wd) (populate-weather))
              (if day?
                  (cl-voice:speak-sentence (forecast-for "This Afternoon"))
                  (cl-voice:speak-sentence (forecast-for "Tonight")))))

;; (hash-table-alist *weather-hash*)

(defpattern *speech-map* (("what's" 1) (battery))
            (let ((ac (stumpwm::acpi)))
              (cl-voice:speak-sentence (subseq ac 10 (+ 1 (position #\% ac))))))

(defpattern *speech-map* ((percent percentage))
            (let ((ac (stumpwm::acpi)))
              (cl-voice:speak-sentence (subseq ac 10 (+ 1 (position #\% ac))))))

;; --- Network Commands ----------------------------------------
;; we need some command words

(defpattern *speech-map* ((wireless) (connect))
            (run-commands "bluetooth"))

;; --- Stop Commands ----------------------------------------
(defatlas *stop*)

(defpattern *stop* (no)
            (activate-topmap)
  ("I'll" continue then))

(defpattern *stop* (yes)
            (stop-clifford)
  (Turning off "voice."))

(defpattern *speech-map* (stop voice)
            (activate-atlas *stop*)
  (Would you like to stop voice functionality?))

;; --- Workspace and Groups ----------------------------------------

(defatlas *workspace*)

(defpattern *speech-map*
    ((group go) (right))
    (stumpwm::gnext))

(defpattern *speech-map*
    ((group go) (left))
    (stumpwm::gprev))

(defpattern *speech-map*
    ((group go) (up pop))
    (stumpwm::ws-next))

(defpattern *speech-map*
    ((group go) (down))
    (stumpwm::ws-prev))

;; (print (hash-table-alist (slot-value (gethash '*speech-map* cl-deepspeech::*atlas-hash*) 'cl-deepspeech::map)))

(defpattern *speech-map* ((workspace controller 1))
            (activate-atlas *workspace*))

(defpattern *speech-map* (work (space 1))
            (activate-atlas *workspace*))

(defpattern *workspace* ((okay ok exit exiter top leave))
  (activate-topmap))

(defpattern *workspace* ((space play))
            (run/s "xdotool key space"))

;; (defpattern *speech-map* (switch to (* group))
;;             (switch-to-group-by-name group))

(defpattern *speech-map* ((switch 1) to (music 1)) (switch-to-group-by-name "music"))
(defpattern *speech-map*  ((switch 1) to head space) (switch-to-group-by-name "head-space"))
(defpattern *speech-map*  ((switch 1) to (development developers 2)) (switch-to-group-by-name "development"))
(defpattern *speech-map*  ((switch 1) to reading) (switch-to-group-by-name "reading"))

(defpattern *speech-map*   ((switch 1) to (meta meadow 1)) (switch-to-group-by-name "meta"))
(defpattern *speech-map*   ((switch 1) to (lisp less list 0)) (switch-to-group-by-name "lisp"))
(defpattern *speech-map*  ((switch 1) to ("com's" 1)) (switch-to-group-by-name "comms"))
(defpattern *speech-map*  ((switch 1) to server) (switch-to-group-by-name "servers"))

(defpattern *speech-map*  ((switch 1) to (connections 2)) (switch-to-group-by-name "connections"))
(defpattern *speech-map*  ((switch 1) to media) (switch-to-group-by-name "media"))
(defpattern *speech-map*  ((switch 1) to (games game gains)) (switch-to-group-by-name "games"))
(defpattern *speech-map*  ((switch 1) to (operations operation)) (switch-to-group-by-name "ops"))

(defpattern *speech-map*  ((switch 1) to (video vidoes)) (switch-to-group-by-name "videos"))
(defpattern *speech-map*  ((switch 1) to (browse brows)) (switch-to-group-by-name "browse-main"))
(defpattern *speech-map*  ((switch 1) to relax) (switch-to-group-by-name "relax"))
(defpattern *speech-map*  ((switch 1) to work) (switch-to-group-by-name "work"))



(defpattern *workspace*  ((music 1)) (switch-to-group-by-name "music"))
(defpattern *workspace*  (head space) (switch-to-group-by-name "head-space"))
(defpattern *workspace*  ((development developers 2)) (switch-to-group-by-name "development"))
(defpattern *workspace*  (reading) (switch-to-group-by-name "reading"))

(defpattern *workspace*   ((meta meadow 1)) (switch-to-group-by-name "meta"))
(defpattern *workspace*   ((lisp less list 0)) (switch-to-group-by-name "lisp"))
(defpattern *workspace*  (("com's" 1)) (switch-to-group-by-name "comms"))
(defpattern *workspace*  (server) (switch-to-group-by-name "servers"))

(defpattern *workspace*  ((connections 2)) (switch-to-group-by-name "connections"))
(defpattern *workspace*  (media) (switch-to-group-by-name "media"))
(defpattern *workspace*  ((games game gains)) (switch-to-group-by-name "games"))
(defpattern *workspace*  ((operations operation)) (switch-to-group-by-name "ops"))

(defpattern *workspace*  ((video vidoes)) (switch-to-group-by-name "videos"))
(defpattern *workspace*  ((browse brows)) (switch-to-group-by-name "browse-main"))
(defpattern *workspace*  (relax) (switch-to-group-by-name "relax"))
(defpattern *workspace*  (work) (switch-to-group-by-name "work"))

