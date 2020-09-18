(in-package :stumpwm)
(ql:quickload :cl-hue)

(import 'inferior-shell:run/s)
(import 'cl-ppcre:scan-to-strings)


(defparameter nebula-values '((49576 204) (48379 234) (47167 118)))
(defparameter forest-values '((34602 135) (40750 128) (23597 68)))
(defparameter sunset-values '((7417 136) (5288 191) (1480 234)))

(defun nmap (&optional portscan?)
  (let ((ps (if portscan? "-sn" ""))
        (lan (scan-to-strings "(?! inet )(\\d+.\\d+.\\d+)"
                              (scan-to-strings "wlp3s0(.*\\n*)*" (run/s "ip addr")))))
    (run/s (format nil "nmap ~a.0/24 ~a" lan ps))))

(defun hue-bridge-address ()
  (scan-to-strings "(?<=Philips-hue \\()(\\d+\\.\\d+\\.\\d+\\.\\d+)" (nmap t)))

;; (defparameter username (cl-hue:create-user (hue-bridge-address) "cl-hue"))
(defparameter username (sosei:pread* "keys/hue-light-username"))

(defun bridge ()
  (if *bridge* *bridge*
      (let ((br (cl-hue:make-bridge (hue-bridge-address) username)))
        (defvar *bridge* br)
        br)))


(defun get-light-state (light)
  (list (slot-value (cl-hue:get-light (bridge) light) 'cl-hue::hue)
        (slot-value (cl-hue:get-light (bridge) light) 'cl-hue::saturation)))

(defun get-room-state ()
  (loop for x from 5 to 7 ;; 5, 6, and 7 are the light numbers for this room
        collect (get-light-state x)))

(defun set-light-state (light brightness hue sat)
  (cl-hue:set-light-state-by-number (bridge) light :on t
                                                 :brightness brightness :hue hue
                                                 :saturation sat :effect "none"))

(defun set-room-state (values)
  (if *bridge*
      (let ((n 0))
        (loop for x from 5 to 7
              do (set-light-state x 255 (car (nth n values)) (cadr (nth n values)))
                 (incf n)))))

;; Example:
;; (set-room-state nebula-values)
