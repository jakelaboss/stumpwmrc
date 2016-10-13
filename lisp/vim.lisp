;; ;;-------~---~----------~----------~----
;; ;; Evil Mode for Stump using Windows Key ;;
;; ;;-------~---~----------~----------~----


;; Number Prefix
;; (defun num-prefix (command)


;;   (loop from 1 to 9 in (define-key *top-map* (kbd \"n\")\"command\")))

;; (num-prefix)

;; (defvar *2-prefix*
;;   (let ((m (stumpwm:make-sparse-keymap)))
;;     (stumpwm:define-key m (stumpwm:kbd "s-h") "move-focus left")
;;     (stumpwm:define-key m (stumpwm:kbd "s-j") "move-focus down")
;;     m ; NOTE: this is important
;;     ))

;; (stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "2") '*2-prefix*)



;; (make-spare-keymap "")

;; (defcommand num-prefix (number command)
;;   (loop for number in  ()\"command\"
;;   "move-focus left")

(defvar *2-key* (kbd "s-2"))

(defvar *2-root-map* nil)


(fill-keymap *top-map*
             *2-key* '*2-root-map*)

(fill-keymap *2-root-map*
             (kbd "s-h") "2-prefix move-focus left"
             (kbd "s-j") "2-prefix move-focus down"
             (kbd "s-h") "2-prefix move-focus left"
             (kbd "s-k") "2-prefix move-focus up"
             (kbd "s-l") "2-prefix move-focus right"
             (kbd "s-J") "2-prefix move-window down"
             (kbd "s-H") "2-prefix move-window left"
             (kbd "s-K") "2-prefix move-window up"
             (kbd "s-L") "2-prefix move-window right"
             (kbd "J") "2-prefix move-window down"
             (kbd "H") "2-prefix move-window left"
             (kbd "K") "2-prefix move-window up"
             (kbd "L") "2-prefix move-window right")


;; (defcommand num-prefix (&optional ) (:rest)
;;   (let ((cmd ()))))

;; (defun numprefix (number command)
;;   (dotimes ((eval-command \"command\") number)))
    
;;   (dotimes (n number)
;;     (eval-command \"command\")))

;;   (loop for x from number
;;         do (eval-command \"command\")))

;; (defun test (number)
;;   (dotimes ((eval-command \"command\") number)
;;   (loop for x from number
;;         do ()))

;; (let )

;; (let (value)      ; otherwise a value is a void variable
;;   (dotimes (number 3 value)
;;     (setq value (cons number value))))

;; (defun num-prefix (number-of-rows)
;;   "Using `dotimes', add up the number of pebbles in a triangle."
;;   (let ((total 0))  ; otherwise a total is a void variable
;;     (dotimes (number number-of-rows total)
;;       (setq total (+ total (1+ number))))))

;; (defun print-num-times (number-of-prints)
;;   (let ((time 0))
;;     (dotimes (number number-of-prints)
;;       (setq time ()))))

;; (defun num-prefix (number commands))
;; (loop for i in commands do
;;       (eval-command i)))

(defcommand 2-prefix (command)
  (stumpwm:run-commands
   \"command\"
   \"command\"))




;; (define-key *top-map* (kbd "kj") 


;; Movement Mapping ;;
(define-key *top-map* (kbd "s-j") "move-focus down")
(define-key *top-map* (kbd "s-h") "move-focus left")
(define-key *top-map* (kbd "s-k") "move-focus up")
(define-key *top-map* (kbd "s-l") "move-focus right")

(define-key *top-map* (kbd "s-J") "move-window down")
(define-key *top-map* (kbd "s-H") "move-window left")
(define-key *top-map* (kbd "s-K") "move-window up")
(define-key *top-map* (kbd "s-L") "move-window right")

(define-key *root-map* (kbd "J") "move-window down")
(define-key *root-map* (kbd "H") "move-window left")
(define-key *root-map* (kbd "K") "move-window up")
(define-key *root-map* (kbd "L") "move-window right")

;; Splits and Windows ;;
(define-key *top-map* (kbd "s-v") "hsplit")
(define-key *top-map* (kbd "s-s") "vsplit")
(define-key *top-map* (kbd "s-r") "remove")
(define-key *top-map* (kbd "s-w") "windows")
(define-key *top-map* (kbd "s-f") "windowlist")
(define-key *top-map* (kbd "s-x") "kill")
(define-key *root-map* (kbd "x") "kill")

;; Mouse Commands ;;
(define-key *top-map* (kbd "s-b")"banish")

;; Group Configuration ;;
(define-key *top-map* (kbd "s-N") "gnext")
(define-key *top-map* (kbd "s-n") "pull-hidden-next")
(define-key *top-map* (kbd "s-p") "pull-hidden-previous")
(define-key *root-map* (kbd "N") "gnext")
(define-key *top-map* (kbd "s-m") "fullscreen")

;; Eval Commands ;;
(define-key *top-map* (kbd "s-;") "colon")
(define-key *top-map* (kbd "s-:") "eval")


