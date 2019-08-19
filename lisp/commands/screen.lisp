
(in-package :stumpwm)


;; So what's the plan?

;; Create a new class for groups

;; I can either extend the currently existing group system to include work-spaces
;; Or I can attempt to just redifine the commands that I use

;; I could also rename all groups not in the workspace to be hidden

;; TODO Should I set the Screen-ID? Or the Screen Itself?
;; Redefine group class to include original Screen ID
;; (defclass group ()
;;   ((screen :initarg :screen :accessor group-screen)
;;    (windows :initform nil :accessor group-windows)
;;    (screen-id :initform nil :accessor group-screen-id) ;; This is the original Screen ID. IT should be set once and not changed
;;    (current-window :initform nil :accessor group-current-window)
;;    (number :initarg :number :accessor group-number)
;;    (name :initarg :name :accessor group-name)
;;    (on-top-windows :initform nil :accessor group-on-top-windows)))

;; Define the workspace class and initialization

(defclass work-space ()
  ((groups :initform nil :accessor ws-groups)
   ;; (current-workspace :accessor ws-current)
   (id :initarg :number :accessor ws-number)
   (name :initform "Default" :accessor ws-name)
   (screen :initform nil :accessor ws-current-screen)
   ;; (current-screen :accessor ws-current-screen)
   (current-group :accessor ws-current-group)))

(defgeneric ws-groups (work-space))
(defgeneric ws-number (work-space))
(defgeneric ws-name (work-space))
(defgeneric ws-current-screen (work-space))
(defgeneric ws-current-group (work-space))


;; (defmethod initialize-instance ((c-ws work-space) &key &allow-other-keys)
;;   (let ((ws (copy-tree (non-hidden-groups (screen-groups (current-screen))))))
;;     (eval
;;      `(progn
;;         (setf (ws-group-tree ,c-ws) ',ws
;;               (ws-current-group ,c-ws) (car ',ws)
;;               ;; (ws-last ,c-ws) (cadr ',ws)
;;               (ws-last-group ,c-ws) (cadr ',ws))))))

(defvar *ws-list* nil)

;; ;; Initialize workspace with an id
;; (defun init-workspace (screen id)
;;   (let ((current-screen (current-screen)))
;;     (setf *ws-list* (cons
;;                      `(make-instance 'work-space :groups ,(screen-groups current-screen)
;;                                                  :id ,id
;;                                                  :screen ,screen
;;                                                  :current-group ,(current-group)))
;;           *ws-list*))))

(defun workspace-start ()
  (let ((current-screen (current-screen))
      (ws (make-instance 'work-space)))
    (setf (ws-groups ws) (screen-groups current-screen))
    (setf (ws-number ws) 0)
    (setf (ws-current-screen ws) (current-screen))
    (setf (ws-current-group ws) (current-group))
    (setf *ws-list* (cons ws *ws-list*)))))

;; (workspace-start)


(defun init-workspace (screen id)
  (let ((current-screen (current-screen))
      (ws (make-instance 'work-space)))
    (setf (ws-groups ws) (screen-groups current-screen))
    (setf (ws-number ws) id)
    (setf (ws-current-screen ws) screen)
    (setf (ws-current-group ws) (current-group))
    (setf *ws-list* (cons ws *ws-list*)))))



;; (defmacro create-new-screen (var id)
;;   `(setf ,var (init-screen
;;                (car (xlib:display-roots *display*)) ,id "")))

;; So heres a thought
;; I could just use screens as they are, then just move windows that get created to the current frame
;; That would allow me to use all the features that I need, and would even be less hacky then what I'm currently doing
(defun create-new-screen (name)
  (init-screen (car (xlib:display-roots *display*)) name ""))

(defun clear-current-screen ()
  (mapcar (lambda (g)
            (mapcar (lambda (f)
                      (clear-frame f g))
                    (group-frames g)))
          (screen-groups (current-screen))))

(defun move-group-to-screen (group screen)
  (let ((g group)
      (cs (group-screen group)))
    (progn
      (setf (group-screen g) screen)
      (setf (screen-groups screen) (cons g (screen-groups screen)))
      (setf (screen-groups cs)
            (remove g (screen-groups cs))))))

(defun switch-screens ()
  ;;  TODO move in different directions
  (let* ((*screen-list* (sort-screens))
         (ns (cadr *screen-list*))
         (cs (car *screen-list*))
         (ng (screen-groups ns))
         (cg (screen-groups cs)))
    ;; (if (mapcar #'find (screen-groups *ns*)
    (mapcar (lambda (g)
              (if (find g ng)
                  (setf (group-name g) (format nil "~a" (gensym)))
                  (move-group-to-screen g ns)))
            cg)
    (mapcar (lambda (g)
              (if (find g cg)
                  (setf (group-name g) (format nil "~a" (gensym)))
                  (move-group-to-screen g cs)))
            ng))
  (gnext))


;; There is def a bug here somewhere
(defun switch-screen-to-active (screen)
  (let* ((*screen-list* (sort-screens))
         (ns screen)
         (current-screen (current-screen))
         (cs (car *screen-list*))
         (ng (screen-groups ns))
         (cg (screen-groups cs)))
    ;; (if (mapcar #'find (screen-groups *ns*)
    (if (equal screen current-screen)
        (error "Screen already active")
        (progn
          (mapcar (lambda (g)
                    (if (find g ng)
                        (setf (group-name g) (format nil "~a" (gensym)))
                        (move-group-to-screen g ns)))
                  cg)
          (mapcar (lambda (g)
                    (if (find g cg)
                        (setf (group-name g) (format nil "~a" (gensym)))
                        (move-group-to-screen g cs)))
                  ng))))
  (gnext))

(defparameter *current-screen* 0)
(defparameter *previous-screen* 1)

;; Maybe try making this a closure?
(defun switch-screen-to-active (screen)
  (let* (
         ;; So (current-screen) does not actually change, so if I'm trying to change from 0 to 1
         ;; To make sure that groups stay with their screen, screens need to be reset before
         ;; Switching again
         (*screen-list* (sort-screens))
         (current-screen (car *screen-list*))
         (next-g (screen-groups ns))
         (current-g (screen-groups cs))
         (original-screen (find screen *ws-list*)))

    ;; Switch all groups to *current-screen*, if it's not active
    ;; Now switch all groups to original screen, then
    ;; (if (mapcar #'find (screen-groups *ns*)
    (if 
        ;; (error "Screen already active")
        (ws-current-screen (car *ws-list*))0
        (ws-number (car *ws-list*))0
        (mapcar (lambda (g)
                  (if (find g))))
        (progn
          ;; Move current group set to original screen
          (mapcar
           (labels ((mv (group-set screen)
                      ;; Move group set to screen
                      (let (group-name ))
                      (mapcar (lambda (g)
                                (if (find g ng)
                                    (setf (group-name g) (format nil "~a" (gensym)))

;;            (move-group-to-screen (find-screen-from-id (group-screen-id g)))
;;            (mapcar (lambda (g)
;;                      (if (find g ng)
;;                          (setf (group-name g) (format nil "~a" (gensym)))
;;                          (progn
;;                            (move-group-to-screen g ns)))
;;                      cg)
;;                    (mapcar (lambda (g)
;;                              (if (find g cg)
;;                                  (setf (group-name g) (format nil "~a" (gensym)))
;;                                  (move-group-to-screen g cs)))
;;                            ng))))
;;         (gnext))



(defun select-screen ()
  (car (select-from-menu (current-screen)
                         (mapcar (lambda (g) (list (format nil "~a" g)))
                                 (mapcar 'screen-id *screen-list*))
                         "Select Screen: ")))

(defun get-screen ()
  (let ((x (select-screen)))
    (if (null x)
        (error "No Screen Selected")
        (car (remove nil
                     (mapcar (lambda (screen)
                               (if (equal x (format nil "~a" (screen-id screen)))
                                   screen))
                             *screen-list*))))))

(defun find-screen-from-id (id)
  (car (remove nil
               (mapcar (lambda (screen)
                         (if (equal id (screen-id screen))
                             screen))
                       *screen-list*))))

;; (defcommand snew (name) ((:string "Name of Screen: "))
;;   ;; (let ((n (find-free-number (mapcar 'screen-id *screen-list*) 1))
;;   (if (null name) (error "Not a valid Name")
;;       (switch-to-screen (create-new-screen name))))


;; Maybe add global variables?

(defcommand snew () ()
  (let* ((n (find-free-number (mapcar 'screen-id *screen-list*) 1))
      (screen (create-new-screen n)))
    ;; (if (null name) (error "Not a valid Name")
    (progn
      (switch-to-screen screen)
      ;; (make-instance 'work-space :id )
      (init-workspace screen n)
      (switch-to-screen (find-screen-from-id 0)))))

(defcommand skill () ()
  "All windows will be merged into current group"
  (when-let ((screen (get-screen)))
    (if (null (screen-groups screen))
        (setf *screen-list* (remove screen *screen-list*))
        (progn
          (mapcar (lambda (g)
                    (kill-group g (current-group)))
                  (screen-groups screen))
          (setf *screen-list* (remove screen *screen-list*))))))

(defun screen-next ()
  (get-current-screen-id)
  (if current /= original ;; what is original?
      (switch-screen to original))
  (switch-screen to next in sequence)

  ;; Ok This doesn't work correctly
  ;; Switch to the next highest number screen
  (let* ((screens (sort-screens))
         (screen-ids (mapcar 'screen-id screens))
         (matches (member (current-screen) screens))
         (ids* (remove nil (mapcar (lambda (x) (if (< *current-screen* x) x)) screen-ids)))
         (next (if (null ids*) (car screen-ids) (car ids*))))
    ;; (if () ;; Return all numbers bigger than x
    ;; Switch to the next biggest number
    (progn
      (if (equal (current-screen) (find-screen-from-id *current-screen*))
          ;; If screens are set correctly
          (progn
            (switch-screen-to-active (find-screen-from-id next))
            (setf *previous-screen* *current-screen*)
            (setf *current-screen* next))
          ;; Otherwise switch
          (progn
            (switch-screen-to-active (find-screen-from-id *current-screen*))
            (switch-screen-to-active (find-screen-from-id next))
            (setf *previous-screen* *current-screen*)
            (setf *current-screen* next))))))


;;     ;; (if (null (cdr matches))
;;     (member *current-screen* (mapcar 'screen-id ))
;;     (lambda (s)
;;       (if (> *current-screen* (screen-id s)))
;;       ;; use the first one.
;;       (switch-screen-to-active (car screens))
;;       ;; Otherwise, use the next one in the list.
;;       (switch-screen-to-active (cadr matches)))))

(defcommand screen-prev () ()
  (let* ((screens (reverse (sort-screens)))
         (matches (member (current-screen) screens)))
    (if (null (cdr matches))
        ;; use the first one.
        (switch-screen-to-active (car screens))
        ;; Otherwise, use the next one in the list.
        (switch-screen-to-active (cadr matches)))))

(defcommand screen-next () ()
  (let* ((screens (sort-screens))
         (matches (member (current-screen) screens)))
    (if (null (cdr matches))
        ;; use the first one.
        (switch-screen-to-active (car screens))
        ;; Otherwise, use the next one in the list.
        (switch-screen-to-active (cadr matches)))))


(defcommand screen-select () ()
  (when-let ((screen (get-screen)))
    (switch-screen-to-active screen)))

;; Right now this will actually move to a screen
(defcommand s-select () ((:rest "Select a screen: "))
  (when-let ((name (car (select-from-menu (current-screen)
                                     (mapcar (lambda (g) (list (format nil "~a" g)))
                                             (mapcar 'screen-id *screen-list*))))))
    (if (null name)
        (error "No Screen Selected")
        (mapcar (lambda (screen)
                  (if (equal name
                             (format nil "~a" (screen-id screen)))
                      (progn
                        (switch-to-screen screen)
                        (gnext))))
                *screen-list*))))

(defmacro srename (name)
  (let ((x (caddr *screen-list*)))
    `(if (null ,name) (error "No a valid name")
        (setf ,(screen-id x) 2))))


;; (srename 2)

;; (defun )


;; ;; Move from current screen others
;; ;; (defun
;; (let ((*ns* (cadr *screen-list*))
;;     (groups (cdr (screen-groups (car *screen-list*))))
;;     (gs (screen-groups *ns*)))
;;   ;; (if (mapcar #'find (screen-groups *ns*)
;;   (mapcar (lambda (g)
;;             (if (find g (screen-groups *ns*))
;;                 (setf (group-name g) (format nil "~a" (gensym)))
;;                 (move-group-to-screen g *ns*)))
;;           groups))
;;   ;; (mapcar #')
;;   )
;; (switch-screens)

;; (defcommand move-group-to-screen (group screen) ((:rest "Select A Group To Move: ") (:rest "Which Screen?: "))
;;   (when-let ())

;;   (move-windows-to-group (group-windows (second (screen-groups (current-screen))))
;;                          (car (screen-groups *ns*)))
;; (move-windows-to-group (group-windows (second (screen-groups (current-screen))))
;;                        *ws*)

;; (defmethod update-ws-tree ((c-ws work-space) &key &allow-other-keys)
;;   ;; Ok, I need to check for overlap
;;   (let ((ws (cons (copy-tree (non-hidden-groups (screen-groups (current-screen)))) (ws-group-tree c-ws))))
;;     ;; Hmmm, member doesn't seem to be working
;;     (labels ()
;;     (cond ((find t (mapcar (lambda (x)
;;                              (equal (non-hidden-groups (screen-groups (current-screen))) x))
;;                            (ws-group-tree *ws*)))
;;            (remove (non-hidden-groups (screen-groups (current-screen))))
;;            (eval
;;             `(progn
;;                (setf (ws-group-tree ,c-ws) ',ws
;;                      (ws-current ,c-ws) (car ',ws)
;;                      (ws-current-group ,c-ws) (caar ',ws)
;;                      (ws-last ,c-ws) (cadr ',ws)
;;                      (ws-last-group ,c-ws) (cadar ',ws))))))

;;     (setf (ws-group-tree *ws*)
;;           (list (non-hidden-groups (screen-groups (current-screen)))
;;              (ws-group-tree *ws*)))
;;     (setf (current-group))
;; ))))


;; (ql:quickload :clgnuplot)
;; (ql:quickload :cgn)

;; (ql:quickload '(:CLNUplot ;; gnu plotting
;;  :cl-opengl ;; opengl FFI
;;  :vgplot
;;  :clinch ;; rendering engine for opengl
;;  :cepl.sdl2
;;  :livesupport))




;; (eval `(vgplot:plot
;;         (loop for x from 0 to 100 collect x)
;;         (let ((a 10)
;;             (b 10)
;;             (c 10))
;;           (loop for x from 0 to 100
;;                 collect ,(d *e* 'x)
;;                 ))))


;; (vgplot:plot
;;  (loop for x from 0 to 100 collect x)
;;  (let ((a 10)
;;      (b 10)
;;      (c 10))
;;    (loop for x from 0 to 100
;;          collect
;;          (+ (+ (* a (* x x))
;;                             (* b x))
;;             c)))

;;  (loop for x from 0 to 100 collect x)
;;  (let ((a 10)
;;        (b 10)
;;        (c 10))
;;    (loop for x from 0 to 100
;;          collect (+ (+ (+ (* A (+ (* X 1) (* X 1))) (* (* X X) 0)) (+ (* B 1) (* X 0))) 0)
;;          ))
;;  (loop for x from 0 to 100 collect x)
;;  (let ((a 10)
;;        (b 10)
;;        (c 10))
;;    (loop for x from 0 to 100
;;          collect (+ (+ (+ (+ (* A (+ (+ (* X 0) (* 1 1)) (+ (* X 0) (* 1 1))))
;;                              (* (+ (* X 1) (* X 1)) 0))
;;                           (+ (* (* X X) 0) (* 0 (+ (* X 1) (* X 1)))))
;;                        (+ (+ (* B 0) (* 1 0)) (+ (* X 0) (* 0 1))))
;;                     0)))))))))

;; (defun d (e x)
;;   (cond ((atom e) (if (eq e x) 1 0))
;;         ((eq (car e) '+) `(+ ,(d (cadr e) x) ,(d (caddr e) x)))
;;         ((eq (car e) '*) `(+ (* ,(cadr e) ,(d (caddr e) x))
;;                              (* ,(caddr e) ,(d (cadr e) x))))))


;; (let ((l (loop for x from 0 to 100))
;;     (z (let ((a 10) (b 10) (c 10))
;;          (loop for x from 0 to 100
;;                collect (+ (+ (* a (* x x)) (* b x)) c))))
;;        (vgplot:plot x y x
;;                     (loop for x from 0 to 10
;;                           collect (+ (* 0.99986964 (expt x 2)) (* 2.0000746 x)  2.999993)))))

;; (let ((l (loop for x from 0 to 100 collect x))
;;         (z (let ((a 10) (b 10) (c 10))
;;                    (loop for x from 0 to 100
;;                          collect (+ (+ (* a (* x x)) (* b x)) c))))
;;       (f (let ((l '(8.659933 10.23725 9.990222 1.4263442e-4 -6.7887555e-7 0.0)))
;;            `(lambda (x)
;;               ,(cons '+ (reverse (loop for y from 1 to (length l) and x in (reverse l)
;;                                        collect `(* ,x (expt x ,y)))))))))
;;     (vgplot:plot l z l (loop for x in l collect (funcall (eval f) x))))

;; (let ((x '(0 1 2 3 4 5 6 7 8 9 10))
;;     ;; (y '(1 6 17 34 57 0 121 62 2039 262 32021)))
;;     (y '(1 6 17 34 57 -34 1214 62 239 262 321)))

;;   (vgplot:plot x y x (loop for x in x collect (funcall f x))))

;; (defmacro 100x (function)
;;   `(loop for x from 0 to 100 collect ,function))))

;;           (defun g)

;; (let ((x (100x (random (+ 1 x))))
;;     (y (loop for x from 0 to 100 collect (random 10000))))

;; (setf f (LAMBDA (X)
;;    (+ (* 168.98601398601073d0 (EXPT X 3)) (* -367.3745143745082d0 (EXPT X 2))
;;       (* 113.17482517482352d0 (EXPT X 1)) (* -7.373737373737264d0 (EXPT X 0)))))

;; (loop for x in (loop for x from 0 to 100 collect x) collect (funcall f x))

;; (setf f (LAMBDA (X)
;;           (+ (* -102.62237762237785d0 (EXPT X 2)) (* 104.95734265734269d0 (EXPT X 1))
;;              (* -6.402097902097902d0 (EXPT X 0)))))

;; (setf f (LAMBDA (X)
;;           (+ (* 67.8531468531267d0 (EXPT X 4)) (* -307.0407925407445d0 (EXPT X 3))
;;              (* 163.50466200464265d0 (EXPT X 2)) (* -23.46969696969436d0 (EXPT X 1))
;;              (* 1.0407925407924277d0 (EXPT X 0)))))

;; (setf f (LAMBDA (X)
;;    (+ (* -70.09090909090185d0 (EXPT X 4)) (* 462.7536907537192d0 (EXPT X 3))
;;       (* -301.88927738930363d0 (EXPT X 2)) (* 59.03651903652448d0 (EXPT X 1))
;;       (* -3.320512820513137d0 (EXPT X 0))))) 


;; (funcall f 0)
;; (funcall f 2)

;; (setf f (lambda (x)
;;    (+ (* 168.98601398601073d0 (EXPT X 3))
;;       (* -367.3745143745082d0 (EXPT X 2))
;;       (* 113.17482517482352d0 (EXPT X 1)) (* -7.373737373737264d0 (EXPT X 0)))))




;; (let ((x '(0 1 2 3 4 5 6 7 8 9 10))
;;     (y  '(1 6 17 34 57 86 121 162 209 262 321)))
;;   (vgplot:plot x y x
;;                (loop for x from 0 to 10
;;                      collect (+ (* 0.99986964 (expt x 2)) (* 2.0000746 x)  2.999993))))

;; (loop for x from 0 to 10
;;       collect (+ (* 0.99986964 (expt x 2)) (* 2.0000746 x)  2.999993))

;; #2A((0.99986964) (2.0000746) (2.999993))

;; (+ (+ (* a (* x x))
;;       (* b x))
;;    c)

;; (defvar *e* '(+ (+ (* a (* x x))
;;                  (* b x))
;;               c))

;; (print (d (d *e* 'x) 'x))


