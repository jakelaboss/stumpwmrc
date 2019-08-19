
(in-package :stumpwm)


;; So what's the plan?

;; Create a new class for groups

;; I can either extend the currently existing group system to include work-spaces
;; Or I can attempt to just redifine the commands that I use

(defvar *ws-list* nil)


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

(defcommand snew () ()
  (let* ((n (find-free-number (mapcar 'screen-id *screen-list*) 1))
      (screen (create-new-screen n)))
    ;; (if (null name) (error "Not a valid Name")
    (progn
      (switch-to-screen screen)
      ;; (make-instance 'work-space :id )
      ;; (init-workspace screen n)
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

(defparameter *current-screen* (current-screen))
(defparameter *current-screen-id* 0)

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


;; (defcommand screen-prev () ()
;;   (let* ((screens (reverse (sort-screens)))
;;          (matches (member *current-screen* screens)))
;;     (if (null (cdr matches))
;;         ;; use the first one.
;;         (progn
;;           (switch-screen-to-active (current-screen))
;;           (switch-screen-to-active (car screens))
;;           (setf *current-screen* (car screens)))
;;         ;; Otherwise, use the next one in the list.
;;         (progn
;;           (switch-screen-to-active (current-screen))
;;           (switch-screen-to-active (cadr matches))
;;           (setf *current-screen* (cadr matches))))))

;; (defcommand screen-next () ()
;;   (let* ((screens (sort-screens))
;;          (matches (member *current-screen* screens)))
;;     (if (null (cdr matches))
;;         ;; use the first one.
;;         (progn
;;           (switch-screen-to-active (current-screen))
;;           (switch-screen-to-active (car screens))
;;           (setf *current-screen* (car screens)))
;;         ;; Otherwise, use the next one in the list.
;;         (progn
;;           (switch-screen-to-active (current-screen))
;;           (switch-screen-to-active (cadr matches))
;;           (setf *current-screen* (cadr matches))))))

(defcommand switch-screen-next () ()
  (switch-next-screen))

;; (defcommand screen-select () ()
;;   (when-let ((screen (get-screen)))
;;     (switch-screen-to-active screen)))

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


