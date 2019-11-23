
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
      (setf (screen-groups screen)
            (cons g (screen-groups screen)))
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
         (current-group (group-number (current-group)))
         (next-screen screen)
         (active-screen (car *screen-list*)) ;; This is always screen 0
         (next-groups (screen-groups next-screen))
         (current-groups (screen-groups active-screen)))
    ;; (if (mapcar #'find (screen-groups *ns*)
    (if (equal screen active-screen)
        (error "Screen already active")
        ;; TODO This is not a meaningful way to switch active screens
        (progn
          (mapcar (lambda (g)
                    (if (find (print g) (print next-groups) :test 'equal) ;;if it does find something we don't want it to save over
                        (setf (group-name g)
                              (format nil "~a" (gensym)))
                        (move-group-to-screen g next-screen)))
                  current-groups)
          (mapcar (lambda (g)
                    (if (find g current-groups :test 'equal)
                        (setf (group-name g) (format nil "~a" (gensym)))
                        (move-group-to-screen g active-screen)))
                  next-groups)))
    (gnext)
    (let ((g (find-if #'(lambda (x) (= current-group (group-number x))) next-groups)))
      (if g (switch-to-group g)))))

;; (switch-screen-to-active (cadr (sort-screens)))

;; (defparameter *current-screen* 0)
;; (defparameter *previous-screen* 1)

(defun select-screen ()
  (car (select-from-menu (current-screen)
                         (mapcar (lambda (g) (list (format nil "~a" g)))
                                 (mapcar #'screen-id *screen-list*))
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


;; (screen-id (car (sort-screens)))

(defun switch-screen-prev ()
  (print (let* ((screens (reverse (sort-screens)))
          (matches (member (current-screen) screens)))
     (if (null (cdr matches))
         ;; use the first one.
         (progn
           (setf *current-screen* (car screens)
                 *current-screen-id* (screen-id (car screens)))
           (switch-screen-to-active (car screens)))
         ;; Otherwise, use the next one in the list.
         (progn
           (setf *current-screen* (cadr matches)
                 *current-screen-id* (screen-id (cadr matches)))
           (switch-screen-to-active (cadr matches)))))))

(defcommand screen-prev () ()
  (switch-screen-prev))

(defun switch-screen-next ()
  (print (let* ((screens (sort-screens))
          (matches (member (current-screen) screens)))
     (if (null (cdr matches))
         ;; use the first one.
         (progn
           (setf *current-screen* (car screens)
                 *current-screen-id* (screen-id (car screens)))
           (switch-screen-to-active (car screens)))
         ;; Otherwise, use the next one in the list.
         (progn
           (setf *current-screen* (cadr matches)
                 *current-screen-id* (screen-id (cadr matches)))
           (switch-screen-to-active (cadr matches)))))))

(defcommand screen-next () ()
  (switch-screen-next))

;; (defun switch-to-screen (screen-id)
;;   (let ((screen-to-switch
;;           (car (find screen-id
;;                      (mapcar #'(lambda (s)
;;                                  (cons s (screen-id s)))
;;                              (sort-screens))
;;                      :key 'cdr))))
;;     (switch-screen-to-active (current-screen)) ;; Always switch-back to current
;;     (switch-screen-to-active screen-to-switch)
;;     (setf *current-screen* screen-to-switch)))

;; ERROR switching back to the current screen is actually what you want
;; So the way this works when theres only 2 is not exactly sustable

(print *current-screen-id*)

(defun switch-screen-next ()
  (let ((screen-to-switch
          (car (cadr (member *current-screen-id*
                             (mapcar #'(lambda (s)
                                         (cons s (screen-id s)))
                                     (sort-screens))
                             :key 'cdr)))))
    (if (null (equal *current-screen* (car (sort-screens)))) ;;if on first, don't switch
        (switch-screen-to-active *current-screen*)) ;; Always switch-back to current
    (print (if screen-to-switch
               (switch-screen-to-active screen-to-switch)))
    (setf *current-screen* screen-to-switch
          *current-screen-id* (screen-id screen-to-switch))))

(defcommand screen-next () ()
  (screen- ))

;; (defcommand screen-prev () ()
;;   (let ((screen-to-switch
;;           (car (cadr (member *current-screen-id*
;;                              (reverse (mapcar #'(lambda (s)
;;                                           (cons s (screen-id s)))
;;                                       (sort-screens)))
;;                              :key 'cdr)))))
;;     (if (null (equal (current-screen) (car (sort-screens)))) ;;if on first, don't switch
;;         (switch-screen-to-active (current-screen))) ;; Always switch-back to current
;;     (switch-screen-to-active screen-to-switch)
;;     (setf *current-screen* screen-to-switch
;;           *current-screen-id* (screen-id screen-to-switch))))

;; (defcommand screen-next () ()
;;   (let* ((screens (mapcar #'screen-id (sort-screens)))
;;          (matches (member *current-screen-id* screens)))
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
;;   (let* ((screens (mapcar #'screen-id (sort-screens)))
;;          (matches (member *current-screen-id* screens)))
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

;; (defcommand switch-screen-next () ()
;;   (switch-next-screen))

;; (defcommand screen-select () ()
;;   (when-let ((screen (get-screen)))
;;     (switch-screen-to-active screen)))

;; Right now this will actually move to a screen
(defcommand s-select () ()
  (when-let ((name (car (select-from-menu (current-screen)
                                     (mapcar (lambda (g) (list (format nil "~a" g)))
                                             (mapcar 'screen-id *screen-list*))))))
    (if (null name)
        (error "No Screen Selected")
        (mapcar (lambda (screen)
                  (if (equal name
                             (format nil "~a" (screen-id screen)))
                      (progn
                        (switch-screen-to-active screen)
                        (gnext))))
                *screen-list*))))

(defmacro srename (name)
  (let ((x (caddr *screen-list*)))
    `(if (null ,name) (error "No a valid name")
        (setf ,(screen-id x) 2))))


