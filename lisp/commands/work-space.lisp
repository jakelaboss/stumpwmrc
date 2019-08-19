
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

(defun make-current-screen ()
  (progn
    (move-groups-to-screen (find-screen-from-id *current-screen*) )
    (move-groups-to-screen ())
    (setf *current-screen*)))

(defun move-groups-to-screen (groups group-set screen)
  ;; Move groups set to screen
  (mapcar (lambda (g)
            (if (find g group-set)
                (setf (group-name g) (format nil "~a" (gensym)))
                ;; (move-group-to-screen g screen))
                (move-group-to-screen g screen)))
          groups))

(defun switch-next-screen ()
  (let* ((*screen-list* (flatten (cons (sort-screens) *screen-list*)))
         (current-screen (find-screen-from-id *current-screen*))
         (next-screen (find-screen-from-id (+ *current-screen* 1))))
    (if next-screen
        (progn
          (move-groups-to-screen
           (screen-groups (current-screen))
           (screen-groups next-screen)
           current-screen)
          (setf *current-screen* (+ 1 *current-screen*))
          (move-groups-to-screen
           (screen-groups (current-screen))
           (screen-groups next-screen)
           (next-screen)))
        (progn
          (move-groups-to-screen
           (screen-groups (current-screen))
           nil
           ;; (screen-groups next-screen)
           current-screen)
          (setf *current-screen* 0)
          (move-groups-to-screen
           (screen-groups (current-screen))
           nil
           (current-screen))))))
;; (next-screen ())))))))


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

(setf *print-circle* t)
(setf *print-pretty* t)
(setf *print)
(car #1=(cons (sort-screens) #1#))))

(let ((*print-circle* t))
  (eval `(print '#1=( . #1#)))
  nil)

;; #0=(1 2 3 . #0#)


;; Maybe try making this a closure?
(defun switch-screen-to-active (screen)
  (let* (
         ;; So (current-screen) does not actually change, so if I'm trying to change from 0 to 1
         ;; To make sure that groups stay with their screen, screens need to be reset before
         ;; Switching again
         (*screen-list* (sort-screens))
         (cs (car *screen-list*))
         ()
         ;; (next-g (screen-groups ns))
         (current-groups (screen-groups cs))
         (current-screen (find-screen-from-id *current-screen*))
         (group-set (screen-groups screen))
         (screen-circle (list (cons (sort-screens) (car (sort-screens))))))
    ;; (original-screen (find screen *ws-list*)))
    (labels ((mv (groups group-set screen)
               ;; Move group set to screen
               (mapcar (lambda (g)
                         (if (find g group-set)
                             (progn (setf (group-name g) (format nil "~a" (gensym)))
                                    (move-group-to-screen g screen))
                             (move-group-to-screen g screen)))
                       groups))
             (current-groups ()
               (screen-groups cs)))
      ;; Switch all groups to *current-screen*, if it's not active
      ;; Now switch all groups to original screen, then
      ;; (if (mapcar #'find (screen-groups *ns*)
      (if 
       ;; (error "Screen already active")
       ;; Do screens need to switch?
       (equal (screen-id cs) *current-screen*)
       (progn
         (mv current-groups group-set screen)
         (setf *previous-screen* *current-screen*)
         (setf *current-screen* (screen-id screen)))
       (progn
         (mv current-groups (screen-groups current-screen) current-screen)
         (mv (current-groups) group-set screen)
         (setf *previous-screen* *current-screen*)
         (setf *current-screen* (screen-id screen))))))))

        (ws-number (car *ws-list*))
        (mapcar (lambda (g)
                  (if (find g))))
        (progn
          ;; Move current group set to original screen
          (mapcar
           )
             (progn
             (mv 


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


(defun run-or-pull (cmd props &optional (all-groups *run-or-raise-all-groups*)
                            (all-screens *run-or-raise-all-screens*))
  "Similar to run-or-raise, but move the matching window to the
current frame instead of switching to the window."
  (let* ((matches (find-matching-windows props all-groups all-screens))
         ;; other-matches is for cycling through matches
         (other-matches (member (current-window) matches))
         (win (if (> (length other-matches) 1)
                  (second other-matches)
                  (first matches))))
    (if win
        (progn
          (move-window-to-group win (current-group))
          (pull-window win))
        (run-shell-command cmd))))

(run-or-pull "emacs")

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

(defcommand switch-screen-next () ()
  (switch-next-screen))

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


*screen-list*

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
