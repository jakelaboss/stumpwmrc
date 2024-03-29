;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Workspaces
;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Defines the workspace class and initialization

(in-package :stumpwm)

(defparameter workspace-hash (make-hash-table :test 'equal))

(defclass workspace ()
  ((id :initarg :number :accessor ws-number)
   (name :initform "Default" :accessor ws-name)
   (screen :initform nil :accessor ws-screen)
   (active-p :initform nil :accessor ws-active-p)
   (current-group :accessor ws-current-group)))

(defgeneric ws-number (workspace))
(defgeneric ws-name (workspace))
(defgeneric ws-current-screen (workspace))
(defgeneric ws-active-p (workspace))
(defgeneric ws-current-group (workspace))

(defun ws-groups (ws)
  (screen-groups (ws-screen ws)))

;; Remove all groups from the metaspace

;; so we have to create a new workspace immeditely
;; them make it our active space

(defstruct meta-space
  (:groups (screen-groups (current-screen)))
  (:screen (current-screen))
  (:meta-group (car (screen-groups (current-screen))))
  (:active-ws nil))

(defcommand refresh-all-heads () ()
  (handler-case
      (mapcar (lambda (screen)
                (head-force-refresh screen
                                    (make-screen-heads screen (screen-root screen))))
              (cons (meta-space-screen *metaspace*)
                    (mapcar #'ws-screen (hash-table-values workspace-hash))))
    (error () nil)))

(defun move-group-to-screen (group screen)
  (let ((g group)
      (cs (group-screen group)))
    (setf (group-screen g) screen
          (screen-groups screen) (cons g (screen-groups screen))
          (screen-groups cs) (remove g (screen-groups cs)))))

(defun move-group-to-screen (group screen)
  (let ((current-screen (group-screen group)))
    ;; Set the group-screen to this group
    (setf (group-screen group) screen)
    ;; IMPORTANT: set the group to this screen
    (setf (screen-groups screen) (cons group (screen-groups screen)))
    ;; Now we check that the previous attempts worked, if not we repeat
    ;; Then remove the group from the current screen
    (if (and (eql (group-screen group) screen)
           (member group (screen-groups screen) :test 'eql))
        (removef (screen-groups current-screen) group)

        ;; Noww return the screen-groups
        (move-group-to-screen group screen))))

(defun activate-ws (ws)
  ;; Let's make sure you can only active if theres nothing currently active
  (let ((active (slot-value *metaspace* :active-ws))
        (meta-screen (slot-value *metaspace* :screen)))
    (if (null active)
        (progn
          ;; First step is moving groups
          (dolist (x (ws-groups ws))
            (move-group-to-screen x meta-screen))
          (move-group-to-screen (slot-value *metaspace* :meta-group)
                                (ws-screen ws))
          ;; Now we actually set the workspace to active
          (setf (ws-active-p ws) t
                (slot-value *metaspace* :active-ws) ws)))))

(defun deactivate-ws ()
  "Deactivates the current ws"
  (let ((active-ws (slot-value *metaspace* :active-ws)))
    (if active-ws
        (progn
          ;; First step is moving groups back
          (dolist (x (screen-groups (slot-value *metaspace* :screen)))
            (move-group-to-screen x (ws-screen active-ws)))
          (move-group-to-screen (slot-value *metaspace* :meta-group)
                                (slot-value *metaspace* :screen))
          ;; Now we actually set the workspace to inactive
          (if (member (slot-value *metaspace* :meta-group)
                      (screen-groups (slot-value *metaspace* :screen)))
              (setf (ws-active-p active-ws) nil
                    (slot-value *metaspace* :active-ws) nil)
              (deactivate-ws))))))

(defun init-workspace (screen name id)
  (let ((ws (make-instance 'workspace)))
    (setf (ws-number ws) id
          (ws-screen ws) screen
          (ws-name ws) name
          (ws-current-group ws) 0
          (gethash id workspace-hash) ws)))

(defun create-new-workspace (name)
  (init-workspace (init-screen
                   (car (xlib:display-roots *display*)) name "")
                  name (find-free-number (hash-table-keys workspace-hash) 1)))

(defun workspace-start ()
  (let ((ms (current-screen))
        (ws (create-new-workspace "Default")))
    (setf (group-name (car (screen-groups ms)))
          (format nil "~a" (gensym "meta")))
    (defparameter *metaspace* (make-meta-space))
    (activate-ws ws)))

(defun find-group-by-id (id group-list)
  (car (stable-sort
        (copy-seq group-list)
        #'(lambda (x y)
            (< (abs (- (group-number x) id))
               (abs (- (group-number y) id)))))))

(defun switch-to-group-from-id (id group-list)
  (switch-to-group (find-group-by-id id group-list)))

(defun workspace-switch (ws &optional next-group)
  "Allows for an alternative next-group"
  (cond ((null ws) nil)
        (t (let ((active-ws (slot-value *metaspace* :active-ws))
                 (id (group-number (current-group))))
             (if (null (equal ws active-ws))
                 (progn
                   (if active-ws
                       (deactivate-ws))
                   (activate-ws ws)
                   (let ((group-list (screen-groups (slot-value *metaspace* :screen))))
                     (if next-group (switch-to-group next-group)
                         (switch-to-group-from-id id group-list))))
                 ;; return active ws
                 (slot-value *metaspace* :active-ws))))))

  ;; "Note: This does not actually handle switching groups!"
  ;; (let ((active-ws (slot-value *metaspace* :active-ws)))
  ;;   (cond ((null (equal ws active-ws))
  ;;          (if active-ws
  ;;              (deactivate-ws))
  ;;          (activate-ws ws)
  ;;          (if next-group (switch-to-group next-group))))
  ;;   (slot-value *metaspace* :active-ws)))

;; We actually don't used this version because it can't bring groups with it
(defun switch-to-workspace (ws)
  ;; Switch to group as long if it's not currently active
  (workspace-switch ws))


(defun switch-to-workspace (ws)
  ;; Switch to group as long if it's not currently active
  (let ((active-ws (slot-value *metaspace* :active-ws))
      (id (group-number (current-group))))
    (if (null (equal ws active-ws))
        (progn
          (if active-ws
              (deactivate-ws))
          (activate-ws ws)
          ;; (group-forward (current-group)
          ;;                (sort-groups (current-screen)))
          (let ((group-list (screen-groups (slot-value *metaspace* :screen))))
            (switch-to-group-from-id id group-list))))
    ;; return active ws
    (slot-value *metaspace* :active-ws)))

(defun current-ws ()
  (slot-value *metaspace* :active-ws))

(defun format-group-for-menu (group)
  (concat "    " (format-expand *group-formatters* *group-format*  group)))

(defun format-ws-for-menu (ws screen)
  (cons (list (ws-name ws) ws)
        (mapcar (lambda (group)
                  (list  (format-group-for-menu group)
                     (cons ws group)))
                  (screen-groups screen))))

(defun select-group-all ()
  (let* ((screens (hash-table-values workspace-hash))
         (names (mapcan (lambda (ws)
                          (if (ws-active-p ws)
                              (format-ws-for-menu ws (current-screen))
                              (format-ws-for-menu ws (ws-screen ws))))
                        screens))
         (p (position (format-group-for-menu (current-group))
                      names :test 'equal :key 'car)))
    (second (select-from-menu (current-screen)
                       names "Select: " (if (null p) 0 p)))))

(defun ws-groups-all ()
  (when-let ((result (select-group-all)))
    (if (listp result)
        (progn (switch-to-workspace (car result))
               (switch-to-group (cdr result)))
        (switch-to-workspace result))))

(defun switch-to-group-by-name (name)
  (let* ((screens (hash-table-values workspace-hash))
         (names (mapcan (lambda (ws)
                          (let ((screen (if (ws-active-p ws) (current-screen) (ws-screen ws))))
                            (mapcar (lambda (group)
                                      (cons group ws))
                                    (screen-groups screen))))
                        screens))
         (group (find-if #'(lambda (x)
                             (equal name (group-name x)))
                         names :key 'car)))
    (switch-to-workspace (cdr group))
    (switch-to-group (car group))))

;; (defun ws-move )

;; Helpful workspace functions
(defun screen-workspace (screen)
  "This actually has different behavior depending on whether or not
a screen is currently active or not. Technically the current screen should
return a metaspace, but for our purposes that would be far too confusing.
Thus, we will return the active-ws for the current-screen, and the active-ws
for the screen on the ws"
  (let ((active-ws (slot-value *metaspace* :active-ws)))
    (if (equal screen (slot-value *metaspace* :screen))
        active-ws
        (loop for ws in (hash-table-alist workspace-hash)
              if (eql screen (ws-screen (cdr ws)))
                return (cdr ws)))))

(defun group-workspace (group)
  (if group
      (let ((active-ws (slot-value *metaspace* :active-ws)))
        (if (member group (screen-groups (current-screen)) :test 'eql)
            active-ws
            (loop for ws in (hash-table-alist workspace-hash)
                  if (find group (ws-groups (cdr ws)) :test 'eql)
                    return (cdr ws))))))

(defun window-workspace (window)
  (if (or (equal (type-of window) 'tile-window)
          (equal (type-of window) 'float-window))
      (group-workspace (window-group window))))

;; Creates a set of hooks to be used in the case of focus not being in the right workspace
(defun group-workspace-check (current-group last-group)
  "Hooks into focus-group-hook"
  (declare (ignorable last-group))
  (let ((ws (group-workspace current-group)))
    (if (not (equal (current-ws) ws))
        (workspace-switch ws current-group))))

(defun window-workspace-check (current-window last-window)
  "Hooks into focus-window-hook"
  (declare (ignorable last-window))
  (cond ((null current-window) nil)
        (current-window
         (let ((ws (group-workspace (window-group current-window))))
           (if (not (equal (current-ws) ws))
               (workspace-switch ws (window-group current-window)))))))

(defun frame-workspace-check (current-frame last-frame)
  "Hooks into focus-frame-hook"
  (declare (ignorable last-frame))
  (let ((current-window (frame-window current-frame)))
    (cond ((null current-window) nil)
          (current-window
           (let ((ws (group-workspace (window-group current-window))))
             (if (not (equal (current-ws) ws))
                 (workspace-switch ws (window-group current-window))))))))

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Redefine screen commands used in stumpwm to properly set the focus when accessing
;; windows from a different screen

;; This is also much cleaner than what is used by stumpwm
;;------------------------------------------------------------------------------------------------------------------------ ;;
(defun workspace-set-focus (workspace window)
  (workspace-switch workspace (window-group window))
  (xlib:set-input-focus *display* (window-xwin window) :POINTER-ROOT))

(defun screen-fix ()
  (defun screen-set-focus (screen window)
    (declare (ignorable screen))
    (workspace-set-focus (group-workspace (window-group window)) window))

  (defun switch-to-screen (screen)
    (workspace-switch (screen-workspace screen))))

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Commands
;;------------------------------------------------------------------------------------------------------------------------ ;;

(defcommand ws-init () ()
  (workspace-start))

;; (meta-space-groups *metaspace*)
;; (grouplist-all)

(defcommand ws-next () ()
  (let* ((ws-array (print (sort (hash-table-alist workspace-hash) #'< :key 'car)))
         (current-ws-id (ws-number (slot-value *metaspace* :active-ws))) ;; check group-id
         (rest (member current-ws-id ws-array :key 'car)))
    (if (null (cdr rest))
        ;; use the first one.
        (switch-to-workspace (cdar ws-array))
        (switch-to-workspace (cdadr rest)))))

(defcommand ws-prev () ()
  (let* ((ws-array (print (sort (hash-table-alist workspace-hash) #'> :key 'car)))
         (current-ws-id (ws-number (slot-value *metaspace* :active-ws)))
         (rest (member current-ws-id ws-array :key 'car)))
    (if (null (cdr rest))
        ;; use the first one.
        (switch-to-workspace (cdar ws-array))
        (switch-to-workspace (cdadr rest)))))

(defcommand ws-new (name) ((:string "Enter name for workspace: "))
  (if (null name) (error "Not a valid name")
      (if (hash-table-values workspace-hash)
          (create-new-workspace name)
          (workspace-start))))

(defcommand ws-next-with-window () ()
  (let* ((win (current-window)))
    (when (ws-next)
      (move-window-to-group win (current-group)))))

(defcommand ws-prev-with-window () ()
  (let* ((win (current-window)))
    (when (ws-prev)
      (move-window-to-group win (current-group)))))

(defcommand ws-next-marked () ()
  "move the marked windows to the next workspace."
  (let ((group (current-group)))
    (when (ws-next)
      (let ((to-group (current-group)))
        (dolist (i (marked-windows group))
          (setf (window-marked i) nil)
          (move-window-to-group i to-group))))))

(defcommand ws-prev-marked () ()
  "move the marked windows to the prev workspace."
  (let ((group (current-group)))
    (when (ws-prev)
      (let ((to-group (current-group)))
        (dolist (i (marked-windows group))
          (setf (window-marked i) nil)
          (move-window-to-group i to-group))))))

(define-stumpwm-type :all-group (input prompt)
  (declare (ignorable input prompt))
  (let ((match (select-group-all)))
    (or match
       (throw 'error "No such group."))))

(defcommand ws-gmove-marked (group) ((:all-group "Group to move to: "))
  "move the marked windows to the prev workspace."
  (let ((ws (car group))
      (to-group (cdr group)))
    (dolist (i (marked-windows (current-group)))
      (setf (window-marked i) nil)
      (move-window-to-group i to-group))
    (switch-to-workspace ws)
    (switch-to-group to-group)))

(defcommand grouplist-all () ()
  (ws-groups-all))

(defun select-all-group ()
  (select-from-menu (current-screen)
                    (reverse (mapcar (lambda (g)
                                       (cons (format nil "~a:~a" (car g) (ws-name (cdr g))) (cdr g)))
                                     (hash-table-alist workspace-hash)))
                    "Workspaces: "))

(defcommand ws-select () ()
  (when-let ((ws (cdr (select-from-menu (current-screen)
                                        (reverse (mapcar (lambda (g)
                                                           (cons (format nil "~a:~a" (car g) (ws-name (cdr g))) (cdr g)))
                                                         (hash-table-alist workspace-hash)))
                                        "Workspaces: "))))
    (switch-to-workspace ws)))

(defcommand ws-rename (name) ((:string "New name for workspace: "))
  "Rename the current workspace."
  (if (null name) (error "Not a valid name")
      (let ((ws (slot-value *metaspace* :active-ws)))
        (setf (ws-name ws) name))))


