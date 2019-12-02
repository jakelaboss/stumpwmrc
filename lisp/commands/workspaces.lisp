;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Workspaces
;;------------------------------------------------------------------------------------------------------------------------ ;;


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

(defun move-group-to-screen (group screen)
  (let ((g group)
        (cs (group-screen group)))
    (progn
      (setf (group-screen g) screen)
      (setf (screen-groups screen)
            (cons g (screen-groups screen)))
      (setf (screen-groups cs)
            (remove g (screen-groups cs))))))

(defun activate-ws (ws)
  ;; Let's make sure you can only active if theres nothing currently active
  (let ((active (slot-value *metaspace* :active-ws))
        (meta-screen (slot-value *metaspace* :screen)))
    (if (null active)
        (progn
          ;; First step is moving groups
          (mapcar #'(lambda (x)
                      (move-group-to-screen x meta-screen))
                  (ws-groups ws))
          (move-group-to-screen (slot-value *metaspace* :meta-group)
                                (ws-screen ws))
          ;; Now we actually set the workspace to active
          (setf (ws-active-p ws) t
                (slot-value *metaspace* :active-ws) ws)
          ;; (gmove (car (ws-groups ws)))
          (gnext)))))


(defun activate-ws (ws)
  ;; Let's make sure you can only active if theres nothing currently active
  (let ((active (slot-value *metaspace* :active-ws))
        (meta-screen (slot-value *metaspace* :screen)))
    (if (null active)
        (progn
          ;; First step is moving groups
          (mapcar #'(lambda (x)
                      (move-group-to-screen x meta-screen))
                  (ws-groups ws))
          (move-group-to-screen (slot-value *metaspace* :meta-group)
                                (ws-screen ws))
          ;; Now we actually set the workspace to active
          (setf (ws-active-p ws) t
                (slot-value *metaspace* :active-ws) ws)
          ;; (gmove (car (ws-groups ws)))
          ))))


(defun deactivate-ws ()
  "Deactivates the current ws"
  (let ((active-ws (slot-value *metaspace* :active-ws)))
    (if active-ws
        (progn
          ;; First step is moving groups back
          (mapcar #'(lambda (x) (move-group-to-screen x (ws-screen active-ws)))
                  (screen-groups (slot-value *metaspace* :screen)))
          ;; (ws-groups active-ws)) ;; This is empty
          (move-group-to-screen (slot-value *metaspace* :meta-group)
                                (slot-value *metaspace* :screen))
          ;; Now we actually set the workspace to deactive
          (setf (ws-active-p active-ws) nil
                (slot-value *metaspace* :active-ws) nil)))))


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

(defun switch-to-group-from-id (id group-list)
  (switch-to-group
   (car (sort group-list #'(lambda (x y)
                             (< (abs (- (group-number x) id))
                                (abs (- (group-number y) id))))))))

(defun switch-to-workspace (ws)
  ;; Switch to group as long if it's not currently active
  (let ((active-ws (slot-value *metaspace* :active-ws))
        (id (group-number (current-group))))
    (if (null (equal ws active-ws))
        (progn
          (if active-ws
              (deactivate-ws))
          (activate-ws ws)
          (switch-to-group-from-id id
          (screen-groups (slot-value *metaspace* :screen)))))))

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

(defun ws-groups-all ()
  (let* ((screens (hash-table-values workspace-hash))
         (names (mapcan (lambda (ws)
                          (if (ws-active-p ws)
                              (format-ws-for-menu ws (current-screen))
                              (format-ws-for-menu ws (ws-screen ws))))
                        screens))
         (p (position (format-group-for-menu (current-group))
                      names :test 'equal :key 'car)))
    (when-let ((result (second (select-from-menu (current-screen) names "Select: " (if (null p) 0 p)))))
      (if (listp result)
          (progn (switch-to-workspace (car result))
                 (switch-to-group (cdr result)))
          (switch-to-workspace result)))))

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Commands
;;------------------------------------------------------------------------------------------------------------------------ ;;

(defcommand ws-init () ()
  (workspace-start))

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

(defcommand grouplist-all () ()
  (ws-groups-all))

(defcommand ws-select () ()
  (when-let ((ws (cdr (select-from-menu (current-screen)
                                          (reverse (mapcar (lambda (g) (cons (format nil "~a:~a" (car g) (ws-name (cdr g))) (cdr g)))
                                                           (hash-table-alist workspace-hash)))
                                          "Workspaces: "))))
    (switch-to-workspace ws)))

(defcommand ws-rename (name) ((:string "New name for workspace: "))
  "Rename the current workspace."
  (let ((ws (slot-value *metaspace* :active-ws)))
    (setf (ws-name ws) name)))
