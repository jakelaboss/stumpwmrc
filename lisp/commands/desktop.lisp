;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Desktop
;;------------------------------------------------------------------------------------------------------------------------ ;;

;; The Desktop System:
;; Allows for the storage

(in-package :stumpwm)


(defhash *group-images*)

(defvar *group-image-path* (concat *stumpwm-storage* "group-images/"))

(defmacro hash? (key hashtable value)
  `(if (gethash ,key ,hashtable) nil
       (setf (gethash ,key ,hashtable) ,value)))

(defmacro form (spec &rest items)
  `(format nil ,spec ,@items))

(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

(defun group-image-id (group)
  (cons (ws-number (group-workspace group))
        (group-number group)))

(defun group-image-format (group)
  (format nil "~aS~a:G~a.jpg" *group-image-path*
          (ws-number (group-workspace group))
          (group-number group)))

(defun multi-head? ()
  (> (length (group-heads (current-group))) 1))

(defun group-head-image-id (group head)
  (let ((group-id (group-image-id group)))
    (list (car group-id) (cdr group-id)
       (position head (sort (group-heads (current-group))
                            #'< :key #'head-x)))))

;; TODO we could create two different images for the two different heads
;; (defun group-picture ()
;;   )

;; (defun head-picture (group head)
;;   (scrot (group-image-format group)
;;          (format nil "~ax~a" (head-x head) (head-y head))
;;          :down))

#|Okay, interesting idea.
What if I use cl-png like I do with the lotus system?
I could grab the rgb values from screen itself like I do in the hue system.

|#


;; (head-picture (current-group) (car (group-heads (current-group))))


(defun group-picture ()
  (let ((id (group-image-id (current-group)))
      (p (group-image-format (current-group))))
    (when
        (setf (gethash id *group-images*)
              (progn (run-shell-command
                      (if (multi-head?)
                          (format nil "scrot -o -q 50 ~a; convert ~a -gravity South -chop 0x2400 ~a"
                                  p p p)
                          (format nil "scrot -o -q 50 ~a" p))
                      p))))))

(defun image-text (image-path text)
  (let ((px (* (length text) 55)))
    (format nil
            "convert ~a -pointsize 200 -fill white -draw 'text ~a,1000 \"~a\"' ~a"
            image-path
            (- 1920 px)
            text
            image-path)))

(defun group-picture ()
  "Takes a picture of the group, then creates an image for each head of the group.
Naming is done using the S-
"
  (loop for head in (group-heads (current-group))
        do (setf (gethash (group-head-image-id (current-group) head) *group-images*)
                 (let ((l (length (group-heads (current-group))))
                     (p (group-head-image-format (current-group) x)))
                   (run-shell-command
                    (format nil "scrot -o -q 50 ~a;~a" p
                            (image-text p (group-name (current-group)))))))))


(defvar *group-storage-path* (concat *stumpwm-storage* "groups/"))

(defun group-state (n)
  (dump-to-file (dump-group (current-group))
                (form "~agroup-~ax~a" *group-image-path*  n n)))

(defun feh (&rest images)
  (run-commands (format nil "exec sh -c 'feh -- ~{~a ~}'" images)))

(defun workspace-groups (ws)
  "This will always return the right value"
             (if (eql ws (current-ws))
                 (screen-groups (current-screen))
                 (ws-groups ws)))

(defun join-images-all ()
  (labels ((find-groups (ws)
             (if (eql ws (current-ws))
                 (screen-groups (current-screen))
                 (ws-groups ws)))
           (ws-id (n)
             (form "~aresults:S~a.jpg"
                   *group-image-path* n)))
    (when (dolist (ws (hash-table-values workspace-hash))
            (run-shell-command
             (form "convert ~aS~a*.jpg +append ~aresults:S~a.jpg"
                   *group-image-path* (ws-number ws)
                   *group-image-path* (ws-number ws))
             t))
      (run-shell-command
       (form "convert -reverse ~aresults:S*.jpg -append ~aresults.jpg"
             *group-image-path*
             *group-image-path*)))))


;; (join-images-all)

;; (setf tree-test (copy-list (tile-group-frame-tree group)))))

;; (print tree-test)

(defun format-images (group)
  (let* ((multi-head (> (length (group-heads group)) 1))
         (heads
           (if multi-head
               (sort (copy-seq (group-heads group))
                     #'(lambda (x y)
                         (> (length (head-frames group x))
                            (length (head-frames group y)))))
               (group-heads group)))
         (dis (car heads))
         (extra (if multi-head (cadr heads))))

    ;; Clear one of the heads
    ;; (print head)
         (if multi-head
             (clear-frame (car (head-frames group extra)) group))

    (mapcar
     #'(lambda (win frame)
         (send-fake-key win (parse-key "RET"))
         (pull-window win frame))

     (print (sort (copy-seq (print (group-windows group)))
                  #'(lambda (x y)
                      (if (< (window-number x) (window-number y)) t
                          (< (ws-number (window-workspace x))
                             (ws-number (window-workspace y)))))))

     (print (sort (copy-seq (head-frames group dis))
                  #'(lambda (x y)
                      (if (> (frame-y x) (frame-y y)) t
                          (< (frame-x x) (frame-x y)))))))))

;; (format-images (slot-value *metaspace* :meta-group))
;; (let ((group (slot-value *metaspace* :meta-group)))
;;   (let ((head (sort (copy-seq (group-heads group))
;;                   #'(lambda (x y)
;;                       (> (length (head-frames group x))
;;                          (length (head-frames group y)))))))
;;     (print head)))

;;     (flat-list (head-frames group head))))

;; (space-this 4)
;;   (mapcar
;;    #'(lambda (win frame)
;;        (send-fake-key win (parse-key "RET"))
;;        (pull-window win frame))

;;    (print (sort (copy-seq (head-windows group head))
;;                 #'(lambda (x y)
;;                     (if (< (window-number x) (window-number y)) t
;;                         (< (ws-number (window-workspace x))
;;                            (ws-number (window-workspace y)))))))

;;    (print (sort (copy-seq (flat-list (head-frames group head)))
;;                 #'(lambda (x y)
;;                     (if (> (frame-y x) (frame-y y)) t
;;                         (< (frame-x x) (frame-x y)))))))))

  ;; (head-frames (current-group))
  ;; (car (tile-group-frame-tree (current-group)))

(defun display-images ()
  (run-commands (print (format nil "exec feh -g 1280x720 -. -w -A \";\" --bg-color black ~aS*.jpg"
                         *group-image-path*))))

(defun feh-refresh ()
  (inferior-shell:run/s (format
                         nil "kill SIGUSR1 ~a"
                         (inferior-shell:run/s "pidof feh")))
  (run-shell-command "ps | grep feh -g"))


(defun parse-image-name (image)
  (let ((p (cl-ppcre:scan-to-strings "S.:G." image)))
    (if p
        (cons (parse-integer (subseq p 1 2))
              (parse-integer (subseq p 4 5))))))


(defun desktop-switch ()
  ;; Switch to group as long if it's not currently active
  (let* ((win (current-window))
         (p (if win (parse-image-name
                     (window-name win))
                nil)))

    ;; To reload the image
    (if win (send-fake-key (current-window) (parse-key "RET")))
    ;; So we are always adding the group to the current screen, so we
    ;; should always deactivate. but let's check anyway
    (if p
        (progn
          (if (current-ws)
              (deactivate-ws))
          (activate-ws (gethash (car p) workspace-hash))
          (let ((group-list (screen-groups (slot-value *metaspace* :screen))))
            (switch-to-group-from-id (cdr p) group-list))))))

;; (parse-image-name (window-name (nth 1 (group-windows (current-group)))))


(defun interactive-switch ()
  (set-focus-color "darkcyan")
  (defparameter *window-border-style* :thin)
  (desktop-switch))

;; click hook
(let ((top-level *standard-output*))
  (defun desktop-click (screen win x y)
    (declare (ignorable screen win x y))
    (exit-interactive-keymap 'desktop-ws)
    (format top-level "Clicked")
    (remove-desktop-click)
    (interactive-switch)))

(defun add-desktop-click ()
  (stumpwm::add-hook *click-hook* 'desktop-click))

(defun remove-desktop-click ()
  (stumpwm::remove-hook *click-hook* 'desktop-click))

(define-interactive-keymap desktop-ws
    (:on-enter #'add-desktop-click :on-exit #'(lambda ()
                                                (remove-desktop-click)
                                                (interactive-switch)))
  ;; Movement Mapping ;;
  ((kbd "j") "move-focus down")
  ((kbd "h") "move-focus left")
  ((kbd "k") "move-focus up")
  ((kbd "l") "move-focus right")
  ((kbd ";") "colon")
  ((kbd ":") "eval"))

(defun restore-desktop (ddump)
  "Restore all frames, all groups, and all screens."
  (dolist (sdump (ddump-screens ddump))
    (let ((screen (find (sdump-number sdump) *screen-list*
                        :key 'screen-id :test '=)))
      (when screen
        (restore-screen screen sdump)))))

(defun restore-from-file (file)
  "Restores screen, groups, or frames from named file, depending on file's
contents. Defaults to the XDG_DATA_HOME location with .dump file types."
  (let ((dump
        (with-open-file (fp file :direction :input)
          (with-standard-io-syntax
            (let ((*package* (find-package :stumpwm)))
              (read fp))))))
    (typecase dump
      (gdump
       (restore-group (current-group) dump)
       (message "Group restored."))
      (sdump
       (restore-screen (current-screen) dump)
       (message "Screen restored."))
      (ddump
       (restore-desktop dump)
       (message "Desktop restored."))
      (t
       (message "Don't know how to restore ~a." dump)))))


(defun switch-to-metaspace ()
  (move-group-to-screen (slot-value *metaspace* :meta-group)
                        (current-screen))
  (switch-to-group (slot-value *metaspace* :meta-group)))


;; (sleep 10)

;; (space-this 4)
;; ((okay let's build a group))

;; I think we should it on the specific head
;; maybe ask?

(defparameter *selected-head* nil)
(defun selected-head ()
  (cond ((null *selected-head*)
         (select-head))
        ((find *selected-head*
               (group-heads (current-group))
               :test 'equal)
         *selected-head*)
        (t (select-head))))

;; (time (selected-head))

(defun select-head ()
  (when-let ((head (select-from-menu (current-screen)
                                     (amapcar (cons (format nil "~a" (head-number x)) x)
                                              (group-heads (current-group)))
                                     "Select head to map desktop ")))
    (setf *selected-head* (cdr head))))

(defun remove-all-head (head)
  "remove head on all screens"
  (mapcar (lambda (screen)
            (remove-head screen head))
          (cons (meta-space-screen *metaspace*)
                (mapcar #'ws-screen (hash-table-values workspace-hash)))))

;; (print (split-head-by (current-head) 4))

#|
Thoughts:
I could change the border width and color when moving to the desktop group to more easily show which group is active.
I could also display text on each screenshot with the name of each group.

|#

;; (move-group-to-screen (slot-value *metaspace* :meta-group)
;;                       (ws-screen (current-ws))))

;; (dump-screen (current-screen))

(defcommand display-ws () ()
  (space-this 4)
  (set-focus-color "white")
  (defparameter *window-border-style* :thick)
  (desktop-ws))


(defcommand group-update-picture () ()
  (group-picture))


(defvar *desktop* (concat *stumpwm-storage* "desktop/"))
;; really all I need is the name, the number, and the screen

;; (defun store-desktop ()
;;   (let ((hs (hash-table-values workspace-hash)))
;;     (loop for ws in hs
;;           do (with-open-file (s (format nil "~ascreen-~a" *desktop* (ws-number ws)))
;;                (write (list (ws-name ws)
;;                          (ws-number ws)
;;                          (dump-screen (if (ws-active-p ws) (current-screen)
;;                                           (ws-screen ws))))
;;                       s)))))


(defun store-desktop ()
  (let ((hs (hash-table-values workspace-hash)))
    (with-open-file (s "desktop" :direction :output)
      (write (loop for ws in hs
                   collect (list (ws-name ws) (ws-number ws)
                                 (loop for g in (screen-groups
                                                 (if (ws-active-p ws) (current-screen)
                                                     (ws-screen ws)))
                                       collect (dump-group g))))
             :stream s))))

;; (print (dump-group (current-group)))

;; ;; sb-pcl:slot-definition-name
;; ;; (defmacro store)

;; ;; (ql:quickload :stumpwm)

;; (sosei:generate-generic-storage-method frame)
;; (sosei:generate-generic-storage-method tile-window)
;; (sosei:generate-generic-storage-method gdump)
;; (sosei::generate-generic-storage-struct fdump)
;; (mapcar #'sb-pcl:slot-definition-name (sb-pcl:class-direct-slots (find-class 'fdump)))

;; (hyperluminal-mem::decl-msize-struct gdump :reader-names
;;                                      '(number name tree current))


;; (defmethod msize-object ((f gdump) index)
;;   (bt:with-recursive-lock-held (*generic-storage-lock*)
;;     (hyperluminal-mem:msize* index
;;                              (slot-value sosei::f 'number) (slot-value sosei::f 'name)
;;                              (slot-value sosei::f 'tree) (slot-value sosei::f 'current))))

;; (defmethod sosei::msize-object ((f gdump) index)
;;   ;; (declare (type sosei::mem-size index))
;;   (sosei::msize* index (gdump-number f)
;;                  (gdump-name f)
;;                  (gdump-tree f)
;;                  (gdump-current f)))

;; (defmethod sosei::msize-object ((f fdump) index)
;;   ;; (declare (type sosei::mem-size index))
;;   (sosei::msize* index (fdump-number f) (fdump-x f)
;;                  (fdump-y f) (fdump-width f) (fdump-height f)
;;                  (fdump-windows f) (fdump-current f)))

;; (defmethod hyperluminal-mem:mwrite-object ((sosei::f fdump) sosei::ptr sosei::index sosei::end-index)
;;   (declare (type hyperluminal-mem:mem-size sosei::index sosei::end-index))
;;   (bordeaux-threads:with-recursive-lock-held (sosei::*generic-storage-lock*)
;;     (hyperluminal-mem:mwrite* sosei::ptr sosei::index sosei::end-index
;;                               (fdump-number sosei::f) (fdump-x sosei::f)
;;                               (fdump-y sosei::f) (fdump-width sosei::f)
;;                               (fdump-height sosei::f) (fdump-windows sosei::f)
;;                               (fdump-current sosei::f))))

;; (sosei:pwrite* "current-group" (type= (classed-p (class-of (dump-group (current-group))) structure-class)))

;; (sosei:pwrite* "current-group" (dump-group (current-group)))

;; (let ((f (current-frame)))
;;   (list (slot-value f 'number) (slot-value f 'x)
;;         (slot-value f 'y) (slot-value f 'width)
;;         (slot-value f 'height) (sosei::msize 0 (slot-value f 'window))))


;; (sosei:msize 0 (current-frame))

;; (sosei:generate-generic-storage-method tile-window)
;; (sosei:generate-generic-storage-method head)
;; (sosei:generate-generic-storage-method group)
;; (sosei:generate-generic-storage-method tile-group)


;; (sosei:msize 0 (eval `(sosei:generate-generic-storage-method
;;                        ,(class-name (class-of (slot-value (current-frame) 'window))))))


;; (sosei:msize 0 (mapcar #'sb-pcl:slot-definition-name (sb-pcl:class-direct-slots (find-class 'head)))
;;              (class-of (slot-value (slot-value (current-frame) 'window) 'frame)))

;; (hyperluminal-mem::decl-mserializable-struct frame)
;; (hyperluminal-mem:msize 0 (make-frame :number 1 ))

;; (current-window)
;; (sosei:msize 0 (current-frame))
;; ;; okay this errors out. Let's investigate
;; (class-of (type-of (current-frame)))
;; ;; -> structure-class *head*
;; (sosei:generate-generic-storage-method stumpwm::head)
;; (sosei:msize 0 (slot-value (current-frame) 'name))

;; (sosei:generate-generic-storage-method window)

;; (print (list (slot-value (current-window) 'xwin) (slot-value (current-window) 'width)
;;        (slot-value (current-window) 'height) (slot-value (current-window) 'x)
;;        (slot-value (current-window) 'y) (slot-value (current-window) 'gravity)
;;        (slot-value (current-window) 'group) (slot-value (current-window) 'number)
;;        (slot-value (current-window) 'parent) (slot-value (current-window) 'title)
;;        (slot-value (current-window) 'user-title) (slot-value (current-window) 'class)
;;        (slot-value (current-window) 'type) (slot-value (current-window) 'res)
;;        (slot-value (current-window) 'role) (slot-value (current-window) 'unmap-ignores)
;;        (slot-value (current-window) 'state) (slot-value (current-window) 'normal-hints)
;;        (slot-value (current-window) 'marked) (slot-value (current-window) 'plist)
;;              (slot-value (current-window) 'fullscreen)))

;; (sosei:msize 0 (current-frame))
;; (class-of (current-window))
;; (sosei:pwrite* "test-frame-sosei" (current-frame))

(defmethod hyperluminal-mem:msize-object ((sosei::f tile-window) sosei::index)
  (hyperluminal-mem:msize* sosei::index (slot-value sosei::f 'frame)
                           (slot-value sosei::f 'normal-size)))

;; (slot-value (current-frame) :frame-tree)
;; frame

;; (eval
;;  `(defmethod sosei::msize-object ((f frame) index)
;;     (hyperluminal-mem:msize* index
;;                              ,@(mapcar #'(lambda (x)
;;                                            `(slot-value f ',x))
;;                                        (mapcar #'sb-pcl:slot-definition-name
;;                                                (sb-pcl:class-direct-slots (class-of (current-frame))))))))

;; (type-of (current-frame))
;; ( (sosei:pread* "test-frame-sosei"))
;; (inferior-shell:run/s "ls")

;; (store-desktop)

(defun subseq-from-end (sequence end)
  (reverse (subseq (reverse sequence) 0 end)))

(defun create-groups (id gname-list)
               (loop for x in gname-list
                     do (let* ((screen (ws-screen (gethash id workspace-hash)))
                               (g (find-group screen "Default")))
                          (if g
                              (setf (group-name g) x)
                              (add-group screen x)))))

;; Screw it! We'll do it live
(defun restore-desktop ()
  (labels ((create-groups (id gname-list)
             (loop for x in gname-list
                   do (let* ((screen (ws-screen (gethash id workspace-hash)))
                             (g (find-group screen "Default")))
                        (if g
                            (setf (group-name g) x)
                            (add-group screen x))))))
    (let ((ms (current-screen)))
      (progn
        (defparameter *metaspace* (make-meta-space))
        (create-new-workspace "screen-1")
        (create-new-workspace "screen-2")
        (create-new-workspace "screen-3")
        (create-new-workspace "screen-4")
        (create-groups 1 '("relax" "browse-main" "work" "videos"))
        (create-groups 2 '("connections" "ops" "games" "media"))
        (create-groups 3 '("servers" "lisp" "meta" "comms"))
        (create-groups 4 '("head-space" "development" "reading" "music"))
        (screen-fix)
        (setf (group-name (car (screen-groups ms)))
              (format nil "~a" (gensym "meta")))
        ;; (activate-ws (gethash 4 workspace-hash))
        (switch-to-workspace (gethash 4 workspace-hash))))))

(defun remove-extra-heads ()
  "Removes the extra heads from every group in the workspace"
  (flet ((remove-heads (h)
           (dolist (x (cdr h))
             (remove-head (car h) x))))
    (let ((head-alist
            (mapcar #'(lambda (x)
                        (cons (ws-screen x)
                              (remove-if #'(lambda (x)
                                             (> (head-width x) 2560))
                                         (screen-heads (ws-screen x)))))
                    (hash-table-values workspace-hash))))

      (dolist (v (cons (cons (meta-space-screen *metaspace*)
                             (screen-heads (meta-space-screen *metaspace*)))
                       head-alist))
      (remove-heads v)))))

(defun apply-to-all-groups (function)
  "Removes the extra heads from every group in the workspace"
    (let ((group-alist
            (mapcar #'(lambda (x)
                        (cons (ws-screen x)
                              (remove-if #'(lambda (x)
                                             (> (head-width x) 2560))
                                         (screen-heads (ws-screen x)))))
                    (hash-table-values workspace-hash))))

      (dolist (v (cons (cons (meta-space-screen *metaspace*)
                             (screen-heads (meta-space-screen *metaspace*)))
                       head-alist))
        (remove-heads v))))

;; (print (dump-group (current-group)))


#| (let ((ms (current-screen)))
(setf (group-name (car (screen-groups ms)))
      (format nil "~a" (gensym "meta")))
(defparameter *metaspace* (make-meta-space))
(activate-ws (gethash 4 workspace-hash)))
|#

;; (defun restore-desktop ()
;;   (let ((ms (current-screen)))
;;     (loop for x in (directory (concat *desktop* "*"))
;;           do (let* ((name (subseq-from-end (namestring x) 8))
;;                     (id (parse-integer (subseq-from-end (namestring x) 1)))
;;                     (screen (init-screen
;;                              (car (xlib:display-roots *display*)) name "")))
;;                (init-workspace screen name id)
;;                (restore-screen screen (read-dump-from-file
;;                                        (namestring x)))))
;;     (setf (group-name (car (screen-groups ms)))
;;           (format nil "~a" (gensym "meta")))
;;     (defparameter *metaspace* (make-meta-space))
;;     (activate-ws (gethash 1 workspace-hash))))

;; (with-open-file (fp file :direction :input)
;;   (with-standard-io-syntax
;;     (let ((*package* (find-package :stumpwm)))
;;       (read fp))))

;;   (concat *desktop* "screen-1")))

;; (workspace-groups (gethash 1 workspace-hash))
;; (workspace-groups (gethash 2 workspace-hash))
;; (restore-screen (current-screen)
;;                 (read-dump-from-file
;;                  (concat *desktop* "screen-1")))

(defcommand init-desktop () ()
  (run-commands "enter-master-key")
  (restore-desktop)
  ;; (start-image-loop)
  )

;; (screen-groups (ws-screen (car (hash-table-values workspace-hash))))       ;

;; (dump-screen (current-screen))
;; (dump-screen-to-file  "screen-4")

;; (if (switch-to-group (find-group (current-screen) ".metaspace"))
;;     (restore-from-file (concat *group-image-path* (form "group-~ax~a" n n))))

;; (dotimes (x n)
;;   (run-commands "exec urxvt"
;;                 "move-focus :right")))


;; (defun query-image (screen-num group-num)
;;     (gethash (cons screen-num group-num) *group-images*))

;; (defun display-image ()
;;   (let ((nums (mapcar #'(lambda (ws)
;;                         (cons (car ws)
;;                               (mapcar #'group-number (ws-groups (cdr ws)))))
;;                     (hash-table-alist workspace-hash))))
;;     (loop for y in (hash-table-alist workspace-hash)
;;           collect (loop for g in (cdr y)
;;                         ;; Set the
;;                         collect
;;                         (query-images)))))

;; (feh (concat *group-image-path* "S1:G1.png"))


;; (defun format-layout-for-images (workspaces)
;;   ;; no, we do splits by number of screens, then by number of groups
;;   ;; Okay this get's us the right frame splits, but let's go farther
;;   (let ((workspaces (hash-table-values workspace-hash)))
;;     (split-frame-eql-parts (current-group) :row (length workspaces))
;;     (loop for ws in workspaces
;;           do
;;              ;; Set's our splits
;;              (let ((len (if (eql (slot-value *metaspace* :active-ws) ws)
;;                           (length (screen-groups (current-screen)))
;;                           (length (ws-groups ws)))))
;;                (split-frame-eql-parts (current-group) :column len)
;;                ;; Honestly I think I should set the group images in here

;;                ;; (dotimes (x len)
;;                  ;; (run-commands "exec urxvt")
;;                  ;; (move-focus :right))
;;                (move-focus :down)))))


;; (defun set-to-frames (workspaces))
;; ;; New we'll grab something new
;; (dotimes (x))

;; (loop for x in (head-frames (current-group) (current-head))
;;       for num unt)


;; (gnew)
;; (add-group (current-screen) ".metaspace")


;; (defun float-display ()
;;   (let ((win (cadr (group-windows (current-group)))))
;;     (float-window  win (current-group))
;;     (run-shell-command "urxvt -c ")
;;     (unfloat-window win (current-group))
;;     (kill-window win)


;; (print *metaspace*)
;; ((current-screen))

;; (screen-current-msg-highlights (current-screen))

;; (defun format-group-for-display (group)
;;   ;; Let's set a limit
;;   (if (group))
;;   (let ((limit 20)
;;       (form (format-expand *group-formatters* *group-format* group)))
;;     (concat form (make-string (- 15 (length form)) :initial-element #\Space))))

;; (grouplist)


;; (defun grouplist-function-all ()
;;   (labels ((group-names (grouplist)
;;              (format nil "~a~%"
;;                      (apply #'concat
;;                             (mapcar #'(lambda (g)
;;                                         (format-group-for-display g))
;;                                     (sort (copy-seq grouplist) #'< :key #'group-number))))))

;;   (let* ((ws-array (print (sort (hash-table-alist workspace-hash) #'> :key 'car)))
;;          (current-ws-id (print (ws-number (slot-value *metaspace* :active-ws))))) ;; check group-id

;;     (defvar *strings*
;;      (mapcar
;;       #'(lambda (x)
;;           (if (eql (slot-value *metaspace* :active-ws) (cdr x))
;;               (group-names (screen-groups (current-screen)))
;;               (group-names (ws-groups (cdr x)))))
;;       ws-array)))))

;; (defcommand grouplist-all () ()
;;   ;; We need to do character limits
;;   (grouplist-function-all))

;; (grouplist-all)

;; (defun find-location-of-group
;;     (let (
;;         (g (current-group))
;;         ()
;;         )))

;;       (ccontext-px )


;; (multiple-value-bind (width height)
;;     (rendered-size *strings* (screen-message-cc (current-screen)))
;;   (setup-message-window (current-screen) width height)
;;   (render-strings-groups (screen-message-cc (current-screen))
;;                   *message-window-padding*
;;                   *message-window-y-padding*
;;                   *strings*
;;                   '((2 10 20))))

;; (defun render-strings-groups (cc padx pady strings highlights)
;;     (let* ((gc (ccontext-gc cc))
;;            (xwin (ccontext-win cc))
;;            (px (ccontext-px cc))
;;            (strings (mapcar (lambda (string)
;;                               (if (stringp string)
;;                                   (parse-color-string string)
;;                                   string))
;;                             strings))
;;            (y 0))
;;   ;; Create a new pixmap if there isn't one or if it doesn't match the
;;       ;; window
;;       (when (or (not px)
;;                (/= (xlib:drawable-width px) (xlib:drawable-width xwin))
;;                (/= (xlib:drawable-height px) (xlib:drawable-height xwin)))
;;         (if px (xlib:free-pixmap px))
;;         (setf px (xlib:create-pixmap :drawable xwin
;;                                      :width (xlib:drawable-width xwin)
;;                                      :height (xlib:drawable-height xwin)
;;                                      :depth (xlib:drawable-depth xwin))
;;               (ccontext-px cc) px))
;;       ;; Clear the background
;;       (xlib:with-gcontext (gc :foreground (xlib:gcontext-background gc))
;;         (xlib:draw-rectangle px gc 0 0
;;                              (xlib:drawable-width px)
;;                              (xlib:drawable-height px) t))
;;       (loop for parts in strings
;;             for row from 0 to (length strings)
;;             for line-height = (max-font-height parts cc)
;;             if

;;             (find row highlights :key 'car :test 'eql)
;;             do
;;                ;; (if (check eatch part, we might need to change that render strings function)
;;                (render-string (subseq (car parts) 0 (cadr (find row highlights :key 'car :test 'eql)))
;;                               cc (+ padx 0) (+ pady y))
;;       (xlib:draw-rectangle px gc 0 (+ pady y) (xlib:drawable-width px) line-height t)

;;                (xlib:with-gcontext (gc :foreground (xlib:gcontext-background gc)
;;                                        :background (xlib:gcontext-foreground gc))
;;                  ;; If we don't switch the default colors, a color operation
;;                  ;; resetting either color to its default value would undo the
;;                  ;; switch.
;;                  (rotatef (ccontext-default-fg cc) (ccontext-default-bg cc))
;;                  (render-string (subseq (car parts) (cadr (find row highlights :key 'car :test 'eql))
;;                                         (caddr (find row highlights :key 'car :test 'eql)))
;;                                 cc (+ padx 0) (+ pady y))
;;                  (rotatef (ccontext-default-fg cc) (ccontext-default-bg cc)))
;;                (render-string (subseq (car parts) (caddr (find row highlights :key 'car :test 'eql)))
;;                               cc (+ padx 0) (+ pady y))
;;             else
;;               do (render-string parts cc (+ padx 0) (+ pady y))
;;             end
;;             do (incf y line-height))
;;       (xlib:copy-area px gc 0 0
;;                       (xlib:drawable-width px)
;;                       (xlib:drawable-height px) xwin 0 0)
;;       (reset-color-context cc)
;;       (values)))
