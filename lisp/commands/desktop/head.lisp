(in-package :stumpwm)

;; (defhash *head-images*)

(defun create-directory (dir)
  (if (not (osicat:directory-exists-p dir))
      (osicat-posix:mkdir dir 16877)))

(defun nested-directory-check (path)
  (labels ((check (lst)
             (cond ((null (cdr lst)))
                   ((consp (cdr lst))
                    (let ((dir (format nil "~a/~a" (car lst) (cadr lst))))
                      (create-directory dir)
                      (check (cons dir (cddr lst))))))))
    (check (cl-ppcre:split #\/ path))))

;; since there are a different number of images depending on the number of heads,
;; we'll concatenate the head-image-path
(let ((checked? (make-hash-table :test 'equal)))
  (defun group-image-path ()
    "Returns the image path based on the number of heads"
    (let ((dir (concat *group-image-path* (format nil "desktop-~a/" (length (group-heads (current-group)))))))
      (unless (gethash dir checked?)
        (nested-directory-check dir)
        (setf (gethash dir checked?) t))
      dir)))

(defun head-image-id (group head)
  ;; add heads
  (cons (ws-number (group-workspace group))
        (cons (group-number group)
              (head-number head))))

(defun head-image-format (group head)
  "Returns nil if a format can't be used"
  (let* ((heads (copy-seq (group-heads group)))
         (path (group-image-path))
         (ws (ws-number (group-workspace group)))
         (group (group-number group))
         (pos (position head (sort heads
                                   (lambda (x y) (< (head-x x) (head-x y)))))))
    (if (and path ws group pos)
        (format nil "~aS~a:G~a:H~a.jpg" path ws group pos))))

(defun multi-head? ()
  (> (length (group-heads (current-group))) 1))

(defun scrot (path x y w h &optional note-text (color (list 255 255 255)))
  (let ((px (* (length note-text) 55))
        (font "'/usr/share/fonts/TTF/DejaVuSans-Oblique/160'"))
    (inferior-shell:run (concat "scrot -o "
                                (format nil "-a ~a,~a,~a,~a " x y w h)
                                (if note-text (format nil "-n \"-f ~a -t '~a' -x ~a -y 1000 -c ~a \" "
                                                      font note-text (- 1920 px)
                                                      (subseq (format nil "~{,~a~}" color) 1)))
                                path))))

;; Example:
;; (scrot "/home/vagabond/test.jpg" 0 0 3840 2160 "test")

(defun head-picture (group head)
  (let* ((path (head-image-format group head))
         (hx (head-x head)) (hy (head-y head))
         (hw (head-width head)) (hh (head-height head)))
    (if path
        (scrot path hx hy hw (+ hy hh)
               (group-name group)))))

(defun group-picture ()
  ;; take the full picutre
  (loop for h in (group-heads (current-group))
        do (head-picture (current-group) h)))

(defun clear-head-images ()
  (inferior-shell:run/s (format nil "rm ~a*" (group-image-path))))

(defun display-images ()
  (run-commands (format nil "exec feh  -. -w -A \";\" --image-bg black ~aS*.jpg"
                        (group-image-path))))

(defun delete-display-images ()
  (let ((pid (inferior-shell:run/s "pidof feh")))
    (if pid (inferior-shell:run/s (format nil "kill ~a"pid)))))

(defun merge-lists-by (n &rest lists)
  "Merge lists by some interval.
Example:
list1 = (0 1 2 3 4 5 6 7)
list2 = (9 10 11 12 13 14 15)
(merge-list-by 4 list1 list2)
> (0 1 2 3 9 10 11 12 4 5 6 7 13 14 15)"
  (let* ((n-lsts (length lists))
         (results))
    (loop for i from 0 to (1- (* n-lsts n))
          do (loop for x from 0 to (1- n)
                   do (setf results (append results
                                            (list (nth (+ x (* (floor i n-lsts) n))
                                                    (nth (mod i n-lsts) lists)))))))
    results))

(defun group-by-into (n lists)
  "Merge lists by some interval.
Example:
list1 = (0 1 2 3 4 5 6 7)
list2 = (9 10 11 12 13 14 15)
(merge-list-by 4 list1 list2)
> (0 1 2 3 9 10 11 12 4 5 6 7 13 14 15)"
  ;; Merge the lists by some interval
  ;; so first we group by 4, then merge lists by 1
  (let* ((results))
    (loop for i from 0 to
          do (loop for x from 0 to (1- n)
                   do (setf results (append results
                                            (list (nth (+ x (* (floor i n-lsts) n))
                                                       (nth (mod i n-lsts) lists)))))))
    results))

(defun get-sorted-frames (group &optional (direction 'horizontal))
  "Return list of frames sorted by the x coordinates of "
    ;;; Merge the two lists, grabs the first 4 of 0, then the first four of 1, then back around
  (labels ((pred (x y) (if (> (frame-y x) (frame-y y)) t
                           (< (frame-x x) (frame-x y)))))
    (let* ((heads (sort (copy-seq (group-heads group)) (lambda (x y) (< (head-x x) (head-x y)))))
           (head-frames (mapcar (lambda (head) (sort (copy-seq (head-frames group head)) #'pred)) heads))
           (len (length (car head-frames)))
           (div (round (sqrt len))))
      (case direction
        (horizontal (apply #'merge-lists-by div head-frames))
        (vertical (let ((d (sosei::group (flat-list head-frames) 4)))
                    (flat-list (loop for x from 0 to (1- (length d)) by 2
                                     collect
                                     (loop for i in (nth x d)
                                           for j in (nth (1+ x) d)
                                           collect i into result
                                           collect j into result
                                           finally (return result))))))))))

(defun format-images-with-heads (group)
  (labels ((direction (heads)
             (if (null (cdr heads)) 'horizontal
                 (loop for x in (cdr heads)
                       for y = (car heads)
                       do (if (or (>= (head-y x) (+ (head-y y) (head-height y)))
                                 (>= (head-y y) (+ (head-y x) (head-height x))))
                              (return 'vertical)
                              (setf y x))
                       finally (return 'horizontal)))))
    (let* ((windows (sort (copy-seq (group-windows group))
                          #'(lambda (x y)
                              (if (< (window-number x) (window-number y)) t
                                  (< (ws-number (window-workspace x))
                                     (ws-number (window-workspace y)))))))

      ;; Okay, so if the x and y of different groups are greater than each other
      (direction (direction (group-heads group)))
      (frames (get-sorted-frames group direction)))
    (mapcar
     #'(lambda (win frame)
         (send-fake-key win (parse-key "RET"))
         (pull-window win frame))
     windows frames))))

(defun switch-to-head (group head)
  (switch-to-group group)
  (focus-frame group (car (head-frames group head))))

(let ((restored? nil))
  (defun space-this (n)
    (switch-to-metaspace)
    (let ((g (meta-space-meta-group *metaspace*)))
      (unless restored?
        (loop for h in (group-heads g)
              do (head-grid g h n))
        (setf restored? t))
      (unless (group-windows g)
        (display-images))
      (format-images-with-heads g))))

;; (space-this 4)

;; (let ((restored? nil))
;;   (defun space-this (n)
;;     (switch-to-metaspace)
;;     (let ((g (meta-space-meta-group *metaspace*)))
;;       (unless restored?
;;         (loop for h in (group-heads g)
;;               do (head-grid g h n))
;;         (setf restored? t))
;;       (let ((displayed (= (length (group-windows g))
;;                         (length (group-frames g)))))
;;         (unless displayed
;;           (delete-display-images)
;;           (display-images)))
;;       (format-images-with-heads g))))

;; let's take a screenshot every 10s
;; (space-this 4)

(defvar image-loop-running nil)

(defun start-image-loop ()
  (unless image-loop-running
    (setf image-loop-running t)
    (bt:make-thread
     (lambda ()
       (loop while image-loop-running
             do (progn (sleep 10)
                       (unless (equal (current-group)
                                      (meta-space-meta-group *metaspace*))
                         (group-picture)))))
     :name "Image loop")))

(defun stop-image-loop ()
  (setf image-loop-running nil))

;; (find-thread "Image loop")
;; (stop-image-loop)
;; (start-image-loop)
