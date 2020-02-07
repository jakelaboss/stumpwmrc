(ql:quickload '(:stumpwm :jonathan :inferior-shell :sqlite))
(in-package :stumpwm)

(defmacro amapcar (function list)
  "implicit lambda with x as argument"
  `(let (x)
     (mapcar (lambda (x) ,function) ,list)))

(defparameter *session-file*
  (concat (namestring (car
                       (directory
                        "/home/jake/.mozilla/firefox/*dev-edition-default/sessionstore-backups/recovery.jsonlz4")))
          " "))

(defvar *menu-max-length* 20)

(defun remove-from-end (sequence end)
  (reverse (subseq (reverse sequence) end)))

;; requires lz4json and jq
;; (time (defparameter *session*
(defun current-session ()
  "Get session from firefox cache"
  (jonathan:parse
   (format
    nil "[ ~a ]"
    (remove-from-end
     (cl-ppcre:regex-replace-all
      "]"
      (inferior-shell:run/ss
       (format nil
               "lz4jsoncat ~a | jq -r '.windows[].tabs | map(.entries[].title)'"
               *session-file*))
      "],")
     1))))


(defun all-workspace-windows ()
  "All windows on all screens"
  (remove nil (flat-list
             (copy-seq
              (amapcar
               (if (eql (current-ws) x)
                   (screen-windows (current-screen))
                   (screen-windows (ws-screen x)))
               (hash-table-values workspace-hash))))))

(defun current-windows ()
  "Get current browser window names and object"
  (remove nil (mapcar #'(lambda (x)
                        (let ((b (cl-ppcre:scan-to-strings ".+(?= - Firefox)" (window-name x))))
                          (if b (cons b x))))
                    (all-workspace-windows))))



;; (current-windows)

(defun browser-session-check (session)
  (let ((tabs (mapcar #'(lambda (tab)
                        (if (> (length tab) 30)
                            (subseq tab 0 30)
                            ;; we do this check because the current method for menu limits doesn't work
                            tab))
                    session)))
    (if (> (length tabs) *menu-max-length*)
        (subseq tabs 0 *menu-max-length*)
        ;; we do this check because the current method for menu limits doesn't work
        tabs)))


;; Really just a re-implementation of run-menu with a length check
(defun run-browser-menu (screen menu)
  "Runs the browser menu. Implement all of the methods in the menu, then pass an instance to this function"
  (declare (type menu menu))
  ;; align the menu, make the pages
  (bound-check-menu menu)
  (catch :menu-quit
    (unwind-protect
         (with-focus (screen-key-window screen)
           (let ((*suppress-echo-timeout* t))
             (loop
                (let* ((sel (menu-selected menu))
                       (start (menu-view-start menu))
                       (end (menu-view-end menu))
                       (len (length (menu-table menu)))
                       (prompt-line (menu-prompt-line menu))
                       (strings (get-menu-items menu))
                       (highlight (- sel start)))
                  ;; (unless (zerop start)
                  ;;   (setf strings (cons "..." strings))
                  ;;   (incf highlight))
                  ;; (unless (= len end)
                  ;;   (setf strings (nconc strings '("..."))))
                  (when prompt-line
                    (push prompt-line strings)
                    (incf highlight))
                  (run-hook-with-args *menu-selection-hook* menu)
                  (echo-string-list screen
                                    ;; string check, only show up to menu-max-length
                                    (browser-session-check strings)
                                    highlight))
                (multiple-value-bind (action key-seq) (read-from-keymap (menu-keymap menu))
                  (if action
                      (progn (funcall action menu)
                             (bound-check-menu menu))
                      (typing-action menu (first key-seq)))))))
      (unmap-all-message-windows))))


;; (defun switch-to-tab (tab-name)
;;   (if (cl-ppcre:scan "Firefox" (window-name (current-window)))
;;       (progn (when (push-key "T")
;;                ;; (sleep 2)
;;                (map nil #'(lambda (x)
;;                           (push-key (format nil "~a" x)))
;;                     tab-name))))

;; (map nil #'(lambda (x) (push-key (format nil "~a" x)))
;;      "file-list")

;; (defcommand switch-tab (tab-name) ((:string "enter tab: "))
;;   (switch-to-tab tab-name))


(defcommand browser-menu () ()
  (unwind-protect
       (let* ((window (current-windows))
              (session (current-session))
              (data (mapcar
                     #'(lambda (y) ;; sorts session and windows into groups
                         (list (car (remove nil (mapcar #'(lambda (x)
                                                       (member (car y) x :test 'equal))
                                                   session)))
                            (cdr y)))
                     window))
              ;; adds window to end of each tab
              (table (remove-duplicates (mapcan #'(lambda (x)
                                                    (mapcar #'(lambda (z)
                                                                (list z (lastcar x)))
                                                            (car x)))
                                                data) :test 'equal)))
         (ignore-errors
          (let ((tab (run-browser-menu (current-screen)
                                     (make-instance 'single-menu
                                                    :table table
                                                    :selected 0
                                                    :prompt "Search for Tab: "
                                                    :view-start 0
                                                    :view-end 0
                                                    :additional-keymap nil
                                                    :FILTER-PRED #'menu-item-matches-regexp))))
            (if tab
                (progn (switch-to-group (window-group (cadr tab)))
                       (select-window-by-number (window-number (cadr tab))))))))))


(defun reset-history-table ()
  (inferior-shell:run
   (format nil "cp ~a ~a"
           (namestring (car (directory "~/.mozilla/firefox/*.dev-edition*/places.sqlite")))
           "~/.mozilla/firefox/tmp/places.sqlite")))

;; let's do it on start
(reset-history-table)

(defparameter browser-places
  (sqlite:connect (namestring (car (directory "~/.mozilla/firefox/tmp/places.sqlite")))))

(defun firefox-newtab (url)
  (stumpwm:run-commands
   (format nil "exec firefox-developer-edition --new-tab \"~a\"" url)))

(defcommand browser-history () ()
  (unwind-protect
       (let ((brower-table (sqlite:execute-to-list browser-places ;; group by the distinct title
                                                 "select title, url from moz_places where title is not null group by title;")))
         (ignore-errors
          (let ((tab (run-browser-menu (current-screen)
                                     (make-instance 'single-menu
                                                    :table browser-table
                                                    :selected 0
                                                    :prompt "Search browser history: "
                                                    :view-start 0
                                                    :view-end 0
                                                    :additional-keymap nil
                                                    :FILTER-PRED #'menu-item-matches-regexp))))
            (if tab
                (firefox-newtab (cadr tab))))))))


;; old data
;; (remove-duplicates (let* ((window (current-windows))
;;                                                                 (session (current-session))
;;                                                                 (data (mapcar #'(lambda (y) ;; sorts session and windows into groups
;;                                                                                   (list (car (remove nil (mapcar #'(lambda (x)
;;                                                                                                                      (member (car y) x :test 'equal))
;;                                                                                                                  session)))
;;                                                                                         (cdr y)))
;;                                                                               window)))
;;                                                            ;; adds window to end of each tab
;;                                                            (mapcan #'(lambda (x) (mapcar #'(lambda (z)
;;                                                                                              (list (if (> (length z) 30)
;;                                                                                                        (subseq z 0 30) z)))
;;                                                                                          (car x)))
;;                                                                    data)) :test 'equal)


                  ;; (let* ((window (current-windows))
                  ;;        (session (current-session))
                  ;;        (data (mapcar #'(lambda (y) ;; sorts session and windows into groups
                  ;;                          (list (car (remove nil (mapcar #'(lambda (x)
                  ;;                                                             (member (car y) x :test 'equal))
                  ;;                                                         session)))
                  ;;                                (cdr y)))
                  ;;                      window)))
                  ;;   ;; adds window to end of each tab
                  ;;   (mapcan #'(lambda (x) (mapcar #'(lambda (z)
                  ;;                                     (list z (lastcar x)))
                  ;;                                 (car x)))
                  ;;           data)))

;; how do I link up a window with current tabs?
;; I could search for an instance of each window inside each tab

;; TODO browser class and methods not currently working
;; (defclass browser-menu (menu)
;;   ((unfiltered-table :initarg :filtered-table
;;                      :initform nil
;;                      :accessor single-menu-unfiltered-table
;;                      :documentation "Holds the values that have been filtered based on
;; current-input and filter-pred")
;;    (filter-pred :initarg :filter-pred
;;                 :initform (error "You must specify a filter predicate")
;;                 :accessor single-menu-filter-pred)
;;    (current-input :initarg current-input
;;                   :initform (make-array 10 :element-type 'character
;;                                            :adjustable t :fill-pointer 0)
;;                   :accessor single-menu-current-input))
;;   (:documentation "Class used when selecting a single item in a menu. Allows searching through the list."))


;; (defun run-browser-menu (screen menu)
;;   "Runs the menu. Implement all of the methods in the menu, then pass an instance to this function"
;;   (declare (type menu menu))
;;   ;; align the menu, make the pages
;;   (bound-check-menu menu)
;;   (catch :menu-quit
;;     (unwind-protect
;;          (with-focus (screen-key-window screen)
;;            (let ((*suppress-echo-timeout* t))
;;              (loop
;;                (let* ((sel (menu-selected menu))
;;                       (start (menu-view-start menu))
;;                       (end (menu-view-end menu))
;;                       (len (length (menu-table menu)))
;;                       (prompt-line (menu-prompt-line menu))
;;                       (strings (get-menu-items menu))
;;                       (highlight (- sel start)))
;;                  (when prompt-line
;;                    (push prompt-line strings)
;;                    (incf highlight))
;;                  (run-hook-with-args *menu-selection-hook* menu)
;;                  (echo-string-list
;;                   screen
;;                   (if (> (length strings) 10)
;;                       (subseq (print strings) 0 10) ;; so let's change strings to maybe funcall that looks at j{z}
;;                       (print strings))
;;                   highlight))
;;                (multiple-value-bind (action key-seq)
;;                    (read-from-keymap (menu-keymap menu))
;;                  (if action
;;                      (progn (funcall action menu)
;;                             (bound-check-menu menu))
;;                      (typing-action menu (first key-seq)))))))
;;       (unmap-all-message-windows))))

;; (menu-selected single-menu-test)

;; is it menu-maximum-height that breaks it?
;; (setf *menu-maximum-height* 10)
;; so after some testing
;; it's either *menu-max-height or
;; it could be any menu table over 30 values

;; I think the best way to solve this is to someone do the search not on the menu,
;; where nothing get's displayed until theres less than 10
;; items

;; (run-menu (current-screen)


;; (defcommand browser-search (&optional (fmt *window-format*) window-list) (:rest)
;;   (let ((windows (current-windows))
;;       (session (current-session)))
;;     (mapcar #'(lambda (y)
;;                 (list y (car (remove nil (mapcar #'(lambda (x)
;;                                                 (member (car y) x :test 'equal))
;;                                             session)))))
;;             windows))
;;   (let ((window-list (or window-list
;;                       )))
;;     (if (null window-list)
;;         (message "No Managed Windows")
;;         ;; (let ((window (select-window-from-menu window-list fmt)))
;;         (let* ((win-cons (select-from-menu (current-screen) window-list))
;;                (window
;;                  (select-window-by-number
;;                   (cadr win-cons)
;;                   (cddr win-cons))))
;;           (if (null win-cons) (throw 'error :abort)
;;               (progn
;;                 (switch-to-group (cddr win-cons))
;;                 (select-window-by-number (cadr win-cons) (cddr win-cons)))))))

;; (time (firefox-sync))

;; (defcommand firefox-s)

;; (time (let* ((windows (cdr (find :windows *session* :test 'equal :key 'car)))
;;              (tabs (mapcar #'(lambda (y)
;;                                (amapcar (cdr (finde :title (cadr (finde :entries x))))
;;                                         (cdr (find :TABS y :test 'equal :key 'car))))
;;                            windows)))
;;         ;; (find (nth 9 *current-windows*))
;;         (amapcar (member (nth 10 *current-windows*) x :test 'equal) tabs))))
;;         ;; (print (cddddr (car windows)))))
;;         (print tabs)))
;; (print (amapcar (cdr (finde :title (cadr (finde :entries x)))) (car tabs)))))

