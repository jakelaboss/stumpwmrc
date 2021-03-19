;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Screen
;;------------------------------------------------------------------------------------------------------------------------ ;;

(in-package :stumpwm)


;; Reinitalize how screens work because of the addition of workspaces

(defun screen-set-focus (screen window)
  (when (eq (window-group window)
            (screen-current-group screen))
    (xlib:set-input-focus *display* (window-xwin window) :POINTER-ROOT)
    (xlib:change-property (screen-root screen) :_NET_ACTIVE_WINDOW
                          (list (window-xwin window))
                          :window 32
                          :transform #'xlib:drawable-id
                          :mode :replace)
    (setf (screen-focus screen) window)
    (move-screen-to-head screen)))


;; The first thing we change is focus-window:
;; We now change the workspace instead of the screen
(defmethod focus-window (window &optional (raise t))
  "Make the window visible and give it keyboard focus. If raise is t, raise the window."
  (dformat 3 "focus-window: ~s~%" window)
  (let* ((group (window-group window))
         (screen (group-screen group))
         (cw (screen-focus screen))
         (xwin (window-xwin window)))
    (when raise
      (raise-window window))
    (cond
      ((eq window cw)
       ;; If window to focus is already focused then our work is done.
       )
      ;; If a WM_TAKE_FOCUS client message is not sent to the window,
      ;; widgets in Java applications tend to lose focus when the
      ;; window gets focused. This is hopefully the right way to
      ;; handle this.
      ((member :WM_TAKE_FOCUS (xlib:wm-protocols xwin) :test #'eq)
       (let ((hints (xlib:wm-hints xwin)))
         (when (or (null hints) (eq (xlib:wm-hints-input hints) :on))
           ;; (screen-set-focus screen window)
           (switch-to-workspace (screen-workspace screen))
           (screen-set-focus (current-screen) window)))
       (setf (group-current-window group) window)
       (update-decoration window)
       (when cw
         (update-decoration cw))
       (move-window-to-head group window)
       (send-client-message window :WM_PROTOCOLS
                            (xlib:intern-atom *display* :WM_TAKE_FOCUS)
                            ;; From reading the ICCCM spec, it's not
                            ;; entirely clear that this is the correct
                            ;; value for time that we send here.
                            (or *current-event-time* 0))
       (update-mode-lines (window-screen window))
       (run-hook-with-args *focus-window-hook* window cw))
      (t
       ;; (screen-set-focus screen window)
       (switch-to-workspace (screen-workspace screen))
       (screen-set-focus (current-screen) window)
       (setf (group-current-window group) window)
       (update-decoration window)
       (when cw
         (update-decoration cw))
       ;; Move the window to the head of the mapped-windows list
       (move-window-to-head group window)
       (update-mode-lines (window-screen window))
       (run-hook-with-args *focus-window-hook* window cw)))))


(defun move-focus-and-or-window (dir &optional win-p)
  (declare (type (member :up :down :left :right) dir))
  (let* ((group (current-group))
         (new-frame (neighbour dir (tile-group-current-frame group) (group-frames group)))
         (window (current-window)))
    (when new-frame
      (if (and win-p window)
          (pull-window window new-frame)
          (focus-frame group new-frame)))))

(defun focus-frame (group f)
  (let ((w (frame-window f))
        (last (tile-group-current-frame group))
        (show-indicator nil))
    (setf (tile-group-current-frame group) f)
    ;; record the last frame to be used in the fother command.
    (unless (eq f last)
      (setf (tile-group-last-frame group) last)
      (run-hook-with-args *focus-frame-hook* f last)
      (setf show-indicator t))
    (if w
        (focus-window w)
        (no-focus group (frame-window last)))
    (if show-indicator
        (show-frame-indicator group)
        (show-frame-outline group))))
