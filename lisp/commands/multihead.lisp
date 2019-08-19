(in-package :stumpwm)

(defun defconnection (swank-client:slime-connect 'dev-server 4006))
(defvar dev-server (swank-client:slime-connect "192.168.0.161" 4006))

(swank-client:with-slime-connection (con "192.168.0.161" 4006)
  (progn
    (stumpwm:move-focus :left)))

(defvar *emacs-port* 4006)
(defvar *swank-client-port* 10000)

(defun start-swank-server-for-emacs (port)
  "Starts a Swank server thread, listening on PORT of the host's loopback
interface, to handle Emacs/Slime connection requests."
  (swank:create-server :port port :dont-close t))

(defun start-swank-server-for-swank-client (port)
  "Starts a Swank server thread, listening on PORT of the host's network
interface, to handle Swank Client connection requests."
  (let ((swank::*loopback-interface* (sb-unix:unix-gethostname)))
    (swank:create-server :port port :dont-close t)))

(defun swank-thread ()
  "Returns a thread that's acting as a Swank server."
  (dolist (thread (sb-thread:list-all-threads))
    (when (com.google.base:prefixp "Swank" (sb-thread:thread-name thread))
      (return thread))))

(defun wait-for-swank-thread ()
  "Wait for the Swank server thread to exit."
  (let ((swank-thread (swank-thread)))
    (when swank-thread
      (sb-thread:join-thread swank-thread))))

(defun main ()
  (setf swank:*configure-emacs-indentation* nil
        swank::*enable-event-history* nil
        swank:*log-events* t)
  (start-swank-server-for-emacs *emacs-port*)
  (start-swank-server-for-swank-client *swank-client-port*)
  (wait-for-swank-thread))

(main)

The code above starts two Swank servers.  You can connect to the server on port
4005 from Emacs using the command M-x slime-connect.  You can programmatically
connect to the second Swank server by loading the Swank Client code into your
Lisp and evaluating:

(swank-client:slime-connect "arch-dev1" 10000)

(swank-client:slime-connect "192.168.0.161" 10000)
