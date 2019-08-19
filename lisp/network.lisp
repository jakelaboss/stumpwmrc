(in-package :stumpwm)

; --- networks ----------------------------------------

(defvar *iw-scan*)

(defparameter *network-hash-table* (make-hash-table :test #'equal))

(defvar *stumpwm-netctl* "/home/vagabond/common-lisp/libraries/linux/stumpwm/storage/networks-files/")

;; Create password data type
(define-stumpwm-type :password (input prompt)
  (let ((history *input-history*)
      (arg (argument-pop input))
      (fn (symbol-function 'draw-input-bucket)))
    (unless arg
      (unwind-protect
           (setf (symbol-function 'draw-input-bucket)
                 (lambda (screen prompt input &optional errorp)
                   (let ((i (copy-structure input)))
                     (setf (input-line-string i)
                           (make-string (length (input-line-string i))
                                        :initial-element #\*))
                     (funcall fn screen prompt i)))
                 arg (read-one-line (current-screen) prompt))
        (setf (symbol-function 'draw-input-bucket) fn
              *input-history* history))
      arg)))

;; (export :password)

;; Get Network State, returns t if network is up, nil if not
(defun net-status (interface)
  (if (stringp interface)
      (if (equal "up" (with-open-file (s (format nil  "/sys/class/net/~a/operstate" interface))
                        (read-line s))) "Up" "Down")
      (error "Interface must be a string")))

;; Prompt for network
(defun select-network-from-menu (screen)
  (labels ((ssid (x)
             (cl-ppcre:regex-replace-all "\\tSSID: "
                                         (cl-ppcre:scan-to-strings "\\tSSID: \\S+.*" x) ""))
           (profile (x)
             (cl-ppcre:regex-replace-all " " (ssid x)
                                         "_")))
    (let* ((hs (make-hash-table :test #'equal))
           (scan (sudo-run "iw dev wlp3s0 scan"))
           (networks (remove nil (cl-ppcre:split "BSS" scan)))
           (net-string (remove nil (mapcar #'profile networks))))
      (setf *iw-scan* scan)
      (mapcar (lambda (x)
                (setf (gethash (profile x) ;; (cl-ppcre:regex-replace-all "\\tSSID: "
                               ;;                                     (cl-ppcre:scan-to-strings "\\tSSID: \\S+.*" x) "")
                               hs)
                      (cons (ssid x)
                            (if (cl-ppcre:scan "RSN" x) t nil))))
              networks)
      (setf *network-hash-table* hs)
      (select-from-menu screen
                        (mapcar (lambda (g) (list g))
                                net-string)
                        "Networks:"))))


;; Creates two commands, *wireless-wpa* and *wireless-open*, which prompts for a password
(defun add-to-known-networks (network)
  (macrolet ((cmd (name args interactive-args &optional body)
               `(defcommand ,name (,@args) (,@interactive-args) ,body))
             (run (name)
               `(stumpwm:run-commands (format nil "~a" ',name))))
    (progn
      ;; if the network requires a password
      (cmd *wireless-wpa* (password) ((:password "Input Network Password: ")) ;; X is not
           (progn
             (with-output-to-file (stream (concat *stumpwm-netctl*
                                                  network)
                                          :if-does-not-exist :create
                                          :if-exists :overwrite)
               (format stream "~a" (cl-ppcre:regex-replace "MyNetwork"
                                                           (cl-ppcre:regex-replace "WirelessKey"
                                                                                   (sudo-run "cat /etc/netctl/lisp/wireless-wpa") password)
                                                           (car (gethash network *network-hash-table*)))))
             (sudo-run (concat "cp " *stumpwm-netctl*
                               network " /etc/netctl/" network))))
      (cmd *wireless-open* () ()
           (progn
             (with-output-to-file (stream (concat *stumpwm-netctl* network)
                                          :if-does-not-exist :create
                                          :if-exists :overwrite)
               (format stream "~a" (cl-ppcre:regex-replace "MyNetwork"
                                                           (sudo-run "cat /etc/netctl/lisp/wireless-open")
                                                           (car (gethash network *network-hash-table*)))))
               (sudo-run (concat "cp " *stumpwm-netctl* network " /etc/netctl/" network))))
           (cmd *network-entry-p* (p) ((:y-or-n "Unkown Network: Would you like to create an entry?"))
                (if p
                    (if (cdr (gethash network *network-hash-table*))
                        ;; (if (gethash network *network-hash-table*)
                        (progn (run-commands "*wireless-wpa*")
                               (sleep 3)
                               (if (sudo-run (concat "netctl start " network))
                                   (sudo-run (concat "netctl switch-to " network))))
                        (progn (run-commands "*wireless-open*")
                               (sleep 3)
                               (if (sudo-run (concat "netctl start " network))
                                   (sudo-run (concat "netctl switch-to " network)))))
                    ;; (progn (run-commands "*wireless-open*")
                    ;; (sleep 3) (sudo-run (concat "netctl start " network))
                    ;; (sleep 3) (sudo-run (concat "netctl switch-to " network))))
                    nil))
           ;; Main event
           (if (run *network-entry-p*)
               "Connected"
               "No entry will be added"))))

(export '(*network-entry-p* *wireless-wpa* *wireless-open*))

;; (progn (run-commands "*wireless-open*")
;; (sudo-run (concat "netctl switch-to " network))))))))

(defcommand netctl () (:rest)
  (when-let ((network (car (select-from-menu (current-screen)
                                        (mapcar (lambda (g) (list g))
                                                (cl-ppcre:split "\\n"
                                                                (sudo-run "netctl list")))
                                        "Networks:"))))
    (sudo-run (format nil "netctl switch-to ~a" network))))


(defcommand net-scan () (:rest)
  (labels ((net (network known-list)
             (if (null (cl-ppcre:scan-to-strings network known-list))
                 (add-to-known-networks network)
                 (sudo-run (format nil "netctl switch-to ~a" network)))))
  (if (net-status "wlp3s0")
      (let* ((network (car (select-network-from-menu (current-screen))))
             (known-networks (sudo-run "netctl list")))
        (net network known-networks))
      (progn (sudo-run "ip link set wlp3s0 up")
             (let* ((network (car (select-network-from-menu (current-screen))))
                    (known-networks (sudo-run "netctl list")))
               (sudo-run "ip link set wlp3s0 down")
               (net network known-networks))))))

      ;;   (if (null (cl-ppcre:scan-to-strings network known-networks))
      ;;       (add-to-known-networks network)
      ;;       (sudo-run (format nil "netctl switch-to ~a" network))))

      ;; (progn (sudo-run "ip link set wlp3s0 up")
      ;;        (let* ((network (car (select-network-from-menu (current-screen))))
      ;;               (known-networks (sudo-run "netctl list")))

(defcommand remove-network () (:rest)
  (when-let* ((known-networks (cl-ppcre:split "\\n"
                                             (sudo-run "netctl list")))
             (network (car (select-from-menu (current-screen)
                                             (mapcar (lambda (g) (list g))
                                                     known-networks)
                                             "Remove Network"))))
    (stumpwm:run-commands (format nil "remove-network-p ~a" network))))

(defcommand remove-network-p (network p) ((:string) (:y-or-n "Remove This Network? "))
  (if p
      (sudo-run (format nil "rm /etc/netctl/~a" network))
      nil))

; --- VPN ----------------------------------------
;; TODO create a VPN interface

(defun vpn (conf)
  (format nil (concatenate 'string
                         "openvpn /etc/openvpn/client"
                         (concatenate 'string conf ".conf"))))

(defcommand list-vpns () (:rest)
  (when-let ((network (car (select-from-menu (current-screen)
                                             (mapcar (lambda (g) (list g))
                                                     (cl-ppcre:split "\\n"
                                                                     (sudo-run "find /etc/openvpn/ \\( -name \"*.conf\" -o -name \"*.ovpn\" \\) ")))
                                             "Networks:"))))
    (sudo-run (format nil "nohup openvpn /etc/openvpn/client/~a &" network))))

(defcommand kill-vpn (p) (:y-or-n "Kill VPN?")
  (if p
      (if-let ((vpn (cl-ppcre:scan-to-strings "openvpn" (sudo-run "ps"))))
        (sudo-run (format nil "pkill ~a" vpn))
        (error "openvpn is not running"))
      "Ok"))



      ;; (defun manual-network-test (SSID passphrase)
      ;;   (format nil "wpa_supplicant -B -i wlp3s0 -c <(wpa_passphrase \"~a\" \"~a\")" SSID passphrase))

;; (defun manual-network-runner (SSID passphrase)
;;   (if (net-status "wlp3s0")
;;       (progn
;;         (sudo-run "ip link set wlp3s0 down")
;;         (sudo-run (format nil "wpa_supplicant -B -i wlp3s0 -c <(wpa_passphrase \"~a\" \"~a\")" SSID passphrase)))))



;; ;; (inferior-shell:run current-network)

;; ;; process to create a ssh link between local 4006 port and port on remote 4005 port

;; ;; connect to aws instance
;; ("ssh -L4006:127.0.0.1:4005 -i "/home/vagabond/library/cloud/aws/keypairs/ARCH-PAIR.pem" root@ec2-54-173-185-85.compute-1.amazonaws.com")

;; ssh client@ec2-54-205-121-221.compute-1.amazonaws.com

;; ;; create ssh link between x11vnc on remote and on localhost:0
;; ("ssh -t -L 5900:localhost:5900 192.168.0.103 'sudo x11vnc -display :0 -auth /home/arch/.Xauthority'")

;; ;; to connect to localhost:0
;; "vncviewer localhost:0"

;; ;; to connect to desktop
;; "sshfs arch@192.168.0.103:/ /home/vagabond/mnt/linux/ -p 22"
;; "sshfs -p 22 arch@192.168.0.103:/ /home/vagabond/mnt/"
;; sshfs -p 22 root@ec2-54-205-121-221.compute-1.amazonaws.com:/ /home/vagabond/common-lisp/libraries/linux/IRC/mnt/ -o IdentityFile="/home/vagabond/library/cloud/aws/keypairs/ARCH-PAIR.pem" 


;; (defun turn-off-ethernet (list)
;;   (setf (infer)))
