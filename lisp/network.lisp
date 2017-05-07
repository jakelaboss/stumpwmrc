(in-package :stumpwm)

(defun manual-network-test (SSID passphrase)
    (format nil "wpa_supplicant -B -i wlp3s0 -c <(wpa_passphrase \"~a\" \"~a\")" SSID passphrase))



;; process to create a ssh link between local 4006 port and port on remote 4005 port
("ssh -L4006:127.0.0.1:4005 192.168.0.100")

;; create ssh link between x11vnc on remote and on localhost:0
("ssh -t -L 5900:localhost:5900 192.168.0.103 'sudo x11vnc -display :0 -auth /home/arch/.Xauthority'")

;; to connect to localhost:0
"vncviewer localhost:0"

;; to connect to desktop
"sshfs arch@192.168.0.103:/ /home/vagabond/mnt/linux/ -p 22"
