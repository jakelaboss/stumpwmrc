(in-package :stumpwm)

(defun manual-network-test (SSID passphrase)
    (format nil "wpa_supplicant -B -i wlp3s0 -c <(wpa_passphrase \"~a\" \"~a\")" SSID passphrase))


(print (manual-network-test "Zoomd63e" "3e6eb12a63"))

(setf work-network (manual-network-test "RecursiveIdiots-5" "let'sgoexploring"))

(setf phone-network (manual-network-test "Vagabond" "let'sgoexploring"))

(print work-network)

(inferior-shell:run current-network)
wpa_supplicant -B -i wlp3s0 -c <(wpa_passphrase "RecursiveIdiots-5" "let'sgoexploring")

;; process to create a ssh link between local 4006 port and port on remote 4005 port
;; ssh -L4006:127.0.0.1:4005 192.168.0.100

;; create ssh link between x11vnc on remote and on localhost:0
;; ssh -t -L 5900:localhost:5900 192.168.0.100 'sudo x11vnc -display :0 -auth /home/arch/.Xauthority'

;; to connect to localhost:0
;; vncviewer localhost:0

