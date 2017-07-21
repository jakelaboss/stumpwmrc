(in-package :stumpwm)

(defun manual-network-test (SSID passphrase)
    (format nil "wpa_supplicant -B -i wlp3s0 -c <(wpa_passphrase \"~a\" \"~a\")" SSID passphrase))


(print (manual-network-test "Zoomd63e" "3e6eb12a63"))

(setf work-network (manual-network-test "RecursiveIdiots-5" "let'sgoexploring"))

(setf work-network (manual-network-test "RecursiveIdiotHome" "let'sgoexploring"))

(setf phone-network (manual-network-test "Vagabond" "let'sgoexploring"))


(setf Sid-network (manual-network-test "intranet"))

("sudo mount -t cifs //192.168.0.17/Users/jakelaboss/Videos /mnt/windows/ -o user=jakelaboss")

;; (print work-network)

;; (inferior-shell:run current-network)
;; wpa_supplicant -B -i wlp3s0 -c <(wpa_passphrase "RecursiveIdiots-5" "let'sgoexploring")

;; process to create a ssh link between local 4006 port and port on remote 4005 port
("ssh -L4006:127.0.0.1:4005 192.168.0.100")

;; connect to aws instance
("ssh -L4006:127.0.0.1:4005 -i "/home/vagabond/library/cloud/aws/keypairs/ARCH-PAIR.pem" root@ec2-54-173-185-85.compute-1.amazonaws.com")

ssh client@ec2-54-205-121-221.compute-1.amazonaws.com

;; create ssh link between x11vnc on remote and on localhost:0
("ssh -t -L 5900:localhost:5900 192.168.0.103 'sudo x11vnc -display :0 -auth /home/arch/.Xauthority'")

;; to connect to localhost:0
"vncviewer localhost:0"

;; to connect to desktop
"sshfs arch@192.168.0.103:/ /home/vagabond/mnt/linux/ -p 22"
"sshfs -p 22 arch@192.168.0.103:/ /home/vagabond/mnt/"
sshfs -p 22 root@ec2-54-205-121-221.compute-1.amazonaws.com:/ /home/vagabond/common-lisp/libraries/linux/IRC/mnt/ -o IdentityFile="/home/vagabond/library/cloud/aws/keypairs/ARCH-PAIR.pem" 


(defun turn-off-ethernet (list)
  (setf (infer)))
