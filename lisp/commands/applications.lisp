(in-package :stumpwm)

(defcommand pulse-sms () ()
  "Start Pulse SMS"
  (run-commands "exec sh -c \"pulse-sms --disable-gpu --no-sandbox\" "))
