(defparameter *my-tid* ())

(defun enroll ()
  (setq *my-tid* (lpvm-my-tid)))

(defun leave ()
  (lpvm-exit)
  (quit))

(defun send-rec (msg msgtype)
  (format t "about to send~%")
  (lpvm-send-message msg *rdr*  msgtype *my-tid*)
  (format t "about to receive~%")
  (lpvm-nonblocking-recv *rdr* *my-tid* msgtype))
