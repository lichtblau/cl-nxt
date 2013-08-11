;;; Contains the bluetooth infrastructure code, communication with the nxt

(in-package :nxt)

(defun mux (tcp nxt)
  (loop
   (when (let* ((length (read-little-endian-short tcp))
		(data (make-array length :element-type '(unsigned-byte 8))))
	   (unless (plusp data)
	     (return))
	   (read-sequence data tcp)
	   (write-to-nxt nxt data)
	   (logtest #x80 (elt data 0)))
     ;; reply expected
     (let ((data (read-from-nxt nxt)))
       (write-to-nxt-us (length data) tcp)
       (write-sequence data tcp)
       (force-output tcp)))))

(defun handle-connection (connection)
  (handler-case
      (let ((stream (usocket:socket-stream connection))
	    (nxt (nxt:open-connection '(:usb))))
	(unwind-protect
	     (mux stream nxt)
	  (nxt:close-connection)))
    (error (c)
      (format t "ignoring error: ~A~%" c))))

(defun run-proxy (&optional (port 30000) (host usocket:*wildcard-host*))
  (usocket:with-server-socket
      (listener (usocket:socket-listen
		 host port
		 :reuse-address t
		 :element-type '(unsigned-byte 8)))
    (loop
       (usocket:with-connected-socket (c (usocket:socket-accept listener))
	 (handle-connection c)))))
