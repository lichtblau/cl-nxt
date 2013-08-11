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

(defmethod write-to-nxt ((nxt bluetooth-nxt) data-vector)
  "Writes the raw data vector prefixed by its length.
   This is to wrap the command in the bluetooth 'frame'
   and send it over the wire, eh, air."
  (wait-for-bluetooth nxt :write)
  (let ((stream (connection nxt)))
    (write-to-nxt-us (length data-vector) stream)
    (write-sequence data-vector stream)
    (force-output stream))
  (note-data-written nxt))

(defmethod read-from-nxt ((nxt bluetooth-nxt))
  "Read a reply from the nxt.  It returns the content of the bluetooth
   or USB 'frame', i.e. without length bytes."
  (wait-for-bluetooth nxt :read)
  (let* ((stream (connection nxt))
	 (length (read-little-endian-short stream))
	 (result (make-array length :element-type '(unsigned-byte 8))))
    (read-sequence result stream)
    (note-data-read nxt)
    result))

(defun wait-for-bluetooth (nxt delay-type)
  (let ((timeout (compute-bluetooth-timeout nxt delay-type)))
    (when (plusp timeout)
      (sleep timeout))))

;;; I'm not entirely certain what the spec for the timeout issues is.
;;; But if I don't have delays in the code at all, the brick regularly fails
;;; to respond.  And other NXT libraries use a strategy like the following,
;;; so it ought to work for us too.

(defparameter *bluetooth-delay-between-writes* 0.005
  "Required delay in seconds between successive writes to the brick.")

(defparameter *bluetooth-delay-between-directions* 0.025
  "Required delay in seconds when switching between reading from
   and writing to the brick in either direction.")

(defun %compute-bluetooth-timeout (timestamp delay)
  (let* ((now (get-internal-real-time))
	 (elapsed (/ (- now timestamp) internal-time-units-per-second)))
    (- delay elapsed)))

(defgeneric compute-bluetooth-timeout (nxt delay-type)
  (:method (nxt (delay-type (eql :read)))
    (max 0
	 (%compute-bluetooth-timeout (write-timestamp nxt)
				     *bluetooth-delay-between-directions*)))
  (:method (nxt (delay-type (eql :write)))
    (max 0
	 (%compute-bluetooth-timeout (write-timestamp nxt)
				     *bluetooth-delay-between-writes*)
	 (%compute-bluetooth-timeout (read-timestamp nxt)
				     *bluetooth-delay-between-directions*))))

(defun note-data-read (nxt)
  (setf (read-timestamp nxt) (get-internal-real-time)))

(defun note-data-written (nxt)
  (setf (write-timestamp nxt) (get-internal-real-time)))
