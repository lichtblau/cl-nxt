(in-package :nxt)

;;;; NXTSetOutputState's tachoLimit argument is useless due to inaccuracy,
;;;; and the latency of the protocol too high in general to do any better.
;;;; The solution is to run a program on the brick which can control motors
;;;; directly instead.
;;;;
;;;; "MotorControl" is such a program, available under GPL from
;;;;   http://www.mindstorms.rwth-aachen.de/trac/wiki/MotorControl
;;;; and compiled using NXC into an .rxe file for the device.
;;;;
;;;; We assume that the .rxe has already been uploaded to the device
;;;; manually (using NeXTool), and proceed to start the program
;;;; automatically once we need motors.

;;;; We provide functions MOTOR-CONTROL (and STOP-MOTOR) that replace
;;;; SET-OUTPUT-STATE, as well as QUERY-MOTOR which wraps GET-OUTPUT-STATE
;;;; in a fashion that is compatible with motor control.
;;;;
;;;; Users can use either the precise motor control or the old direct
;;;; functions, but shouldn't blindly mix new and old functions.
;;;; (This restriction applies to each motor separately.)

;;;; *motor-control-filename* gives the on-brick filename of the program.


(defvar *motor-control-filename* "MotorControl22.rxe")

;;;; Starting and stopping stuff (not specific to MotorControl)

(defun running-a-program-p (&optional (nxt *nxt*))
  (ecase (nxt::nxt-get-current-program-name :nxt nxt :return-style :values)
    (:success t)
    (:no-active-program nil)))

(defvar *start/stop-program-timeout* 3)

(defun stop-current-program (&key (nxt *nxt*) (waitp t))
  (let ((timeout
	 (+ (get-internal-real-time)
	    (* *start/stop-program-timeout* internal-time-units-per-second))))
    (when (running-a-program-p nxt)
      (nxt::nxt-stop-program :nxt nxt)
      (when waitp
	(loop while (running-a-program-p nxt) do
	     (when (> (get-internal-real-time) timeout)
	       (error
		"failed to stop current program in a timely fashion")))))))

(defun start-program (program-name &key (nxt *nxt*) (stop-and-wait-p t))
  (cond
    (stop-and-wait-p
     (stop-current-program :nxt nxt :waitp t))
    ((running-a-program-p nxt)
     (error "cannot start a program while another is already running")))
  (let ((timeout
	 (+ (get-internal-real-time)
	    (* *start/stop-program-timeout* internal-time-units-per-second))))
    (loop
       (nxt::nxt-start-program :nxt nxt
			       :check-status :success
			       :file-name program-name)
       ;; Okay, so why are we looping here?
       ;;   a. I don't know, really.
       ;;   b. But if we call NXTStartProgram only once, it's not sufficient
       ;;      and we land in a dialog instead of the actual program.
       ;;   c. Calling NXTStartProgram exactly twice in a row works for me,
       ;;      with the loop only to wait until it's there, but other
       ;;      people have the NXTStartProgram itself in the loop, so maybe
       ;;      they know what they're doing?  And it doesn't seem to hurt.
       (sleep 0.05)
       (when (running-a-program-p nxt)
	 (return))
       (when (> (get-internal-real-time) timeout)
	 (error
	  "failed to start current program in a timely fashion")))
    (sleep 0.03)))

(defun restart-motor-control (&key (nxt *nxt*))
  (start-program *motor-control-filename* :nxt nxt))

;;;; Talking to MotorControl

(defun check-motor-control-ports (a b)
  (check-type a (or null (integer 0 2)))
  (check-type b (or null (integer 0 2)))
  (unless (or a b)
    (error "no motor specified"))
  (when (eql a b)
    (error "same motor specified twice")))

(defun encode-motor-control-ports (a b)
  (check-motor-control-ports a b)
  (if (and a b)
      (+ 2 a b)
      (or a b)))

(defconstant +nxc-delay+ 0.015)

(defun note-motor-commanded (motor)
  (setf (command-timestamp motor) (get-internal-real-time)))

(defun compute-motor-timeout (motors)
  (/ (max 0 (- (+ (reduce #'max motors :key #'command-timestamp)
		  (* +nxc-delay+ internal-time-units-per-second))
	       (get-internal-real-time)))
     internal-time-units-per-second))

;;;; Note: The MOTOR-CONTROL function does not check whether the motor is
;;;; ready.  We expect the caller to have done (the equivalent of) a call
;;;; to SLEEP-UNTIL-MOTOR-READY. Failure to do so will not be detected on
;;;; the Lisp side (MotorControl beeps when it ignores a message it isn't
;;;; ready for).

(defun motor-control (first-motor second-motor ;or nil
		      &key (nxt *nxt*)
		           (power 0)
		           (tacho-limit nil)   ;degrees
		           speed-regulation-p
		           (action-at-tacho-limit :brake)
		           smooth-start-p)
  (check-type tacho-limit (or null (integer 1 999999)))
  (check-type action-at-tacho-limit (member :coast :brake :hold-brake))
  (assert (not (and smooth-start-p (eq action-at-tacho-limit :coast))))
  (assert (not (and speed-regulation-p (and first-motor second-motor))))
  (let* ((ports (encode-motor-control-ports first-motor second-motor))
	 (power (if (minusp power) (- 100 power) power))
	 (tacho-limit (or tacho-limit
			  (progn
			    #+nil (assert (eq action-at-tacho-limit :coast))
			    0)))
	 (motor-state-vector (motor-states nxt))
	 (motor-states (mapcar (lambda (idx)
				 (elt motor-state-vector idx))
			       (remove nil (list first-motor second-motor))))
	 (timeout (compute-motor-timeout motor-states))
	 (message
	  (multiple-value-bind (command-char mode)
	      (if (eq action-at-tacho-limit :coast)
		  (values #\4 (if speed-regulation-p 1 0))
		  (let ((hold-brake-p (eq action-at-tacho-limit :hold-brake)))
		    (values #\1 (+ (if hold-brake-p       1 0)
				   (if speed-regulation-p 2 0)
				   (if smooth-start-p     4 0)))))
	    (format nil "~C~D~3,'0D~6,'0D~D"
		    command-char ports power tacho-limit mode))))
    (when (plusp timeout)
      (sleep timeout))
    (send-string-message 1 message :nxt nxt)
    (mapc #'note-motor-commanded motor-states)))

(defun send-string-message
    (inbox message &key (nxt *nxt*) (check-status :success))
  (nxt-message-write :nxt nxt
		     :inbox inbox
		     :message-data (babel:string-to-octets
				    (concatenate 'string
						 message
						 (string (code-char 0)))
				    :encoding :iso-8859-1)
		     :check-status check-status))

(defmacro with-motor-delays ((nxt port) &body body)
  `(call-with-motor-delays (lambda () ,@body) ,nxt ,port))

(defun call-with-motor-delays (fun nxt port)
  (let* ((motor (elt (motor-states nxt) port))
	 (timeout (compute-motor-timeout (list motor))))
    (when (plusp timeout)
      (sleep timeout))
    (prog1
	(funcall fun)
      (note-motor-commanded motor))))

;;;; They following functions are wrappers around direct NXT commands.
;;;; They differ from the original versions in that they interact
;;;; correctly with the MOTOR-CONTROL control function, in that they observe
;;;; the necessary timeouts.

(defun ensure-message-read (nxt remote-inbox local-inbox remove)
  (let ((timeout (+ (get-internal-real-time)
		    (* 3 internal-time-units-per-second))))
    (loop
       (multiple-value-bind (status port length data)
	   (nxt::nxt-message-read :nxt nxt
				  :return-style :values
				  :remote-inbox remote-inbox
				  :local-inbox local-inbox
				  :remove (if remove 1 0))
	 (declare (ignore length))
	 (ecase status
	   ((:success :specified-mailbox-queue-is-empty)
	    (when (plusp (length data))
	      (return (values data port))))
	   (:no-active-program
	    (error "MotorControl not running"))))
       ;; This delay may look like it's the same as in MOTOR-READY-P, but
       ;; actually we only want to avoid busy polling here.  Docs state that
       ;; we don't have to do this for Bluetooth, but I suppose that's only
       ;; because Bluetooth would impose an (even longer) timeout anyway.
       ;; Since any shorter sleep we do here will be taken into account
       ;; by Bluetooth, I don't see the need for an IF.
       (sleep 1/100)
       (when (> (get-internal-real-time) timeout)
	 (error "no response from motor control")))))

(defun stop-motor (port &key (nxt *nxt*))
  (check-type port (integer 0 2))
  (with-motor-delays (nxt port)
    (nxt::nxt-set-output-state :nxt nxt
			       :output-port port
			       :power-set-point 0
			       :mode 1
			       :regulation-mode 0
			       :turn-ratio 0
			       :run-state 0
			       :tacho-limit 0)))

(defun motor-ready-p (port &key (nxt *nxt*))
  (check-type port (integer 0 2))
  (with-motor-delays (nxt port)
    (send-string-message 1 (format nil "3~D" port) :nxt nxt)
    (sleep 1/100)
    (let ((data (ensure-message-read nxt 0 0 t)))
      (cond
	((eql (elt data 0) (digit-char port))
	 (eql (elt data 1) #\1))
	(t
	 (warn "unexpected MotorControl response for port ~D (expected ~D); ignoring"
	       (elt data 0) port)
	 nil)))))

(defun sleep-until-motor-ready (port &key (nxt *nxt*))
  (loop until (motor-ready-p port :nxt nxt)))

(defun query-motor
    (port &rest keys &key (nxt *nxt*) result-style assert-status)
  (declare (ignore result-style assert-status))
  (check-type port (integer 0 2))
  (with-motor-delays (nxt port)
    (apply #'nxt::nxt-get-output-state :output-port port keys)))
