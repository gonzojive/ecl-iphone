From daemon Fri Jul  8 22:43:26 1994
>From clisp-list@ma2s2.mathematik.uni-karlsruhe.de Fri Jul  8 22:43:16 1994
Return-Path: <clisp-list@ma2s2.mathematik.uni-karlsruhe.de>
Date: Fri, 8 Jul 94 22:45:40 +0200
Errors-To: haible@ma2s2.mathematik.uni-karlsruhe.de
Originator: clisp-list@ma2s2.mathematik.uni-karlsruhe.de
Errors-To: haible@ma2s2.mathematik.uni-karlsruhe.de
Reply-To: clisp-list <clisp-list@ma2s2.mathematik.uni-karlsruhe.de>
Sender: clisp-list@ma2s2.mathematik.uni-karlsruhe.de
Version: 5.5 -- Copyright (c) 1991/92, Anastasios Kotsikonas
From: donc@ISI.EDU (Don Cohen)
To: Multiple recipients of list <clisp-list@ma2s2.mathematik.uni-karlsruhe.de>
Subject: recording function calls

  From: "Edward G. Kovach" <kovach@franus.edu>
  Is there a way to ... get a listing of..
             A. How many times a particular function is called?
             B. How much time it takes to run each function?

I've seen several such facilities.  The one I like, though, is
my own, included below.  At the cost of some extra space, it
records not only the number of calls and total time, but each
individual call, its inputs and outputs, its start/finish time.
This is much more useful for debugging and tuning, since you get
to see WHICH calls took a lot of time, which ones got the wrong
inputs or computed the wrong results, etc.  

;;; -*- Mode: LISP; Package: USER; Syntax: Common-lisp -*-
(lisp::in-package "USER")
; ----   Record the calls to given functions   ----
#| 2/17/89 - try to avoid advice, not so much because it's not commonlisp
  as because it's not compiled!  In fact, I want to be able to turn on and
  off recording at high frequency and encapsulations seem to get in the way
  of this.  For now I'll assume that one does not encapsulate and record the
  same functions.

  In order to monitor a function one first prepares it for monitoring, then
  one can turn monitoring on and off at high frequency.  One can also reset
  or read the monitoring data for a function.  Finally one can forget about
  monitoring a function.

  *monitored-fns* is a list of functions currently prepared for monitoring.
  (prepare-record-calls '(f1 f2 f3)) prepares the functions named.
    additional keyword arguments: entryforms, exitforms, test
    The entryforms are evaluated at function entry, the exitforms at function
    exit.  The results are recorded along with inputs, outputs, entry time
    and exit time.  Test is a form (default is T) that determines whether
    this particular call will be recorded.  It runs in an environment where
    ARGS is bound to the argument list of the function.
  (record-on '(f1 f2 f3)) turns on recording for these functions.
  (record-off '(f1 f2 f3)) turns it off.
  (initialize-records '(f1 f2 f3)) discards all monitoring data for the
    functions (but does not turn recording off or on and does not forget
    preparation).
  (recorded-calls 'f1) returns a list of the call records for f1.
    This is a list of records of the form
    (inputs outputs start-time1 start-time2 end-time1 end-time2
	<values of entry forms> <values of exit forms>)
    Times are represented as 2 numbers since some clocks wrap around.
    The second is a wrap around count that is incremented whenever the
    finish time comes out lower than the start time.
  (summarize-calls '(f1 f2 f3)) prints a summary of the calls.
    The argument defaults to *monitored-fns*.
    Additional optional argument: name-alist
    Name-alist is something like ((f1 . "updating database") (f2 . "waiting"))
    and is used to translate function names into something more meaningful.
  (forget-record-calls '(f1 f2 f3)) discards all monitoring data and preparation

  (longest-n-calls 'f2 3) lists the 3 longest recorded calls of f2
   additional keyword arguments: start end filterfn
   filterfn - a function of 1 arg (inputs outputs start finish)
    should return T if the call is "interesting"
   start/end are special cases - filter out anything that starts before start
    or ends after end

  (time-line '(f1 f2 f3) produces a time line of activity
   additional keyword arguments: (width 80) filterfn start end name-alist

  Both symbolics and TI have a fast short clock and a slow long one.
  We use the fast one on symbolics, slow one on TI.
                    time before wrap around / #usec to read clock
                    --------------------------------------------
                    symbolics 3600           TI explorer II
             fast   >.5 hour / 67   *        16 sec. / 260
             slow   >100 yrs / 218           >1 hour / 260   *

  Actually we notice wrap around and record it - whenever a clock access
  returns a smaller value than the previous one we increment a counter.
  Therefore all events are ordered correctly, but if you fail to read the
  clock for an hour or so, it's as if that time never passed.  This is bad
  if you time things on such a coarse scale, but good if you time one thing
  for a minute today and something else for a minute tomorrow - the time
  line between such events never separates them by much more than an hour.
  In practice I don't think this will matter much.

  Since calls are recorded by pushing onto a list at exit, they are ordered
  by decreasing exit time.  This is handy for finding the outermost calls
  in the case where the calls all come from the same process (and must thus
  be properly nested).
  (outermost (recorded-calls 'foo))
  returns the subset of the calls to foo that are outermost.

|#

(defvar *monitored-fns* nil)
(defvar *clock-cycle* 0)
(defvar *last-time* 0)
(defun prepare-record-calls (fns &key entryforms exitforms (test t))
  (loop for fn in fns do (prepare-record-call fn entryforms exitforms test)))

; record-calls-fn prop is cons substitute and original fns
(defun prepare-record-call (fn entryforms exitforms test &aux prop)
  (cond ((not (fboundp fn)) (error "no such function as ~A" fn))
	#+zetalisp
	((and (si:function-encapsulated-p fn)
	      (warn "~A is an encapsulation") nil))
	#+ignore ; might be called with different entryforms/exitforms
	((and (setf prop (get fn 'record-calls-fn))
	      (eq (cdr prop) (symbol-function fn)))
	 #+ignore (warn "~A already recorded" fn))
	((eq (symbol-function fn) (car prop))
	 #+ignore (warn "~A already prepared" fn))
	(t ; not cached ...
	 (setf (get fn 'record-calls-fn)
	       (cons (make-record-fn fn entryforms exitforms test)
		     (symbol-function fn)))
	 (pushnew fn *monitored-fns*))))

(defun make-record-fn (fn entryforms exitforms test)
  (compile nil
      `(lambda (&rest args &aux start start1 values finish finish1 entryvals)
	 (if ,test
	     (unwind-protect
		 (progn (setq entryvals (list ,@entryforms)
			      start (microsec-time)
			      start1 *clock-cycle*
			      values (multiple-value-list
				       (apply ',(symbol-function fn) args))
			      finish (microsec-time) finish1 *clock-cycle*)
			(values-list values))
	       (record-1-call ',fn (copy-list args)
			      (if finish values :abnormal-exit)
			      start start1
			      (or finish (microsec-time))
			      (or finish1 *clock-cycle*)
			      entryvals
			      (list ,@exitforms)))
	     (apply ',(symbol-function fn) args)))))
; perhaps we should try to correct for the time spent in the new function?

(defun forget-record-calls (fns)
  (record-off fns)
  (loop for fn in fns do
    (setq *monitored-fns* (delete fn *monitored-fns*))
    (setf (get fn 'record-calls-fn) nil)
    (setf (get fn 'recorded-calls) nil)))

(defun record-on (fns)
  (loop for fn in fns do
        (let ((prop (get fn 'record-calls-fn)))
	  (cond ((not prop) (cerror "skip turning on recording"
				    "~A not prepared for recording" fn))
		((eq (cdr prop) (symbol-function fn))
		 (setf (symbol-function fn) (car prop)))
		((eq (car prop) (symbol-function fn)))
		(t (cerror "skip turning on recording"
			   "~A has changed since last prepared for recording"
			   fn))))))

(defun record-off (fns)
  (loop for fn in fns do
        (let ((prop (get fn 'record-calls-fn)))
	  (cond ((not prop)
		 (cerror "continue" "~A not prepared for recording" fn))
		((eq (car prop) (symbol-function fn))
		 (setf (symbol-function fn) (cdr prop)))
		((eq (cdr prop) (symbol-function fn)))
		(t (cerror "continue"
			   "~A has changed since recording last turned on"
			   fn))))))

(defun microsec-time (&aux time)
  (setq time
        #-(or symbolics ti) (get-internal-run-time)
	#+symbolics (time:fixnum-microsecond-time)
	#+TI (time:microsecond-time))
  (when (< time *last-time*) (incf *clock-cycle*))
  (setf *last-time* time))

(defun record-1-call (fn inputs results t1 t11 t2 t21 entryvals exitvals)
  (push (list inputs results t1 t11 t2 t21 entryvals exitvals)
	(get fn 'recorded-calls)))

(defun initialize-records (fns)
  (loop for fn in fns do (setf (get fn 'recorded-calls) nil)))

(defun recorded-calls (fn) (get fn 'recorded-calls))

(defun summarize-calls (&optional (fns *monitored-fns*) name-alist)
  (loop for fn in fns do
    (summarize-record fn (get fn 'recorded-calls) name-alist)))

(defun summarize-record (fn calls name-alist)
  (when calls (loop for x in calls sum 1 into ncalls
		    sum (elapsed (third x) (fourth x) (fifth x) (sixth x))
		    into time finally
		    (print-summarize-record fn ncalls time name-alist))))

(defun print-summarize-record (fn ncalls time name-alist)
  (multiple-value-bind (total tunits)
      (standardized-time-units time)
    (multiple-value-bind (avg aunits)
      (standardized-time-units (float (/ time ncalls)))
      (format *standard-output* "~%~A: ~A calls, ~A ~A (avg. ~A~:[ ~a~; ~])"
	  (or (cdr (assoc fn name-alist)) fn)
	  ncalls total tunits avg (eq aunits tunits) aunits))))

(defun standardized-time-units (usec)
  (cond ((> usec 999999) (values (float (/ usec 1000000)) "sec."))
	((> usec 999) (values (float (/ usec 1000))  "msec."))
	(t (values usec "usec."))))

(defun elapsed (t1 t11 t2 t21)
  (+ (- t2 t1) (* (- t21 t11) (* 1024 1024 2048 #+TI 2))))

(defun longest-n-calls (fn n &key start end filterfn
			&aux next time current
			(candidates (recorded-calls fn)) (i 0))
  ; filterfn decides whether a record is "interesting"
  ; special cases: start/end filters out anything that starts before start
  ; or ends after end
  (flet ((filter (e) (and (or (null start)
			      (plusp (elapsed start 0 (third e) (fourth e))))
			  (or (null end)
			      (plusp (elapsed (fifth e) (sixth e) end 0)))
			  (or (null filterfn) (funcall filterfn e)))))
    (loop while (and (< i n) (setq next (pop candidates)))
	  when (filter next)
	  do (incf i) (push (cons (elapsed (third next) (fourth next)
					   (fifth next) (sixth next))
				  next) current))
    (setq current (sort current #'<= :key #'car))
    (loop while (setq next (pop candidates))
	  when (filter next)
	  when (< (caar current)
		  (setq time (elapsed (third next) (fourth next)
				      (fifth next) (sixth next))))
	  do (setq current (merge 'list (cdr current)
				  (list (cons time next))
				  #'<= :key #'car)))
    (nreverse current)))

(defvar *time-line-key*
  "Start time = ~A, End time = ~A, Width = ~A, ~
  ~& each column represents ~A ~A~
  ~& Key: ( = 1 entry, ) = 1 exit, * = more than one entry/exit~
  ~&      if no entry/exit, a digit indicates number of active calls,~
  ~&         blank indicates no change, + indicates >9 ~% ")

(defun time-line (fns &key (width 80) filterfn start end len name-alist
		    &aux events)
  (flet ((filter (e) (and (or (null start)
			      (plusp (elapsed start 0 (third e) (fourth e))))
			  (or (null end)
			      (plusp (elapsed (fifth e) (sixth e) end 0)))
			  (or (null filterfn) (funcall filterfn e)))))
    (setq events (loop for f in fns collect
	 	       (cons f (loop for e in (recorded-calls f)
				     when (filter e) collect e))))
    (unless (and start end)
      (loop for e in events do
	    (loop for r in (cdr e) do
	          (when (or (null start)
			    (minusp (elapsed start 0 (third r) (fourth r))))
		    (setq start (totalt (third r) (fourth r))))
		  (when (or (null end)
			    (minusp (elapsed (fifth r) (sixth r) end 0)))
		    (setq end (totalt (fifth r) (sixth r)))))))
    (when (and start end) (setq len (- end start)))
    (unless (and len (> len 0)) (return-from time-line "empty interval"))
    (multiple-value-bind (number unit)
	(when (and start end width)
	  (standardized-time-units (/ (- end start 0.0) width)))
      (apply #'concatenate 'string
	     (format nil *time-line-key* start end width number unit)
	     (loop for f in events collect
		   (concatenate 'string
		       (let ((string (make-string width
						  :initial-element #\space))
			     index
			     (countstart
			       (make-array (list width)
					   :initial-element 0
					   :element-type 'integer))
			     (countend
			       (make-array (list width) :initial-element 0
					   :element-type 'integer)))
			 (loop for e in (cdr f) do
			   (setq index
				 (min (1- width)
				      (floor (* width (/ (- (totalt (third e)
								    (fourth e))
								  start)
							       len)))))
			   (incf (aref countstart index))
			   (setf (aref string index)
				 (if (char= #\space (aref string index))
				     #\( #\*))
			   (setq index
				 (min (1- width)
				      (floor (* width (/ (- (totalt (fifth e)
								    (sixth e))
								  start)
							       len)))))
			   (decf (aref countend index))
			   (setf (aref string index)
				 (if (char= #\space (aref string index))
				     #\) #\*)))
			 (loop for i below width with sum = 0 do
			   (setf sum (+ sum (aref countstart i)
					(aref countend i)))
			   (when (and (/= i 0)
				      (/= (aref countstart (1- i)) 0)
				      (/= (aref countend (1- i)) 0)
				      (char= #\space (aref string i))
				      (> sum 0))
			     (setf (aref string i)
				   (if (> sum 9) #\+ (aref "0123456789" sum)))))
			 string)
		       (format nil "  ~A~& "
			       (symbol-name (or (cdr (assoc (car f) name-alist))
						(car f))))))))))


(defun outermost (calls &aux outer)
  (loop for c in calls
	unless (and outer (<= (totalt (third outer) (fourth outer))
			      (totalt (third c) (fourth c))
			      (totalt (fifth c) (sixth c))
			      (totalt (fifth outer) (sixth outer))))
	collect (setf outer c)))

; get the time represented by the two numbers x (low order) and y (high order)
(defun totalt (x y) (elapsed 0 0 x y))



