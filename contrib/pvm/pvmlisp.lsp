;;;-*-Mode: LISP; Syntax: Common LISP; Base: 10-*-
;;;
;;; File = pvmlisp.lsp
;;;
;;; New version of reader structure using vectors.
;;;
;;;
;;; This code only works with Common LISP. It should not be included
;;; in a CLOS program (yet). It will also not work with CLiCC.
;;;
;;;
;;; Message-start-p is used to detect the start of a complex message.
;;; It is true if it is applied to a message tag.
;;;

(defun message-start-p (mty)
  (and (integerp mty)
       (= MESSAGE_START mty)))

;;;;****************************************************************;;;;
;;;;                                                                ;;;;
;;;;                                                                ;;;;
;;;;****************************************************************;;;;

;;;;****************************************************************;;;;
;;;;                                                                ;;;;
;;;; We define the reader object. This is a structure containing    ;;;;
;;;; the function closures which perform the encoding and decoding. ;;;;
;;;; We begin by defining the encoder and decoder structures and    ;;;;
;;;; manipulation functions (this will be a dream in CLOS or        ;;;;
;;;; TELOS!)                                                        ;;;;
;;;;                                                                ;;;;
;;;;****************************************************************;;;;

;;;
;;; The encoder structure. 
;;; The design of the encoder is such that it allows users to configure
;;; their own encoders. For example, CMU CL calls a SIMPLE-STRING a 
;;; SIMPLE-BASE-STRING. This can be accomodated within this organisation 
;;; at the cost of a little effort.
;;;
(defstruct encoder-rec  
  typename  ;; value returned by type-of and used to index the
            ;; encoder function
  msgtypeno ;; the numeric message type
  encoder-fn)

;;;
;;; Encoders are held in hash tables. The following function (which
;;; should be inline) creates such a table.
;;;
;(declaim (inline make-encoder-structure))
(proclaim '(inline make-encoder-structure))

(defun make-encoder-structure ()
  (make-hash-table :test #'eq))

;;;
;;; encoder-present-p is true if there is an encoder for the
;;; named type in the encoder table.
;;;

(defun encoder-present-p (enc-struc typename)
  (multiple-value-bind (encrec there)
                       (gethash typename enc-struc)
    (declare (ignore encrec))
    there))

;;;
;;; Retrieval function for encoders. Given a type name, it returns the
;;; encoder function associated with the type.
;;;

(defun get-encoder (enc-struc typename)
  (multiple-value-bind (encoder-rec known-type)
                       (gethash typename enc-struc)
    (if known-type
      (encoder-rec-encoder-fn encoder-rec)
      ())))

;;;
;;; Routine to store an encoder function.
;;; Assumes that typename and typeno have been checked.
;;;

(defun put-encoder (enc-struc typename typeno encoder-fn)
  (setf (gethash typename enc-struc)
        (make-encoder-rec :encoder-fn encoder-fn
                          :typename   typename
                          :msgtypeno  typeno))
  (values))

;;;****************************************************************;;;
;;;                                                                ;;;
;;;                                                                ;;;
;;; A routine to replace the encoder function and a routine to     ;;;
;;; remove an encode could be added here.                          ;;;
;;;                                                                ;;;
;;;                                                                ;;;
;;;****************************************************************;;;

;;;
;;; message-type-number returns the type number associated with a
;;; symbolic type name. Its input is an encoder structure.
;;;

(defun message-type-number (enc-struc typename)
  (multiple-value-bind (enc-rec known-type)
                       (gethash typename enc-struc)
    (if known-type
      (encoder-rec-msgtypeno enc-rec)
      (error "cannot return type number for type ~a: unknown type.~%"
             typename))))

;;;;****************************************************************;;;;
;;;;                                                                ;;;;
;;;; The decoder structure and containing object.                   ;;;;
;;;;                                                                ;;;;
;;;;                                                                ;;;;
;;;;****************************************************************;;;;

;;;
;;; The decoder is indexed by its message type number.
;;; Decoders have a symbolic identifier associated with them.
;;;

(defstruct decoder-rec
  typename
  decoder-fn)

;;;
;;; Decoders are held in a hash table. The table is indexed by the
;;; message number. The hash table representation is used so that
;;; users can have gaps in their message number sequences.
;;;

;(declaim (inline make-decoder-structure))
(proclaim '(inline make-decoder-structure))

(defun make-decoder-structure ()
  (make-hash-table :test #'eql))
   
;;;
;;; decoder-present-p is true if there is a decoder structure
;;; in the decoder table at the point indexed by the numeric
;;; message type.
;;;

(defun decoder-present-p (dec-struc msg-type-no)
  (multiple-value-bind (decrec there)
		       (gethash msg-type-no dec-struc)
		       (declare (ignore decrec))
     there))

;;;
;;; get-decoder returns the decoder function associated with a
;;; message type number. If there is no such message, an error is raised.
;;;

(defun get-decoder (decoder-struc msg-no)
  (multiple-value-bind (decrec there)
		       (gethash msg-no decoder-struc)
      (if there
	  (decoder-rec-decoder-fn decrec)
	())))

;;;
;;; put-decoder inserts a decoder record into the decoder vector.
;;; If a decoder structure is already in the vector at the place 
;;; indexed by the message number, an error is raised.
;;;
;;; Note that this function will expand the vector if there is 
;;; insufficient room.
;;;

(defun put-decoder (decoder-struc msg-no msg-typename decoder-fn)
  (setf (gethash msg-no decoder-struc)
          (make-decoder-rec :typename msg-typename
                            :decoder-fn decoder-fn))
  (values))
         

;;;****************************************************************;;;
;;;                                                                ;;;
;;;                                                                ;;;
;;; A routine to replace the decoder function and a routine to     ;;;
;;; remove an encode could be added here.                          ;;;
;;;                                                                ;;;
;;;                                                                ;;;
;;;****************************************************************;;;

;;;
;;; message-number-type returns the symbolic name associated with
;;; a numeric message type.
;;;

(defun message-number-type (decoder-struc msg-type-no)
  (decoder-rec-typename
   (aref decoder-struc msg-type-no)))

      
;;;;****************************************************************;;;;
;;;;                                                                ;;;;
;;;; The reader object and its associated functions.                ;;;;
;;;; Note that encoder and decoders can be added or removed at      ;;;;
;;;; runtime.                                                       ;;;;
;;;;                                                                ;;;;
;;;;****************************************************************;;;;

(defstruct reader-object
  (encoders (make-encoder-structure))
  (decoders (make-decoder-structure))
  (known-type-names ()))

;;;
;;; A creation function for readers.
;;;

(defun make-object-reader ()
  (make-reader-object))

;;;
;;; add-type-name adds a symbolic type name to the reader object.
;;;

;(declaim (inline add-type-name))
(proclaim '(inline add-type-name))

(defun add-type-name (reader-obj typename)
  (pushnew typename
            (reader-object-known-type-names reader-obj)
           :test #'eq)
  (values))

;;;; A deletion function can easily be defined.

;;;
;;; valid-type-name-p is true iff the type name supplied as
;;; the second argument is known to the reader supplied as the
;;; first argument.
;;;

;(declaim (inline valid-type-name-p))
(proclaim '(inline valid-type-name-p))

(defun valid-type-namex-p (reader-obj typename)
  (member typename
	  (reader-object-known-type-names reader-obj)
          :test #'eq))

;(declaim (inline known-type-name-p))
(proclaim '(inline known-type-name-p))

(defun known-type-name-p (reader-obj typename)
  (member typename
          (reader-object-known-type-names reader-obj)
          :test #'eq))

;;;
;;; valid-message-type-no-p is true if the message type number
;;; supplied as the second argument is (i) positive and (ii) in the
;;; range 0 .. (length decoders)
;;;

;(declaim (inline valid-message-type-no-p))
(proclaim '(inline valid-message-type-no-p))

(defun valid-message-type-no-p (reader-obj msg-typeno)
  (multiple-value-bind (decrec present)
		       (gethash msg-typeno
				(reader-object-decoders reader-obj))
		       (declare (ignore decrec))
      present))

;(declaim (inline known-type-number-p))
(proclaim '(inline known-type-number-p))

(defun known-type-number-p (reader-obj msg-typeno)
  (multiple-value-bind (decrec present)
		       (gethash msg-typeno
				(reader-object-decoders reader-obj))
		       (declare (ignore decrec))
      present))

;;;
;;; Routines to add encoder and decoder functions to a reader object.
;;; They can be called at runtime as well as at configuration time.
;;; Procedures to replace readers and writers could be defined if
;;; necessary---they won't be too difficult. 
;;;

(defun add-encoder (reader-obj         ;; the reader object
                    message-type-no    ;; the numeric type of the
                                       ;; message type
                    message-type-name  ;; the symbolic name of the
		                       ;; message type
                    encoder-function)  ;; the encoder function proper
  ; start by checking that the type is not already known.
  (when (and (known-type-name-p reader-obj message-type-name)
             (encoder-present-p (reader-object-encoders reader-obj)
                                 message-type-name))
    (error
     "add-encoder: cannot add encoder for ~a -- one already present~%"
     message-type-name))
  ; try to add the type name (a decoder might have put it there already)
  (add-type-name reader-obj message-type-name)
  ; add the encoder function
  (put-encoder (reader-object-encoders reader-obj)
               message-type-name
               message-type-no
               encoder-function)
  (values))

  
(defun add-decoder (reader-obj         ;; the reader object
                    message-type-no    ;; the numeric type of the
                                       ;; message type
                    message-type-name  ;; the symbolic name of the
                                        ;; message type
                    decoder-function)  ;; the encoder function proper
  ; start by checking that the type is not already known
  (when (and (known-type-name-p reader-obj message-type-name)
             (decoder-present-p (reader-object-decoders reader-obj)
                                message-type-no))
    (error
     "add-decoder: cannot add decoder for ~a -- one already present~%"
     message-type-name))
  ; try to add the type name (an encoder might have already added it)
  (add-type-name reader-obj message-type-name)
  ; add the decoder function
  (put-decoder (reader-object-decoders reader-obj)
               message-type-no
               message-type-name
               decoder-function)
  (values))

;;;;****************************************************************;;;;
;;;;                                                                ;;;;
;;;; Some utility functions.                                        ;;;;
;;;;                                                                ;;;;
;;;;                                                                ;;;;
;;;;****************************************************************;;;;

;;;
;;; Only proper lists can be transmitted and received -- sorry.
;;;

(defun proper-listp (l)
  (and (not (null l))
       (list l)
       (null (cdr (last l)))))

;;;
;;; type-name is used in indexing the encoders.
;;;

(defun type-name (typ)
  (if (symbolp typ)
    typ
    (car typ)))

;;;
;;; initialise-reader-object takes a reader object as its first
;;; argument and a list of lists of the following form:
;;; (typename -- a symbol
;;;  typeno   -- a natural number (one of the LISP_X_TYPEs)
;;;  encoder  -- a closure or the symbol '*
;;;  decoder  -- a closure or the symbol '*
;;; )
;;;

(defun initialise-reader-object (reader-obj  ;; the reader to be started.
                                 specs)      ;; a list of reader and writer
                                             ;; specifications
  (dolist (spec specs)
    (let ((typename (first  spec))
          (typeno   (second spec))
          (encfn    (third  spec))
          (decfn    (fourth spec)))
      (when (and (symbolp encfn)
                 (eq encfn '*)
                 (symbolp decfn)
                 (eq decfn '*))
        (error
         "initialise reader: reader and writer for ~a both unspecified.~%"
         typename))
      (unless (and (symbolp encfn)
                   (eq '*   encfn))
        ; add an encoder.
        (add-encoder reader-obj typeno typename encfn))
      (unless (and (symbolp decfn)
                   (eq '*   decfn))
        (add-decoder reader-obj typeno typename decfn))))
  (values))


;;;;****************************************************************;;;;
;;;;                                                                ;;;;
;;;; Routines to apply encoders and decoders. These are the core    ;;;;
;;;; of the module.                                                 ;;;;
;;;;                                                                ;;;;
;;;;****************************************************************;;;;

(defun apply-encoder (objectreader  ;; reader in which to look for encoder
                      lisp-object)  ;; object to encode
  (let ((tname (type-name (type-of lisp-object))))
    (cond ((not (known-type-name-p objectreader tname))
           (error
            "apply-encoder: cannot encode -- unknown type ~a for object ~a~%"
            tname
            lisp-object))
          (t
           (let ((encode-fn (get-encoder
                             (reader-object-encoders objectreader)
                             tname)))
             (cond ((null encode-fn)
                    (error
                     "apply-encoder: no writer function for type ~a~%"
                     tname))
                   (t
                     (funcall encode-fn lisp-object objectreader)))))))
  (values))

(defun apply-decoder (objectreader     ;; the reader in which to look
                      message-type-no) ;; the number of the message
  (cond ((not (known-type-number-p objectreader message-type-no))
         (error
          "apply-decoder: cannot decode -- unknown message type number ~d~%"
          message-type-no))
        (t
         (let ((decoder-struc (reader-object-decoders objectreader)))
           (let ((decoder-fn (get-decoder decoder-struc message-type-no)))
             (if (null decoder-fn)
               (error
                "apply-decoder: no reader function for type ~a~%"
                (message-number-type decoder-struc message-type-no))
               (funcall decoder-fn objectreader)))))))



;;;****************************************************************;;;
;;;                                                                ;;;
;;; User interface functions.                                      ;;;
;;;                                                                ;;;
;;;****************************************************************;;;

(defun write-object (object reader)
  (apply-encoder reader object))

(defun write-user-object-type (object reader)
  (let ((encoders (reader-object-encoders reader)))
    (multiple-value-bind (encrec there)
                         (gethash (type-name (type-of object))
                                  encoders)
      (if there
        (let ((msgno (encoder-rec-msgtypeno encrec)))
          (when (>= msgno LISP_MIN_USER_TYPE)
            (C-set-message-type msgno)))
        (error
          "write-object: no encoder information for type ~a~%"
         (type-name (type-of object)))))))

(defun read-object (reader)
  (let ((next-object-type (C-next-msg-type)))
(format t "got next type: ~A~%" (type-of next-object-type))
    (when (message-start-p next-object-type)
      (setf next-object-type (C-next-type-name)))
    (apply-decoder reader next-object-type)))

(defun add-writer (reader type-no type-name writer-fn)
  (add-encoder reader type-no type-name writer-fn))

(defun add-reader (reader type-no type-name writer-fn)
  (add-decoder reader type-no type-name writer-fn))



;;;;****************************************************************;;;;
;;;;                                                                ;;;;
;;;; Readers and writers for vectors and lists.                     ;;;;
;;;; These should be used as default (they are, in any case,        ;;;;
;;;; portable).                                                     ;;;;
;;;;                                                                ;;;;
;;;;****************************************************************;;;;

;(declaim (inline ok-message-type))
(proclaim '(inline ok-message-type))

(defun ok-message-type (rdr type-no)
  (known-type-number-p rdr type-no))

;;;
;;; Writer (encoder) for vectors.
;;; Vectors must be of type SIMPLE-VECTOR.
;;;

(defun encode-vector (vec objreader)
  (let ((len (length vec))) ;; get the length for the header.
    ;; call the C primitive for stuffing the length
    ;; into the PVM buffer
    (C-obuffer-vector-header len)
    ;; iterate over the vector, encoding each item and
    ;; stuffing it into the buffer.
    (dotimes (i len)
      (apply-encoder objreader (aref vec i)))
    ;; when control drops out of the bottom of this loop,
    ;; the vector has been encoded.
    ))

;;;
;;; Reader (decoder) for vectors.
;;;

(defun decode-vector (objreader)
  ;; we know we have a vector, so get the length by
  ;; calling the C primitive.
  (let ((vector-len (C-ibuffer-vector-length)))
    (cond ((minusp vector-len)
           (error "Cannot read vector: negative length ~d~%"
                  vector-len))
          ((zerop vector-len)
           (make-array '(0)))
          (t
           (let ((vec (make-array (list vector-len))))
             ;; create a new vector and try to fill its elements
             (dotimes (i vector-len)
               (let ((next-obj-type ;; get the type of the next
                                    ;; object to be decoded from a C
                                    ;; routine
                      (C-next-msg-type)))
                 (when (not (ok-message-type objreader next-obj-type))
                   ;; call a routine to check that there is an object
                   ;; that comes next.
                   (error "Cannot read vector: invalid type ~s~%"
			  next-obj-type))
                 (when (message-start-p next-obj-type)
                   (setq next-obj-type (C-next-type-name)))
                 (let ((next-elem (apply-decoder objreader next-obj-type)))
                   (setf (aref vec i) next-elem))))
             vec)))))

;;;
;;; Writer (encoder) for lists.
;;; Lists must be PROPER lists.
;;;

(defun encode-list (list-to-go objreader)
  ;; First ensure that we have a proper list.
  (unless (proper-listp list-to-go)
    (error
     "encode-list: input list is not proper~% ~s ~%-- cannot encode, sorry.~%"
     list-to-go))
  ;; The list header should have been put into the output buffer.
  ;; Remember that the end of the list has to be a nil message object.
  ;; So: mark the object to go as a list by calling the C routine.
  ;; (Perhaps the length could also be encoded for extra checking---
  ;; perhaps not.)
  ;; OK. Run over the list and encode the elements.
  (C-obuffer-list-header)
  (mapc ; or dolist or explicit manipulation---it doesn't matter
   #'(lambda (element)
       (apply-encoder objreader element))
   list-to-go)
  ;; finally, put a NIL into the output buffer to say that it's the
  ;; end: do this by calling the C routine.
  (C-obuffer-nil)
  (values))

;;;
;;; Reader (decoder) for lists.
;;;

(defun decode-list (objreader)
  ;; When we're called, we know we have a list.
  ;; We need to iterate until we get a nil object.
  ;; (Problem: what happens if there is no nil at the end??)
  (let ((newlist ()) ;; the list we're going to build.
        (next-item-type ())) ;; the type of the next object in the
                             ;; input buffer
    (loop
      (setq next-item-type (C-next-msg-type))
      (when (not (ok-message-type objreader next-item-type))
        (error "cannot decode list: invalid type ~s~%"
               next-item-type))
      (cond ((= next-item-type LISP_NIL_TYPE)
             (return)) ; got the end of the list.
            ((message-start-p next-item-type)
             (setq next-item-type (C-next-type-name))
             (push (apply-decoder objreader next-item-type) newlist))
            (t
             (push (apply-decoder objreader next-item-type) newlist))))
    (reverse newlist)))


;;;;****************************************************************;;;;
;;;;                                                                ;;;;
;;;; An example of how to define a reader and a writer for a        ;;;;
;;;; structure (the same outline applies to classes).               ;;;;
;;;;                                                                ;;;;
;;;;****************************************************************;;;;
#|

(defparameter *rdr* (make-object-reader))

(defstruct foo slot1 slot2)

(defconstant foo-type 32)

(defun write-foo (obj rdr)
  (write-object (foo-slot1 obj) rdr)
  (write-object (foo-slot2 obj) rdr))

(defun read-foo (rdr)
  (let ((s1 (read-object rdr))
        (s2 (read-object rdr)))
    (make-foo :slot1 s1 :slot2 s2)))

(add-writer *rdr* foo-type 'foo #'write-foo)
(add-reader *rdr* foo-type 'foo #'read-foo)
|#
