(in-package "BOOT")
;;; We redefine an internal function of FriCAS that is responsible
;;; for returning the format in which the type is output.
;;; Instead of returning the default from src/share/doc/msgs/s2-us.msgs
;;; we return something that we can better catch in the ouput.
(DEFUN |getKeyedMsg| (|key|)
  (PROG ()
    (RETURN
     (COND
      ((EQ |key| 'S2GL0012) "%rjon --BEGIN-TYPE %l %1 %l --END-TYPE %rjoff")
      ((EQ |key| 'S2GL0013) "%rjon --BEGIN-TIME %l %1 %l --END-TIME %rjoff")
      ((EQ |key| 'S2GL0014)
       "%rjon --BEGIN-TYPE %l %1 %l --END-TYPE %l --BEGIN-TIME %l %2 %l --END-TIME %rjoff")
      ('T
       (PROGN
        (COND
         ((NULL |$msg_hash|) (SETQ |$msg_hash| (MAKE_HASHTABLE 'ID))
          (|cacheKeyedMsg| |$defaultMsgDatabaseName|)))
        (HGET |$msg_hash| |key|)))))))

; Following function calls FriCAS for evaluation of code and returns
; true if ther is an error and nil otherwise.
(DEFUN |webspad-parseAndEvalStr| (code)
    (EQ (CATCH 'SPAD_READER (CATCH '|top_level| (boot::|parseAndEvalStr| code)))
        '|restart|))


;;; ---------------------------------------------------------------

(defpackage webspad
    (:use common-lisp)
    (:documentation "see docs"))


;;; ====
;;; EVAL
;;; ====

(in-package :webspad)

; Get result and sent back the stream to its original.
(defmacro gr (v s saved)
  `(progn
    (setf ,v (get-output-stream-string ,s))
    (setf ,s ,saved)))

(defstruct r
  (step      "" :type string) ; step number in FriCAs
  (error?    "" :type string)
  (algebra   "" :type string) ; optionally contains time and type
  (formatted "" :type string)
  (tex       "" :type string)
  (html      "" :type string)
  (mathml    "" :type string)
  (fortran   "" :type string)
  (texmacs   "" :type string)
  (openmath  "" :type string)
  (stderr    "" :type string)
  (stdout    "" :type string)
  (input     "" :type string))

(defun spad-eval (code)
  (format nil "~{~A~%~}" (boot::|parseAndEvalToString| code)))

(defun webspad-eval (input)
  (let* (
        ; store original input argument
         (data (make-r :step (format nil "~S" boot::|$IOindex|) :input input))
         ; Because we want to read from the fricas streams via
         ; get-output-stream-string, we must make sure that the
         ; streams are created via make-string-output-stream.
         ; Therefore, we first save the original streams.
         (s-algebra   boot::|$algebraOutputStream|)
         (s-formatted boot::|$formattedOutputStream|)
         (s-tex       boot::|$texOutputStream|)
         (s-html      boot::|$htmlOutputStream|)
         (s-mathml    boot::|$mathmlOutputStream|)
         (s-fortran   boot::|$fortranOutputStream|)
         (s-texmacs   boot::|$texmacsOutputStream|)
         (s-openmath  boot::|$openMathOutputStream|)
         (s-stderr    boot::*error-output*)
         (s-stdout    boot::*standard-output*)

         ; Check for multiline input and create a temporary file for it.
         (code (if (> (count #\newline input) 0)
                   ; Tempfile for multiline input -> )read
                   (let* ((tmp (format nil ".tmp-ispad-~S.input"
                                      (random 100000))))
                     (with-open-file
                      (stream tmp :direction :output :if-exists :supersede)
                      (format stream input))
                     (format nil ")read ~S )quiet )ifthere" tmp))
                 input))

        )

    ; create empty streams
    (setf boot::|$algebraOutputStream|   (make-string-output-stream))
    (setf boot::|$formattedOutputStream| (make-string-output-stream))
    (setf boot::|$texOutputStream|       (make-string-output-stream))
    (setf boot::|$htmlOutputStream|      (make-string-output-stream))
    (setf boot::|$mathmlOutputStream|    (make-string-output-stream))
    (setf boot::|$formattedOutputStream| (make-string-output-stream))
    (setf boot::|$fortranOutputStream|   (make-string-output-stream))
    (setf boot::|$texmacsOutputStream|   (make-string-output-stream))
    (setf boot::|$openMathOutputStream|  (make-string-output-stream))
    (setf boot::*error-output*           (make-string-output-stream))
    (setf boot::*standard-output*        (make-string-output-stream))

    ; eval and return true if there was an error
    (setf (r-error? data) (if (boot::|webspad-parseAndEvalStr| code) "T" "F"))

    ; extract the output from the streams and reset stream
    (gr (r-algebra   data) boot::|$algebraOutputStream|   s-algebra)
    (gr (r-formatted data) boot::|$formattedOutputStream| s-formatted)
    (gr (r-tex       data) boot::|$texOutputStream|       s-tex)
    (gr (r-html      data) boot::|$htmlOutputStream|      s-html)
    (gr (r-mathml    data) boot::|$mathmlOutputStream|    s-mathml)
    (gr (r-fortran   data) boot::|$fortranOutputStream|   s-fortran)
    (gr (r-texmacs   data) boot::|$texmacsOutputStream|   s-texmacs)
    (gr (r-openmath  data) boot::|$openMathOutputStream|  s-openmath)
    (gr (r-stderr    data) boot::*error-output*           s-stderr)
    (gr (r-stdout    data) boot::*standard-output*        s-stdout)

    ; return the data record
    data))

;;; ====
;;; JSON
;;; ====

(in-package :webspad)

(defun encode-json (data)
  (format nil "{ \"step\":~S,~
                 \"error?\":~S,~
                 \"algebra\":~S,~
                 \"formatted\":~S,~
                 \"tex\":~S,~
                 \"html\":~S,~
                 \"mathml\":~S,~
                 \"fortran\":~S,~
                 \"texmacs\":~S,~
                 \"openmath\":~S,~
                 \"stderr\":~S,~
                 \"stdout\":~S,~
                 \"input\":~S~
               }"
          (r-step data)
          (r-error? data)
          (r-algebra data)
          (r-formatted data)
          (r-tex data)
          (r-html data)
          (r-mathml data)
          (r-fortran data)
          (r-texmacs data)
          (r-openmath data)
          (r-stderr data)
          (r-stdout data)
          (r-input data)))


;;; ======
;;; SERVER
;;; ======

(in-package :webspad)

;;;
;;; Config
;;;
(defparameter +port+ 4242)


;;;
;;; WEB server
;;;
(hunchentoot:define-easy-handler (fricas-eval :uri "/eval") (code)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "~A~%" (spad-eval code)))

(hunchentoot:define-easy-handler (fricas-raw :uri "/raw") (code)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "~A~%" (webspad-eval code)))

(hunchentoot:define-easy-handler (fricas-json :uri "/json") (code)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "~A~%" (encode-json (webspad-eval code))))


;;;(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port +port+))

;;; add :address "localhost"  if you wish local access only

;(defvar fricas-default-acceptor)
;(setf fricas-default-acceptor (make-instance 'hunchentoot:easy-acceptor :port +port+))
;(hunchentoot:start fricas-default-acceptor)

;;; Get the port in Fricas
;;; )lisp (hunchentoot::acceptor-port webspad::fricas-default-acceptor)


(defun start (port address)
  (hunchentoot:start
    (make-instance 'hunchentoot:easy-acceptor :port port :address address)))

 ; usage:
; )lisp (defvar webspad::fricas-acceptor (webspad::start 4242 "localhost"))
; )lisp (hunchentoot::acceptor-port webspad::fricas-acceptor)
