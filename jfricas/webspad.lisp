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

;;; ---------------------------------------------------------------

(defpackage webspad
    (:use common-lisp)
    (:documentation "see docs"))


;;; ====
;;; EVAL
;;; ====

(in-package :webspad)

(defstruct ws-data
  (algebra   "" :type string)
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
         (data (make-ws-data :input input))
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

    ; output of the follwing command appears in the streams
    (boot::|parseAndEvalStr| code)

    ; extract the output from the streams
    (setf (ws-data-algebra   data)
          (get-output-stream-string boot::|$algebraOutputStream|))

    (setf (ws-data-formatted data)
          (get-output-stream-string boot::|$formattedOutputStream|))

    (setf (ws-data-tex       data)
          (get-output-stream-string boot::|$texOutputStream|))

    (setf (ws-data-html      data)
          (get-output-stream-string boot::|$htmlOutputStream|))

    (setf (ws-data-mathml    data)
          (get-output-stream-string boot::|$mathmlOutputStream|))

    (setf (ws-data-fortran   data)
          (get-output-stream-string boot::|$fortranOutputStream|))

    (setf (ws-data-texmacs   data)
          (get-output-stream-string boot::|$texmacsOutputStream|))

    (setf (ws-data-openmath  data)
          (get-output-stream-string boot::|$openMathOutputStream|))

    (setf (ws-data-stderr    data)
          (get-output-stream-string boot::*error-output*))

    (setf (ws-data-stdout    data)
          (get-output-stream-string boot::*standard-output*))

    ; Put the old streams back.
    (setf boot::|$algebraOutputStream|   s-algebra)
    (setf boot::|$formattedOutputStream| s-formatted)
    (setf boot::|$texOutputStream|       s-tex)
    (setf boot::|$htmlOutputStream|      s-html)
    (setf boot::|$mathmlOutputStream|    s-mathml)
    (setf boot::|$formattedOutputStream| s-formatted)
    (setf boot::|$fortranOutputStream|   s-fortran)
    (setf boot::|$texmacsOutputStream|   s-texmacs)
    (setf boot::|$openMathOutputStream|  s-openmath)
    (setf boot::*error-output*           s-stderr)
    (setf boot::*standard-output*        s-stdout)

    ; return the data record
    data))

;;; ====
;;; JSON
;;; ====

(in-package :webspad)

(defun encode-json (data)
  (format nil "{ \"algebra\":~S,~
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
          (ws-data-algebra data)
          (ws-data-formatted data)
          (ws-data-tex data)
          (ws-data-html data)
          (ws-data-mathml data)
          (ws-data-fortran data)
          (ws-data-texmacs data)
          (ws-data-openmath data)
          (ws-data-stderr data)
          (ws-data-stdout data)
          (ws-data-input data)))


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
