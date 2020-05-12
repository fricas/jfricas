(require :asdf)
(require :hunchentoot)

(in-package "BOOT")

(defun ws-fmt (marker)
  (|sayMSG| (format nil "--FORMAT:~A:~D" marker boot::|$IOindex|)))

(setf |$ioHook|
      (lambda (x &optional args)
        (cond
         ((eq x '|startAlgebraOutput|) (ws-fmt "BEG:Algebra"))
         ((eq x '|endOfAlgebraOutput|) (ws-fmt "END:Algebra"))
         ((eq x '|startPatternMsg|)    (ws-fmt "BEG:EROOR"))
         ((eq x '|endPatternMsg|)      (ws-fmt "END:ERROR"))
         ((eq x '|startKeyedMsg|)
          (cond
           ((eq (car args) 'S2GL0012) (ws-fmt "BEG:Type"))
           ((eq (car args) 'S2GL0013) (ws-fmt "BEG:Time"))
           ((eq (car args) 'S2GL0014) (ws-fmt "BEG:TypeTime"))
           ((eq (car args) 'S2GL0016) (ws-fmt "BEG:Storage"))
           ('T                        (ws-fmt "BEG:KeyedMsg"))))
         ((eq x '|endOfKeyedMsg|)
          (cond
           ((eq (car args) 'S2GL0012) (ws-fmt "END:Type"))
           ((eq (car args) 'S2GL0013) (ws-fmt "END:Time"))
           ((eq (car args) 'S2GL0014) (ws-fmt "END:TypeTime"))
           ((eq (car args) 'S2GL0016) (ws-fmt "END:Storage"))
           ('T                        (ws-fmt "END:KeyedMsg"))))
        )))


; Following function calls FriCAS for evaluation of code and returns
; true if ther is an error and nil otherwise.
(defun |webspad-parseAndEvalStr| (code)
  (setf |$printTypeIfTrue| T) ; Make sure we get "Type:" line.
  (setf |$printTimeIfTrue| T) ; Make sure we get "Time:" line.
  (setf |$printStorageIfTrue| T) ; Make sure we get "Storage:" line.
  (eq (catch 'SPAD_READER (catch '|top_level| (boot::|parseAndEvalStr| code)))
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
  (stdout    "" :type string)
  (fortran   "" :type string)
  (html      "" :type string)
  (openmath  "" :type string)
  (error?    "" :type string)
  (input     "" :type string))

(defun spad-eval (code)
  (format nil "~{~A~%~}" (boot::|parseAndEvalToString| code)))

(defun webspad-eval (input)
  (let* (
        ; store original input argument
         (data (make-r :input input))
         ; Because we want to read from the fricas streams via
         ; get-output-stream-string, we must make sure that the
         ; streams are created via make-string-output-stream.
         ; Therefore, we first save the original streams.
         (s-stdout    boot::*standard-output*)
         (s-stderr    boot::*error-output*)
         (s-algebra   boot::|$algebraOutputStream|)
         (s-formatted boot::|$formattedOutputStream|)
         (s-tex       boot::|$texOutputStream|)
         (s-mathml    boot::|$mathmlOutputStream|)
         (s-texmacs   boot::|$texmacsOutputStream|)
         (s-fortran   boot::|$fortranOutputStream|)
         (s-html      boot::|$htmlOutputStream|)
         (s-openmath  boot::|$openMathOutputStream|)

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
    (setf boot::*standard-output*        (make-string-output-stream))
    (setf boot::*error-output*           boot::*standard-output*)
    (setf boot::|$algebraOutputStream|   boot::*standard-output*)
    (setf boot::|$formattedOutputStream| boot::*standard-output*)
    (setf boot::|$texOutputStream|       boot::*standard-output*)
    (setf boot::|$mathmlOutputStream|    boot::*standard-output*)
    (setf boot::|$formattedOutputStream| boot::*standard-output*)
    (setf boot::|$texmacsOutputStream|   boot::*standard-output*)
    ; The following stream have no begin/end markers
    (setf boot::|$htmlOutputStream|      (make-string-output-stream))
    (setf boot::|$fortranOutputStream|   (make-string-output-stream))
    (setf boot::|$openMathOutputStream|  (make-string-output-stream))

    ; eval and return true if there was an error
    (setf (r-error? data) (if (boot::|webspad-parseAndEvalStr| code) "T" "F"))

    ; extract the output from the streams and reset stream
    (gr (r-stdout    data) boot::*standard-output*        s-stdout)
    (gr (r-fortran   data) boot::|$fortranOutputStream|   s-fortran)
    (gr (r-html      data) boot::|$htmlOutputStream|      s-html)
    (gr (r-openmath  data) boot::|$openMathOutputStream|  s-openmath)

    (setf boot::*error-output*           s-stderr)
    (setf boot::|$algebraOutputStream|   s-algebra)
    (setf boot::|$formattedOutputStream| s-formatted)
    (setf boot::|$texOutputStream|       s-tex)
    (setf boot::|$mathmlOutputStream|    s-mathml)
    (setf boot::|$texmacsOutputStream|   s-texmacs)

    ; return the data record
    data))

;;; ====
;;; JSON
;;; ====

(in-package :webspad)

(defun encode-json (data)
  (format nil "{ \"stdout\":~S,~
                 \"fortran\":~S,~
                 \"html\":~S,~
                 \"openmath\":~S,~
                 \"error?\":~S,~
                 \"input\":~S~
               }"
          (r-stdout data)
          (r-fortran data)
          (r-html data)
          (r-openmath data)
          (r-error? data)
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
