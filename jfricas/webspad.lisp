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
         ((eq x '|startPatternMsg|)    (ws-fmt "BEG:ERROR"))
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

; interpret-block takes a code string that is interpreted as if it
; comes from a .input file.
(DEFUN |interpret-block| (|code|)
  (PROG (|$ncMsgList| |$erMsgToss| |$lastPos| |$EchoLines| |st|)
    (DECLARE (SPECIAL |$ncMsgList| |$erMsgToss| |$lastPos| |$EchoLines|))
    (RETURN
     (PROGN
      (SETQ |$EchoLines| NIL)
      (SETQ |$lastPos| |$nopos|)
      (SETQ |$erMsgToss| NIL)
      (SETQ |$ncMsgList| NIL)
      (SETQ |st| (MAKE-STRING-INPUT-STREAM |code|))
      (|intloopInclude0| |st| '|webspad| 0)))))

; Following function calls FriCAS for evaluation of code and returns
; true if ther is an error and nil otherwise.
(defun |webspad-parseAndEvalStr| (code)
;  (setf |$printTypeIfTrue| T)    ; Make sure we get "Type:" line.
;  (setf |$printTimeIfTrue| T)    ; Make sure we get "Time:" line.
  (setf |$printStorageIfTrue| T) ; Make sure we get "Storage:" line.
  (setf |$fortranFormat| NIL)    ; we don't want Fortran output
  (setf |$htmlFormat| NIL)       ; we don't want Html output
  (setf |$openMathFormat| NIL)   ; we don't want OpenMath output
  (setf |$MARGIN| 0)             ; we don't want indentation

  ; code might consist of multiple lines (i.e. contain newline characters)
  (eq (catch 'SPAD_READER (catch '|top_level| (|interpret-block| code)))
      '|restart|))

;;; ---------------------------------------------------------------

(defpackage webspad
    (:use common-lisp)
    (:documentation "see docs"))


;;; ====
;;; EVAL
;;; ====

(in-package :webspad)

(defstruct r
  (stdout    "" :type string)
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
         (s-texmacs   boot::|$texmacsOutputStream|))

    ; create empty streams
    (setf boot::*standard-output*        (make-string-output-stream))
    (setf boot::*error-output*           boot::*standard-output*)
    (setf boot::|$algebraOutputStream|   boot::*standard-output*)
    (setf boot::|$formattedOutputStream| boot::*standard-output*)
    (setf boot::|$texOutputStream|       boot::*standard-output*)
    (setf boot::|$mathmlOutputStream|    boot::*standard-output*)
    (setf boot::|$formattedOutputStream| boot::*standard-output*)
    (setf boot::|$texmacsOutputStream|   boot::*standard-output*)

    ; eval and return true if there was an error
    (setf (r-error? data) (if (boot::|webspad-parseAndEvalStr| input) "T" "F"))

    (setf (r-stdout data) (get-output-stream-string boot::*standard-output*))
    (setf boot::*standard-output*        s-stdout)
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
                 \"error?\":~S,~
                 \"input\":~S~
               }"
          (r-stdout data)
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


(defun start (port address)
  (hunchentoot:start
    (make-instance 'hunchentoot:easy-acceptor :port port :address address)))
