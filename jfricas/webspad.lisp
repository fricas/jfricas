;;; https://jfricas.readthedocs.io
;;; https://github.com/fricas/jfricas.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file is loaded into FriCAS at initialization time.
;;; It serves the following purposes
;;; (a) makes a webserver (Hunchentoot) available,
;;; (b) extends/modifies some FriCAS (package "BOOT") behavior,
;;; (c) provides functions (package "webspad") that take a multiline
;;;     (FriCAS) code string, send it to FriCAS and catch the
;;;     FriCAS output.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BOOT is a Package in FriCAS.
;;  This part adds a few functions to the BOOT package of FriCAS
;;; to make the FriCAS output contain a number of markers (via the
;;; |$ioHook| facility) that make parsing of the output easier.
;;; Furthermore, it contains a function, interpret-block, which
;;; interprets a (potentially multiline) block of code.

;;; The function "interpret-block" might be a candidate for inclusion
;;; in FriCAS.

;;; The output formats TexFormat, TexmacsFormat, MathMLFormat can
;;; easily be recognized in the output stream and thus need no extra
;;; markers. However, the algebra output, which is switched on via
;;; ")set output algebra on" in a FriCAS session, does not have any
;;; begin/end markers. They are added here. We also add markers for
;;; the output of the 'error' function in FriCAS and the messages that
;;; are used in FriCAS to inform the user (KeyedMsg). Also type, time,
;;; and storage information come on the KeyedMsg channel. They are,
;;; however, so important to us, that we catch them here by their
;;; message tags and mark them in a special way.

;;; In particular, the storage message is important here, since its
;;; markers are used in fricaskernel.py to extract the step number of
;;; FriCAS and synchronize the Out[n] number in the Jupyter notebook
;;; with it.

;;; Note that storage output is normally not switched on. Since it is
;;; vital for us, we turn it on before we send any code to FriCAS.

;;; We explicitly ignore (turn off) formats that do not seem to be
;;; relevant in a Jupyter notebook and are, in fact, because of
;;; missing begin/end markers hard to detect in the output stream.
;;; Yes, it was a design decision to turn off the FriCAS formats
;;; FortranFormat, HtmlFormat, and OpenMathFormat.

;;; All markers that we introduce here look like
;;;   --FORMAT:BEG:formatname:stepnumber
;;;   --FORMAT:END:formatname:stepnumber
;;; where the stepnumber is given by boot::|$IOindex| of FriCAS and
;;; formatname is given below.

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

;;; Following function calls FriCAS for evaluation of code and returns
;;; true if ther is an error and nil otherwise.
(defun |webspad-parseAndEvalStr| (code)
;  (setf |$printTypeIfTrue| T)    ; Make sure we get "Type:" line.
;  (setf |$printTimeIfTrue| T)    ; Make sure we get "Time:" line.
  (setf |$printStorageIfTrue| T) ; Make sure we get "Storage:" line.
  (setf |$fortranFormat| NIL)    ; we don't want Fortran output
  (setf |$htmlFormat| NIL)       ; we don't want Html output
  (setf |$openMathFormat| NIL)   ; we don't want OpenMath output
  (setf |$MARGIN| 0)             ; we don't want indentation
  (setf |$print_equatnum| NIL)   ; we don't want indentation

  ;;; code might consist of multiple lines (i.e. contain newline
  ;;; characters)
  (eq (catch 'SPAD_READER (catch '|top_level| (|interpret_block| code)))
      '|restart|))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage webspad
    (:use common-lisp)
    (:documentation "see source code"))

(in-package :webspad)

;;; The function webspad-eval is called from the Jupyter notebook via
;;; fricaskernel.py and the Hunchentoot webserver. It receives the
;;; code of a cell as input. Before this code is sent to FriCAS, all
;;; relevant streams that FriCAS writes to are stored and replaced by
;;; a new and empty string output stream. After evaluation by FriCAs,
;;; the output is fetched and the saved stream are restored.

(defstruct r
  (stdout    "" :type string)
  (error?    "" :type string)
  (input     "" :type string))

;;; One stream that catches all output.
(defvar webspad-stream (make-string-output-stream))

(defun webspad-eval (input)
  (let* (
        ;;; store original input argument
         (data (make-r :input input))
         ;;; Because we want to read from the fricas streams via
         ;;; get-output-stream-string, we must make sure that the
         ;;; streams are created via make-string-output-stream.
         ;;; Therefore, we first save the original streams.
         ;;; If |$formattedOutputStream| is not in the FriCAS code base,
         ;;; then that stream is not considered.
         (formattedp (boundp 'boot::|$formattedOutputStream|))
         (s-formatted (if formattedp boot::|$formattedOutputStream|))
         (s-stdout    boot::*standard-output*)
         (s-stderr    boot::*error-output*)
         (s-algebra   boot::|$algebraOutputStream|)
         (s-tex       boot::|$texOutputStream|)
         (s-mathml    boot::|$mathmlOutputStream|)
         (s-texmacs   boot::|$texmacsOutputStream|)
         (boot::*OBEY-STDOUT* T)
         (*standard-output* webspad-stream)
         (*error-output* *standard-output*)
         (s           (boot::|mkOutputConsoleStream|))) ; use *standard-output*

    ;;; create empty streams
    (setf boot::|$algebraOutputStream|   s)
    (setf boot::|$texOutputStream|       s)
    (setf boot::|$mathmlOutputStream|    s)
    (setf boot::|$texmacsOutputStream|   s)
    (if formattedp (setf boot::|$formattedOutputStream| s))

    ;;; eval and return true if there was an error
    (setf (r-error? data) (if (boot::|webspad-parseAndEvalStr| input) "T" "F"))

    (setf (r-stdout data) (get-output-stream-string boot::*standard-output*))
    (if formattedp (setf boot::|$formattedOutputStream| s-formatted))

    (setf boot::|$algebraOutputStream|   s-algebra)
    (setf boot::|$texOutputStream|       s-tex)
    (setf boot::|$mathmlOutputStream|    s-mathml)
    (setf boot::|$texmacsOutputStream|   s-texmacs)

    ;;; return the data record
    data))

;;; The function spad-eval is only intended to call FriCAS and return
;;; its result with one-line code.
(defun spad-eval (code)
  (format nil "~{~A~%~}" (boot::|parseAndEvalToString| code)))


;;; fricaskernel.py expects the output in form of a json record.
(defun encode-json (data)
  (format nil "{ \"stdout\":~S,~
                 \"error?\":~S,~
                 \"input\":~S~
               }"
          (r-stdout data)
          (r-error? data)
          (r-input data)))


;;; Make the url http://localhost:PORT/eval?code=sin(x) available.
(hunchentoot:define-easy-handler (fricas-eval :uri "/eval") (code)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "~A~%" (spad-eval code)))

;;; Make the url
;;; http://localhost:PORT/raw?code=integrate(sin(x),x) available.
(hunchentoot:define-easy-handler (fricas-raw :uri "/raw") (code)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "~A~%" (webspad-eval code)))

;;; Make the url
;;; http://localhost:PORT/json?code=D(sin(x),x) available.
(hunchentoot:define-easy-handler (fricas-json :uri "/json") (code)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "~A~%" (encode-json (webspad-eval code))))

;;; Start the Hunchentoot webserver.
(defun start (port address)
  (hunchentoot:start
    (make-instance 'hunchentoot:easy-acceptor :port port :address address)))
