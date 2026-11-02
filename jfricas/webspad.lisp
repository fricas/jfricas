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
  ;;; we don't want Fortran, Html, OpenMath output
  (if (boundp '|$fortran_out_rec|)
      ;;; newer FriCAS: on/off flag is slot 2 of the output record
      (dolist (r '(|$fortran_out_rec| |$html_out_rec| |$openmath_out_rec|))
        (when (boundp r) (setf (aref (symbol-value r) 2) NIL)))
      (progn
        (setf |$fortranFormat| NIL)
        (setf |$htmlFormat| NIL)
        (setf |$openMathFormat| NIL)))
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

;;; Newer FriCAS keeps each output stream in slot 0 of a record
;;; boot::|$<format>_out_rec| as (NIL . lisp-stream) instead of in
;;; variables like boot::|$algebraOutputStream|.
(defvar webspad-out-recs
  '(boot::|$algebra_out_rec| boot::|$tex_out_rec| boot::|$mathml_out_rec|
    boot::|$texmacs_out_rec| boot::|$formatted_out_rec|))

(defvar webspad-old-stream-vars
  '(boot::|$algebraOutputStream| boot::|$texOutputStream|
    boot::|$mathmlOutputStream| boot::|$texmacsOutputStream|
    boot::|$formattedOutputStream|))

(defun webspad-eval (input)
  (let* (
        ;;; store original input argument
         (data (make-r :input input))
         (newp (boundp 'boot::|$algebra_out_rec|))
         (recs (remove-if-not #'boundp webspad-out-recs))
         (vars (remove-if-not #'boundp webspad-old-stream-vars))
         ;;; save the original streams
         (saved (if newp
                    (mapcar (lambda (r) (aref (symbol-value r) 0)) recs)
                    (mapcar #'symbol-value vars)))
         (boot::*OBEY-STDOUT* T)
         (*standard-output* webspad-stream)
         (*error-output* *standard-output*)
         (s (if newp
                (cons nil *standard-output*)
                (boot::|mkOutputConsoleStream|)))) ; use *standard-output*

    (unwind-protect
         (progn
           ;;; redirect all format streams to our string stream
           (if newp
               (dolist (r recs) (setf (aref (symbol-value r) 0) s))
               (dolist (v vars) (setf (symbol-value v) s)))
           ;;; eval and return true if there was an error
           (setf (r-error? data)
                 (if (boot::|webspad-parseAndEvalStr| input) "T" "F")))
      ;;; restore the original streams
      (if newp
          (loop for r in recs for o in saved
                do (setf (aref (symbol-value r) 0) o))
          (loop for v in vars for o in saved
                do (setf (symbol-value v) o))))

    (setf (r-stdout data) (get-output-stream-string webspad-stream))

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
