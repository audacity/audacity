$nyquist plug-in
$version 4
$type tool
$name (_ "EQ XML to TXT Converter")
$manpage "EQ_XML_to_TXT_Converter"
$debugbutton false
$preview disabled
$author (_ "Steve Daulton")
$release 3.0.4
$copyright (_ "GNU General Public License v2.0 or later")


;; Released under terms of the GNU General Public License v2.0 or later:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference


$control fxname (_ "Select target EQ effect") choice (("Graphic" (_ "Graphic EQ"))
                                                      ("FilterCurve" (_ "Filter Curve EQ"))) 0

$control infile (_ "Equalization XML file") file "" "*default*/EQCurves.xml" (((_ "XML file") (xml XML))
                        ((_ "All files") ("")))  "open,exists"

$control overwrite (_ "If output text file exists") choice (("Append" (_ "Append number"))
                                                            ("Overwrite" (_ "Overwrite"))
                                                            ("Error" (_ "Error"))) 0

; output message
(setf success
  (format nil "Input:~%~a~%~%Output:~%" infile))


(defun process-xml (filename)
  (let ((fp-in (open filename :direction :input))
        curve-name
        iscurve
        gotcurve
        (frequencies ())
        (values ()))
    (unless fp-in
      (throw 'err
          (format nil (_ "Error.~%Unable to open file~%~s") filename)))
    ;; Read each line and get the (frequency value) pair, and the curve name.
    (do ((line (read-line fp-in) (read-line fp-in)))
        ((not line))
      (let ((fxname (get-name line)))
        (when fxname
          ;; trim in two steps so that we don't remove characters from the curve name.
          (setf curve-name (string-trim "\t name=" fxname))
          (setf curve-name (string-trim "\t \"" curve-name))
          (setf iscurve t)))
      (when iscurve
        (let ((f-v-pair (get-f-v-pair (string-trim " <>/point\t\r" line))))
          (when f-v-pair
            (push (first f-v-pair) frequencies)
            (push (second f-v-pair) values)))
        (when (get-end-of-curve line)
          (setf gotcurve t)
          (setf iscurve nil)))
      (when gotcurve
        (write-curve-txt curve-name frequencies values)
        (psetq gotcurve nil
               curve-name nil
               frequencies ()
               values ())))
    (close fp-in)
    (format nil success)))

(defun get-f-v-pair (line &aux (f "")(v ""))
  ;;Return (list frequency value) or nil
  (cond
    ;; Get frequency
    ((and (> (length line) 3)(string-equal (subseq line 0 3) "f=\""))
      (setf line (subseq line 3))
      (setf idx ;number of characters to remove from string
        (dotimes (i (length line))
          (setf ch (char line i))
          (cond
            ((char= ch #\-) (setf f (format nil "~a~a" f #\-)))
            ((digit-char-p ch) (setf f (format nil "~a~a" f ch)))
            ((char= ch #\.) (setf f (format nil "~a~a" f #\.)))
            (t  (return (1+ i))))))
      ;; We now have the frequency., and the number of characters.
      (setf v (string-trim " \td=\"" (subseq line idx)))
      (list f v))))

(defun get-name (line)
  ;; If line is in the form: "  <curve name=\"Name of curve\">  "
  ;; Return "Name of curve", else NIL.
  (let ((line (string-trim " <>\t\r" line)))
    (cond
      ((< (length line) 6) nil)
      ((string-equal (subseq line 0 6) "curve ")
        (subseq line 5))
      (t nil))))

(defun get-end-of-curve (line)
  ;; If the line is "   </curve>  "
  ;; Return T else NIL.
  (let ((line (string-trim " \t\r" line)))
    (if (string-equal line "</curve>") t nil)))

(defun write-curve-txt (curvename frequencies values)
  ;; Write new style curve file(s), ensuring no overwrites unless explicitly allowed.
  (let ((outfile (get-out-file-name (get-path infile) curvename))
        fp-out)
    (if outfile
        (setf fp-out (open outfile :direction :output))
        (throw 'err
            (format nil
                    ;i18n-hint: Do not translate "~a".txt
                    (_ "Error.~%File overwrite disallowed:~%\"~a.txt\"")
                    curvename)))
    (unless fp-out
      (throw 'err (format nil
                          (_ "Error.~%File cannot be written:~%\"~a.txt\"")
                          curvename)))
    (format fp-out "~a" (if (= fxname 0) "GraphicEq:" "FilterCurve:"))
    (dotimes (i (length frequencies))
      (format fp-out "f~a=~s " i (nth i frequencies)))
    (format fp-out
            "filterLength=\"8191\" InterpolateLin=\"0\" ~
            InterpolationMethod=\"B-spline\"")
    (dotimes (i (length frequencies))
      (format fp-out " v~a=~s" i (nth i values)))
    (close fp-out)
    (string-append success outfile "\n")))

(defun get-path (fqname)
  ;; Return file path from fully qualified file name.
  (do ((i (1- (length fqname)) (1- i)))
      ((= i 0) nil)
    (when (char= (char fqname i) *file-separator*)
      (return-from get-path (subseq fqname 0 (1+ i))))))

(defun get-out-file-name (path fname)
  ; Return a fully qualified name or nil.
  (case overwrite
    (1  ;Overwrite
      (format nil "~a~a.txt" path fname))
    (2  ;Error
      (let* ((fqname (format nil "~a~a.txt" path fname))
             (fp (open fqname)))
        (cond
          (fp (close fp)
              nil)
          (t fqname))))
    (t  ; If file already exists, append a number to the name (up to 99).
      (dotimes (i 99 nil)
        (let* ((fqname  (format nil "~a~a~a.txt"
                                path fname (if (> i 0) i "")))
               (fp (open fqname)))
          (if fp
              (close fp)
              (return-from get-out-file-name fqname)))))))

(catch 'err (process-xml infile))
