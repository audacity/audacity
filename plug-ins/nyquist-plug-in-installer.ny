$nyquist plug-in
$version 4
$type tool
$name (_ "Nyquist Plug-in Installer")
$manpage "Nyquist_Plug-in_Installer"
;$debugbutton false
$preview disabled
$author "Steve Daulton"
$release 2.3.1
$copyright (_ "Released under terms of the GNU General Public License version 2")


;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference


$control plug-in (_ "Select plug-in file") file (_ "File Browser") "~/Desktop/" "Plug-in|*.ny;*.NY|Text file|*.txt;*.TXT|All files|*.*;*" "open,exists"

;; As this plug-in is intended primarily to help novice users, it is unsafe to allow overwriting.
;$control overwrite (_ "If plug-in is already installed") choice ((_ "Keep original") (_ "Overwrite")) 0

(if (not (boundp 'overwrite))
    (setf overwrite 0))


(defun audacity-version-ok (min-version)
  (cond
    ((get '*audacity* 'version)
      (mapc (lambda (x y)
              (cond
                ((boundp 'isok))
                ((> x y) (setf isok t))
                ((< x y) (setf isok nil))))
            (get '*audacity* 'version)
            min-version)
      (or (not (boundp 'isok)) isok))
    (t nil)))

;; Extract file name and extension from fully qualified file name.
(defun get-file-name (fqname &aux (fname ""))
  (dotimes (i (length fqname) fname)
    (if (char= (char fqname i) *file-separator*)
        (setf fname "")
        (setf fname (format nil "~a~a" fname (char fqname i))))))

;; Predicate, is file name
(defun isfilename (fname)
  (let ((ln (length fname)))
    (cond
      ((= ln 0) nil)
      ((char= (char fname (- ln 1)) *file-separator*) nil)
      (t t))))

;; Predicate, file exists.
(defun existsp (fname)
  (let ((fp (open fname)))
    (cond
      (fp (close fp) t)
      (t nil))))

;Predicate, file is writeable.
(defun writeablep (fname)
  (let ((fp (open fname :direction :output)))
    (cond
      (fp (close fp) t)
      (t nil))))

;; Copy from input file to output file.
(defun copy-file (input output)
  (let ((ifp (open input :direction :input))
        (ofp (open output :direction :output)))
    (do ((line (read-line ifp)(read-line ifp)))
        ((not line))
      (format ofp "~a~%" line))
    (close ifp)
    (close ofp)))

;;Predicate, looks like a Nyquist plug-in.
(defun isplugin (fname)
  (let ((fp (open fname))
        (teststring "nyquist plug-in"))
    ;First char may be #\; or #\$
    (setf b (read-byte fp))
    (cond
      ((and (/= b (char-code #\;))(/= b (char-code #\$)))
        (close fp)
        nil)
      ((do* ((i 0 (1+ i))
             (b (read-byte fp) (read-byte fp))
             (test (char-code (char teststring i))
                   (char-code (char teststring i))))
            ((= i (1- (length teststring))) t)
          (when (/= b test)
            (return)))
        (close fp)
        t)
      (t
        (close fp)
        nil))))

;If string ends in ".ny.txt", replace with ".ny"
(defun fix-ext (fname)
  (setf ln (length fname))
  (if (and (> ln 7)
           (string-equal (subseq fname (- ln 7) ln) ".ny.txt"))
    (subseq fname 0 (- ln 4))
    fname))

(defun install (fname)
  (setf out-path (get '*system-dir* 'user-plug-in))
  (setf short-name (get-file-name fname))
  (cond
    ((not (existsp fname))
      (format nil (_ "Error.~%~s not found or cannot be read.~%") short-name))
    ((not (isplugin fname))
      (format nil (_ "Error.~%~s is not a supported plug-in.~%") short-name))
    (t
      (setf short-name (fix-ext short-name))
      (setf out-fname
          (format nil "~a~a~a" out-path *file-separator* short-name))
      (cond
        ((string-not-equal short-name ".ny" :start1 (- (length short-name) 3))
            (format nil (_ "Error.~%~s is not a valid Nyquist plug-in.~%") short-name))
        ((and (existsp out-fname) (= overwrite 0))
            (format nil (_ "Error.~%~s is already installed.~%") short-name))
        ((not (writeablep out-fname))
            (format nil (_ "Error.~%~s cannot be written.~%") out-fname))
        (t
            (copy-file fname out-fname)
        (format nil (_ "~s installed to:~%~s~%~%~
            Use the Plug-in Manager to enable the effect.")
            short-name out-fname))))))


(cond
  ((or (not (boundp 'plug-in))(not (audacity-version-ok '(2 3 1))))
      (_ "This plug-in requires Audacity 2.3.1 or later."))
  ((not (isfilename plug-in))
      (_ "Error.\nNo file selected."))
  (t  (install plug-in)))
