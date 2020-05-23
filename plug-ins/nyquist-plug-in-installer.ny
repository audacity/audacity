$nyquist plug-in
$version 4
$type tool
$name (_ "Nyquist Plug-in Installer")
$manpage "Nyquist_Plug-in_Installer"
$debugbutton false
$preview disabled
$author "Steve Daulton"
$release 2.4.0
$copyright (_ "Released under terms of the GNU General Public License version 2")

;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference


;i18n-hint: "Browse..." is text on a button that launches a file browser.
$control files (_ "Select file(s) to install") file (_ "Browse...") "~/Desktop/" (((_ "Plug-in") (ny NY))
										  ((_ "Lisp file") (lsp LSP))
										  ((_ "HTML file") (htm HTM html HTML))
										  ((_ "Text file") (txt TXT))
										  ((_ "All supported") (ny NY lsp LSP htm HTM html HTML txt TXT))
										  ((_ "All files") (""))) "open,exists,multiple"
$control overwrite (_ "Allow overwriting") choice ((_ "Disallow") (_ "Allow")) 0


(defun audacity-version-ok (min-version)
  ;; No longer required as this plug-in is shipped with Audacity.
  ;; Left in for illustration purposes.
  ;; min-version is a list of three numbers (the minimum Audacity version number).
  ;; Example, if the minimum version required is Audacity 2.4.0, then
  ;; call (audacity-version-ok '(2 4 0))
  ;; Treturns t if plug-in is running on 2.4.0 or later, otherwise nil.
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

(defun get-file-name (fqname &aux (fname ""))
  ;; Return file name . extension from fully qualified file name.
  (dotimes (i (length fqname) fname)
    (if (char= (char fqname i) *file-separator*)
        (setf fname "")
        (setf fname (format nil "~a~a" fname (char fqname i))))))

(defun isfilename (fname)
  ;; Return t if fname looks like valid file name, else nil.
  (let ((ln (length fname)))
    (cond
      ((= ln 0) nil)
      ((char= (char fname (- ln 1)) *file-separator*) nil)
      (t t))))

(defun existsp (fname)
  ;; Return t if file exists, else nil.
  (let ((fp (open fname)))
    (cond
      (fp (close fp)
          ;overwrite: 0=disallow, 1=allow, 2=is overwriting.
          (when (= overwrite 1)
            (setf overwrite 2))
          t)
      (t nil))))

(defun writeablep (fname)
  ;; Return t if file is writeable.
  (let ((fp (open fname :direction :output)))
    (cond
      (fp (close fp) t)
      (t nil))))

(defun copy-file (input output)
  ;; Copy from input file to output file.
  (let ((ifp (open input :direction :input))
        (ofp (open output :direction :output)))
    (do ((line (read-line ifp)(read-line ifp)))
        ((not line))
      (format ofp "~a~%" line))
    (close ifp)
    (close ofp)))

(defun issupported (fname)
  ;; Return true if it looks like a supported file.
  ;; For .lsp and .html files, we only check the file extension.
  ;; For .ny files, we have additional sanity checks that it is a
  ;; plug-in and not just a Nyquist Prompt script.
  (let ((goodfname (fix-ext fname)))
    (cond
      ((check-ext goodfname ".lsp") t)
      ((check-ext goodfname ".htm") t)
      ((check-ext goodfname ".html") t)
      ((check-ext goodfname ".txt") t)
      ((not (check-ext goodfname ".ny")) nil)
      ((has-plugin-header fname) t)
      (t nil))))

(defun check-ext (fname ext)
  ;; Return true if fname has extension ext.
  (let* ((fnameln (length fname))
         (extln (length ext))
         (restln (- fnameln extln)))
    (cond
      ((< fnameln (1+ extln)) nil)  ;too short to be valid
      ((string-equal (subseq fname restln fnameln) ext) t)
      (t nil))))

(defun fix-ext (fname)
  ;; If string ends in ".ny.txt" or ".lsp.txt", strip off ".txt"
  (macrolet ((striptxt (fname) `(setf ,fname (subseq ,fname 0 (- ln 4)))))
    (let ((ln (length fname)))
      (cond
        ((and (> ln 8) (string-equal (subseq fname (- ln 8) ln) ".lsp.txt"))
          (striptxt fname))
        ((and (> ln 7) (string-equal (subseq fname (- ln 7) ln) ".ny.txt"))
          (striptxt fname)))
      fname)))

(defun has-plugin-header (fname)
  ;; Return t if file looks like valid Nyquist plug-in, else nil.
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

(defun get-file-list (file-string)
  ;; See https://wiki.audacityteam.org/wiki/Nyquist_File-Button_Tutorial#Open_Multiple_Files
  (let ((path-string (format nil "(list ~s )" (string-trim "\"" file-string))))
    (eval-string path-string)))

(defun install (fname)
  ;; Install file fname (fully qualified file name).
  ;; Push result to list install-success or install-fail.
  (setf out-path (get '*system-dir* 'user-plug-in))
  (setf short-name (get-file-name fname))
  (cond
    ((not (existsp fname))
      (push (list 3 fname) install-fail))
    ((not (issupported fname))
      (push (list 4 fname) install-fail))
    (t
      (setf short-name (fix-ext short-name))
      (setf out-fname
          (format nil "~a~a~a" out-path *file-separator* short-name))
      (cond
        ;; Check for fails
        ((and (existsp out-fname) (= overwrite 0))
          (push (list 5 short-name) install-fail))
        ((not (writeablep out-fname))
          (push (list 6 short-name) install-fail))
        ;; Now the successes
        ((check-ext short-name ".ny")
            (copy-file fname out-fname)
            (if (= overwrite 2)
                (push (list 1 short-name) install-success)
                (push (list 0 short-name) install-success)))
        (t  (copy-file fname out-fname)
            (push (list 2 short-name) install-success))))))

(defun print-results (&aux msg results)
  ;; Format results and display in human readable form.
  (cond
    ((isempty install-success)
      (setf msg (_ "Error.\n")))
    ((isempty install-fail)
      (setf msg (format nil (_ "Success.~%Files written to:~%~s~%")
                        (get '*system-dir* 'user-plug-in))))
    (t (setf msg (_ "Warning.\nFailed to copy some files:\n"))))
  (setf results (append install-success install-fail))
  (setf results (sort-results results))
  (let ((status -1))
    (dolist (m results msg)
      (when (/= (first m) status)
        (setf msg (format nil "~a~%~a~%" msg (status (first m))))
        (setf status (first m)))
      (setf msg (format nil "~a~a~%" msg (second m))))))

(defun isempty (x)
  ;;Return t if x is an empty list.
  (unless (listp x)
    (error "Not a list" x))
  (if (= (length x) 0) t nil))

(defun isnotempty (x)
  (not (isempty x)))

(defun status (num)
  ;; Return status message corresponding to the installation status number.
  ;; This allows result messages to be grouped according to installation status.
  (case num
    ;; Success
    (0 (_ "Plug-ins installed.\n(Use the Plug-in Manager to enable effects):"))
    (1 (_ "Plug-ins updated:"))
    (2 (_ "Files copied to plug-ins folder:"))
    ;; Fail
    (3 (_ "Not found or cannot be read:"))
    (4 (_ "Unsupported file type:"))
    (5 (_ "Files already installed ('Allow Overwriting' disabled):"))
    (6 (_ "Cannot be written to plug-ins folder:"))))

(defun sort-results (results)
  ;; 'results' are either 'install-success' or 'install-fail'.
  ;; Each item in results is (list status file-name).
  ;; Returns 'results' sorted by status number.
  (sort results #'(lambda (x y) (< (car x) (car y)))))

;; Global lists
(setf install-success ())
(setf install-fail ())

(let ((files (get-file-list files)))
  (if (= (length files) 0)
      (format nil (_ "Error.~%No file selected."))
      (dolist (file files (print-results))
        (install file))))
