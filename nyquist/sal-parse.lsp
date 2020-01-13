;; SAL parser -- replaces original pattern-directed parser with
;;    a recursive descent one
;;
;; Parse functions either parse correctly and return
;; compiled code as a lisp expression (which could be nil)
;; or else they call parse-error, which does not return
;; (instead, parse-error forces a return from parse)
;; In the original SAL parser, triples were returned
;; including the remainder if any of the tokens to be
;; parsed. In this parser, tokens are on the list
;; *sal-tokens*, and whatever remains on the list is
;; the list of unparsed tokens.

;; scanning delimiters.

(setfn nreverse reverse)

(defconstant +quote+ #\")                ; "..." string 
(defconstant +kwote+ #\')                ; '...' kwoted expr
(defconstant +comma+ #\,)                ; positional arg delimiter
(defconstant +pound+ #\#)                ; for bools etc
(defconstant +semic+ #\;)                ; comment char
(defconstant +lbrace+ #\{)               ; {} list notation 
(defconstant +rbrace+ #\})
(defconstant +lbrack+ #\[)               ; unused for now
(defconstant +rbrack+ #\])
(defconstant +lparen+ #\()               ; () expr and arg grouping
(defconstant +rparen+ #\))

;; these are defined so that SAL programs can name these symbols
;; note that quote(>) doesn't work, so you need quote(symbol:greater)

(setf symbol:greater '>)
(setf symbol:less '<)
(setf symbol:greater-equal '>=)
(setf symbol:less-equal '<=)
(setf symbol:equal '=)
(setf symbol:not '!)
(setf symbol:not-equal '/=)


(defparameter +whites+ (list #\space #\tab #\newline (code-char 13)))

(defparameter +kwstyle+ (list :suffix #\:)) ; let's try dylan

(defparameter +operators+
  ;; each op is: (<token-class> <sal-name> <lisp-form>)
  '((:+ "+" sal-plus)
    (:- "-" diff)
    (:* "*" mult)
    (:/ "/" /)
    (:% "%" rem)
    (:^ "^" expt)
    (:= "=" sal-equal)   ; equality and assigment
    (:!= "!=" not-sal-equal)
    (:< "<" <)
    (:> ">" >)
    (:<= "<=" <=) ; leq and assignment minimization
    (:>= ">=" >=) ; geq and assignment maximization
    (:~= "~=" sal-about-equal) ; general equality
    (:+= "+=" +=) ; assignment increment-and-store
    (:-= "-=" -=) ; assignment increment-and-store
    (:*= "*=" *=) ; assignment multiply-and-store
    (:/= "/=" /=) ; assignment multiply-and-store
    (:&= "&=" &=) ; assigment list collecting
    (:@= "@=" @=) ; assigment list prepending
    (:^= "^=" ^=) ; assigment list appending
    (:! "!" not)
    (:& "&" and)
    (:\| "|" or)
    (:~ "~" sal-stretch)
    (:~~ "~~" sal-stretch-abs)
    (:@ "@" sal-at)
    (:@@ "@@" sal-at-abs)
    ))

(setf *sal-local-variables* nil) ;; used to avoid warning about variable
 ;; names when the variable has been declared as a local

(defparameter *sal-operators*
  '(:+ :- :* :/ :% :^ := :!= :< :> :<= :>= :~= :+= :*= :&= :@= :^= :! :& :\|
    :~ :~~ :@ :@@))

(defparameter +delimiters+
  '((:lp #\()
    (:rp #\))
    (:lc #\{)                                ; left curly
    (:rc #\})
    (:lb #\[)
    (:rb #\])
    (:co #\,)
    (:kw #\')                                ; kwote
    (nil #\")                                ; not token
   ; (nil #\#)
    (nil #\;)
    ))

(setf *reserved-words* '((::+ ":+") (::- ":-") (::* ":*") (::/ ":/") (::= ":=")
                         (::!= ":!=") (::< ":<") (::> ":>") (::<= ":<=")
                         (::>= ":>=") (::~= ":~=") (::! ":!") (::& ":&")
                         (::\| ":|") (:IF "if") (:THEN "then") (:ELSE "else")
                         (:WHEN "when") (:UNLESS "unless") (:SET "set")
                         (:= "=") (:+= "+=") (:*= "*=") (:&= "&=") (:@= "@=")
                         (:^= "^=") (:<= "<=") (:>= ">=") (:PRINT "print")
                         (:LOOP "loop")
                         (:RUN "run") (:REPEAT "repeat") (:FOR "for")
                         (:FROM "from") (:IN "in") (:BELOW "below") (:TO "to")
                         (:ABOVE "above") (:DOWNTO "downto") (:BY "by")
                         (:OVER "over") (:WHILE "while") (:UNTIL "until")
                         (:FINALLY "finally") (:RETURN "return")
                         (:WAIT "wait") (:BEGIN "begin") (:WITH "with")
                         (:END "end") (:VARIABLE "variable")
                         (:FUNCTION "function") (:PROCESS "process")
                         (:CHDIR "chdir") (:DEFINE "define") (:LOAD "load")
                         (:PLAY "play") (:PLOT "plot")
                         (:EXEC "exec") (:exit "exit") (:DISPLAY "display")
                         (:~ "~") (:~~ "~~") (:@ ":@") (:@@ ":@@")))


(setf *sal-fn-name* nil)

(defun make-sal-error (&key type text (line nil) start)
  ; (error 'make-sal-error-was-called-break)
  (list 'sal-error type text line start))
(setfn sal-error-type cadr)
(setfn sal-error-text caddr)
(setfn sal-error-line cadddr)
(defun sal-error-start (x) (cadddr (cdr x)))
(defun is-sal-error (x) (and x (eq (car x) 'sal-error)))
(defun sal-tokens-error-start (start)
  (cond (start 
         start)
        (*sal-tokens*
         (token-start (car *sal-tokens*)))
        (t
         (length *sal-input-text*))))


(defmacro errexit (message &optional start)
  `(parse-error (make-sal-error :type "parse"
                 :line *sal-input-text* :text ,message
                 :start ,(sal-tokens-error-start start))))

(defmacro sal-warning (message &optional start)
  `(pperror (make-sal-error :type "parse" :line *sal-input-text*
                            :text ,message
                            :start ,(sal-tokens-error-start start))
            "warning"))

(setf *pos-to-line-source* nil)
(setf *pos-to-line-pos* nil)
(setf *pos-to-line-line* nil)

(defun pos-to-line (pos source)
  ;; this is really inefficient to search every line from
  ;; the beginning, so cache results and search forward
  ;; from there if possible
  (let ((i 0) (line-no 1)) ;; assume no cache
    ;; see if we can use the cache
    (cond ((and (eq source *pos-to-line-source*)
                *pos-to-line-pos* *pos-to-line-line*
                (>= pos *pos-to-line-pos*))
           (setf i *pos-to-line-pos*)
           (setf line-no *pos-to-line-line*)))
    ;; count newlines up to pos
    (while (< i pos)
      (if (char= (char source i) #\newline)
          (incf line-no))
      (setf i (1+ i)))
    ;; save results in cache
    (setf *pos-to-line-source* source
          *pos-to-line-pos* pos
          *pos-to-line-line* line-no)
    ;; return the line number at pos in source
    line-no))


;; makes a string of n spaces, empty string if n <= 0
(defun make-spaces (n)
  (cond ((> n 16)
         (let* ((half (/ n 2))
                (s (make-spaces half)))
           (strcat s s (make-spaces (- n half half)))))
        (t
         (subseq "                " 0 (max n 0)))))


(defun pperror (x &optional (msg-type "error"))
  (let* ((source (sal-error-line x))
         (llen (length source))
         line-no
         beg end)
    ; (display "pperror" x (strcat "|" (sal-error-line x) "|"))
    ;; isolate line containing error
    (setf beg (sal-error-start x))
    (setf beg (min beg (1- llen)))
    (do ((i beg (- i 1))
         (n nil)) ; n gets set when we find a newline
        ((or (< i 0) n)
         (setq beg (or n 0)))
      (if (char= (char source i) #\newline)
          (setq n (+ i 1))))
    (do ((i (sal-error-start x) (+ i 1))
         (n nil))
        ((or (>= i llen) n)
         (setq end (or n llen)))
      (if (char= (char source i) #\newline)
          (setq n i)))
    (setf line-no (pos-to-line beg source))
    ; (display "pperror" beg end (sal-error-start x))
      
    ;; print the error. include the specfic line of input containing
    ;; the error as well as a line below it marking the error position
    ;; with an arrow: ^
    (let* ((pos (- (sal-error-start x) beg))
           (line (if (and (= beg 0) (= end llen)) 
                     source
                     (subseq source beg end)))
           (mark (make-spaces pos)))
      (format t "~%>>> ~A ~A: ~A.~%>>> in ~A, line ~A, col ~A.~%~%~A~%~A^~%"
              (sal-error-type x) msg-type (sal-error-text x)
              *sal-input-file-name* line-no (1+ pos)
              line mark)
;      (format t "~%>>> ~A error in \"~A\", line ~A, col ~A: ~A.~%~%~A~%~A^~%" 
;              (sal-error-type x) *sal-input-file-name* line-no pos
;              (sal-error-text x) line mark)
      x)))


;;;
;;; the lexer. right now it assumes input string is complete and ready
;;; to be processed as a valid expression.
;;;

(defun advance-white (str white start end)
  ;; skip "white" chars, where white can be a char, list of chars
  ;; or predicate test
  (do ((i start )
       (p nil))
      ((or p (if (< start end)
                 (not (< -1 i end))
                 (not (> i end -1))))
       (or p end))
    (cond ((consp white)
           (unless (member (char str i) white :test #'char=)
             (setq p i)))
          ((characterp white)
           (unless (char= (char str i) white)
             (setq p i)))
          ((functionp white)
           (unless (funcall white (char str i))
             (setq p i))))
    (if (< start end)
        (incf i)
        (decf i))))


(defun search-delim (str delim start end)
  ;; find position of "delim" chars, where delim can be
  ;; a char, list of chars or predicate test
  (do ((i start (+ i 1))
       (p nil))
      ((or (not (< i end)) p)
       (or p end))
    (cond ((consp delim)
           (if (member (char str i) delim :test #'char=)
               (setq p i)))
          ((characterp delim)
           (if (char= (char str i) delim)
               (setq p i)))
          ((functionp delim)
           (if (funcall delim (char str i))
               (setq p i))))))


;; UNBALANCED-INPUT AND TOKENIZE HAVE BEEN REWRITTEN, SEE BELOW. THIS ONE IS 
;; OLD AND JUST KEPT HERE FOR REFERENCE
#|
(defun unbalanced-input (errf line toks par bra brk kwo)
  ;; search input for the starting position of some unbalanced
  ;; delimiter, toks is reversed list of tokens with something
  ;; unbalanced
  (let (char text targ othr levl pos)
    (cond ((> par 0) (setq char #\( targ ':lp othr ':rp levl par))
          ((< par 0) (setq char #\) targ ':rp othr ':lp levl 0))
          ((> bra 0) (setq char #\{ targ ':lc othr ':rc levl bra))
          ((< bra 0) (setq char #\} targ ':rc othr ':lc levl 0))
          ((> brk 0) (setq char #\[ targ ':ls othr ':rs levl brk))
          ((< brk 0) (setq char #\] targ ':rs othr ':ls levl 0))
          ((> kwo 0) (setq char #\' targ ':kw othr ':kw levl kwo)))
    (setq text (format nil "Unmatched '~A'" char))
    ;; search for start of error in token list
    (do ((n levl)
         (tail toks (cdr tail)))
        ((or (null tail) pos)
         (or pos (error (format nil "Shouldn't! can't find op ~A in ~A."
                                 targ (reverse toks)))))
      (if (eql (token-type (car tail)) targ)
          (if (= n levl)
              (setq pos (token-start (car tail)))
              (decf n))
          (if (eql (token-type (car tail)) othr)
              (incf n))))    
    (errexit text pos)))

;; REMINDER: THIS IS PART OF A BIG BLOCK COMMENT
(defun tokenize (str reserved error-fn)
  ;&key (start 0) (end (length str)) 
  ;                 (white-space +whites+) (delimiters +delimiters+)
  ;                 (operators +operators+) (null-ok t)
  ;              (keyword-style +kwstyle+) (reserved nil) 
  ;                 (error-fn nil)
  ;                 &allow-other-keys)
  ;; return zero or more tokens or a sal-error
  (let ((toks (list t))
        (start 0)
        (end (length str))
        (all-delimiters +whites+)
        (errf (or error-fn 
                  (lambda (x) (pperror x) (return-from tokenize x)))))
    (dolist (x +delimiters+)
      (push (cadr x) all-delimiters))
    (do ((beg start)
             (pos nil)
         (all all-delimiters)
         (par 0)
         (bra 0)
         (brk 0)
         (kwo 0)
         (tok nil)
         (tail toks))
        ((not (< beg end))
             ;; since input is complete check parens levels.
         (if (= 0 par bra brk kwo)
                 (if (null (cdr toks))
                 (list)
                 (cdr toks))
         (unbalanced-input errf str (reverse (cdr toks)) 
                                       par bra brk kwo)))
      (setq beg (advance-white str +whites+ beg end))
      (setf tok
        (read-delimited str :start beg :end end 
                        :white +whites+ :delimit all
                        :skip-initial-white nil :errorf errf))
      ;; multiple values are returned, so split them here:
      (setf pos (second tok)) ; pos is the end of the token (!)
      (setf tok (first tok))

      ;; tok now string, char (delimiter), :eof or token since input
      ;; is complete keep track of balancing delims
      (cond ((eql tok +lbrace+) (incf bra))
            ((eql tok +rbrace+) (decf bra))
            ((eql tok +lparen+) (incf par))
            ((eql tok +rparen+) (decf par))
            ((eql tok +lbrack+) (incf brk))
            ((eql tok +rbrack+) (decf brk))
            ((eql tok +kwote+) (setq kwo (mod (+ kwo 1) 2))))
      (cond ((eql tok ':eof)
             (setq beg end))
            
            (t
             ;; may have to skip over comments to reach token, so
             ;; token beginning is computed by backing up from current
             ;; position (returned by read-delimited) by string length
             (setf beg (if (stringp tok)
                           (- pos (length tok))
                           (1- pos)))
             (setq tok (classify-token tok beg str errf
                                       +delimiters+ +operators+
                                       +kwstyle+ reserved))
             ;(display "classify-token-result" tok)
             (setf (cdr tail) (list tok ))
             (setf tail (cdr tail))
             (setq beg pos))))))
|#


;; old tokenize (above) counted delimiters to check for balance,
;; but that does not catch constructions like ({)}. I think
;; we could just leave this up to the parser, but this rewrite
;; uses a stack to check balanced parens, braces, quotes, etc.
;; The checking establishes at least some minimal global properties
;; of the input before evaluating anything, which might be good
;; even though it's doing some extra work. In fact, using a
;; stack rather than counts is doing even more work, but the
;; problem with counters is that some very misleading or just
;; plain wrong error messages got generated.
;;
;; these five delimiter- functions do checks on balanced parens,
;; braces, and brackets, leaving delimiter-mismatch set to bad
;; token if there is a mismatch
(defun delimiter-init ()
  (setf delimiter-stack nil)
  (setf delimiter-mismatch nil))
(defun delimiter-match (tok what)
  (cond ((eql (token-string (first delimiter-stack)) what)
         (pop delimiter-stack))
        ((null delimiter-mismatch)
         ;(display "delimiter-mismatch" tok)
         (setf delimiter-mismatch tok))))
(defun delimiter-check (tok)
  (let ((c (token-string tok)))
    (cond ((member c '(#\( #\{ #\[))
           (push tok delimiter-stack))
          ((eql c +rbrace+)
           (delimiter-match tok +lbrace+))
          ((eql c +rparen+)
           (delimiter-match tok +lparen+))
          ((eql c +rbrack+)
           (delimiter-match tok +lbrack+)))))
(defun delimiter-error (tok)
  (errexit (format nil "Unmatched '~A'" (token-string tok))
           (token-start tok)))
(defun delimiter-finish ()
  (if delimiter-mismatch
      (delimiter-error delimiter-mismatch))
  (if delimiter-stack
      (delimiter-error (car delimiter-stack))))
(defun tokenize (str reserved error-fn)
  ;; return zero or more tokens or a sal-error
  (let ((toks (list t))
        (start 0)
        (end (length str))
        (all-delimiters +whites+)
        (errf (or error-fn 
                  (lambda (x) (pperror x) (return-from tokenize x)))))
    (dolist (x +delimiters+)
      (push (cadr x) all-delimiters))
    (delimiter-init)
    (do ((beg start)
         (pos nil)
         (all all-delimiters)
         (tok nil)
         (tail toks))
        ((not (< beg end))
         ;; since input is complete check parens levels.
         (delimiter-finish)
         (if (null (cdr toks)) nil (cdr toks)))
      (setq beg (advance-white str +whites+ beg end))
      (setf tok
        (read-delimited str :start beg :end end 
                        :white +whites+ :delimit all
                        :skip-initial-white nil :errorf errf))
      ;; multiple values are returned, so split them here:
      (setf pos (second tok)) ; pos is the end of the token (!)
      (setf tok (first tok))

      (cond ((eql tok ':eof)
             (setq beg end))
            (t
             ;; may have to skip over comments to reach token, so
             ;; token beginning is computed by backing up from current
             ;; position (returned by read-delimited) by string length
             (setf beg (if (stringp tok)
                           (- pos (length tok))
                           (1- pos)))
             (setq tok (classify-token tok beg str errf
                                       +delimiters+ +operators+
                                       +kwstyle+ reserved))
             (delimiter-check tok)
             ;(display "classify-token-result" tok)
             (setf (cdr tail) (list tok ))
             (setf tail (cdr tail))
             (setq beg pos))))))


(defun read-delimited (input &key (start 0) end (null-ok t)
                       (delimit +delims+) ; includes whites...
                       (white +whites+)
                       (skip-initial-white t)
                       (errorf #'pperror))
  ;; read a substring from input, optionally skipping any white chars
  ;; first. reading a comment delim equals end-of-line, input delim
  ;; reads whole input, pound reads next token. call errf if error
  ;(FORMAT T "~%READ-DELIMITED: ~S :START ~S :END ~S" input start end)
  (let ((len (or end (length input))))
    (while t ;; loop over comment lines
      (when skip-initial-white
        (setq start (advance-white input white start len)))
        (if (< start len)
          (let ((char (char input start)))
            (setq end (search-delim input delimit start len))
            (if (equal start end)                ; have a delimiter
               (cond ((char= char +semic+)
                      ;; comment skips to next line and trys again...
                      (while (and (< start len)
                                  (char/= (char input start) #\newline))
                        (incf start))
                      (cond ((< start len) ;; advance past comment and iterate
                             (incf start)
                             (setf skip-initial-white t))
                            (null-ok
                             (return (list ':eof end)))
                            (t
                             (errexit "Unexpected end of input"))))
;                     ((char= char +pound+)
;                      ;; read # dispatch
;                      (read-hash input delimit start len errorf))
                     ((char= char +quote+)
                      ;; input delim reads whole input
                      (return (sal:read-string input delimit start len errorf)))
                     ((char= char +kwote+)
                      (errexit "Illegal delimiter" start))
                     (t ;; all other delimiters are tokens in and of themselves
                      (return (list char (+ start 1)))))
            ; else part of (equal start end), so we have token before delimiter
              (return (list (subseq input start end) end))))
        ; else part of (< start len)...
          (if null-ok 
              (return (list ':eof end))
              (errexit "Unexpected end of input" start))))))


(defparameter hash-readers 
  '(( #\t sal:read-bool)
    ( #\f sal:read-bool)
    ( #\? read-iftok)
    ))


(defun read-hash (str delims pos len errf)
  (let ((e (+ pos 1)))
    (if (< e len)
        (let ((a (assoc (char str e) hash-readers)))
          (if (not a)
              (errexit "Illegal # character" e)
              (funcall (cadr a) str delims e len errf)))
        (errexit "Missing # character" pos))))


(defun read-iftok (str delims pos len errf)
  str delims len errf
  (list (make-token :type ':? :string "#?" :lisp 'if
                         :start (- pos 1))
        (+ pos 1)))

; (sal:read-string str start len)

(defun sal:read-bool (str delims pos len errf)
  delims len errf
  (let ((end (search-delim str delims pos len)))
    (unless (= end (+ pos 1))
      (errexit "Illegal # expression" (- pos 1)))
    (list (let ((t? (char= (char str pos) #\t) ))
            (make-token :type ':bool 
                           :string (if t? "#t" "#f")
                           :lisp t?
                           :start (- pos 1)))
          (+ pos 1))))


(defun sal:read-string (str delims pos len errf)
  (let* ((i (1+ pos)) ; i is index into string; start after open quote
         c c2; c is the character at str[i]
         (string (make-string-output-stream)))
    ;; read string, processing escaped characters
    ;; write the chars to string until end quote is found
    ;; then retrieve the string. quotes are not included in result token

    ;; in the loop, i is the next character location to examine
    (while (and (< i len) 
                (not (char= (setf c (char str i)) +quote+)))
      (if (char= c #\\) ;; escape character, does another character follow this?
          (cond ((< (1+ i) len)
                 (incf i) ;; yes, set i so we'll get the escaped char
                 (setf c2 (char str i))
                 (setf c (assoc c2 `((#\n . #\newline) (#\t . #\tab) 
                                     (#\r . ,(char "\r" 0))
                                     (#\f . ,(char "\f" 0)))))
                 (setf c (if c (cdr c) c2))) ;; use c2 if c wasn't listed
                (t ;; no, we've hit the end of input too early
                 (errexit "Unmatched \"" i))))
      ;; we're good to take this character and move on to the next one
      (write-char c string)
      (incf i))
    ;; done with loop, so either we're out of string or we found end quote
    (if (>= i len) (errexit "Unmatched \"" i))
    ;; must have found the quote
    (setf string (get-output-stream-string string))
    (list (make-token :type :string :start pos :string string :lisp string)
          (1+ i))))

;;;
;;; tokens
;;;

(defun make-token (&key (type nil) (string "") start (info nil) lisp)
  (list :token type string start info lisp))
(setfn token-type cadr)
(setfn token-string caddr)
(defun token-start (x) (cadddr x))
(defun token-info (token) (car (cddddr token)))
(defun token-lisp (token) (cadr (cddddr token)))
(defmacro set-token-type (tok val) `(setf (car (cdr ,tok)) ,val))
(defmacro set-token-lisp (tok val) `(setf (car (cdr (cddddr ,tok))) ,val))
(defun tokenp (tok) (and (consp tok) (eq (car tok) :token)))

(defun token=? (tok op)
  (if (tokenp tok)
      (equal (token-type tok) op)
      (eql tok op)))

(defmethod token-print (obj stream)
  (let ((*print-case* ':downcase))
    (format stream "#<~s ~s>" 
            (token-type obj) 
            (token-string obj))))

(defun parse-token ()
  (prog1 (car *sal-tokens*)
         (setf *sal-tokens* (cdr *sal-tokens*))))

;;;
;;; token classification. types not disjoint!
;;;

(defun classify-token (str pos input errf delims ops kstyle res)
  (let ((tok nil))
    (cond ((characterp str)
           ;; normalize char delimiter tokens
           (setq tok (delimiter-token? str pos input errf delims)))
          ((stringp str)
           (setq tok (or (number-token? str pos input errf)
                         (operator-token? str pos input errf ops)
                         (keyword-token? str pos input errf kstyle)
                         (class-token? str pos input errf res)
                         (reserved-token? str pos input errf res)
                         (symbol-token? str pos input errf)
                         ))
           (unless tok
             (errexit "Not an expression or symbol" pos)))
          (t (setq tok str)))
    tok))


(defun delimiter-token? (str pos input errf delims)
  (let ((typ (member str delims :test (lambda (a b) (char= a (cadr b))))))
    ;; member returns remainder of the list
    ;(display "delimiter-token?" str delims typ)
    (if (and typ (car typ) (caar typ))
        (make-token :type (caar typ) :string str
                       :start pos)
        (+ (break) (errexit "Shouldn't: non-token delimiter" pos)))))


(defun string-to-number (s)
  (read (make-string-input-stream s)))


(defun number-token? (str pos input errf)
  errf input
  (do ((i 0 (+ i 1))
       (len (length str))
       (c nil)
       (dot 0)
       (typ ':int)
       (sig 0)
       (sla 0)
       (dig 0)
       (non nil))
      ((or (not (< i len)) non)
       (if non nil
           (if (> dig 0) 
               (make-token :type typ :string str
                              :start pos :lisp (string-to-number str))
               nil)))
    (setq c (char str i))
    (cond ((member c '(#\+ #\-))
           (if (> i 0) (setq non t)
               (incf sig)))
          ((char= c #\.)
           (if (> dot 0) (setq non t)
               (if (> sla 0) (setq non t)
                   (incf dot))))
; xlisp does not have ratios
;          ((char= c #\/)
;           (setq typ ':ratio)
;           (if (> sla 0) (setq non t)
;               (if (= dig 0) (setq non t)
;                   (if (> dot 0) (setq non t)
;                       (if (= i (1- len)) (setq non t)
;                           (incf sla))))))
          ((digit-char-p c)
           (incf dig)
           (if (> dot 0) (setq typ ':float)))
          (t (setq non t)))))

#||
(number-token? "" 0 "" #'pperror)
(number-token? " " 0 "" #'pperror)
(number-token? "a"  0 "" #'pperror)
(number-token? "1" 0 "" #'pperror)
(number-token? "+" 0 "" #'pperror)
(number-token? "-1/2" 0 "" #'pperror)
(number-token? "1." 0 "" #'pperror)
(number-token? "1.." 0 "" #'pperror)
(number-token? ".1." 0 "" #'pperror)
(number-token? ".1" 0 "" #'pperror)
(number-token? "-0.1" 0 "" #'pperror)
(number-token? "1/2" 0 "" #'pperror)
(number-token? "1//2" 0 "" #'pperror)
(number-token? "/12" 0 "" #'pperror)
(number-token? "12/" 0 "" #'pperror)
(number-token? "12/1" 0 "" #'pperror)
(number-token? "12./1" 0 "" #'pperror)
(number-token? "12/.1" 0 "" #'pperror)
||#

(defun operator-token? (str pos input errf ops)
  ;; tok can be string or char
  (let ((typ (member str ops :test (lambda (a b) (equal a (cadr b))))))
    (cond (typ 
           (setf typ (car typ)) ;; member returns remainder of list
           (make-token :type (car typ) :string str
                       :start pos :lisp (or (third typ)
                                            (read-from-string str)))))))

(defun str-to-keyword (str)
  (intern (strcat ":" (string-upcase str))))


(defun keyword-token? (tok pos input errf style)
  (let* ((tlen (length tok))
         (keys (cdr style))
         (klen (length keys)))
    (cond ((not (< klen tlen)) nil)
          ((eql (car style) ':prefix)
           (do ((i 0 (+ i 1))
                (x nil))
               ((or (not (< i klen)) x)
                (if (not x)
                    (let ((sym (symbol-token? (subseq tok i)
                                              pos input errf )))
                      (cond (sym
                             (set-token-type sym ':key)
                             (set-token-lisp sym
                                (str-to-keyword (token-string sym)))
                             sym)))
                    nil))
             (unless (char= (char tok i) (nth i keys))
               (setq x t))))
          ((eql (car style) ':suffix)
           (do ((j (- tlen klen) (+ j 1))
                (i 0 (+ i 1))
                (x nil))
               ((or (not (< i klen)) x)
                (if (not x)
                    (let ((sym (symbol-token? (subseq tok 0 (- tlen klen))
                                              pos input errf )))
                      (cond (sym
                             (set-token-type sym ':key)
                             (set-token-lisp sym
                                (str-to-keyword (token-string sym)))
                             sym)))
                    nil))
             (unless (char= (char tok j) (nth i keys))
               (setq x t)))))))


(setfn alpha-char-p both-case-p)


(defun class-token? (str pos input errf res)
  res
  (let ((a (char str 0)))
    (if (char= a #\<)
        (let* ((l (length str))
               (b (char str (- l 1))))
          (if (char= b #\>)
              (let ((tok (symbol-token? (subseq str 1 (- l 1))
                                        pos input errf)))
                ;; class token has <> removed!
                (if tok (progn (set-token-type tok ':class)
                               tok)
                    (errexit "Not a class identifer" pos)))
              (errexit "Not a class identifer" pos)))
        nil)))

; (keyword-token? ":asd" '(:prefix #\:))
; (keyword-token? "asd" KSTYLE)
; (keyword-token? "asd:"  KSTYLE)
; (keyword-token? "123:"  KSTYLE)
; (keyword-token? ":foo" '(:prefix #\:))
; (keyword-token? "foo=" '(:suffix #\=))
; (keyword-token? "--foo" '(:prefix #\- #\-))
; (keyword-token? ":123" '(:suffix #\:))
; (keyword-token? "--asd" '(:prefix #\-)) ; ok since -asd is legal symbol


;; determine if str is a reserved word using reserved as the list of
;; reserved words, of the form ((id string) (id string) ...) where
;; id identifies the token, e.g. :to and string is the token, e.g. "to"
;;
(defun reserved-token? (str pos input errf reserved)
  errf input
  (let ((typ (member str reserved :test 
                     (lambda (a b) (string-equal a (cadr b))))))
    (if typ 
        (make-token :type (caar typ) :string str
                       :start pos)
        nil)))


(defun sal-string-to-symbol (str)
  (let ((sym (intern (string-upcase str)))
        sal-sym)
    (cond ((and sym ;; (it might be "nil")
                (setf sal-sym (get sym :sal-name)))
           sal-sym)
          (t sym))))


(putprop 'simrep 'sal-simrep :sal-name)
(putprop 'seqrep 'sal-seqrep :sal-name)

(defun contains-op-char (s)
  ;; assume most identifiers are very short, so we search
  ;; over identifier letters, not over operator characters
  ;; Minus (-) is so common, we don't complain about it.
  (dotimes (i (length s))
    (if (string-search (subseq s i (1+ i)) "*/+=<>!%^&|")
        (return t))))

(defun test-for-suspicious-symbol (token)
  ;; assume token is of type :id
  (let ((sym (token-lisp token))
        (str (token-string token))
        (pos (token-start token)))
    (cond ((and sym  ; nil is not suspicious, but it's not "boundp"
                (not (fboundp sym)) ; existing functions not suspicious
                (not (boundp sym))  ; existing globals not suspicious
                (not (member sym *sal-local-variables*))
                (not (eq sym '->))  ; used by make-markov, so let it pass
                (contains-op-char str)) ; suspicious if embedded operators
           (sal-warning
             (strcat "Identifier contains operator character(s).\n"
                     "        Perhaps you omitted spaces around an operator")
             pos)))))


(defun symbol-token? (str pos input errf)
  ;; if a potential symbol is preceded by #, drop the #
  (if (and (> (length str) 1)
           (char= (char str 0) #\#))
      ;; there are a couple of special cases: SAL defines #f and #?
      (cond ((equal str "#f")
             (return-from symbol-token?
               (make-token :type ':id :string str :start pos :lisp nil)))
            ((equal str "#?")
             (return-from symbol-token?
               (make-token :type ':id :string str :start pos :lisp 'if)))
            (t
             (setf str (subseq str 1)))))
  ;; let's insist on at least one letter for sanity's sake
  ;; exception: allow '-> because it is used in markov pattern specs
  (do ((i 0 (+ i 1))  ; i is index into string
       (bad "Not an expression or symbol")
       (chr nil)
       (ltr 0)        ; ltr is count of alphabetic letters in string
       (dot nil)      ; dot is index of "."
       (pkg nil)      ; pkg is index if package name "xxx:" found
       (len (length str))
       (err nil))
      ;; loop ends when i is at end of string or when err is set
      ((or (not (< i len)) err)
       (if (or (> ltr 0) ; must be at least one letter, or
               (equal str "->")) ; symbol can be "->"
           (let ((info ()) sym)
             (if pkg (push (cons ':pkg pkg) info))
             (if dot (push (cons ':slot dot) info))
             ;(display "in symbol-token?" str)
             (setf sym (sal-string-to-symbol str))
             (make-token :type ':id :string str
                            :info info :start pos
                            :lisp sym))
           nil))
    (setq chr (char str i))
    (cond ((alpha-char-p chr) (incf ltr))
; need to allow arbitrary lisp symbols
;          ((member chr '(#\* #\+)) ;; special variable names can start/end 
;           (if (< 0 i (- len 2))   ;; with + or *
;               (errexit bad pos)))
          ((char= chr #\/) ;; embedded / is not allowed
           (errexit bad pos))
          ;((char= chr #\-) ;; hyphens are allowed anywhere in symbol
          ; (if (= ltr 0) 
          ;     (errexit errf input bad pos )
          ;     (setq ltr 0)
          ;     ))
          ((char= chr #\$) (incf ltr)) ;; "$" is treated as a letter
          ((char= chr #\:)
           ; allowable forms are :foo, foo:bar, :foo:bar
           (if (> i 0) ;; lisp keyword symbols ok
               (cond ((= ltr 0)
                      (errexit bad pos))
                     ((not pkg)
                      (setq pkg i))
                     (t (errexit errf input
                                 (format nil "Too many colons in ~s" str)
                                 pos))))
           (setq ltr 0))
          ((char= chr #\.)
           (if (or dot (= i 0) (= i (- len 1)))
               (errexit bad pos)
               (progn (setq dot i) (setq ltr 0)))))))


; (let ((i "foo")) (symbol-token? i 0 i #'pperror))
; (let ((i "foo..bar")) (symbol-token? i 0 i #'pperror))
; (let ((i ".bar")) (symbol-token?  i 0 i #'pperror))
; (let ((i "bar.")) (symbol-token?  i 0 i #'pperror))
; (let ((i "1...")) (symbol-token?  i 0 i #'pperror))
; (let ((i "a1..." )) (symbol-token? i 0 i #'pperror))
; (let ((i  "a{b")) (symbol-token? i 0 i #'pperror))
; (let ((i "foo-bar")) (symbol-token?  i 0 i #'pperror))
; (let ((i "123-a")) (symbol-token?  i 0 i #'pperror))
; (let ((i "1a23-a")) (symbol-token?  i 0 i #'pperror))
; (let ((i "*foo*")) (symbol-token?  i 0 i #'pperror))
; (let ((i "+foo+")) (symbol-token?  i 0 i #'pperror))
; (let ((i "foo+bar")) (symbol-token?  i 0 i #'pperror))
; (let ((i "foo/bar")) (symbol-token?  i 0 i #'pperror))

; (let ((i ":bar")) (symbol-token?  i 0 i #'pperror))
; (let ((i "::bar")) (symbol-token?  i 0 i #'pperror))
; (let ((i "foo:bar")) (symbol-token?  i 0 i #'pperror))
; (let ((i "cl-user:bar")) (symbol-token?  i 0 i #'pperror))
; (let ((i "cl-user::bar")) (symbol-token?  i 0 i #'pperror))
; (tokenize "aaa + bbb \"asdasdd\" aaa(1,2,3)")
; (tokenize "aaa+bbb \"asdasdd\" aaa(1,2,3)")


(setf *in-sal-parser* nil)

;; line number info for debugging
(setf *sal-line-number-info* t)
(setf *sal-line* 0)

(defun add-line-info-to-expression (expr token)
  (let (line-no)
    (cond ((and token ;; null token means do not change expr
                *sal-line-number-info* ;; is this feature enabled?
                (stringp *sal-input-text*))
           ;; first, get line number
           (setf line-no (pos-to-line (token-start token) *sal-input-text*))
           `(prog2 (setf *sal-line* ,line-no) ,expr))
          (t expr))))

;; single statement is handled just like an expression
(setfn add-line-info-to-stmt add-line-info-to-expression)

;; list of statements is simple to handle: prepend SETF
(defun add-line-info-to-stmts (stmts token)
  (let (line-no)
    (cond ((and *sal-line-number-info* ;; is this feature enabled?
                (stringp *sal-input-text*))
           (setf line-no (pos-to-line (token-start token) *sal-input-text*))
           (cons `(setf *sal-line* ,line-no) stmts))
          (t stmts))))


;; PARSE-ERROR -- print error message, return from top-level
;;
(defun parse-error (e)
  (unless (sal-error-line e)
    (setf (sal-error-line e) *sal-input*))
  (pperror e)
  (return-from sal-parse (values nil e *sal-tokens*)))


;; SAL-PARSE -- parse string or token input, translate to Lisp
;;
;; If input is text, *sal-input-text* is set to the text and
;;   read later (maybe) by ERREXIT. 
;; If input is a token list, it is assumed these are leftovers
;;   from tokenized text, so *sal-input-text* is already valid.
;; *Therefore*, do not call sal-parse with tokens unless 
;;   *sal-input-text* is set to the corresponding text.
;;
(defun sal-parse (grammar pat input multiple-statements file)
  (progv '(*sal-input-file-name*) (list file)
    (let (rslt expr rest)
      ; ignore grammar and pat (just there for compatibility)
      ; parse input and return lisp expression
      (cond ((stringp input)
             (setf *sal-input-text* input)
             (setq input (tokenize input *reserved-words* #'parse-error))))
      (setf *sal-input* input) ;; all input
      (setf *sal-tokens* input) ;; current input
      (cond ((null input)
             (values t nil nil)) ; e.g. comments compile to nil
            (t
             (setf rslt (or (maybe-parse-command)
                            (maybe-parse-block)
                            (maybe-parse-conditional)
                            (maybe-parse-assignment)
                            (maybe-parse-loop)
                            (maybe-parse-exec)
                            (maybe-parse-exit)
                            (errexit "Syntax error")))
             ;; note: there is a return-from parse in parse-error that
             ;; returns (values nil error <unparsed-tokens>)
             (cond ((and *sal-tokens* (not multiple-statements))
                    (errexit "leftover tokens")))
                    ;((null rslt)
                    ; (errexit "nothing to compile")))
             (values t rslt *sal-tokens*))))))


;; TOKEN-IS -- test if the type of next token matches expected type(s)
;;
;; type can be a list of possibilities or just a symbol
;; Usually, suspicious-id-warn is true by default, and any symbol
;; with embedded operator symbols, e.g. x+y results in a warning
;; that this is an odd variable name. But if the symbol is declared
;; as a local, a parameter, a function name, or a global variable,
;; then the warning is supressed.
;;
(defun token-is (type &optional (suspicious-id-warn t))
  (let ((token-type
         (if *sal-tokens* (token-type (car *sal-tokens*)) nil))
        rslt)
    ; input can be list of possible types or just a type:
    (setf rslt (or (and (listp type) 
                        (member token-type type))
                   (and (symbolp type) (eq token-type type))))
    ; test if symbol has embedded operator characters:
    (cond ((and rslt suspicious-id-warn (eq token-type :id))
           (test-for-suspicious-symbol (car *sal-tokens*))))
    rslt))


(defun maybe-parse-command ()
  (if (token-is '(:define :load :chdir :variable :function
                  ;  :system 
                  :play :print :display :plot))
      (parse-command)
      (if (and (token-is '(:return)) *audacity-top-level-return-flag*)
          (parse-command))))


(defun parse-command ()
  (cond ((token-is '(:define :variable :function))
         (parse-declaration))
        ((token-is :load)
         (parse-load))
        ((token-is :chdir)
         (parse-chdir))
        ((token-is :play)
         (parse-play))
;        ((token-is :system)
;         (parse-system))
        ((token-is :print)
         (parse-print-display :print 'sal-print))
        ((token-is :display)
         (parse-print-display :display 'display))
        ((token-is :plot)
         (parse-plot))
        ((and *audacity-top-level-return-flag* (token-is :return))
         (parse-return))
;        ((token-is :output)
;         (parse-output))
        (t
         (errexit "Command not found"))))


(defun parse-stmt ()
  (cond ((token-is :begin)
         (parse-block))
        ((token-is '(:if :when :unless))
         (parse-conditional))
        ((token-is :set)
         (parse-assignment))
        ((token-is :loop)
         (parse-loop))
        ((token-is :print)
         (parse-print-display :print 'sal-print))
        ((token-is :display)
         (parse-print-display :display 'display))
        ((token-is :plot)
         (parse-plot))
;        ((token-is :output)
;         (parse-output))
        ((token-is :exec)
         (parse-exec))
        ((token-is :exit)
         (parse-exit))
        ((token-is :return)
         (parse-return))
        ((token-is :load)
         (parse-load))
        ((token-is :chdir)
         (parse-chdir))
;        ((token-is :system)
;         (parse-system))
        ((token-is :play)
         (parse-play))
        (t
         (errexit "Command not found"))))
        

;; GET-PARM-NAMES -- given parms like (a b &key (x 1) (y 2)),
;;   return list of parameters: (a b x y)
(defun get-parm-names (parms)
  (let (rslt)
    (dolist (p parms)
      (cond ((symbolp p) 
             (if (eq p '&key) nil (push p rslt)))
            (t (push (car p) rslt))))
    (reverse rslt)))


;; RETURNIZE -- make a statement (list) end with a sal-return-from
;;
;;   SAL returns nil from begin-end statement lists
;;
(defun returnize (stmt)
  (let (rev)
    (setf rev (reverse stmt))
    (setf expr (car rev)) ; last expression in list
    (cond ((and (consp expr) (eq (car expr) 'sal-return-from))
           stmt) ; already ends in sal-return-from
          (t
           (reverse (cons (list 'sal-return-from *sal-fn-name* nil)
                          rev))))))


(defun parse-declaration ()
  (if (token-is :define) (parse-token)) ; SAL extension: "define" is optional
  (let (bindings setf-args formals parms stmt locals loc)
    (cond ((token-is :variable)
           (setf bindings (parse-bindings))
           (setf loc *rslt*) ; the "variable" token
           (dolist (b bindings)
             (cond ((symbolp b)
                    (push b setf-args)
                    (push `(if (boundp ',b) ,b) setf-args))
                   (t
                    (push (first b) setf-args)
                    (push (second b) setf-args))))
           (add-line-info-to-stmt (cons 'setf (reverse setf-args)) loc))
          ((token-is :function)
           (parse-token)
           (if (token-is :id nil)
               (setf *sal-fn-name* (token-lisp (parse-token)))
               (errexit "function name expected here"))
           (setf locals *sal-local-variables*)
           (setf formals (parse-parms))
           (setf stmt (parse-stmt))
           ;; stmt may contain a return-from, so make this a progn or prog*
           (cond ((and (consp stmt) 
                       (not (eq (car stmt) 'progn))
                       (not (eq (car stmt) 'prog*)))
                  (setf stmt (list 'progn stmt))))
           ;; need return to pop traceback stack
           (setf stmt (returnize stmt))
           ;; get list of parameter names
           (setf parms (get-parm-names formals))
           (setf *sal-local-variables* locals)
           ;; build the defun
           (prog1 (list 'defun *sal-fn-name* formals 
                        (list 'sal-trace-enter 
                              (list 'quote *sal-fn-name*) 
                              (cons 'list parms)
                              (list 'quote parms))
                        stmt)
                  (setf *sal-fn-name* nil)))
          (t
           (errexit "bad syntax")))))


(defun parse-one-parm (kargs)
  ;; kargs is a flag indicating previous parameter was a keyword (all
  ;;   the following parameters must then also be keyword parameters)
  ;; returns: (<keyword> <default>) or (nil <identifier>)
  ;;   where <keyword> is a keyward parameter name (nil if not a keyword parm)
  ;;         <default> is an expression for the default value
  ;;         <identifier> is the parameter name (if not a keyword parm)
  (let (key default-value id)
    (cond ((and kargs (token-is :id))
           (errexit "positional parameter not allowed after keyword parameter"))
          ((token-is :id)
           ;(display "parse-one-1" (token-is :id) (car *sal-tokens*))
           (setf id (token-lisp (parse-token)))
           (push id *sal-local-variables*)
           (list nil id))
          ((token-is :key)
           (setf key (sal-string-to-symbol (token-string (parse-token))))
           (cond ((or (token-is :co) (token-is :rp))) ; no default value
                 (t
                  (setf default-value (parse-sexpr))))
           (list key default-value)) 
          (kargs
           (errexit "expected keyword name"))
          (t
           (errexit "expected parameter name")))))


(defun parse-parms ()
  ;(display "parse-parms" *sal-tokens*)
  (let (parms parm kargs expecting)
    (if (token-is :lp)
        (parse-token) ;; eat the left paren
        (errexit "expected left parenthesis"))
    (setf expecting (not (token-is :rp)))
    (while expecting
      (setf parm (parse-one-parm kargs))
      ;(display "parm" parm)
      ;; returns list of (kargs . parm)
      (if (and (car parm) (not kargs)) ; kargs just set
          (push '&key parms))
      (setf kargs (car parm))
      ;; normally push the <id>; for keyword parms, push id and default value
      (push (if kargs parm (cadr parm)) parms)
      (if (token-is :co)
          (parse-token)
          (setf expecting nil)))
    (if (token-is :rp) (parse-token)
        (errexit "expected right parenthesis"))
    ;(display "parse-parms" (reverse parms))
    (reverse parms)))


(defun parse-bindings ()
  (let (bindings bind)
    (setf *rslt* (parse-token)) ; skip "variable" or "with"
      ; return token as "extra" return value
    (setf bind (parse-bind))
    (push (if (second bind) bind (car bind))
          bindings)
    (while (token-is :co)
      (parse-token)
      (setf bind (parse-bind))
      ;; if non-nil initializer, push (id expr)
      (push (if (second bind) bind (car bind))
            bindings))
    (reverse bindings)))


(defun parse-bind ()
  (let (id val)
    (if (token-is :id nil)
        (setf id (token-lisp (parse-token)))
        (errexit "expected a variable name"))
    (cond ((token-is :=)
           (parse-token)
           (setf val (parse-sexpr))))
    (push id *sal-local-variables*)
    (list id val)))


(defun parse-chdir ()
  ;; assume next token is :chdir
  (or (token-is :chdir) (error "parse-chdir internal error"))
  (let (path loc)
   (setf loc (parse-token))
   (setf path (parse-path))
   (add-line-info-to-stmt (list 'setdir path) loc)))


(defun parse-play ()
 ;; assume next token is :play
 (or (token-is :play) (error "parse-play internal error"))
 (let ((loc (parse-token)))
   (add-line-info-to-stmt (list 'sal-play (parse-sexpr)) loc)))


(defun parse-return ()
  (or (token-is :return) (error "parse-return internal error"))
  (let (loc expr)
    ;; this seems to be a redundant test
    (if (and (null *sal-fn-name*)
             (not *audacity-top-level-return-flag*))
        (errexit "Return must be inside a function body"))
    (setf loc (parse-token))
    (setf expr (parse-sexpr))
    (if *sal-fn-name*
      (add-line-info-to-stmt (list 'sal-return-from *sal-fn-name* expr) loc)
      (list 'defun 'main '() (list 'sal-trace-enter '(quote main) '() '())
                             (add-line-info-to-stmt expr loc)))))


(defun parse-load ()
  ;; assume next token is :load
  (or (token-is :load) (error "parse-load internal error"))
  (let (path args loc)
   (setf loc (parse-token))
   (setf path (parse-path)) ; must return path or raise error
   (setf args (parse-keyword-args))
   (add-line-info-to-stmt (cons 'sal-load (cons path args)) loc)))

(defun parse-keyword-args ()
  (let (args)
    (while (token-is :co)
      (parse-token)
      (cond ((token-is :key)
             (push (token-value) args)
             (push (parse-sexpr) args))))
    (reverse args)))


'(defun parse-system ()
  ;; assume next token is :system
  (or (token-is :system) (error "parse-system internal error"))
  (let (path arg args)
   (parse-token)
   (setf path (parse-sexpr))
   (list 'sal-system path)))


(defun parse-path ()
  (if (token-is '(:id :string))
      (token-lisp (parse-token))
      (errexit "path not found")))


(defun parse-print-display (token function)
  ;; assumes next token is :print
  (or (token-is token) (error "parse-print-display internal error"))
  (let (args arg loc)
   (setf loc (parse-token))
   (setf arg (parse-sexpr))
   (setf args (list arg))
   (while (token-is :co)
    (parse-token) ; remove and ignore the comma
    (setf arg (parse-sexpr))
    (push arg args))
   (add-line-info-to-stmt (cons function (reverse args)) loc)))

(defun parse-plot ()
  ;; assumes next token is :plot
  (or (token-is :plot) (error "parse-plot internal error"))
  (let (arg args loc)
   (setf loc (parse-token))
   (setf arg (parse-sexpr))
   (setf args (list arg))
   (cond ((token-is :co) ; get duration parameter
          (parse-token) ; remove and ignore the comma
          (setf arg (parse-sexpr))
          (push arg args)
          (cond ((token-is :co) ; get n points parameter
                 (parse-token) ; remove and ignore the comma
                 (setf arg (parse-sexpr))))))
   (add-line-info-to-stmt (cons 's-plot (reverse args)) loc)))

;(defun parse-output ()
; ;; assume next token is :output
; (or (token-is :output) (error "parse-output internal error"))
; (parse-token)
; (list 'sal-output (parse-sexpr)))


(defun maybe-parse-block ()
  (if (token-is :begin) (parse-block)))


(defun parse-block ()
  ;; assumes next token is :block
  (or (token-is :begin) (error "parse-block internal error"))
  (let (args stmts (locals *sal-local-variables*))
   (parse-token)
   (cond ((token-is :with)
          (setf args (parse-bindings))))
   (while (not (token-is :end))
    (push (parse-stmt) stmts))
   (parse-token)
   (setf stmts (reverse stmts))
   ;(display "parse-block" args stmts)
   (setf *sal-local-variables* locals)
   (cons 'prog* (cons args stmts))))

 
;; MAKE-STATEMENT-LIST -- convert stmt to a stmt list
;;
;; if it is a (PROGN ...) then return cdr -- it's already a list
;; otherwise, put single statement into a list
;;
(defun make-statement-list (stmt)
  (cond ((atom stmt)
         (list stmt))
        ((eq (car stmt) 'progn)
         (cdr stmt))
        (t
         (list stmt))))

(setf *conditional-tokens* '(:if :when :unless))


(defun maybe-parse-conditional ()
  (if (token-is *conditional-tokens*)
      (parse-conditional)))


(defun parse-conditional ()
  ;; assumes next token is :if
  (or (token-is *conditional-tokens*)
      (error "parse-conditional internal error"))
  (let (test then-stmt else-stmt if-token)
    (cond ((token-is :if)
           (setf if-token (parse-token))
           (setf test (parse-sexpr if-token))
           (if (not (token-is :then))
               (errexit "expected then after if"))
           (parse-token)
           (if (not (token-is :else)) ;; no then statement
               (setf then-stmt (parse-stmt)))
           (cond ((token-is :else)
                  (parse-token)
                  (setf else-stmt (parse-stmt))))
           ;(display "cond" test then-stmt else-stmt)
           (if else-stmt
               (list 'if test then-stmt else-stmt)
               (list 'if test then-stmt)))
          ((token-is :when)
           (parse-token)
           (setf test (parse-sexpr))
           (setf then-stmt (parse-stmt))
           (cons 'when (cons test (make-statement-list then-stmt))))
          ((token-is :unless)
           (parse-token)
           (setf test (parse-sexpr))
           (setf else-stmt (parse-stmt))
           (cons 'unless (cons test (make-statement-list else-stmt)))))))


(defun maybe-parse-assignment ()
  (if (token-is :set) (parse-assignment)))


(defun parse-assignment ()
  ;; first token must be set
  (or (token-is :set) (error "parse-assignment internal error"))
  (let (assignments rslt vref op expr set-token)
    (setf set-token (parse-token))
    (push (parse-assign) assignments) ; returns (target op value)
    (while (token-is :co)
      (parse-token) ; skip the comma
      (push (parse-assign) assignments))
    ; now assignments is ((target op value) (target op value)...)
    (dolist (assign assignments)
      (setf vref (first assign) op (second assign) expr (third assign))
      (cond ((eq op '=))
            ((eq op '-=) (setf expr `(diff ,vref ,expr)))
            ((eq op '+=) (setf expr `(sum ,vref ,expr)))
            ((eq op '*=) (setq expr `(mult ,vref ,expr)))
            ((eq op '/=) (setq expr `(/ ,vref ,expr)))
            ((eq op '&=) (setq expr `(nconc ,vref (list ,expr))))
            ((eq op '@=) (setq expr `(cons ,expr ,vref)))
            ((eq op '^=) (setq expr `(nconc ,vref (append ,expr nil))))
            ((eq op '<=) (setq expr `(min ,vref ,expr)))
            ((eq op '>=) (setq expr `(max ,vref ,expr)))
            (t (errexit (format nil "unknown assigment operator ~A" op))))
      (push (list 'setf vref expr) rslt))
    (setf rslt (add-line-info-to-stmts rslt set-token))
    (if (> (length rslt) 1)
        (cons 'progn rslt)
        (car rslt))))

    
;; PARSE-ASSIGN -- based on parse-bind, but with different operators
;;
;; allows arbitrary term on left because it could be an array
;; reference. After parsing, we can check that the target of the
;; assignment is either an identifier or an (aref ...)
;;
(defun parse-assign ()
  (let ((lhs (parse-term) op val))
    (cond ((token-is '(:= :-= :+= :*= :/= :&= :@= :^= :<= :>=))
           (setf op (parse-token))
           (setf op (if (eq (token-type op) ':=) '= (token-lisp op)))
           (setf val (parse-sexpr))))
    (cond ((and (consp lhs) (eq (car lhs) 'aref))) ;; aref good
          ((symbolp lhs)) ;; id good
          (t (errexit "expected a variable name or array reference")))
    (list lhs op val)))


(defun maybe-parse-loop ()
  (if (token-is :loop) (parse-loop)))


;; loops are compiled to do*
;; bindings go next as usual, but bindings include for variables
;; and repeat is converted to a for +count+ from 0 to <sexpr>
;; stepping is done after statement
;; termination clauses are combined with OR and
;; finally goes after termination
;; statement goes in do* body
;;
(defun parse-loop ()
  (or (token-is :loop) (error "parse-loop: internal error"))
  (let (bindings termination-tests stmts sexpr rslt finally
        loc
        (locals *sal-local-variables*))
    (parse-token) ; skip "loop"
    (if (token-is :with)
        (setf bindings (reverse (parse-bindings))))
    (while (token-is '(:repeat :for))
      (cond ((token-is :repeat)
             (setf loc (parse-token))
             (push (list 'sal:loopcount 0 '(1+ sal:loopcount)) bindings)
             (setf sexpr (parse-sexpr loc)) ; get final count expression
             (push (list 'sal:loopfinal sexpr) bindings)
             (push '(>= sal:loopcount sal:loopfinal) termination-tests))
            ((token-is :for)
             (setf rslt (parse-for-clause))
             ; there can be multiple bindings, build bindings in reverse
             (cond ((first rslt)
                    (setf bindings (append (reverse (first rslt))
                                           bindings))))
             (if (second rslt) (push (second rslt) termination-tests)))))
    (while (token-is '(:while :until))
      (cond ((token-is :while)
             (setf loc (parse-token))
             (push (list 'not (parse-sexpr loc)) termination-tests))
            ((token-is :until)
             (setf loc (parse-token))
             (push (parse-sexpr loc) termination-tests))))
    ; (push (parse-stmt) stmts)
    (while (not (token-is '(:end :finally)))
      (push (parse-stmt) stmts))
    (cond ((token-is :finally)
           (parse-token) ; skip "finally"
           (setf finally (parse-stmt))))
    (if (token-is :end)
        (parse-token)
        (errexit "expected end after loop"))
    (setf *sal-local-variables* locals)
    `(do* ,(reverse bindings)
          ,(list (or-ize (reverse termination-tests)) finally) 
          ,@(reverse stmts))))


;; OR-IZE -- compute the OR of a list of expressions
;;
(defun or-ize (exprs)
 (if (> (length exprs) 1) (cons 'or exprs)
     (car exprs)))


(defun maybe-parse-exec ()
  (if (token-is :exec) (parse-exec)))


(defun parse-exec ()
  (or (token-is :exec) (error "parse-exec internal error"))
  (let ((loc (parse-token))) ;  skip the :exec
    (parse-sexpr loc)))
          

(defun maybe-parse-exit ()
  (if (token-is :exit) (parse-exit)))


(defun parse-exit ()
  (let (tok loc)
    (or (token-is :exit) (error "parse-exit internal error"))
    (setf loc (parse-token)) ; skip the :exit
    (cond ((token-is :id)
           (setf tok (parse-token))
           (cond ((eq (token-lisp tok) 'nyquist)
                  (add-line-info-to-stmt '(exit) loc))
                 ((eq (token-lisp tok) 'sal)
                  (add-line-info-to-stmt '(sal-exit) loc))
                 (t
                  (errexit "expected \"nyquist\" or \"sal\" after \"exit\""))))
          (t
           (add-line-info-to-stmt '(sal-exit) loc)))))


;; PARSE-FOR-CLAUSE - returns (bindings term-test)
;;
(defun parse-for-clause ()
  (or (token-is :for) (error "parse-for-clause: internal error"))
  (let (id init next rslt binding term-test list-id loc)
    (setf loc (parse-token)) ; skip for
    (if (token-is :id)
        (setf id (token-lisp (parse-token)))
        (errexit "expected identifier after for"))
    (cond ((token-is :=)
           ;; if the clause is just for id = expr, then assume that
           ;; expr depends on something that changes each iteration:
           ;; recompute and assign expr to id each time around
           (parse-token) ; skip "="
           (setf init (parse-sexpr loc))
           (cond ((token-is :then)
                  (parse-token) ; skip "then"
                  (setf binding (list id init (parse-sexpr loc))))
                 (t
                  (setf binding (list id init init))))
           (setf binding (list binding)))
          ((token-is :in)
           (setf loc (parse-token)) ; skip "in"
           (setf list-id (intern (format nil "SAL:~A-LIST" id)))
           (setf binding 
                 (list (list list-id (parse-sexpr loc)
                             (list 'cdr list-id))
                       (list id (list 'car list-id) (list 'car list-id))))
           (setf term-test (list 'null list-id)))
          ((token-is :over)
           (setf loc (parse-token)) ; skip "over"
           (setf start (parse-sexpr loc))
#|         (cond ((token-is :by)
                  (parse-token) ; skip "by"
                  (parse-sexpr))) ;-- I don't know what "by" means - RBD |#
           (setf list-id (intern (format nil "SAL:~A-PATTERN" id)))
           (setf binding
                 (list (list list-id start)
                       (list id (list 'next list-id) (list 'next list-id)))))
          ((token-is '(:from :below :to :above :downto :by))
           (cond ((token-is :from)
                  (setf loc (parse-token)) ; skip "from"
                  (setf init (parse-sexpr loc)))
                 (t
                  (setf init 0)))
           (cond ((token-is :below)
                  (setf loc (parse-token)) ; skip "below"
                  (setf term-test (list '>= id (parse-sexpr loc))))
                 ((token-is :to)
                  (setf loc (parse-token)) ; skip "to"
                  (setf term-test (list '> id (parse-sexpr loc))))
                 ((token-is :above)
                  (setf loc (parse-token)) ; skip "above"
                  (setf term-test (list '<= id (parse-sexpr loc))))
                 ((token-is :downto)
                  (setf loc (parse-token)) ; skip "downto"
                  (setf term-test (list '< id (parse-sexpr loc)))))
           (cond ((token-is :by)
                  (setf loc (parse-token)) ; skip "by"
                  (setf binding (list id init (list '+ id (parse-sexpr loc)))))
                 ((or (null term-test)
                      (and term-test (member (car term-test) '(>= >))))
                  (setf binding (list id init (list '1+ id))))
                 (t ; loop goes down because of "above" or "downto"
                    ; (display "for step" term-test)
                  (setf binding (list id init (list '1- id)))))
           (setf binding (list binding)))
          (t
           (errexit "for statement syntax error")))
    (list binding term-test)))

    
;; parse-sexpr works by building a list: (term op term op term ...)
;; later, the list is parsed again using operator precedence rules
(defun parse-sexpr (&optional loc)
  (let (term rslt)
    (push (parse-term) rslt)
    (while (token-is *sal-operators*)
      (push (token-type (parse-token)) rslt)
      (push (parse-term) rslt))
    (setf rslt (reverse rslt))
    ;(display "parse-sexpr before inf->pre" rslt)
    (setf rslt (if (consp (cdr rslt))
                (inf->pre rslt)
                (car rslt)))
    (if loc
        (setf rslt (add-line-info-to-expression rslt loc)))
    rslt))


(defun get-lisp-op (op)
  (third (assoc op +operators+)))


;; a term is <unary-op> <term>, or
;;           ( <sexpr> ), or
;;           ? ( <sexpr> , <sexpr> , <sexpr> ), or
;;           <id>, or
;;           <id> ( <args> ), or
;;           <term> [ <sexpr> ]
;; Since any term can be followed by indexing, handle everything
;; but the indexing here in parse-term-1, then write parse-term
;; to do term-1 followed by indexing operations
;;
(defun parse-term-1 ()
  (let (sexpr id)
    (cond ((token-is '(:- :!))
           (list (token-lisp (parse-token)) (parse-term)))
          ((token-is :lp)
           (parse-token) ; skip left paren
           (setf sexpr (parse-sexpr))
           (if (token-is :rp)
               (parse-token)
               (errexit "right parenthesis not found"))
           sexpr)
          ((token-is :?)
           (parse-ifexpr))
          ((token-is :lc)
           (list 'quote (parse-list)))
          ((token-is '(:int :float :bool :list :string))
           ;(display "parse-term int float bool list string" (car *sal-tokens*))
           (token-lisp (parse-token)))
          ((token-is :id) ;; aref or funcall
           (setf id (token-lisp (parse-token)))
           ;; array indexing was here, but that only allows [x] after
           ;; identifiers. Move this to expression parsing.
           (cond ((token-is :lp)
                  (parse-token)
                  (setf sexpr (cons id (parse-pargs t)))
                  (if (token-is :rp)
                      (parse-token)
                      (errexit "right paren not found"))
                  sexpr)
                 (t id)))
          (t
           (errexit "expression not found")))))


(defun parse-term ()
  (let ((term (parse-term-1)))
    ; (display "parse-term" term (token-is :lb))
    (while (token-is :lb)
      (parse-token)
      (setf term (list 'aref term (parse-sexpr)))
      (if (token-is :rb)
          (parse-token)
          (errexit "right bracket not found")))
    term))


(defun parse-ifexpr ()
  (or (token-is :?) (error "parse-ifexpr internal error"))
  (let (condition then-sexpr else-sexpr)
    (parse-token) ;  skip the :?
    (if (token-is :lp) (parse-token) (errexit "expected left paren"))
    (setf condition (parse-sexpr))
    (if (token-is :co) (parse-token) (errexit "expected comma"))
    (setf then-sexpr (parse-sexpr))
    (if (token-is :co) (parse-token) (errexit "expected comma"))
    (setf else-sexpr (parse-sexpr))
    (if (token-is :rp) (parse-token) (errexit "expected left paren"))
    (list 'if condition then-sexpr else-sexpr)))


(defun keywordp (s)
  (and (symbolp s) (eq (type-of (symbol-name s)) 'string)
       (equal (char (symbol-name s) 0) #\:)))


(defun functionp (x) (eq (type-of x) 'closure))


(defun parse-pargs (keywords-allowed)
  ;; get a list of sexprs. If keywords-allowed, then at any point
  ;; the arg syntax can switch from [<co> <sexpr>]* to
  ;; [<co> <keyword> <sexpr>]*
  ;; also if keywords-allowed, it's a function call and the
  ;; list may be empty. Otherwise, it's a list of indices and
  ;; the list may not be empty
  (let (pargs keyword-expected sexpr keyword)
   (if (and keywords-allowed (token-is :rp))
       nil ; return empty parameter list
       (loop ; look for one or more [keyword] sexpr
         ; optional keyword test
         (setf keyword nil)
         ;(display "pargs" (car *sal-tokens*))
         (if (token-is :key)
             (setf keyword (token-lisp (parse-token))))
         ; (display "parse-pargs" keyword)
         ; did we need a keyword?
         (if (and keyword-expected (not keyword))
             (errexit "expected keyword"))
         ; was a keyword legal
         (if (and keyword (not keywords-allowed))
             (errexit "keyword not allowed here"))
         (setf keyword-expected keyword) ; once we get a keyword, we need
                                         ; one before each sexpr
         ; now find sexpr
         (setf sexpr (parse-sexpr))
         (if keyword (push keyword pargs))
         (push sexpr pargs)
         ; (display "parse-pargs" keyword sexpr pargs)
         (cond ((token-is :co)
                (parse-token))
               (t
                (return (reverse pargs))))))))


;; PARSE-LIST -- parse list in braces {}, return list not quoted list
;;
(defun parse-list ()
  (or (token-is :lc) (error "parse-list internal error"))
  (let (elts)
    (parse-token)
    (while (not (token-is :rc))
           (cond ((token-is '(:int :float :id :bool :key :string))
                  (push (token-lisp (parse-token)) elts))
                 ((token-is *sal-operators*)
                  (push (intern (token-string (parse-token))) elts))
                 ((token-is :lc)
                  (push (parse-list) elts))
                 ((token-is :co)
                  (errexit "expected list element or right brace; do not use commas inside braces {}"))
                 (t
                  (errexit "expected list element or right brace"))))
    (parse-token)
    (reverse elts)))


(defparameter *op-weights*
  '(
    (:\| 1)
    (:& 2)
    (:! 3)
    (:= 4)
    (:!= 4)
    (:> 4)
    (:>= 4)
    (:< 4)
    (:<= 4)
    (:~= 4) ; general equality
    (:+ 5)
    (:- 5)
    (:% 5)
    (:* 6)
    (:/ 6)
    (:^ 7)
    (:~ 8)
    (:~~ 8)
    (:@ 8)
    (:@@ 8)))


(defun is-op? (x)
  ;; return op weight if x is operator
  (let ((o (assoc (if (listp x) (token-type x) x)
                 *op-weights*)))
    (and o (cadr o))))


(defun inf->pre (inf)
  ;; this does NOT rewrite subexpressions because parser applies rules
  ;; depth-first so subexprs are already processed
  (let (op lh rh w1)
    (if (consp inf)
        (do ()
            ((null inf) lh)
          (setq op (car inf))                ; look at each element of in
          (pop inf)
          (setq w1 (is-op? op))
          (cond ((numberp w1)                ; found op (w1 is precedence)
                 (do ((w2 nil)
                      (ok t)
                      (li (list)))
                     ((or (not inf) (not ok))
                      (setq rh (inf->pre (nreverse li)))
                      (setq lh (if lh (list (get-lisp-op op) lh rh)
                                   (list (get-lisp-op op) rh nil))))
                   (setq w2 (is-op? (first inf)))
                   (cond ((and w2 (<= w2 w1))
                          (setq ok nil))
                         (t
                          (push (car inf) li)
                          (pop inf)))))
                (t
                 (setq lh op))))
        inf)))

