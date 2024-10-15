;*************
;           Change Log
;  Date     | Change
;-----------+------------------------------------
; 18-Dec-91 | [1.2] <jmn> Created
; 18-Dec-91 | [1.2] <jmn> added *ANSI* tests
; 13-Jan-92 | [1.2] <jmn> ANSI header includes stdlib.h, excludes decl of
;           | malloc
; 13-Jan-92 | [1.2] <jmn> upgraded to support new sound block structure
; 15-Jan-92 | [1.2] <jmn> added declarations for UNKNOWN, isunknown()
; 15-Jan-92 | [1.2] <jmn> commented out boolean, true, false now declared
;           | in sound.h
;*************
;; translate.lsp -- build signal processing code from high level descr.

(setf *ANSI* t)
(setf *debug* t)

;;**********
;;      combinations - generate all combinations
;; Inputs:
;;      n - number of combinations to generate
;; Result:
;;      list of the form
;;      ( (a1 b1) (a2 b2) (a3 b3) ... (an bn) )
;;      
;;**********

(defun combinations (n)
  (let (comb)
    (cond ((eq n 0) '(nil))
      (t
       (setf comb (combinations (1- n)))
       (append (insert 'ramp comb)
           (insert 'interp comb)
           (insert 'scale comb)
           (insert 'none comb))))))

(print 'comb)

(defun lt () (load "translate"))
(defun ls () (load "writesusp"))
(defun lm () (load "writemake"))
(defun lo () (load "writetoss"))
(defun li () (load "innerloop"))

(defun ma () (translate "partial"))
(defun mb () (translate "buzz"))
(defun mal () (translate "alpass"))
(defun macv () (translate "alpasscv"))
(defun mavc () (translate "alpassvc"))
(defun mavv () (translate "alpassvv"))
(defun mf () (translate "follow"))
(defun mfas () (translate "fromarraystream"))
(defun mfo () (translate "fromobject"))
(defun mp () (translate "prod"))
(defun mc () (translate "const"))
(defun mct () (translate "coterm"))
(defun mcl () (translate "clip"))
(defun meqb () (translate "eqbandvvv"))
(defun me () (translate "exp"))
(defun mg () (translate "gate"))
;(defun mr () (translate "ramp"))
(defun ms () (translate "sine"))
(defun msh () (translate "shape"))
(defun mpw () (translate "pwl"))
;(defun msfr () (translate "sfread"))
(defun mde () (translate "delaycc"))
(defun mdcv () (translate "delaycv"))
; note: downproto is hand retouched to make downsample
;(defun md () (translate "downproto"))
(defun mu () (translate "upsample"))
(defun ml () (translate "scale"))
(defun mlo () (translate "log"))
(defun mm () (translate "maxv"))
(defun mo () (translate "osc"))
(defun mof () (translate "offset"))
(defun mam () (translate "amosc"))
(defun mfm () (translate "fmosc"))
(defun mi () (translate "integrate"))
(defun msl () (translate "slope"))
(defun mw () (translate "white"))
(defun mt () (translate "tone"))
(defun mta () (translate "tapv"))
(defun mtf () (translate "tapf"))
(defun mat () (translate "atone"))
(defun mre () (translate "reson"))
(defun mrec () (translate "recip"))
(defun mar () (translate "areson"))
(defun mtv () (translate "tonev"))
(defun matv () (translate "atonev"))
(defun mrvc () (translate "resonvc"))
(defun mrcv () (translate "resoncv"))
(defun marvc () (translate "aresonvc"))
(defun marcv () (translate "aresoncv"))
(defun mrvv () (translate "resonvv"))
(defun marvv () (translate "aresonvv"))
(defun msa () (translate "sampler"))
(defun msio () (translate "siosc"))
(defun mq () (translate "quantize"))
(defun mbq () (translate "biquadfilt"))
(defun mabs () (translate "abs"))
(defun msqrt () (translate "sqrt"))

(defun mifft () (translate "ifft"))

(defun mcg () (translate "congen"))
(defun mcv () (translate "convolve")) ;; this does not generate the final version
    ;; see the hand-modified version of convolve.c in nyqsrc directory
(defun mos () (translate "oneshot"))
(defun mch () (translate "chase"))
(defun mpl () (translate "pluck"))
(defun icl () (translate "instrclar"))
(defun isx () (translate "instrsax"))
(defun ifl () (translate "instrflute"))
(defun icla () (translate "instrclarall"))
(defun isxa () (translate "instrsaxall"))
(defun ifla () (translate "instrfluteall"))
(defun iclf () (translate "instrclarfreq"))
(defun isxf () (translate "instrsaxfreq"))
(defun iflf () (translate "instrflutefreq"))
(defun mla () (translate "allpoles"))
(defun mlr () (translate "lpreson"))
(defun ibf () (translate "instrbowedfreq"))
(defun mib () (translate "instrbow"))
(defun mps () (translate "stkpitshift"))
(defun mcho () (translate "stkchorus"))
(defun mrv () (translate "stkrev"))
(defun mbd () (translate "instrbanded"))
(defun mmd () (translate "instrmandolin"))
(defun mst () (translate "instrsitar"))
(defun mmb () (translate "instrmodalbar"))
 

(defun mstk () (icl) (isx) (ifl) (icla) (isxa) (ifla) (iclf) (isxf) (iflf)
               (ibf) (mib) (mps) (mcho) (mrv) (mbd) (mmd) (mst) (mmb))
(defun mfmfb () (translate "fmfb") (translate "fmfbv"))

(defun m () (mf) (mp) (mc) (mcl) (mg)
;;;;;;      (mr) (msfr) (md)
        (meqb)
        (mm) (ms) (msh) (mpw) (ma) (mb) (mde) (mdcv)
        (mi) (mu) (ml) (mlo)
        (mo) (mof) (mam) (mfm) (mw) (msl) (mt) (mat) (mre) (mrec)
        (mar) (mtv) (mta) (mtf) (matv) (mrvc) (mrcv) (marvc) (marcv)
        (mrvv) (marvv) (me) (msa) (msio) (mq) (mcg) (mifft) 
        (mfas) (mfo) (mct) (mal) (mos) (mch) (mbq) (mpl)
        (mabs) (msqrt) (macv) (mavc) (mavv) ; (mcv) must be managed by hand
        (mstk) (mla) (mlr) (mfmfb))

; call this when you change writesusp.lsp: "N"ew "S"usp
(defun ns () (ls) (m))
; call this when you change writemake.lsp:
(defun nm () (lm) (m))
; call this when you change innerloop.lsp:
(defun ni () (li) (m))


;;**********
;; any-ramp-in -- see if interpolation-list has 'ramp
;;
;; note: lis is a list of lists of atoms
;;**********
(defun any-ramp-in (lis)
  (dolist (spec lis)
    (cond ((member 'RAMP spec)
       (return t)))))


;;**********
;; any-ramp-or-interp-in -- see if interpolation-list has 'ramp or 'interp
;;
;;**********
(defun any-ramp-or-interp-in (lis)
  (or (any-ramp-in lis)
      (dolist (spec lis)
    (cond ((member 'INTERP spec)
           (return t))))))


;;**********
;; encode -- come up with ascii string for interp spec
;;
;; e.g. (none ramp) -> "nr"
;;
;;**********
(defun encode (interpolation)
  (let (first-letter 
    (result ""))
    (dolist (interp interpolation)
      (setf first-letter (string (char (symbol-name interp) 0)))
      (setf result (strcat result first-letter)))
    (string-downcase result)))


;; ****************
;;      header-list
;;
;; Result:
;;      '( "s1" "s2" ... "sn" )
;;      where s1, s2, etc. are the strings for the header part of the
;;      resulting .c file
;; Notes:
;;      Kludgy.  Fix this up for easier maintenance
;; ****************

(if *ANSI* 
    ; ANSI
    (setf header-list 
     '("#include \"stdio.h\"\n"
       "#ifndef mips\n"
       "#include \"stdlib.h\"\n"
       "#endif\n"
       "#include \"xlisp.h\"\n"
       "#include \"sound.h\"\n\n"
       "#include \"falloc.h\"\n"
       "#include \"cext.h\"\n"
       ))
    ; non-ANSI
    (setf header-list 
     '("#include \"stdio.h\"\n"
       "#include \"xlisp.h\"\n"                             
       "#include \"sound.h\"\n"
       "#include \"falloc.h\"\n")))


(setf h-boilerplate nil)

;--------------obsolete boilerplate-------------
;; Note that we use "-1" and "< 0".  We rely upon C's semantics to
;; make this work correctly if it is being assigned to a long, float, or
;; double, and if a long, float, or double is being compared
;      '("\n#ifndef UNKNOWN\n"
;       "#define UNKNOWN -1\n"
;       "#define isunknown(x) ( (x) < 0)\n"
;       "#endif /* UNKNOWN */\n"))
;-------------------------


;;**********
;; code-gen -- do the output
;;
;; Inputs:
;;      alg -
;;      stream -
;;      hstream -
;;**********

(defun code-gen (alg stream hstream)
  (let (interpolation-list
    (support-functions (get-slot alg 'support-functions))
    (support-header    (get-slot alg 'support-header))
    (name (get-slot alg 'name)))
    ;(display "code-gen: " alg stream hstream)
    (print-strings header-list stream)
    (format stream "#include \"~A\"~%" (get-slot alg 'hfile))
    (display "code-gen: printed header")
    (format stream "~%void ~A_free(snd_susp_type a_susp);~%" name)
    (setf interpolation-list (make-interpolation-list alg))
    (display "code-gen: " interpolation-list)
    (put-slot alg interpolation-list 'interpolation-list)
    (put-slot alg (make-interpolation-rationale alg) 
          'interpolation-rationale)
      
    (write-typedef alg stream)
    (display "code-gen: wrote typedef")

    (cond (support-functions
       (format stream "~%~A" support-functions)))

    (dolist (interpolation interpolation-list)
      (put-slot alg interpolation 'interpolation)
      (display "code-gen: going to write susp for " interpolation)
      (write-susp alg stream)
      (display "code-gen: wrote susp for" interpolation))

    ;; this is a special case for no sound arguments
    (cond ((null interpolation-list)
       (write-susp alg stream)))

    ;; write the function that is called to read and toss
    ;; samples up to the start time (but only if there are sound arguments)
    (cond ((get-slot alg 'sound-names)
       (write-toss alg stream)))

    ;; write the GC marking function        
    (cond ((needs-mark-routine alg)
       (write-mark alg stream)))

    (write-make alg stream)
    (display "code-gen: wrote make")

    (write-xlmake alg stream)
    (display "code-gen: wrote xlmake")

    (write-header alg hstream)
    (cond ( support-header
            (print-strings support-header hstream)))
    (print-strings h-boilerplate hstream)
    (display "code-gen: wrote header")))


;;**********
;; commute-check -- 
;;
;; Purpose:
;;      see if interpolation spec is redundant due to commutativity
;; Algorithm: 
;;      for each list of "commutable" sounds, make sure spec asks for
;;      cannonical ordering:  NONE > SCALE > INTERP > RAMP
;;**********
(defun commute-check (alg spec)
  (let ((sounds (get-slot alg 'sound-args))
    (commute-list (get-slot alg 'commutative))
    (result t)
    s1 s2)
    (dolist (commute commute-list)
      (dotimes (n (1- (length commute)))  ; look at all pairs
    (setf s1 (nth n commute))
    (setf s2 (nth (1+ n) commute))
    (setf s1 (index s1 sounds))
    (setf s2 (index s2 sounds))
    (setf s1 (nth s1 spec))
    (setf s2 (nth s2 spec))
    (cond ((< (eval s1) (eval s2))
           (setf result nil)
           (return)))))
    result))

(setf NONE 4) (setf SCALE 3) (setf INTERP 2) (setf RAMP 1)


(print 'ramp)


;;**********
;; concatenate -- string concatenation
;;
;; Inputs:
;;      "s1" - string
;;      "s2" - string
;; Result:
;;      "s1s2"
;;**********

(defun concatenate (type s1 s2)
    (cond ((eq type 'string) (strcat s1 s2))
          (t (error "concatenate type"))))


;;**********
;; get-slot -- access the algorithm description, return single value
;;
;;**********

(setfn get-slot get)


;;**********
;; index -- find location of list element
;;
;; Inputs:
;;      atom - atom to be found in list
;;      lis  - list searched for
;; Result:
;;      integer - index of atom in lis
;;      NIL     - atom not member of lis
;;**********

(defun index (atom lis)
  (let ((i 0))
    (dolist (elt lis)
      (cond ((eq elt atom)
         (return i)))
      (setf i (1+ i)))))


;;**********
;; insert -- insert an atom at the front of each element of a list
;;
;; Inputs:
;;      atom - 
;;      list-of-lists - lists of the form ( (L1) (L2) ... (Ln))
;; Result:
;;      ( (atom L1) (atom L2) ... (atom Ln) )
;;**********
(defun insert (atom list-of-lists)
  (mapcar '(lambda (lis) (cons atom lis)) list-of-lists))

(print 'insert)

;; interp-check -- check to see that no interpolation is being done
;;   (unless the algorithm is the up-sample algorithm, a special case
;;
(defun interp-check (alg spec)
  (let ((ili *INLINE-INTERPOLATION*)
        (ili-spec (get alg 'inline-interpolation)))
    (if ili-spec (setf ili t))
    (if (eq ili-spec 'no) (setf ili nil))
    (or ili (and (not (member 'INTERP spec))
                 (not (member 'RAMP spec))))))
    
(print 'interp-check)


;;**********
;; make-interpolation-list -- figure out the possible interpolation forms
;;
;; Inputs:
;;       alg - algorithm description
;; Output:
;;  List of interpolation styles, e.g. 
;;  ((NONE NONE) (NONE INTERP) (NONE RAMP)), where the styles
;; are in the same order as the sound arguments (sound-args)
;; 
;;**********
(defun make-interpolation-list (alg)
  (let (sound-args specs real-specs sound-names sound-to-name
    (sr (get-slot alg 'sample-rate))
    (not-in-inner-loop (get-slot alg 'not-in-inner-loop)))
    ; derive some lists:
    ;   sound-args are atom names of sound-type arguments
    ;   sound-names are the corresponding string names
    ;   sound-to-name is an assoc list mapping atom to case-sensitive string
;    (display "make-interpolation-list")

    (dolist (arg (get-slot alg 'arguments))
    (cond ((and (equal (car arg) "sound_type")
            (not (member (cadr arg) not-in-inner-loop :test #'equal)))
           (setf sound-names (cons (cadr arg) sound-names))
           (setf sound-args (cons (name-to-symbol (cadr arg))
                      sound-args))
           (setf sound-to-name (cons (cons (car sound-args)
                           (car sound-names))
                     sound-to-name))
;              (display "in make-interpolation-list" sound-to-name)
          )))
;    (display "make-interpolation-list: " (reverse sound-args))
    (put-slot alg (reverse sound-args) 'sound-args)
;    (display "make-interpolation-list: " (reverse sound-names))
    (put-slot alg (reverse sound-names) 'sound-names)
    (put-slot alg sound-to-name 'sound-to-name)
    ; make all combinations of interpolations
    (setf specs (combinations (length sound-args)))
    ;; don't print this or you'll die when the list is huge
    ;; (display "make-interpolation-list: " specs)
    ;; we really should have filtered with match-check inside combinations
    ;; to avoid exponential explosion
    ; reject combinations based on commutativity, linearity, and sample rate:
    ; if sample-rate is not specified, then some interpolation must be 'NONE,
    ; i.e. sample-rate is specified OR an interpolation is 'NONE:
    ; if INLINE-INTERPOLATION is turned off, don't allow RAMP or INTERP
     ; if INTERNAL-SCALING applies, then don't allow SCALE
    (dolist (spec specs)
      (cond ((and spec
                  (interp-check alg spec)
                  (commute-check alg spec)
                  (scale-check alg spec)
                  (match-check alg spec)
                  (sr-check alg spec))
             (setf real-specs (cons spec real-specs)))))
    (cond ((and (car specs) (null real-specs))
       (error "no interpolation specs")))
    (print real-specs)))


; MAKE-INTERPOLATION-RATIONALE -- record the rationale for
; interpolation combinations:
;   NIL means no special considerations
;   ALWAYS-SCALE means 'n' eliminated, use 's' instead
;   LINEAR means 's' eliminated and unnecessary
;   INTERNAL-SCALING means 's' eliminated, use 'n' instead
;
(defun make-interpolation-rationale (alg)
  (let (interpolation-rationale len snd
    (sounds (get-slot alg 'sound-args))
    (linear (get-slot alg 'linear))
    (internal-scaling (get-slot alg 'internal-scaling))
    (always-scale (get-slot alg 'always-scale)))
    (setf interpolation-rationale (mapcar #'return-nil sounds))
    (setf len (length interpolation-rationale))
    (dotimes (n len)
         (setf snd (nth n sounds))
         (cond ((member snd always-scale)
            (setf (nth n interpolation-rationale) 'ALWAYS-SCALE)))
         (cond ((member snd linear)
            (cond ((nth n interpolation-rationale)
               (error "parameter is both linear and always-scale"
                  snd)))
            (setf (nth n interpolation-rationale) 'LINEAR)))
         (cond ((member snd internal-scaling)
            (cond ((nth n interpolation-rationale)
               (error 
 "parameter is both linear and always-scale or internal-scaling" snd)))
            (setf (nth n interpolation-rationale) 'INTERNAL-SCALING))))
    (display "make-interpolation-rationale" interpolation-rationale)
    interpolation-rationale))


(print 'hi)

;;**********
;; make-schema-from-slots -- take attr/value pairs and make property list
;;
;; Inputs:
;;      slots - a list of the form 
;;                      (name
;;                         (attribute1 value1) (attribute2 value2)
;;                                      ... (attributen valuen) )
;; Result:
;;      The atom 'name' with the attached property list
;; Effect:
;;      Adds properties to the atom 'name' based on the attribute-value
;;      pairs. 
;; Notes:
;;      The property-list representation is chosen for time efficiency of
;;      access
;;**********

(defun make-schema-from-slots (slots)
  (let ((name (car slots)))
    (setf (symbol-plist name) nil)
    (dolist (slot (cdr slots))
      (putprop name (cdr slot) (car slot)))
    name))

;;****************
;; name-to-symbol -- convert from case-sensitive C name to internal symbol
;;****************
(defun name-to-symbol (name) (intern (string-upcase name)))



;;**********
;; position -- find a pattern in a string
;;
;; Inputs:
;;      s -
;;      p -
;;**********

(defun position (s p)
  (let (result (len (length p)))
    (dotimes (n (+ 1 (length s) (- len)))
      (cond ((equal (subseq s n (+ n len)) p)
         (setf result n)
         (return))))
    result))


;;**********
;; print a list of strings to a stream
;;
;; Inputs:
;;      strings - a list of strings
;;      stream - stream on which to write the strings
;; Effect:
;;      
;;**********

(defun print-strings (strings stream)
  (dolist (s strings) (princ s stream)))



;;**********
;; put-slot: 
;;      
;; Inputs:
;;      schema - name of the schema
;;      value - value of the attribute to be added or modified
;;      property - name of the attribute to be modified
;;
;;**********

(setfn put-slot putprop)


(defun return-nil (ignore) nil)

;;**********
;; scale-check -- make sure scale method is not used on linear input or
;;      on input where scaling is factored into other computation; 
;;      Also, don't use NONE scale method if sound appears on always-scale
;;      list (these sounds have low likelihood of ever using 'NONE method -
;;      see fmosc for an example).  Note that if you say always-scale (removing
;;                NONE) and linear or internal-scaling (removing SCALE),
;;                then you'll be in big trouble.
;;
;; Inputs:
;;      alg - algorithm description
;;      spec -
;; Notes:
;;      
;;**********

(defun scale-check (alg spec)
  (let ((sounds (get-slot alg 'sound-args))
        (linear (get-slot alg 'linear))
        (internal-scaling (get-slot alg 'internal-scaling))
        (always-scale (get-slot alg 'always-scale))
        snd
        (result t)
       )
    ; initially, the rationale list is nil for each sound:
    (cond (always-scale
       (dotimes (n (length spec))  ; look at each method in spec
         (cond ((eq 'NONE (nth n spec))
            (setf snd (nth n sounds))
            (cond ((member snd always-scale)
               (setf result nil)
               (return))))))))
    (cond ((member 'SCALE spec) ; quick test
       (dotimes (n (length spec))  ; look at each method in spec
         (cond ((eq 'SCALE (nth n spec))
            (setf snd (nth n sounds))
            (cond ((or (member snd linear) 
                   (member snd internal-scaling))
               (if (member snd internal-scaling) 
                 (format t "WARNING internal scaling not fully debugged, check your results...\n"))
               (setf result nil)
               (return))))))))
    result))


;; match-check -- make sure spec is consistent with inputs whose sample-rates
;; are matched. If a set of inputs appears on a MATCHED-SAMPLE-RATE clause,
;; then the spec for each input must be the same. This is used to control
;; combinatorial explosions.
;;
(defun match-check (alg spec)
  (let ((sounds (get-slot alg 'sound-args))
        (matched-sample-rate (get-slot alg 'matched-sample-rate))
        kind ;; kind of access used by all matched sounds
        snd  ;; the current sound in list
        (result t))
    ;; algorithm: scan list for members of matched-sample-rate
    ;; when first is found, set kind; after than, insist that
    ;; other members have matching spec
    (cond (matched-sample-rate
           (dotimes (n (length spec))
             (setf snd (nth n sounds))
             (cond ((member snd matched-sample-rate)
                    (cond ((null kind)
                           (setf kind (nth n spec)))
                          ((eq (nth n spec) kind))
                          (t
                           (setf result nil))))))))
    result))


;;****************
;; space-if-no-trailing-star -- returns "" if arg ends with "*", else space
;;****************
(defun space-if-no-trailing-star (str)
  (if (equal #\* (char str (1- (length str))))
    ""
    #\Space))


;; SPEC-IS-NONE-OR-SCALE -- see if spec is none or scale, called by sr-check
;;
;; sig is the search key
;; sound-args is a list, one element matches sig
;; spec is list of specs corresponding to elements in sound-args
;; return t if (eq sig (nth n sound-args)) and (nth n spec) is 
;; either 'none or 'scale
;;
(defun spec-is-none-or-scale (sig sound-args spec)                      
  (dolist (arg sound-args)
      (cond ((eq sig arg)
         (return (member (car spec) '(NONE SCALE)))))
      (setf spec (cdr spec))))


;;****************
;; sr-check -- see if interpolation spec is ok wrt sample rate spec
;;****************
(defun sr-check (alg spec)
  (let ((sample-rate (get-slot alg 'sample-rate))
    (sound-args (get-slot alg 'sound-args))
    (result nil))
    ;; if expression given, then anything is ok
    (cond ((stringp sample-rate)
           t)
          ;; if (MAX ...) expression given, then one of signals
          ;; must be NONE or SCALE
          ((and (listp sample-rate) (eq (car sample-rate) 'MAX))
           (dolist (sig (cdr sample-rate))  ; for all sig in max list ...
             (cond ((spec-is-none-or-scale sig sound-args spec)
                    (setf result t))))
           result)
          ;; if no expression given, then one signal must be NONE or SCALE
          ((or (member 'NONE spec) (member 'SCALE spec)) t)
          ;; o.w. return false
          (t nil))))


;;****************
;; symbol-to-name -- convert from internal symbol to case-sensitive C name
;;****************
(defun symbol-to-name (symbol) (get symbol 'string-name))



;;**********
;; translate -- main procedure
;;
;; Inputs:
;;      name - string which is name of file to translate
;; Effect:
;;      Reads the algorithm specification as "name.alg"
;;      Generates output files "name.c" and "name.h"
;;**********
(defun translate (name)
  (prog* ((infile (concatenate 'string name ".alg"))
      (outfile (concatenate 'string name ".c"))
      (hfile (concatenate 'string name ".h"))
      (inf (open infile :direction :input))
      (hf (open hfile :direction :output))
      (outf (open outfile :direction :output)))

   (if (null inf) (error "translate: couldn't open inf"))
   (if (null hf) (error "translate: couldn't open hf"))
   (if (null outf) (error "translate: couldn't open outf"))

   (display "FILES" inf hf outf)

   (if *WATCH*
      (print "**** TRACING HOOKS ENABLED!  ****")
      (print "**** NO TRACING ****")
   )
   loop
    ;; read the algorithm description 
    (setq alg (read inf))

    ;; if the algorithm is NIL, we had some sort of failure
    (cond ((null alg) 
       (close inf)
       (close hf)
       (close outf)
       (return)))

    ;; we have read in the high-level schema specification
    ;; convert it to a schema
    (display "translate: " infile alg)
    (setf alg (make-schema-from-slots alg))
    (display "translate: schema " alg)

    ;; save the .h file name
    (put-slot alg hfile 'hfile)
    ;; perform the type-check on the schema parameters
    (type-check-and-transform alg)
    (display "translate: transformed schema" alg)
    (code-gen alg outf hf)
    (display "translate: finished code-gen")
    (setf save-alg alg)
    (go loop)
 )
)
    

(print 'translate)

;;**********
;; type-check-and-transform -- fix up slots in an algorithm schema
;;
;; Inputs:
;;      alg - the name of the algorithm; values are its property list
;; Notes:
;;      Report an error if required slot values are absent
;;      Any slot which should be a single value and is a list is
;;      coerced to be the car of the list
;;      Put argument string names on argument symbols for conversion.
;;**********

(defun type-check-and-transform (alg)

  ;; the quoted list that follows 'slot' is the list of required
  ;; parameters.  If any parameter is missing, this will cause an
  ;; error

  (dolist (slot '(name inner-loop)) ; other necessarily non-nil slots go here
    (cond ((null (get-slot alg slot))
       (error "missing slot"))))

  ; fix single-value slots to not be stored as lists:
  ;     If the value is a list, the value is coerced to
  ;     be the car of the list

  (dolist 
       (slot 
     '(name lispname inner-loop sample-rate support-functions inline-interpolation delay
       )) 
       (put-slot alg (car (get-slot alg slot)) slot))

  ; Make sure there are no strings, only symbols, in TERMINATE and
  ; LOGICAL-STOP MIN lists: (TERMINATE (MIN "s1")) is wrong, it should be
  ; (TERMINATE (MIN s1))

  (dolist (field '(terminate logical-stop))
    (setf spec (get-slot alg field))
    (display "type-check" spec field)
    (cond ((and spec
        (listp (car spec))
        (member (caar spec) '(MIN MAX)))
       (dolist (entry (cdar spec))
         (display "type-check" spec field entry)
         (cond ((eq (type-of entry) 'STRING)
            (error "MIN and MAX args are symbols, not strings"
               spec)))))))

  ; (ARGUMENTS ( "type1" "name1") ("type2" "name2") ... ("typen" "namen") )
  ; if "sr" is the name of an argument, its type must be "rate_type"
  ; i.e. ("rate_type" "sr")

  (dolist (arg (get-slot alg 'arguments))
    (cond ((and (equal (cadr arg) "sr")
           (not (equal (car arg) "rate_type")))
       (error "argument sr must be of type rate_type"))
      ((equal (car arg) "sound_type")
       (putprop (name-to-symbol (cadr arg)) (cadr arg) 'string-name)))))



;;**********
;; union-of-nth -- get the union of the nth element of each sublist
;;
;;**********
(defun union-of-nth (lis n)
  (let (result a)
    (dolist (sublis lis)
      (setf a (nth n sublis))
      (cond ((not (member a result)) 
         (setf result (cons a result)))))
    result))


(print 'union-of-nth)

;;**********
;; write-header -- write a header file for the suspension create routine
;;
;; Inputs:
;;      alg - algorithm name
;;      stream - output stream for .h file
;; Effect:
;;      Writes to the stream
;;              sound_type snd_make_NAME();
;; Notes:
;;      Uses NAME property of algorithm to emit the procedure header to
;;      the .h file
;;**********

(setf c-to-xlisp-type '(
  ("double" . "ANYNUM")
  ("float" . "ANYNUM")
  ("time_type" . "ANYNUM")
  ("rate_type" . "ANYNUM")
  ("sample_type" . "ANYNUM")
  ("sound_type" . "SOUND")
  ("char *" . "STRING")
  ("LVAL" . "ANY")
  ("int" . "LONG")
  ("long" . "LONG")
  ("boolean" . "BOOLEAN")
))
  

(defun write-header (alg stream)
;;  (format stream "sound_type snd_make_~A();~%" (get-slot alg 'name))
    (let ((arguments (get-slot alg 'arguments))
      (name (get-slot alg 'name))
      (lisp-name (get-slot alg 'lispname)))
       (cond ((null lisp-name) (setf lisp-name name)))
       (format stream "sound_type snd_make_~A" name)
       (write-ansi-prototype-list stream "" arguments)
       (format stream ";~%")

       ; write the xlisp interface routine
       (format stream "sound_type snd_~A" name)
       (write-ansi-prototype-list stream "" arguments)
       (format stream ";~%")

       ; write the type specification for intgen
       (format stream "    /* LISP: (snd-~A" lisp-name)
       (dolist (arg arguments)
         (let ((xltype (assoc (car arg) c-to-xlisp-type :test #'equal)))
           (cond ((null xltype)
                  (error "couldn't translate c-type" (car arg))))
           (format stream " ~A" (cdr xltype))))
       (format stream ") */~%")))


;;**********
;; write-typedef -- compile the suspension type definition
;;
;; Inputs:
;;      alg - the algorithm specification
;;      stream - stream to which to write it
;; Effect:
;;      typedef struct NAME_susp_struct {
;;              ...
;;      } NAME_susp_node, *NAME_susp_type;
;;
;;  A side-effect of write-typedef is the initialization
;;  of slot xlisp-pointers in alg.  This is used later by
;;  write-mark to generate the garbage collection mark routine.
;;**********

(defun write-typedef (alg stream)
  (let (arg-type args interpolation-list sound-names arg
    (alg-name (get-slot alg 'name))
    name xlisp-pointers
    (state-list (get-slot alg 'state))
    (logical-stop (car (get-slot alg 'logical-stop)))
    (terminate (car (get-slot alg 'terminate))))
    ;----------------------------
    ;  typedef struct NAME_susp_strct {
     ;     snd_susp_node susp;
    ;----------------------------
    (format stream "~%~%typedef struct ~A_susp_struct {~%~A~%"
    alg-name "    snd_susp_node susp;")

    ; go through interpolation list:
    ;   NONE means use each sample
    ;   INTERP means interpolate between samples
    ;   RAMP means do ramp generation between samples
    ;   NIL means this is not a signal

    (setf interpolation-list (get-slot alg 'interpolation-list))
    (setf sound-names (get-slot alg 'sound-names))

    ; declare started flag if there is a ramp or interp signal anywhere
    (cond ((any-ramp-or-interp-in interpolation-list)
           ;---------------------
       ; INTERP/RAMP:
       ;        boolean started;
           ;---------------------
       (format stream "    boolean started;~%")))

    (display "in translate.lsp" 
         terminate alg (terminate-check-needed terminate alg))
    (cond ((terminate-check-needed terminate alg)
       ;----------------
       ; int64_t terminate_cnt;
       ;----------------
       (format stream "    int64_t terminate_cnt;~%")))

    (cond ((logical-stop-check-needed logical-stop)
       ;----------------
       ; boolean logically_stopped;
       ;----------------
       (format stream
        "    boolean logically_stopped;~%")))

    ; each sound argument has a variety of ways it might be
    ; interpolated.  These are stored on interpolation-list, and union-of-nth
     ; is used to gather all the interpolation styles that must be supported 
     ; for a given signal - we then declare whatever state is necessary for
     ; each possible interpolation
    (dotimes (n (length (get alg 'sound-args)))
      (let ((interpolation (union-of-nth interpolation-list n)))
    (setf name (nth n sound-names))     ; get name of signal
    ;------------------------
    ;   sound_type NAMEi;
    ;   int  NAME_cnt;
    ;   sample_block_values_type NAME_ptr;
    ;------------------------
    (format stream "    sound_type ~A;~%" name)
    (format stream "    int ~A_cnt;~%" name)
    (format stream "    sample_block_values_type ~A_ptr;~%" name)
    (cond ((or (member 'INTERP interpolation)
           (member 'RAMP interpolation))
           ;-----------------
           ;  /* support for interpolation of NAMEi */
           ;-----------------
           (format stream 
        "~%    /* support for interpolation of ~A */~%" name)

           ;-----------------
           ;  sample_type NAME_x1_sample;
           ;-----------------
           (format stream "    sample_type ~A_x1_sample;~%" name)

           ;-----------------
           ; double NAME_pHaSe;
           ; double NAME_pHaSe_iNcR;
           ;-----------------
           (format stream "    double ~A_pHaSe;~%" name)
           (format stream "    double ~A_pHaSe_iNcR;~%" name)))

    (cond ((member 'RAMP interpolation)
           ;-----------------
           ; RAMP:
           ;    /* support for ramp between samples of NAME */
           ;    double output_per_NAME;
           ; int64_t NAME_n;
           ;-----------------
           (format stream
        "~%    /* support for ramp between samples of ~A */~%" name)
           (format stream "    double output_per_~A;~%" name)
           (format stream "    int64_t ~A_n;~%" name) ))))

    ;----------------------------
    ; STATE
    ;   TYPEi VARNAMEi ;
    ;----------------------------
    ;; now write state variables
    ;; (STATE (s1) (s2)... (sn) )
    ;; each (si) is of the form
    ;;          ("type" "varname" "?" [TEMP])
    (cond (state-list (format stream "~%")))
    (dolist (state state-list)
      (cond ((equal "LVAL" (car state))
         (push (cadr state) xlisp-pointers)))
      (cond ((and (cdddr state)
          (cadddr state)
          (eq (cadddr state) 'TEMP))
         ; no field allocated for local/temp variables
        )
         (t
         (let ((sep (space-if-no-trailing-star (car state))))
           (format stream "    ~A~A~A;~%" 
        (car state) sep (cadr state))))))
    (put-slot alg xlisp-pointers 'xlisp-pointers)

    ;----------------------------
    ;   } ALG-NAME_susp_node, *ALG-NAME_susp_type;
    ;----------------------------
    (format stream "} ~A_susp_node, *~A_susp_type;~%" alg-name alg-name)))

(print 'end)
