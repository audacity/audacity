(expand 50)

(load "../runtime/xlinit.lsp")

(load "../runtime/misc.lsp")

;; set to T to get interpolation within inner loops,
;; inline interpolation adds about 10% to code size and
;; should increase performance substantially when using
;; "light" primitives such as time-varying filters and
;; multiplies with envelopes
(setf *INLINE-INTERPOLATION* T)

;; set to T to get ANSI headers and NIL to get antique headers
(setf *ANSI* NIL)

;; set to T to generate tracing code, NIL to disable tracing code
(setf *WATCH* NIL)

(load "translate")
(load "writesusp")
(load "writemake")
(load "writetoss")
(load "innerloop")

(setf *gc-flag* t)

(setf *watch* nil)

(print "Assuming you ran Nyquist in nyquist/tran, type (m) to regenerate all source code from .alg files")
