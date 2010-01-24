(expand 50)

(load "../runtime/xlinit.lsp")

(load "../runtime/misc.lsp")

;; set to T to get interpolation within inner loops
(setf *INLINE-INTERPOLATION* nil)

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
