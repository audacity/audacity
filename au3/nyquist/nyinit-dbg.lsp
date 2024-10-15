(expand 5)

(load "xlinit.lsp" :verbose NIL)
(setf *gc-flag* nil)
(load "misc.lsp" :verbose NIL)
(load "evalenv.lsp" :verbose NIL)
(load "printrec.lsp" :verbose NIL)

(load "sndfnint.lsp" :verbose NIL)
(load "seqfnint.lsp" :verbose NIL)

(load "velocity.lsp" :verbose NIL) ; linear-to-vel etc
(load "nyquist-dbg.lsp" :verbose NIL)
(load "compress.lsp" :verbose NIL)

(load "system.lsp" :verbose NIL)

(load "seqmidi.lsp" :verbose NIL)
(load "nyqmisc.lsp" :verbose NIL)
(load "stk.lsp" :verbose NIL)
(load "envelopes.lsp" :verbose NIL)
(load "equalizer.lsp" :verbose NIL)
(load "xm.lsp" :verbose NIL)
(load "sal.lsp" :verbose NIL)

;; set to T to get ANSI headers and NIL to get antique headers
(setf *ANSI* NIL)

;; set to T to generate tracing code, NIL to disable tracing code
(setf *WATCH* NIL)

(format t "~%Nyquist -- A Language for Sound Synthesis and Composition~%")
(format t "    Copyright (c) 1991,1992,1995,2007-2012 by Roger B. Dannenberg~%")
(format t "    Version 3.10~%~%")

;(setf *gc-flag* t)


