/* seqinterf.h -- interface to sequence data type for XLISP */

boolean seq_next(seq_type seq);	/* LISP: (SEQ-NEXT SEQ) */
void seq_get(seq_type seq, long *eventtype, long *time, long *line, long *chan, 
    long *value1, long *value2, long *dur);
    /* LISP: (SEQ-GET SEQ LONG^ LONG^ LONG^ LONG^ LONG^ LONG^ LONG^) */
/* LISP-SRC:
    (setfn seq-tag first)
    (setfn seq-time second)
    (setfn seq-line third)
    (setfn seq-channel fourth)
    (defun seq-value1 (e) (nth 4 e))
    (setfn seq-pitch seq-value1) ; pitch of a note
    (setfn seq-control seq-value1) ; control number of a control change
    (setfn seq-program seq-value1) ; program number of a program change
    (setfn seq-bend seq-value1) ; pitch bend amount
    (setfn seq-touch seq-value1) ; aftertouch amount
    (defun seq-value2 (e) (nth 5 e))
    (setfn seq-velocity seq-value2) ; velocity of a note
    (setfn seq-value seq-value2) ; value of a control change
    (defun seq-duration (e) (nth 6 e))
    */
#define SEQ_DONE 0
/* LISP-SRC: (setf seq-done-tag 0) */
#define SEQ_OTHER 1
/* LISP-SRC: (setf seq-other-tag 1) */
#define SEQ_NOTE 2
/* LISP-SRC: (setf seq-note-tag 2) */
#define SEQ_CTRL 3
/* LISP-SRC: (setf seq-ctrl-tag 3) */
#define SEQ_PRGM 4
/* LISP-SRC: (setf seq-prgm-tag 4) */
#define SEQ_TOUCH 5
/* LISP-SRC: (setf seq-touch-tag 5) */
#define SEQ_BEND 6
/* LISP-SRC: (setf seq-bend-tag 6) */

void seq_xlwrite_smf(seq_type seq, LVAL outfile);
/* LISP: (SEQ-WRITE-SMF SEQ ANY) */
 
