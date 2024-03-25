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
    (defun seq-value2 (e) (nth 5 e))
    (setfn seq-pitch seq-value1) ; pitch of a note
    (setfn seq-control seq-value1) ; control number of a control change
    (setfn seq-program seq-value2) ; program number of a program change
    (setfn seq-bend seq-value2) ; pitch bend amount
    (setfn seq-touch seq-value2) ; aftertouch amount
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
#define SEQ_CTRL 11
/* LISP-SRC: (setf seq-ctrl-tag 11) */
#define SEQ_PRGM 12
/* LISP-SRC: (setf seq-prgm-tag 12) */
#define SEQ_CPRESS 13
/* LISP-SRC: (setf seq-cpress-tag 13) */
#define SEQ_BEND 14
/* LISP-SRC: (setf seq-bend-tag 14) */

void seq_insert_note(seq_type seq, time_type ntime, int nline, int chan,
                     int pitch, time_type dur, int loud);
/* LISP: (SEQ-INSERT-NOTE SEQ LONG LONG LONG LONG LONG LONG) */

void seq_insert_ctrl(seq_type seq, long time, long line, long ctrltype,
                     long chan, long ctrlnum, long value);
/* LISP: (SEQ-INSERT-CTRL SEQ LONG LONG LONG LONG LONG LONG) */

void seq_xlwrite_smf(seq_type seq, LVAL outfile);
/* LISP: (SEQ-WRITE-SMF SEQ ANY) */
 
