/* seqinterf.c -- interface to sequence data type for XLISP */

#include "switches.h"
#include "xlisp.h"
#include "stdio.h"
#include "cext.h"
#include "userio.h"
#include "midifns.h"
#include "timebase.h"
#include "moxc.h"
#include "seq.h"
#include "seqinterf.h"
#include "seqmwrite.h"

/* seq_next -- advance to the next event, return TRUE if found */
/**/
boolean seq_next(seq_type seq)
{
    if (seq->current) {
    seq->current = seq->current->next;
    }
    return seq->current != NULL;
}


/* seq_get -- get event data for the current event */
/**/
void seq_get(seq_type seq, long *eventtype, long *ntime, long *line, long *chan,
             long *value1, long *value2, long *dur)
{
    event_type ev = seq->current;
    if (!ev) *eventtype = SEQ_DONE;
    else if (is_note(ev)) {
        if (ev->value != NO_PITCH) {
            *eventtype = SEQ_NOTE;
            *ntime = ev->ntime;
            *line = ev->nline;
            *chan = vc_voice(ev->nvoice);
            *value1 = ev->value;
            *value2 = ev->u.note.ndur & 0xFF;
            *dur = ev->u.note.ndur >> 8;
        } else {
            *eventtype = SEQ_OTHER;
        }
    } else {
        *eventtype = SEQ_CTRL;
        *ntime = ev->ntime;
        *line = ev->nline;
        *chan = vc_voice(ev->nvoice);
        *value2 = ev->value;
        
        switch (vc_ctrl(ev->nvoice)) {
          case PSWITCH_CTRL:
            *value1 = PORTASWITCH;
            break;
          case MODWHEEL_CTRL:
            *value1 = MODWHEEL;
            break;
          case TOUCH_CTRL:
              *eventtype = SEQ_TOUCH;
            *value1 = ev->value;
            break;
          case VOLUME_CTRL:
            *value1 = VOLUME;
            break;
          case BEND_CTRL:
              *eventtype = SEQ_BEND;
            *value1 = ev->value << 6;
            break;
          case PROGRAM_CTRL:
            *eventtype = SEQ_PRGM;
            *value1 = ev->value + 1;
            break;
          case ESC_CTRL:
            switch (ev->value) {
              case CALL_VALUE:
              case CLOCK_VALUE:
              case MACRO_VALUE:
              case CTRLRAMP_VALUE:
              case DEFRAMP_VALUE:
              case SETI_VALUE:
                *eventtype = SEQ_OTHER;
                break;
              case MACCTRL_VALUE:
                *value1 = ev->u.macctrl.ctrl_number;
                *value2 = ev->u.macctrl.value;
                break;
              default:
                xlabort("unexpected ESC_CTRL value\n");
                break;
            }
            break;
          default:
            xlabort("unexpected seq data\n");
            break;
        }
    }
}

/* seq_xlwrite_smf -- invoke seq_write_smf and mark file as closed */
void seq_xlwrite_smf(seq_type seq, LVAL outfile)
{
    if (streamp(outfile)) {
        if (getfile(outfile) == NULL) {
            xlfail("file for seq_write_smf not open");
        }
        seq_write_smf(seq, getfile(outfile));
        setfile(outfile, NULL); /* mark file as closed */
    } else {
        xlerror("seq_write_smf 2nd arg must be a STREAM", outfile);
    }
}
