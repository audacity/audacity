/* handlers.c -- null handlers to avoid link errors due to callouts in moxc.c */

#include "musiprog.h"

char *app_syntax = "";

/* note -- a simple way to make a midi note on channel 1 */
/**/
void note(int pitch, int dur)
{
}

/* asciievent -- ascii event handler */
/**/
void asciievent(char c)
{
}


/* keyup -- key up handler */
/**/
void keyup(int c, int k)
{
    /* insert key up actions here */
}


/* keydown -- key down handler */
/**/
void keydown(int c, int k, int v)
{
    /* insert key down actions here */
}


/* midievent -- handle a midi message */
/**/
void midievent(midi_data)
  byte midi_data[4];
{
    /* this is only called if mididecode is false so */
    /* you can assume this function is never called  */
}


/* prgmchange -- program change handler */
/**/
void prgmchange(int chan, int value)
{
    /* insert program change actions here */
}


/* bendchange -- pitch bend handler */
/**/
void bendchange(int chan, int value)
{
    /* insert pitchbend actions here */
}


/* ctrlchange -- control change handler */
/**/
void ctrlchange(int chan, int ctrl, int value)
{
    /* insert control change actions here */
}


/* peddown -- pedal down handler */
/**/
void peddown(int c)
{
    /* insert pedal down actions here */
    /* the following default action invokes your control change handler */
    ctrlchange(c, SUSTAIN, 127);
}


/* pedup -- pedal up handler */
/**/
void pedup(int c)
{
    /* insert pedal up actions here */
    /* the following default action invokes your control change handler */
    ctrlchange(c, SUSTAIN, 0);
}


/* touchchange -- after touch handler */
/**/
void touchchange(int chan, int value)
{
    /* insert after touch actions here */
}


void sysex(void)
{
}


/*
 * NOTE: this is called just before closing down the midi interface.
 */
void coda(void)
{
}


#ifdef AMIGA
void buttonchange(int number, int value)
{
}

void propchange(int number, int value)
{
        /* insert propchange actions here */
}
#endif
