/* moxc.h -- functions exported by moxie.c */
/* Copyright 1989 Carnegie Mellon University */

#define maxparms 8

extern timebase_type timebase;
extern time_type eventtime, virttime;
extern int debug;
extern int mididecode;
extern int moxcdone;

void    catchup(void);
void    callallcancel(void);
#ifdef DOTS_FOR_ARGS
void    cause(delay_type delay, void (*fn)(call_args_type), call_args_type p);
void    causepri(delay_type delay, int pri, void (*fn)(call_args_type), call_args_type p);
#else
THIS CODE IS OBSOLETE
void    cause();
void	causepri();
#endif
void    m_rest(time_type time);
void    m_restuntil(time_type time);
void    quit(void);
boolean moxcinit(int argc, char * argv[]);
void    moxcrun(void);
void    moxcwait(time_type dateoftimeout);

void asciievent(char k);
void bendchange(int chan, int value);
void coda(void);
void ctrlchange(int chan, int ctrl, int value);
void keydown(int chan, int key, int vel);
void keyup(int chan, int key);
void mainscore(void);
void midievent(byte midi_data[4]);
void peddown(int chan);
void pedup(int chan);
void prgmchange(int chan, int prgm);
void touchchange(int chan, int value);
#ifdef AMIGA
void buttonchange(int number, int value);
void propchange(int number, int value);
#endif
void sysex(void);
