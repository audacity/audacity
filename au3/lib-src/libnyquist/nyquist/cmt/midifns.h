/* midifns.h -- definitions for users of midifns.c */

/*****************************************************************************
*           Change Log
*  Date     | Change
*-----------+-----------------------------------------------------------------
*  5-Mar-92 | GWL : insert definitions and logs from JMN's mpu.h
*                   for LATTICE322, only variable type in prototypes
* 28-Apr-03 |  DM : random() is now named cmtrand() to avoid conflicts
*****************************************************************************/

#ifndef _MIDIFNS_H_
#define _MIDIFNS_H_

/* declaration types */

typedef unsigned long time_type;
typedef long sgnd_time_type;

/* Maximum time value: */
#define MAXTIME 0xFFFFFFFFL

#define delay_type      long

/* controller numbers */

#define MODWHEEL        1
#define BREATH          2
#define FOOT            4
#define PORTARATE       5
#define VOLUME          7
#define SUSTAIN         64
#define PORTASWITCH     65

#include "midierr.h"

extern char *midifns_syntax;

/* support for allocating sysex buffer - examples in mm.c & exget.c */
#ifdef DOS
#define midibuff_alloc(size) (byte huge *) halloc(size, 1)
#endif
#ifndef midibuff_alloc
#define midibuff_alloc (byte *) MALLOC
#endif

/* DMH: from mpu.h -- definitions for users of mpu.c */

#ifdef OLD_PROTOTYPES

void    eventwait();
void    exclusive(boolean);
boolean getbuf(boolean, unsigned char * );
long	get_excl();
boolean getxbuf();
boolean testxbuf();
short   getkey(boolean);
ulong   gettime(void);          /*DMH: note- now unsigned*/
void    l_rest(long);
void    l_restuntil(long);
void    metronome(boolean);
void    midi_bend(short,short);
boolean midi_buffer(byte * , ulong);
void    midi_cont(boolean);
void    midi_clock();
void    midi_ctrl(short, short, short);
void    midi_exclusive(unsigned char * );
void    midi_note(short, short, short);
void    midi_program(short, short);
void    midi_real();
void    midi_start();
void    midi_stop();
#ifdef AMIGA
/* MIDI_THRU defined means that it is really implemented. */
#define MIDI_THRU
#endif
void    midi_thru();/*boolean onflag*/
void    midi_touch(short, short);
void    midi_write();
void    musicinit();
short	  cmtrand(short, short);
void    read_tuning();/*char *filename*/
void    settime();
void    synth_init();/*void*/
void    timereset();
void    trace();
void    tracemidi();
boolean is_exclusive(void);
unsigned char get_exclusive(void);

#else

void alloff(void);
void eventwait(long timeout);
void exclusive(boolean onflag);
long get_excl(byte *buffer, long len);
boolean getbuf(boolean waitflag, unsigned char * p);
short getkey(boolean waitflag);
ulong gettime(void);
void l_rest(long time);
void l_restuntil(long time);
void metronome(boolean onflag);
void midi_bend(int channel, int value);
boolean midi_buffer(byte *buffer, ulong size);
void midi_clock(void);
void midi_cont(boolean onflag);
void midi_ctrl(int channel, int control, int value);
void midi_exclusive(unsigned char *msg);
void midi_flush(void);
void midi_note(int channel, int pitch, int velocity);
void midi_program(int channel, int program);
void midi_real(boolean onflag);
void midi_start(void);
void midi_stop(void);
void midi_thru(boolean onflag);
void midi_touch(int channel, int value);
void read_tuning(char *filename);
void midi_write(int n, int port, unsigned char c1, unsigned char c2, unsigned char c3);
void midi_write_trace(int n, int port,
              unsigned char c1, unsigned char c2, unsigned char c3);
void musicinit(void);
void settime(time_type newtime);
void timereset(void);
void trace(boolean flag);
void tracemidi(boolean flag);


boolean check_midi(void);
#endif  /* ifdef OLD_PROTOTYPES */


#ifdef AMIGA
byte    *head_of_excl();
byte    *tail_of_excl();
#endif

#endif /* _MIDIFNS_H_ */
