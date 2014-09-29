/* midibuff.h -- defines the size of the midi input buffer */

/* midi input buffer */
/* WARNING: BUFF_SIZE must be a power of 2 so we can use masking to wrap */
#define EVENT_COUNT 128
#define EVENT_SIZE 4
#define BUFF_SIZE (EVENT_COUNT * EVENT_SIZE)
#define BUFF_MASK (BUFF_SIZE - 1)

#ifdef WINDOWS
#define huge 
#endif

extern byte huge *xbuff;	/* application-supplied sysex buffer */
extern long xbufmask;	    /* mask for circular buffer */
extern long xbufhead;       /* buffer head and tail offsets */
extern long xbuftail;
extern int midi_error;
/* midi input buffer */
/* data buffer, declared long to get 32-bit alignment: */
extern long buff[BUFF_SIZE/4];
extern int buffhead;     /* buffer head and tail pointers */
extern int bufftail;
