/* midierr.h -- error codes */

#define NESTERR 1 	/* nested interrupts */
#define BUFFOVFL 2	/* input buffer overflow */
#define CMDERR 3	/* unknown command from mpu-401 */
#define TIMEOUTERR 4	/* interface timeout */
#define MSGERR 5	/* invalid message */
#define SYSEXOVFL 6	/* sysex buffer overflow */
#define MEMERR 7	/* internal out of memory */
#define RECVERR 8	/* receive error or device buffer overflow */
#define MIDIMGRERR 9	/* error reported by midi manager (Macintosh) */

#define SYSEX_ERR ((1<<RECVERR) | (1<<MEMERR) | (1<<SYSEXOVFL))
extern short midi_error_flags;

void midi_show_errors(void);

