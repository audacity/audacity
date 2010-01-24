sound_type snd_make_sine(time_type t0, double hz, rate_type sr, time_type d);
sound_type snd_sine(time_type t0, double hz, rate_type sr, time_type d);
    /* LISP: (snd-sine ANYNUM ANYNUM ANYNUM ANYNUM) */
#define SINE_TABLE_LEN 2048
#define SINE_TABLE_MASK 0x7FFFFFFF
#define SINE_TABLE_SHIFT 20
void sine_init();
extern sample_type sine_table[];
