sound_type snd_make_flute(double freq, sound_type breath_env, rate_type sr);
sound_type snd_flute(double freq, sound_type breath_env, rate_type sr);
    /* LISP: (snd-flute ANYNUM SOUND ANYNUM) */
#define FLUTE_CONTROL_CHANGE_CONST 128
