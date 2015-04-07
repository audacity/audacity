sound_type snd_make_clarinet_all(double freq, sound_type breath_env, sound_type freq_env, double vibrato_freq, double vibrato_gain, sound_type reed_stiffness, sound_type noise_env, rate_type sr);
sound_type snd_clarinet_all(double freq, sound_type breath_env, sound_type freq_env, double vibrato_freq, double vibrato_gain, sound_type reed_stiffness, sound_type noise_env, rate_type sr);
    /* LISP: (snd-clarinet_all ANYNUM SOUND SOUND ANYNUM ANYNUM SOUND SOUND ANYNUM) */
#define CLAR_CONTROL_CHANGE_CONST 128
