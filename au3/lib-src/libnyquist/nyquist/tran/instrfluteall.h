sound_type snd_make_flute_all(double freq, sound_type breath_env, sound_type freq_env, double vibrato_freq, double vibrato_gain, sound_type jet_delay, sound_type noise_env, rate_type sr);
sound_type snd_flute_all(double freq, sound_type breath_env, sound_type freq_env, double vibrato_freq, double vibrato_gain, sound_type jet_delay, sound_type noise_env, rate_type sr);
    /* LISP: (snd-flute_all ANYNUM SOUND SOUND ANYNUM ANYNUM SOUND SOUND ANYNUM) */
#define FLUTE_CONTROL_CHANGE_CONST 128
