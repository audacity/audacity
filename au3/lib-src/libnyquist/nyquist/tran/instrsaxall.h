sound_type snd_make_sax_all(double freq, sound_type breath_env, sound_type freq_env, double vibrato_freq, double vibrato_gain, sound_type reed_stiffness, sound_type noise_env, sound_type blow_pos, sound_type reed_table_offset, rate_type sr);
sound_type snd_sax_all(double freq, sound_type breath_env, sound_type freq_env, double vibrato_freq, double vibrato_gain, sound_type reed_stiffness, sound_type noise_env, sound_type blow_pos, sound_type reed_table_offset, rate_type sr);
    /* LISP: (snd-sax_all ANYNUM SOUND SOUND ANYNUM ANYNUM SOUND SOUND SOUND SOUND ANYNUM) */
#define SAX_CONTROL_CHANGE_CONST 128
