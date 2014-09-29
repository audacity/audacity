/* this typedef goes here because it is needed by multiseq.c */

typedef struct add_susp_struct {
    snd_susp_node               susp;
    boolean                     started;
    int                         terminate_bits;
    long                        terminate_cnt;
    int                         logical_stop_bits;
    boolean                     logically_stopped;
    sound_type                  s1;
    long                        s1_cnt;
    sample_block_type           s1_bptr;        /* block pointer */
    sample_block_values_type    s1_ptr;
    sound_type                  s2;
    long                        s2_cnt;
    sample_block_type           s2_bptr;        /* block pointer */
    sample_block_values_type    s2_ptr;

#ifdef UPSAMPLECODE
    /* support for interpolation of s2 */
    sample_type s2_x1_sample;
    double s2_phase;
    double s2_phase_incr;

    /* support for ramp between samples of s2 */
    double output_per_s2;
#endif
    /* pointer used to synchronize adds in multiseq */
    struct multiseq_struct      *multiseq;
    long                        s1_prepend; /* offset to susp.current */
} add_susp_node, *add_susp_type;

sound_type snd_make_add();
sound_type snd_add();
    /* LISP: (SND-ADD SOUND SOUND) */

/* we export these for seq.c and multiseq.c */ 
void add_zero_fill_nn_fetch(add_susp_type susp, snd_list_type snd_list);
void add_s1_s2_nn_fetch(add_susp_type susp, snd_list_type snd_list);
void add_s2_nn_fetch(add_susp_type susp, snd_list_type snd_list);
void add_s1_nn_fetch(add_susp_type susp, snd_list_type snd_list);
void add_mark(add_susp_type susp);
void add_print_tree(add_susp_type susp, int n);
void add_free(add_susp_type susp);
