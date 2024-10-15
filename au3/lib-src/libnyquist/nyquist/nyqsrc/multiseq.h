/* this typedef goes here because it is needed by add */

typedef struct multiseq_struct {
    int			not_logically_stopped_cnt;
    int			nchans;
    /* greatest lower bound on logical stop time: */
    time_type		horizon;
    /* lowest time corresp to sample count on a snd_list: */
    time_type	        low_water; 
    snd_list_type	*chans;
    time_type		t0;
    rate_type		sr;
    LVAL closure;
} multiseq_node, *multiseq_type;



LVAL snd_multiseq(LVAL s1, LVAL closure);
    /* LISP: (SND-MULTISEQ ANY ANY) */
