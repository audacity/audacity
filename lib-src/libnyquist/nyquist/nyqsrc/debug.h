#ifndef DEBUG_H

#ifdef PARTIAL_DECLARATIONS
typedef struct partial_susp_struct {
    snd_susp_node susp;
    boolean started;
    long terminate_cnt;
    long logical_stop_cnt;
    boolean logically_stopped;
    sound_type env;
    long env_cnt;
    sample_block_values_type env_ptr;

    /* support for interpolation of env */
    sample_type env_x1_sample;
    double env_pHaSe;
    double env_pHaSe_iNcR;

    /* support for ramp between samples of env */
    double output_per_env;
    long env_n;

    long phase;
    long ph_incr;
    double max_diff;
    double prev_output;
} partial_susp_node, *partial_susp_type;
#endif

extern sound_type watch_table_sound;

void print_sound_type(sound_type s);
void print_sample_block_type(char *label, 
       sample_block_type sampblock, int len);
void watch_susp(snd_susp_type s);
void watch_sound(sound_type s);
void snd_list_debug(snd_list_type snd_list, char *s);
void watch_snd_list(snd_list_type s);
void dbg_mem_allocated(void *p, char *who);
void dbg_mem_freed(void *p, char *who);
void dbg_mem_print(char *msg, void *p);
/* #define TRACESNDGC */

#define DEBUG_H
#endif
