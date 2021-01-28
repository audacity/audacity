#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "tapf.h"

void tapf_free(snd_susp_type a_susp);


typedef struct tapf_susp_struct {
    snd_susp_node susp;
    boolean started;
    int64_t terminate_cnt;
    boolean logically_stopped;
    sound_type s1;
    int s1_cnt;
    sample_block_values_type s1_ptr;
    sound_type vardelay;
    int vardelay_cnt;
    sample_block_values_type vardelay_ptr;

    /* support for interpolation of vardelay */
    sample_type vardelay_x1_sample;
    double vardelay_pHaSe;
    double vardelay_pHaSe_iNcR;

    /* support for ramp between samples of vardelay */
    double output_per_vardelay;
    int64_t vardelay_n;

    double offset;
    double vdscale;
    long maxdelay;
    long bufflen;
    long index;
    sample_type *buffer;
} tapf_susp_node, *tapf_susp_type;


void tapf_nn_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    tapf_susp_type susp = (tapf_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double offset_reg;
    register double vdscale_reg;
    register long maxdelay_reg;
    register long bufflen_reg;
    register long index_reg;
    register sample_type * buffer_reg;
    register sample_block_values_type vardelay_ptr_reg;
    register sample_block_values_type s1_ptr_reg;
    falloc_sample_block(out, "tapf_nn_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the s1 input sample block: */
        susp_check_term_log_samples(s1, s1_ptr, s1_cnt);
        togo = min(togo, susp->s1_cnt);

        /* don't run past the vardelay input sample block: */
        susp_check_term_samples(vardelay, vardelay_ptr, vardelay_cnt);
        togo = min(togo, susp->vardelay_cnt);

        /* don't run past terminate time */
        if (susp->terminate_cnt != UNKNOWN &&
            susp->terminate_cnt <= susp->susp.current + cnt + togo) {
            togo = (int) (susp->terminate_cnt - (susp->susp.current + cnt));
            if (togo < 0) togo = 0;  /* avoids rounding errros */
            if (togo == 0) break;
        }


        /* don't run past logical stop time */
        if (!susp->logically_stopped && susp->susp.log_stop_cnt != UNKNOWN) {
            int64_t to_stop = susp->susp.log_stop_cnt - (susp->susp.current + cnt);
            /* break if to_stop == 0 (we're at the logical stop)
             * AND cnt > 0 (we're not at the beginning of the
             * output block).
             */
            if (to_stop < 0) to_stop = 0; /* avoids rounding errors */
            if (to_stop < togo) {
                if (to_stop == 0) {
                    if (cnt) {
                        togo = 0;
                        break;
                    } else /* keep togo as is: since cnt == 0, we
                            * can set the logical stop flag on this
                            * output block
                            */
                        susp->logically_stopped = true;
                } else /* limit togo so we can start a new
                        * block at the LST
                        */
                    togo = (int) to_stop;
            }
        }

        n = togo;
        offset_reg = susp->offset;
        vdscale_reg = susp->vdscale;
        maxdelay_reg = susp->maxdelay;
        bufflen_reg = susp->bufflen;
        index_reg = susp->index;
        buffer_reg = susp->buffer;
        vardelay_ptr_reg = susp->vardelay_ptr;
        s1_ptr_reg = susp->s1_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            long phase;
            phase = (long) (*vardelay_ptr_reg++ * vdscale_reg + offset_reg);
            /* now phase should give number of samples of delay */
            if (phase < 0) phase = 0;
            else if (phase > maxdelay_reg) phase = maxdelay_reg;
            phase = index_reg - phase;
            /* now phase is a location in the buffer_reg (before modulo) */
    
            /* Time out to update the buffer_reg:
             * this is a tricky buffer_reg: buffer_reg[0] == buffer_reg[bufflen_reg]
             * the logical length is bufflen_reg, but the actual length
             * is bufflen_reg + 1 to allow for a repeated sample at the
             * end. This allows for efficient interpolation.
             */ 
            buffer_reg[index_reg++] = *s1_ptr_reg++;
            if (index_reg >= bufflen_reg) {
                index_reg = 0;
            }
    
            /* back to the phase calculation: 
             * use conditional instead of modulo
             */
            if (phase < 0) phase += bufflen_reg;
            *out_ptr_reg++ = (sample_type) (buffer_reg[phase]);
        } while (--n); /* inner loop */

        susp->bufflen = bufflen_reg;
        susp->index = index_reg;
        /* using vardelay_ptr_reg is a bad idea on RS/6000: */
        susp->vardelay_ptr += togo;
        /* using s1_ptr_reg is a bad idea on RS/6000: */
        susp->s1_ptr += togo;
        out_ptr += togo;
        susp_took(s1_cnt, togo);
        susp_took(vardelay_cnt, togo);
        cnt += togo;
    } /* outer loop */

    /* test for termination */
    if (togo == 0 && cnt == 0) {
        snd_list_terminate(snd_list);
    } else {
        snd_list->block_len = cnt;
        susp->susp.current += cnt;
    }
    /* test for logical stop */
    if (susp->logically_stopped) {
        snd_list->logically_stopped = true;
    } else if (susp->susp.log_stop_cnt == susp->susp.current) {
        susp->logically_stopped = true;
    }
} /* tapf_nn_fetch */


void tapf_ni_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    tapf_susp_type susp = (tapf_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type vardelay_x2_sample;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double offset_reg;
    register double vdscale_reg;
    register long maxdelay_reg;
    register long bufflen_reg;
    register long index_reg;
    register sample_type * buffer_reg;
    register double vardelay_pHaSe_iNcR_rEg = susp->vardelay_pHaSe_iNcR;
    register double vardelay_pHaSe_ReG;
    register sample_type vardelay_x1_sample_reg;
    register sample_block_values_type s1_ptr_reg;
    falloc_sample_block(out, "tapf_ni_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
        susp->started = true;
        susp_check_term_samples(vardelay, vardelay_ptr, vardelay_cnt);
        susp->vardelay_cnt--;
        susp->vardelay_x1_sample = *(susp->vardelay_ptr);
    }

    susp_check_term_samples(vardelay, vardelay_ptr, vardelay_cnt);
    vardelay_x2_sample = *(susp->vardelay_ptr);

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the s1 input sample block: */
        susp_check_term_log_samples(s1, s1_ptr, s1_cnt);
        togo = min(togo, susp->s1_cnt);

        /* don't run past terminate time */
        if (susp->terminate_cnt != UNKNOWN &&
            susp->terminate_cnt <= susp->susp.current + cnt + togo) {
            togo = (int) (susp->terminate_cnt - (susp->susp.current + cnt));
            if (togo < 0) togo = 0;  /* avoids rounding errros */
            if (togo == 0) break;
        }


        /* don't run past logical stop time */
        if (!susp->logically_stopped && susp->susp.log_stop_cnt != UNKNOWN) {
            int64_t to_stop = susp->susp.log_stop_cnt - (susp->susp.current + cnt);
            /* break if to_stop == 0 (we're at the logical stop)
             * AND cnt > 0 (we're not at the beginning of the
             * output block).
             */
            if (to_stop < 0) to_stop = 0; /* avoids rounding errors */
            if (to_stop < togo) {
                if (to_stop == 0) {
                    if (cnt) {
                        togo = 0;
                        break;
                    } else /* keep togo as is: since cnt == 0, we
                            * can set the logical stop flag on this
                            * output block
                            */
                        susp->logically_stopped = true;
                } else /* limit togo so we can start a new
                        * block at the LST
                        */
                    togo = (int) to_stop;
            }
        }

        n = togo;
        offset_reg = susp->offset;
        vdscale_reg = susp->vdscale;
        maxdelay_reg = susp->maxdelay;
        bufflen_reg = susp->bufflen;
        index_reg = susp->index;
        buffer_reg = susp->buffer;
        vardelay_pHaSe_ReG = susp->vardelay_pHaSe;
        vardelay_x1_sample_reg = susp->vardelay_x1_sample;
        s1_ptr_reg = susp->s1_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            long phase;
            if (vardelay_pHaSe_ReG >= 1.0) {
                vardelay_x1_sample_reg = vardelay_x2_sample;
                /* pick up next sample as vardelay_x2_sample: */
                susp->vardelay_ptr++;
                susp_took(vardelay_cnt, 1);
                vardelay_pHaSe_ReG -= 1.0;
                susp_check_term_samples_break(vardelay, vardelay_ptr, vardelay_cnt, vardelay_x2_sample);
            }
            phase = (long) (
                (vardelay_x1_sample_reg * (1 - vardelay_pHaSe_ReG) + vardelay_x2_sample * vardelay_pHaSe_ReG) * vdscale_reg + offset_reg);
            /* now phase should give number of samples of delay */
            if (phase < 0) phase = 0;
            else if (phase > maxdelay_reg) phase = maxdelay_reg;
            phase = index_reg - phase;
            /* now phase is a location in the buffer_reg (before modulo) */
    
            /* Time out to update the buffer_reg:
             * this is a tricky buffer_reg: buffer_reg[0] == buffer_reg[bufflen_reg]
             * the logical length is bufflen_reg, but the actual length
             * is bufflen_reg + 1 to allow for a repeated sample at the
             * end. This allows for efficient interpolation.
             */ 
            buffer_reg[index_reg++] = *s1_ptr_reg++;
            if (index_reg >= bufflen_reg) {
                index_reg = 0;
            }
    
            /* back to the phase calculation: 
             * use conditional instead of modulo
             */
            if (phase < 0) phase += bufflen_reg;
            *out_ptr_reg++ = (sample_type) (buffer_reg[phase]);
            vardelay_pHaSe_ReG += vardelay_pHaSe_iNcR_rEg;
        } while (--n); /* inner loop */

        togo -= n;
        susp->bufflen = bufflen_reg;
        susp->index = index_reg;
        susp->vardelay_pHaSe = vardelay_pHaSe_ReG;
        susp->vardelay_x1_sample = vardelay_x1_sample_reg;
        /* using s1_ptr_reg is a bad idea on RS/6000: */
        susp->s1_ptr += togo;
        out_ptr += togo;
        susp_took(s1_cnt, togo);
        cnt += togo;
    } /* outer loop */

    /* test for termination */
    if (togo == 0 && cnt == 0) {
        snd_list_terminate(snd_list);
    } else {
        snd_list->block_len = cnt;
        susp->susp.current += cnt;
    }
    /* test for logical stop */
    if (susp->logically_stopped) {
        snd_list->logically_stopped = true;
    } else if (susp->susp.log_stop_cnt == susp->susp.current) {
        susp->logically_stopped = true;
    }
} /* tapf_ni_fetch */


void tapf_nr_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    tapf_susp_type susp = (tapf_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type vardelay_DeLtA;
    sample_type vardelay_val;
    sample_type vardelay_x2_sample;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double offset_reg;
    register double vdscale_reg;
    register long maxdelay_reg;
    register long bufflen_reg;
    register long index_reg;
    register sample_type * buffer_reg;
    register sample_block_values_type s1_ptr_reg;
    falloc_sample_block(out, "tapf_nr_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
        susp->started = true;
        susp->vardelay_pHaSe = 1.0;
    }

    susp_check_term_samples(vardelay, vardelay_ptr, vardelay_cnt);
    vardelay_x2_sample = *(susp->vardelay_ptr);

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the s1 input sample block: */
        susp_check_term_log_samples(s1, s1_ptr, s1_cnt);
        togo = min(togo, susp->s1_cnt);

        /* grab next vardelay_x2_sample when phase goes past 1.0; */
        /* we use vardelay_n (computed below) to avoid roundoff errors: */
        if (susp->vardelay_n <= 0) {
            susp->vardelay_x1_sample = vardelay_x2_sample;
            susp->vardelay_ptr++;
            susp_took(vardelay_cnt, 1);
            susp->vardelay_pHaSe -= 1.0;
            susp_check_term_samples(vardelay, vardelay_ptr, vardelay_cnt);
            vardelay_x2_sample = *(susp->vardelay_ptr);
            /* vardelay_n gets number of samples before phase exceeds 1.0: */
            susp->vardelay_n = (int64_t) ((1.0 - susp->vardelay_pHaSe) *
                                        susp->output_per_vardelay);
        }
        togo = (int) min(togo, susp->vardelay_n);
        vardelay_DeLtA = (sample_type) ((vardelay_x2_sample - susp->vardelay_x1_sample) * susp->vardelay_pHaSe_iNcR);
        vardelay_val = (sample_type) (susp->vardelay_x1_sample * (1.0 - susp->vardelay_pHaSe) +
                 vardelay_x2_sample * susp->vardelay_pHaSe);

        /* don't run past terminate time */
        if (susp->terminate_cnt != UNKNOWN &&
            susp->terminate_cnt <= susp->susp.current + cnt + togo) {
            togo = (int) (susp->terminate_cnt - (susp->susp.current + cnt));
            if (togo < 0) togo = 0;  /* avoids rounding errros */
            if (togo == 0) break;
        }


        /* don't run past logical stop time */
        if (!susp->logically_stopped && susp->susp.log_stop_cnt != UNKNOWN) {
            int64_t to_stop = susp->susp.log_stop_cnt - (susp->susp.current + cnt);
            /* break if to_stop == 0 (we're at the logical stop)
             * AND cnt > 0 (we're not at the beginning of the
             * output block).
             */
            if (to_stop < 0) to_stop = 0; /* avoids rounding errors */
            if (to_stop < togo) {
                if (to_stop == 0) {
                    if (cnt) {
                        togo = 0;
                        break;
                    } else /* keep togo as is: since cnt == 0, we
                            * can set the logical stop flag on this
                            * output block
                            */
                        susp->logically_stopped = true;
                } else /* limit togo so we can start a new
                        * block at the LST
                        */
                    togo = (int) to_stop;
            }
        }

        n = togo;
        offset_reg = susp->offset;
        vdscale_reg = susp->vdscale;
        maxdelay_reg = susp->maxdelay;
        bufflen_reg = susp->bufflen;
        index_reg = susp->index;
        buffer_reg = susp->buffer;
        s1_ptr_reg = susp->s1_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            long phase;
            phase = (long) (vardelay_val * vdscale_reg + offset_reg);
            /* now phase should give number of samples of delay */
            if (phase < 0) phase = 0;
            else if (phase > maxdelay_reg) phase = maxdelay_reg;
            phase = index_reg - phase;
            /* now phase is a location in the buffer_reg (before modulo) */
    
            /* Time out to update the buffer_reg:
             * this is a tricky buffer_reg: buffer_reg[0] == buffer_reg[bufflen_reg]
             * the logical length is bufflen_reg, but the actual length
             * is bufflen_reg + 1 to allow for a repeated sample at the
             * end. This allows for efficient interpolation.
             */ 
            buffer_reg[index_reg++] = *s1_ptr_reg++;
            if (index_reg >= bufflen_reg) {
                index_reg = 0;
            }
    
            /* back to the phase calculation: 
             * use conditional instead of modulo
             */
            if (phase < 0) phase += bufflen_reg;
            *out_ptr_reg++ = (sample_type) (buffer_reg[phase]);
            vardelay_val += vardelay_DeLtA;
        } while (--n); /* inner loop */

        susp->bufflen = bufflen_reg;
        susp->index = index_reg;
        /* using s1_ptr_reg is a bad idea on RS/6000: */
        susp->s1_ptr += togo;
        out_ptr += togo;
        susp_took(s1_cnt, togo);
        susp->vardelay_pHaSe += togo * susp->vardelay_pHaSe_iNcR;
        susp->vardelay_n -= togo;
        cnt += togo;
    } /* outer loop */

    /* test for termination */
    if (togo == 0 && cnt == 0) {
        snd_list_terminate(snd_list);
    } else {
        snd_list->block_len = cnt;
        susp->susp.current += cnt;
    }
    /* test for logical stop */
    if (susp->logically_stopped) {
        snd_list->logically_stopped = true;
    } else if (susp->susp.log_stop_cnt == susp->susp.current) {
        susp->logically_stopped = true;
    }
} /* tapf_nr_fetch */


void tapf_sn_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    tapf_susp_type susp = (tapf_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double offset_reg;
    register double vdscale_reg;
    register long maxdelay_reg;
    register long bufflen_reg;
    register long index_reg;
    register sample_type * buffer_reg;
    register sample_block_values_type vardelay_ptr_reg;
    register sample_type s1_scale_reg = susp->s1->scale;
    register sample_block_values_type s1_ptr_reg;
    falloc_sample_block(out, "tapf_sn_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the s1 input sample block: */
        susp_check_term_log_samples(s1, s1_ptr, s1_cnt);
        togo = min(togo, susp->s1_cnt);

        /* don't run past the vardelay input sample block: */
        susp_check_term_samples(vardelay, vardelay_ptr, vardelay_cnt);
        togo = min(togo, susp->vardelay_cnt);

        /* don't run past terminate time */
        if (susp->terminate_cnt != UNKNOWN &&
            susp->terminate_cnt <= susp->susp.current + cnt + togo) {
            togo = (int) (susp->terminate_cnt - (susp->susp.current + cnt));
            if (togo < 0) togo = 0;  /* avoids rounding errros */
            if (togo == 0) break;
        }


        /* don't run past logical stop time */
        if (!susp->logically_stopped && susp->susp.log_stop_cnt != UNKNOWN) {
            int64_t to_stop = susp->susp.log_stop_cnt - (susp->susp.current + cnt);
            /* break if to_stop == 0 (we're at the logical stop)
             * AND cnt > 0 (we're not at the beginning of the
             * output block).
             */
            if (to_stop < 0) to_stop = 0; /* avoids rounding errors */
            if (to_stop < togo) {
                if (to_stop == 0) {
                    if (cnt) {
                        togo = 0;
                        break;
                    } else /* keep togo as is: since cnt == 0, we
                            * can set the logical stop flag on this
                            * output block
                            */
                        susp->logically_stopped = true;
                } else /* limit togo so we can start a new
                        * block at the LST
                        */
                    togo = (int) to_stop;
            }
        }

        n = togo;
        offset_reg = susp->offset;
        vdscale_reg = susp->vdscale;
        maxdelay_reg = susp->maxdelay;
        bufflen_reg = susp->bufflen;
        index_reg = susp->index;
        buffer_reg = susp->buffer;
        vardelay_ptr_reg = susp->vardelay_ptr;
        s1_ptr_reg = susp->s1_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            long phase;
            phase = (long) (*vardelay_ptr_reg++ * vdscale_reg + offset_reg);
            /* now phase should give number of samples of delay */
            if (phase < 0) phase = 0;
            else if (phase > maxdelay_reg) phase = maxdelay_reg;
            phase = index_reg - phase;
            /* now phase is a location in the buffer_reg (before modulo) */
    
            /* Time out to update the buffer_reg:
             * this is a tricky buffer_reg: buffer_reg[0] == buffer_reg[bufflen_reg]
             * the logical length is bufflen_reg, but the actual length
             * is bufflen_reg + 1 to allow for a repeated sample at the
             * end. This allows for efficient interpolation.
             */ 
            buffer_reg[index_reg++] = (s1_scale_reg * *s1_ptr_reg++);
            if (index_reg >= bufflen_reg) {
                index_reg = 0;
            }
    
            /* back to the phase calculation: 
             * use conditional instead of modulo
             */
            if (phase < 0) phase += bufflen_reg;
            *out_ptr_reg++ = (sample_type) (buffer_reg[phase]);
        } while (--n); /* inner loop */

        susp->bufflen = bufflen_reg;
        susp->index = index_reg;
        /* using vardelay_ptr_reg is a bad idea on RS/6000: */
        susp->vardelay_ptr += togo;
        /* using s1_ptr_reg is a bad idea on RS/6000: */
        susp->s1_ptr += togo;
        out_ptr += togo;
        susp_took(s1_cnt, togo);
        susp_took(vardelay_cnt, togo);
        cnt += togo;
    } /* outer loop */

    /* test for termination */
    if (togo == 0 && cnt == 0) {
        snd_list_terminate(snd_list);
    } else {
        snd_list->block_len = cnt;
        susp->susp.current += cnt;
    }
    /* test for logical stop */
    if (susp->logically_stopped) {
        snd_list->logically_stopped = true;
    } else if (susp->susp.log_stop_cnt == susp->susp.current) {
        susp->logically_stopped = true;
    }
} /* tapf_sn_fetch */


void tapf_si_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    tapf_susp_type susp = (tapf_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type vardelay_x2_sample;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double offset_reg;
    register double vdscale_reg;
    register long maxdelay_reg;
    register long bufflen_reg;
    register long index_reg;
    register sample_type * buffer_reg;
    register double vardelay_pHaSe_iNcR_rEg = susp->vardelay_pHaSe_iNcR;
    register double vardelay_pHaSe_ReG;
    register sample_type vardelay_x1_sample_reg;
    register sample_type s1_scale_reg = susp->s1->scale;
    register sample_block_values_type s1_ptr_reg;
    falloc_sample_block(out, "tapf_si_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
        susp->started = true;
        susp_check_term_samples(vardelay, vardelay_ptr, vardelay_cnt);
        susp->vardelay_cnt--;
        susp->vardelay_x1_sample = *(susp->vardelay_ptr);
    }

    susp_check_term_samples(vardelay, vardelay_ptr, vardelay_cnt);
    vardelay_x2_sample = *(susp->vardelay_ptr);

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the s1 input sample block: */
        susp_check_term_log_samples(s1, s1_ptr, s1_cnt);
        togo = min(togo, susp->s1_cnt);

        /* don't run past terminate time */
        if (susp->terminate_cnt != UNKNOWN &&
            susp->terminate_cnt <= susp->susp.current + cnt + togo) {
            togo = (int) (susp->terminate_cnt - (susp->susp.current + cnt));
            if (togo < 0) togo = 0;  /* avoids rounding errros */
            if (togo == 0) break;
        }


        /* don't run past logical stop time */
        if (!susp->logically_stopped && susp->susp.log_stop_cnt != UNKNOWN) {
            int64_t to_stop = susp->susp.log_stop_cnt - (susp->susp.current + cnt);
            /* break if to_stop == 0 (we're at the logical stop)
             * AND cnt > 0 (we're not at the beginning of the
             * output block).
             */
            if (to_stop < 0) to_stop = 0; /* avoids rounding errors */
            if (to_stop < togo) {
                if (to_stop == 0) {
                    if (cnt) {
                        togo = 0;
                        break;
                    } else /* keep togo as is: since cnt == 0, we
                            * can set the logical stop flag on this
                            * output block
                            */
                        susp->logically_stopped = true;
                } else /* limit togo so we can start a new
                        * block at the LST
                        */
                    togo = (int) to_stop;
            }
        }

        n = togo;
        offset_reg = susp->offset;
        vdscale_reg = susp->vdscale;
        maxdelay_reg = susp->maxdelay;
        bufflen_reg = susp->bufflen;
        index_reg = susp->index;
        buffer_reg = susp->buffer;
        vardelay_pHaSe_ReG = susp->vardelay_pHaSe;
        vardelay_x1_sample_reg = susp->vardelay_x1_sample;
        s1_ptr_reg = susp->s1_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            long phase;
            if (vardelay_pHaSe_ReG >= 1.0) {
                vardelay_x1_sample_reg = vardelay_x2_sample;
                /* pick up next sample as vardelay_x2_sample: */
                susp->vardelay_ptr++;
                susp_took(vardelay_cnt, 1);
                vardelay_pHaSe_ReG -= 1.0;
                susp_check_term_samples_break(vardelay, vardelay_ptr, vardelay_cnt, vardelay_x2_sample);
            }
            phase = (long) (
                (vardelay_x1_sample_reg * (1 - vardelay_pHaSe_ReG) + vardelay_x2_sample * vardelay_pHaSe_ReG) * vdscale_reg + offset_reg);
            /* now phase should give number of samples of delay */
            if (phase < 0) phase = 0;
            else if (phase > maxdelay_reg) phase = maxdelay_reg;
            phase = index_reg - phase;
            /* now phase is a location in the buffer_reg (before modulo) */
    
            /* Time out to update the buffer_reg:
             * this is a tricky buffer_reg: buffer_reg[0] == buffer_reg[bufflen_reg]
             * the logical length is bufflen_reg, but the actual length
             * is bufflen_reg + 1 to allow for a repeated sample at the
             * end. This allows for efficient interpolation.
             */ 
            buffer_reg[index_reg++] = (s1_scale_reg * *s1_ptr_reg++);
            if (index_reg >= bufflen_reg) {
                index_reg = 0;
            }
    
            /* back to the phase calculation: 
             * use conditional instead of modulo
             */
            if (phase < 0) phase += bufflen_reg;
            *out_ptr_reg++ = (sample_type) (buffer_reg[phase]);
            vardelay_pHaSe_ReG += vardelay_pHaSe_iNcR_rEg;
        } while (--n); /* inner loop */

        togo -= n;
        susp->bufflen = bufflen_reg;
        susp->index = index_reg;
        susp->vardelay_pHaSe = vardelay_pHaSe_ReG;
        susp->vardelay_x1_sample = vardelay_x1_sample_reg;
        /* using s1_ptr_reg is a bad idea on RS/6000: */
        susp->s1_ptr += togo;
        out_ptr += togo;
        susp_took(s1_cnt, togo);
        cnt += togo;
    } /* outer loop */

    /* test for termination */
    if (togo == 0 && cnt == 0) {
        snd_list_terminate(snd_list);
    } else {
        snd_list->block_len = cnt;
        susp->susp.current += cnt;
    }
    /* test for logical stop */
    if (susp->logically_stopped) {
        snd_list->logically_stopped = true;
    } else if (susp->susp.log_stop_cnt == susp->susp.current) {
        susp->logically_stopped = true;
    }
} /* tapf_si_fetch */


void tapf_sr_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    tapf_susp_type susp = (tapf_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type vardelay_DeLtA;
    sample_type vardelay_val;
    sample_type vardelay_x2_sample;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double offset_reg;
    register double vdscale_reg;
    register long maxdelay_reg;
    register long bufflen_reg;
    register long index_reg;
    register sample_type * buffer_reg;
    register sample_type s1_scale_reg = susp->s1->scale;
    register sample_block_values_type s1_ptr_reg;
    falloc_sample_block(out, "tapf_sr_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
        susp->started = true;
        susp->vardelay_pHaSe = 1.0;
    }

    susp_check_term_samples(vardelay, vardelay_ptr, vardelay_cnt);
    vardelay_x2_sample = *(susp->vardelay_ptr);

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the s1 input sample block: */
        susp_check_term_log_samples(s1, s1_ptr, s1_cnt);
        togo = min(togo, susp->s1_cnt);

        /* grab next vardelay_x2_sample when phase goes past 1.0; */
        /* we use vardelay_n (computed below) to avoid roundoff errors: */
        if (susp->vardelay_n <= 0) {
            susp->vardelay_x1_sample = vardelay_x2_sample;
            susp->vardelay_ptr++;
            susp_took(vardelay_cnt, 1);
            susp->vardelay_pHaSe -= 1.0;
            susp_check_term_samples(vardelay, vardelay_ptr, vardelay_cnt);
            vardelay_x2_sample = *(susp->vardelay_ptr);
            /* vardelay_n gets number of samples before phase exceeds 1.0: */
            susp->vardelay_n = (int64_t) ((1.0 - susp->vardelay_pHaSe) *
                                        susp->output_per_vardelay);
        }
        togo = (int) min(togo, susp->vardelay_n);
        vardelay_DeLtA = (sample_type) ((vardelay_x2_sample - susp->vardelay_x1_sample) * susp->vardelay_pHaSe_iNcR);
        vardelay_val = (sample_type) (susp->vardelay_x1_sample * (1.0 - susp->vardelay_pHaSe) +
                 vardelay_x2_sample * susp->vardelay_pHaSe);

        /* don't run past terminate time */
        if (susp->terminate_cnt != UNKNOWN &&
            susp->terminate_cnt <= susp->susp.current + cnt + togo) {
            togo = (int) (susp->terminate_cnt - (susp->susp.current + cnt));
            if (togo < 0) togo = 0;  /* avoids rounding errros */
            if (togo == 0) break;
        }


        /* don't run past logical stop time */
        if (!susp->logically_stopped && susp->susp.log_stop_cnt != UNKNOWN) {
            int64_t to_stop = susp->susp.log_stop_cnt - (susp->susp.current + cnt);
            /* break if to_stop == 0 (we're at the logical stop)
             * AND cnt > 0 (we're not at the beginning of the
             * output block).
             */
            if (to_stop < 0) to_stop = 0; /* avoids rounding errors */
            if (to_stop < togo) {
                if (to_stop == 0) {
                    if (cnt) {
                        togo = 0;
                        break;
                    } else /* keep togo as is: since cnt == 0, we
                            * can set the logical stop flag on this
                            * output block
                            */
                        susp->logically_stopped = true;
                } else /* limit togo so we can start a new
                        * block at the LST
                        */
                    togo = (int) to_stop;
            }
        }

        n = togo;
        offset_reg = susp->offset;
        vdscale_reg = susp->vdscale;
        maxdelay_reg = susp->maxdelay;
        bufflen_reg = susp->bufflen;
        index_reg = susp->index;
        buffer_reg = susp->buffer;
        s1_ptr_reg = susp->s1_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            long phase;
            phase = (long) (vardelay_val * vdscale_reg + offset_reg);
            /* now phase should give number of samples of delay */
            if (phase < 0) phase = 0;
            else if (phase > maxdelay_reg) phase = maxdelay_reg;
            phase = index_reg - phase;
            /* now phase is a location in the buffer_reg (before modulo) */
    
            /* Time out to update the buffer_reg:
             * this is a tricky buffer_reg: buffer_reg[0] == buffer_reg[bufflen_reg]
             * the logical length is bufflen_reg, but the actual length
             * is bufflen_reg + 1 to allow for a repeated sample at the
             * end. This allows for efficient interpolation.
             */ 
            buffer_reg[index_reg++] = (s1_scale_reg * *s1_ptr_reg++);
            if (index_reg >= bufflen_reg) {
                index_reg = 0;
            }
    
            /* back to the phase calculation: 
             * use conditional instead of modulo
             */
            if (phase < 0) phase += bufflen_reg;
            *out_ptr_reg++ = (sample_type) (buffer_reg[phase]);
            vardelay_val += vardelay_DeLtA;
        } while (--n); /* inner loop */

        susp->bufflen = bufflen_reg;
        susp->index = index_reg;
        /* using s1_ptr_reg is a bad idea on RS/6000: */
        susp->s1_ptr += togo;
        out_ptr += togo;
        susp_took(s1_cnt, togo);
        susp->vardelay_pHaSe += togo * susp->vardelay_pHaSe_iNcR;
        susp->vardelay_n -= togo;
        cnt += togo;
    } /* outer loop */

    /* test for termination */
    if (togo == 0 && cnt == 0) {
        snd_list_terminate(snd_list);
    } else {
        snd_list->block_len = cnt;
        susp->susp.current += cnt;
    }
    /* test for logical stop */
    if (susp->logically_stopped) {
        snd_list->logically_stopped = true;
    } else if (susp->susp.log_stop_cnt == susp->susp.current) {
        susp->logically_stopped = true;
    }
} /* tapf_sr_fetch */


void tapf_toss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    tapf_susp_type susp = (tapf_susp_type) a_susp;
    time_type final_time = susp->susp.t0;
    int n;

    /* fetch samples from s1 up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->s1->t0) * susp->s1->sr)) >=
           susp->s1->current)
        susp_get_samples(s1, s1_ptr, s1_cnt);
    /* fetch samples from vardelay up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->vardelay->t0) * susp->vardelay->sr)) >=
           susp->vardelay->current)
        susp_get_samples(vardelay, vardelay_ptr, vardelay_cnt);
    /* convert to normal processing when we hit final_count */
    /* we want each signal positioned at final_time */
    n = (int) ROUNDBIG((final_time - susp->s1->t0) * susp->s1->sr -
         (susp->s1->current - susp->s1_cnt));
    susp->s1_ptr += n;
    susp_took(s1_cnt, n);
    n = (int) ROUNDBIG((final_time - susp->vardelay->t0) * susp->vardelay->sr -
         (susp->vardelay->current - susp->vardelay_cnt));
    susp->vardelay_ptr += n;
    susp_took(vardelay_cnt, n);
    susp->susp.fetch = susp->susp.keep_fetch;
    (*(susp->susp.fetch))(a_susp, snd_list);
}


void tapf_mark(snd_susp_type a_susp)
{
    tapf_susp_type susp = (tapf_susp_type) a_susp;
    sound_xlmark(susp->s1);
    sound_xlmark(susp->vardelay);
}


void tapf_free(snd_susp_type a_susp)
{
    tapf_susp_type susp = (tapf_susp_type) a_susp;
    free(susp->buffer);
    sound_unref(susp->s1);
    sound_unref(susp->vardelay);
    ffree_generic(susp, sizeof(tapf_susp_node), "tapf_free");
}


void tapf_print_tree(snd_susp_type a_susp, int n)
{
    tapf_susp_type susp = (tapf_susp_type) a_susp;
    indent(n);
    stdputstr("s1:");
    sound_print_tree_1(susp->s1, n);

    indent(n);
    stdputstr("vardelay:");
    sound_print_tree_1(susp->vardelay, n);
}


sound_type snd_make_tapf(sound_type s1, double offset, sound_type vardelay, double maxdelay)
{
    register tapf_susp_type susp;
    rate_type sr = s1->sr;
    time_type t0 = max(s1->t0, vardelay->t0);
    int interp_desc = 0;
    sample_type scale_factor = 1.0F;
    time_type t0_min = t0;
    falloc_generic(susp, tapf_susp_node, "snd_make_tapf");
    susp->offset = offset * s1->sr;
    susp->vdscale = vardelay->scale * s1->sr;
    susp->maxdelay = (long)(maxdelay * s1->sr);
    susp->bufflen = max(2, (long) (susp->maxdelay + 0.5));
    susp->index = susp->bufflen;
    susp->buffer = (sample_type *) calloc(susp->bufflen + 1, sizeof(sample_type));

    /* make sure no sample rate is too high */
    if (vardelay->sr > sr) {
        sound_unref(vardelay);
        snd_badsr();
    }

    /* select a susp fn based on sample rates */
    interp_desc = (interp_desc << 2) + interp_style(s1, sr);
    interp_desc = (interp_desc << 2) + interp_style(vardelay, sr);
    switch (interp_desc) {
      case INTERP_ns: /* handled below */
      case INTERP_nn: susp->susp.fetch = tapf_nn_fetch; break;
      case INTERP_ni: susp->susp.fetch = tapf_ni_fetch; break;
      case INTERP_nr: susp->susp.fetch = tapf_nr_fetch; break;
      case INTERP_ss: /* handled below */
      case INTERP_sn: susp->susp.fetch = tapf_sn_fetch; break;
      case INTERP_si: susp->susp.fetch = tapf_si_fetch; break;
      case INTERP_sr: susp->susp.fetch = tapf_sr_fetch; break;
      default: snd_badsr(); break;
    }

    susp->terminate_cnt = UNKNOWN;
    /* handle unequal start times, if any */
    if (t0 < s1->t0) sound_prepend_zeros(s1, t0);
    if (t0 < vardelay->t0) sound_prepend_zeros(vardelay, t0);
    /* minimum start time over all inputs: */
    t0_min = min(s1->t0, min(vardelay->t0, t0));
    /* how many samples to toss before t0: */
    susp->susp.toss_cnt = (long) ((t0 - t0_min) * sr + 0.5);
    if (susp->susp.toss_cnt > 0) {
        susp->susp.keep_fetch = susp->susp.fetch;
        susp->susp.fetch = tapf_toss_fetch;
    }

    /* initialize susp state */
    susp->susp.free = tapf_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = tapf_mark;
    susp->susp.print_tree = tapf_print_tree;
    susp->susp.name = "tapf";
    susp->logically_stopped = false;
    susp->susp.log_stop_cnt = logical_stop_cnt_cvt(s1);
    susp->started = false;
    susp->susp.current = 0;
    susp->s1 = s1;
    susp->s1_cnt = 0;
    susp->vardelay = vardelay;
    susp->vardelay_cnt = 0;
    susp->vardelay_pHaSe = 0.0;
    susp->vardelay_pHaSe_iNcR = vardelay->sr / sr;
    susp->vardelay_n = 0;
    susp->output_per_vardelay = sr / vardelay->sr;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_tapf(sound_type s1, double offset, sound_type vardelay, double maxdelay)
{
    sound_type s1_copy = sound_copy(s1);
    sound_type vardelay_copy = sound_copy(vardelay);
    return snd_make_tapf(s1_copy, offset, vardelay_copy, maxdelay);
}
