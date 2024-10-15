/* multiread.c -- read multichannel sound file */

/* CHANGE LOG
 * --------------------------------------------------------------------
 * 28Apr03  dm  changes for portability and fix compiler warnings
 */

#include "stdio.h"
#ifdef UNIX
#include "sys/file.h"
#endif
#ifndef mips
#include "stdlib.h"
#endif
#include "sndfmt.h"
#include "xlisp.h"
#include "sound.h"
#include "falloc.h"
#include "sndfile.h"
#include "sndread.h"
#include "multiread.h"

/* allocate input buffer space for this many bytes/frame,
 * e.g. 8 allows 2 channels
 * If frames are bigger, then multiple reads will be issued.
 */
#define max_bytes_per_frame (sizeof(float) * 2)
#define input_buffer_max (max_sample_block_len * max_bytes_per_frame)
#define input_buffer_samps (max_sample_block_len * 2)


/* multiread_fetch - read samples into multiple channels.  */
/*
 * The susp is shared by all channels.  The susp has backpointers
 * to the tail-most snd_list node of each channels, and it is by
 * extending the list at these nodes that sounds are read in.
 * To avoid a circularity, the reference counts on snd_list nodes
 * do not include the backpointers from this susp.  When a snd_list
 * node refcount goes to zero, the multiread susp's free routine
 * is called.  This must scan the backpointers to find the node that
 * has a zero refcount (the free routine is called before the node
 * is deallocated, so this is safe).  The backpointer is then set
 * to NULL.  When all backpointers are NULL, the susp itself is
 * deallocated, because it can only be referenced through the
 * snd_list nodes to which there are backpointers.
 */
void multiread_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    read_susp_type susp = (read_susp_type) a_susp;
    int i, j;
    int frames_read = 0; /* total frames read in this call to fetch */
    int n;
    sample_block_type out;
    // char input_buffer[input_buffer_max];
    float input_buffer[input_buffer_samps];
    int file_frame_size;

    /* when we are called, the caller (SND_get_first) will insert a new
     * snd_list node.  We need to do this here for all other channels.
     */
    for (j = 0; j < susp->sf_info.channels; j++) {

/*        nyquist_printf("multiread_fetch: chan[%d] = ", j);
        print_snd_list_type(susp->chan[j]);
        stdputstr("\n");
 */
        if (!susp->chan[j]) {   /* ignore non-existent channels */
/*          nyquist_printf("multiread_fetch: ignore channel %d\n", j);*/
            continue;
        }
        falloc_sample_block(out, "multiread_fetch");
/*      nyquist_printf("multiread: allocated block %x\n", out); */
        /* Since susp->chan[i] exists, we want to append a block of samples.
         * The block, out, has been allocated.  Before we insert the block,
         * we must figure out whether to insert a new snd_list_type node for
         * the block.  Recall that before SND_get_next is called, the last
         * snd_list_type in the list will have a null block pointer, and the
         * snd_list_type's susp field points to the suspension (in this case,
         * susp).  When SND_get_next (in sound.c) is called, it appends a new
         * snd_list_type and points the previous one to internal_zero_block 
         * before calling this fetch routine.  On the other hand, since 
         * SND_get_next is only going to be called on one of the channels, the
         * other channels will not have had a snd_list_type appended.
         * SND_get_next does not tell us directly which channel it wants (it
         * doesn't know), but we can test by looking for a non-null block in the
         * snd_list_type pointed to by our back-pointers in susp->chan[].  If
         * the block is null, the channel was untouched by SND_get_next, and
         * we should append a snd_list_type.  If it is non-null, then it
         * points to internal_zero_block (the block inserted by SND_get_next)
         * and a new snd_list_type has already been appended.
         */
        /* Before proceeding, it may be that garbage collection ran when we
         * allocated out, so check again to see if susp->chan[j] is Null:
         */
        if (!susp->chan[j]) {
            ffree_sample_block(out, "multiread_fetch");
            continue;
        }
        if (!susp->chan[j]->block) {
            snd_list_type snd_list = snd_list_create((snd_susp_type) susp);
            /* Now we have a snd_list to append to the channel, but a very
             * interesting thing can happen here.  snd_list_create, which
             * we just called, MAY have invoked the garbage collector, and
             * the GC MAY have freed all references to this channel, in which
             * case multread_free(susp) will have been called, and susp->chan[j]
             * will now be NULL!
             */
            if (!susp->chan[j]) {
                nyquist_printf("susp %p Channel %d disappeared!\n", susp, j);
                ffree_snd_list(snd_list, "multiread_fetch");
            } else {
                susp->chan[j]->u.next = snd_list;
            }
        }
        /* see the note above: we don't know if susp->chan still exists */
        /* Note: We DO know that susp still exists because even if we lost
         * some channels in a GC, someone is still calling SND_get_next on
         * some channel.  I suppose that there might be some very pathological
         * code that could free a global reference to a sound that is in the
         * midst of being computed, perhaps by doing something bizarre in the
         * closure that snd_seq activates at the logical stop time of its first
         * sound, but I haven't thought that one through.
         */
        if (susp->chan[j]) {
            susp->chan[j]->block = out;
            /* check some assertions */
            if (susp->chan[j]->u.next->u.susp != (snd_susp_type) susp) {
                nyquist_printf("didn't find susp at end of list for chan %d\n", j);
            }
        } else { /* we allocated out, but don't need it anymore due to GC */
            ffree_sample_block(out, "multiread_fetch");
        }
    }

    file_frame_size = susp->sf_info.channels;

    /* now fill sample blocks with frames from the file 
       until eof or end of blocks */
    while (true) {

        /* compute how many frames to read to fill sample blocks */
        int frame_count = max_sample_block_len - frames_read;
        int actual;         /* how many frames actually read */

        /* make sure frames will fit in buffer */
        if (frame_count * file_frame_size > input_buffer_samps) {
            frame_count = input_buffer_samps / file_frame_size;
        }

        actual = (int) sf_readf_float(susp->sndfile, input_buffer, frame_count);
        n = actual;  

        /* don't read too many */
        if (n > (susp->cnt - susp->susp.current)) {
            n = (int) (susp->cnt - susp->susp.current);
        }

        /* process one channel at a time, multiple passes through input */
        for (j = 0; j < susp->sf_info.channels; j++) {
            register sample_block_values_type out_ptr;
            /* offset by channel number: */
            float *float_ptr = input_buffer + j;

            /* ignore nonexistent channels */
            if (!susp->chan[j]) continue;

            /* find pointer to sample buffer */
            out_ptr = susp->chan[j]->block->samples + frames_read;

            /* copy samples */
            for (i = 0; i < n; i++) {
                *out_ptr++ = *float_ptr;
                float_ptr += susp->sf_info.channels;
            }
            susp->chan[j]->block_len = frames_read + n;
        }

	/* jlh BECAUSE, at this point, all the code cares about is
	   that n frames have been read and the samples put into their
	   appropriate snd_node buffers. */

        frames_read += n;
        susp->susp.current += n;

        if (frames_read == 0) {
            /* NOTE: this code should probably be removed -- how could we
               ever get here? Since file formats know the sample count, we'll
               always read frames. When we hit the end-of-file, the else
               clause below will run and terminate the sound, so we'll never
               try and read samples that are not there. The only exception is
               an empty sound file with no samples, in which case we could omit
               this if test and execute the else part below.

               This code *might* be good for formats that do not encode a
               sample count and where reading the end of file is the only way
               to detect the end of the data.

               Since it seeems to work, I'm going to leave this in place.
               One tricky point of the algorithm: when we get here, we set up
               susp->chan[j] to point to the right place and then call
               snd_list_terminate(). This deletes the snd_list that chan[j]
               is pointing to, but not before calling multiread_free(), which
               upon detecting that the sound is being freed, sets chan[j] to
               NULL. This works sequentially on each channel and than last
               time, this susp is freed because no channels are active.
             */
            /* we didn't read anything, but can't return length zero, so
             * convert snd_list's to pointer to zero block.  This loop
             * will free the susp via snd_list_unref().
             */
            for (j = 0; j < susp->sf_info.channels; j++) {
                if (susp->chan[j]) {
                    snd_list_type the_snd_list = susp->chan[j];
                    /* this is done so that multiread_free works right: */
                    susp->chan[j] = susp->chan[j]->u.next;
                    /* nyquist_printf("end of file, terminating channel %d\n", j); */
                    /* this fixes up the tail of channel j */
                    snd_list_terminate(the_snd_list);
                }
            }
            return;
        } else if (susp->cnt == susp->susp.current || actual < frame_count) {
            /* we've read the requested number of frames or we
             * reached end of file
             * last iteration will close file and free susp:
             */
            for (j = 0; j < susp->sf_info.channels; j++) {
                snd_list_type the_snd_list = susp->chan[j];
                /* nyquist_printf("reached susp->cnt, terminating chan %d\n", j); */
                if (the_snd_list) {
                    /* assert: */
                    if (the_snd_list->u.next->u.susp != (snd_susp_type) susp) {
                        stdputstr("assertion violation");
                    }
                    /* this is done so that multiread_free works right: */
                    susp->chan[j] = the_snd_list->u.next;
                    snd_list_unref(the_snd_list->u.next);
                    /* terminate by pointing to zero block */
                    the_snd_list->u.next = zero_snd_list;
                }
            }
            return;
        } else if (frames_read >= max_sample_block_len) {
            /* move pointer to next list node */
            for (j = 0; j < susp->sf_info.channels; j++) {
                if (susp->chan[j]) susp->chan[j] = susp->chan[j]->u.next;
            }
            return;
        }
    }
} /* multiread__fetch */
  

void multiread_free(snd_susp_type a_susp)
{
    read_susp_type susp = (read_susp_type) a_susp;
    int j;
    boolean active = false;
/*    stdputstr("multiread_free: "); */
    for (j = 0; j < susp->sf_info.channels; j++) {
        if (susp->chan[j]) {
            if (susp->chan[j]->refcnt) active = true;
            else {
                susp->chan[j] = NULL;
                /* nyquist_printf("deactivating channel %d\n", j); */
            }
        }
    }
    if (!active) {
        /* stdputstr("all channels freed, freeing susp now\n"); */
        read_free(a_susp);
    }
}


LVAL multiread_create(susp)
  read_susp_type susp;
{
    LVAL result;
    int j;

    xlsave1(result);

    result = newvector(susp->sf_info.channels);      /* create array for sounds */
    falloc_generic_n(susp->chan, snd_list_type, susp->sf_info.channels, 
                     "multiread_create");
    /* create sounds to return */
    for (j = 0; j < susp->sf_info.channels; j++) {
        sound_type snd = sound_create((snd_susp_type)susp, 
                                      susp->susp.t0, susp->susp.sr, 1.0);
        LVAL snd_lval = cvsound(snd);
/*      nyquist_printf("multiread_create: sound %d is %x, LVAL %x\n", j, snd, snd_lval); */
        setelement(result, j, snd_lval);
        susp->chan[j] = snd->list;
    }
    xlpop();
    return result;
}
