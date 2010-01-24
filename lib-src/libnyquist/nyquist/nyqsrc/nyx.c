/**********************************************************************

  nyx.c

  Nyx: A very simple external interface to Nyquist

  Dominic Mazzoni

**********************************************************************/

/* system includes */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <math.h>

#ifndef WIN32
#include <unistd.h>
#endif

/* nyx includes */
#include "nyx.h"

/* xlisp includes */
#include "switches.h"
#include "xlisp.h"
#include "cext.h"

/* nyquist includes */
#include "sound.h"
#include "samples.h"
#include "falloc.h"

/* use full copy */
#define NYX_FULL_COPY 1

/* show memory stats */
// #define NYX_MEMORY_STATS 1

/* show details of obarray copy */
// #define NYX_DEBUG_COPY 1

/* macro to compute the size of a segment (taken from xldmem.h) */
#define segsize(n) (sizeof(SEGMENT)+((n)-1)*sizeof(struct node))

/* xldmem external variables */
extern long nnodes;
extern long nfree;
extern long total;
extern int nsegs;
extern SEGMENT *segs;
extern SEGMENT *lastseg;
extern LVAL fnodes;

/* nyquist externs */
extern LVAL a_sound;
extern snd_list_type zero_snd_list;

/* globals */
LOCAL nyx_os_callback     nyx_os_cb = NULL;
LOCAL void               *nyx_os_ud;
LOCAL nyx_output_callback nyx_output_cb;
LOCAL void               *nyx_output_ud;
LOCAL int                 nyx_expr_pos;
LOCAL int                 nyx_expr_len;
LOCAL const char         *nyx_expr_string;
LOCAL LVAL                nyx_result;
LOCAL nyx_rval            nyx_result_type = nyx_error;
LOCAL XLCONTEXT           nyx_cntxt;
LOCAL int                 nyx_first_time = 1;
LOCAL LVAL                nyx_obarray;
LOCAL FLOTYPE             nyx_warp_stretch;

/* Suspension node */
typedef struct nyx_susp_struct {
   snd_susp_node       susp;        // Must be first
   nyx_audio_callback  callback;
   void               *userdata;
   long                len;
   int                 channel;
} nyx_susp_node, *nyx_susp_type;

#if defined(NYX_DEBUG_COPY) && NYX_DEBUG_COPY
static const char *_types_[] = 
{
   "FREE_NODE",
   "SUBR",
   "FSUBR",
   "CONS",
   "SYMBOL",
   "FIXNUM",
   "FLONUM",
   "STRING",
   "OBJECT",
   "STREAM",
   "VECTOR",
   "CLOSURE",
   "CHAR",
   "USTREAM",
   "EXTERN"
};

// Dump the contents of the obarray
LOCAL void nyx_show_obarray()
{
   LVAL array = getvalue(obarray);
   LVAL sym;
   int i;

   for (i = 0; i < HSIZE; i++) {
      for (sym = getelement(array, i); sym; sym = cdr(sym)) {
         LVAL syma = car(sym);

         printf("_sym_ = ");
         xlprint(getvalue(s_stdout), syma, TRUE);

         if (getvalue(syma)) {
            printf(" _type_ = %s _val_ = ", _types_[ntype(getvalue(syma))]);
            xlprint(getvalue(s_stdout), getvalue(syma), TRUE);
         }

         if (getfunction(syma)) {
            printf(" _type_ = %s _fun_ = ", _types_[ntype(getfunction(syma))]);
            xlprint(getvalue(s_stdout), getfunction(syma), TRUE);
         }

         printf("\n");
      }
   }
}
#endif

//
// Free empty segments
//
LOCAL void freesegs()
{
   SEGMENT *seg;
   SEGMENT *next;

   // Free up as many nodes as possible
   gc();

   // Reset free node tracking
   fnodes = NIL;
   nfree = 0L;

   // Reset the last segment pointer
   lastseg = NULL;

   // Scan all segments
   for (seg = segs; seg != NULL; seg = next) {
      int n = seg->sg_size;
      int empty = TRUE;
      int i;
      LVAL p;

      // Check this segment for in-use nodes
      p = &seg->sg_nodes[0];
      for (i = n; --i >= 0; ++p) {
         if (ntype(p) != FREE_NODE) {
            empty = FALSE;
            break;
         }
      }

      // Retain pointer to next segment
      next = seg->sg_next;

      // Was the current segment empty?
      if (empty) {
         // Free the segment;
         free((void *) seg);

         // Unlink it from the list.  No need to worry about a NULL lastseg
         // pointer here since the fixnum and char segments will always exist
         // at the head of the list and they will always have nodes.  So, lastseg
         // will have been set before we find any empty nodes.
         lastseg->sg_next = next;

         // Reduce the stats
         total -= (long) segsize(n);
         nsegs--;
         nnodes -= n;
      }
      else {
         // Not empty, so remember this node as the last segment
         lastseg = seg;

         // Add all of the free nodes in this segment to the free list
         p = &seg->sg_nodes[0];
         for (i = n; --i >= 0; ++p) {
            if (ntype(p) == FREE_NODE) {
               rplaca(p, NIL);
               rplacd(p, fnodes);
               fnodes = p;
               nfree++;
            }
         }
      }
   }
}

#if defined(NYX_FULL_COPY) && NYX_FULL_COPY

// Copy a node (recursively if appropriate)
LOCAL LVAL nyx_dup_value(LVAL val)
{
   LVAL nval = val;

   // Protect old and new values
   xlprot1(val);
   xlprot1(nval);

   // Copy the node
   if (val != NIL) {
      switch (ntype(val))
      {
         case FIXNUM:
            nval = cvfixnum(getfixnum(val));
         break;

         case FLONUM:
            nval = cvflonum(getflonum(val));
         break;

         case CHAR:
            nval = cvchar(getchcode(val));
         break;

         case STRING:
            nval = cvstring((char *) getstring(val));
         break;

         case VECTOR:
         {
            int len = getsize(val);
            int i;

            nval = newvector(len);
            nval->n_type = ntype(val);

            for (i = 0; i < len; i++) {
               if (getelement(val, i) == val) {
                  setelement(nval, i, val);
               }
               else {
                  setelement(nval, i, nyx_dup_value(getelement(val, i)));
               }
            }
         }
         break;

         case CONS:
            nval = cons(nyx_dup_value(car(val)), nyx_dup_value(cdr(val)));
         break;

         case SUBR:
         case FSUBR:
            nval = cvsubr(getsubr(val), ntype(val), getoffset(val));
         break;

         // Symbols should never be copied since their addresses are cached
         // all over the place.
         case SYMBOL:
            nval = val;
         break;

         // Streams are not copied (although USTREAM could be) and reference
         // the original value.
         case USTREAM:
         case STREAM:
            nval = val;
         break;

         // Externals aren't copied because I'm not entirely certain they can be.
         case EXTERN:
            nval = val;
         break;

         // For all other types, just allow them to reference the original
         // value.  Probably not the right thing to do, but easier.
         case OBJECT:
         case CLOSURE:
         default:
            nval = val;
         break;
      }
   }

   xlpop();
   xlpop();

   return nval;
}

// Make a copy of the original obarray, leaving the original in place
LOCAL void nyx_save_obarray()
{
   LVAL newarray;
   int i;

   // This provide permanent protection for nyx_obarray as we do not want it
   // to be garbage-collected.
   xlprot1(nyx_obarray);
   nyx_obarray = getvalue(obarray);

   // Create and set the new vector.  This allows us to use xlenter() to
   // properly add the new symbol.  Probably slower than adding directly,
   // but guarantees proper hashing.
   newarray = newvector(HSIZE);
   setvalue(obarray, newarray);

   // Scan all obarray vectors
   for (i = 0; i < HSIZE; i++) {
      LVAL sym;

      // Scan all elements
      for (sym = getelement(nyx_obarray, i); sym; sym = cdr(sym)) {
         LVAL syma = car(sym);
         char *name = (char *) getstring(getpname(syma));
         LVAL nsym = xlenter(name);

         // Ignore *OBARRAY* since there's no need to copy it
         if (strcmp(name, "*OBARRAY*") == 0) {
            continue;
         }

         // Duplicate the symbol's values
         setvalue(nsym, nyx_dup_value(getvalue(syma)));
         setplist(nsym, nyx_dup_value(getplist(syma)));
         setfunction(nsym, nyx_dup_value(getfunction(syma)));
      }
   }

   // Swap the obarrays, so that the original is put back into service
   setvalue(obarray, nyx_obarray);
   nyx_obarray = newarray;
}

// Restore the symbol values to their original value and remove any added
// symbols.
LOCAL void nyx_restore_obarray()
{
   LVAL obvec = getvalue(obarray);
   int i;

   // Scan all obarray vectors
   for (i = 0; i < HSIZE; i++) {
      LVAL last = NULL;
      LVAL dcon;

      for (dcon = getelement(obvec, i); dcon; dcon = cdr(dcon)) {
         LVAL dsym = car(dcon);
         char *name = (char *)getstring(getpname(dsym));
         LVAL scon;

         // Ignore *OBARRAY* since setting it causes the input array to be
         // truncated.
         if (strcmp(name, "*OBARRAY*") == 0) {
            continue;
         }

         // Find the symbol in the original obarray.
         for (scon = getelement(nyx_obarray, hash(name, HSIZE)); scon; scon = cdr(scon)) {
            LVAL ssym = car(scon);

            // If found, then set the current symbols value to the original.
            if (strcmp(name, (char *)getstring(getpname(ssym))) == 0) {
               setvalue(dsym, nyx_dup_value(getvalue(ssym)));
               setplist(dsym, nyx_dup_value(getplist(ssym)));
               setfunction(dsym, nyx_dup_value(getfunction(ssym)));
               break;
            }
         }

         // If we didn't find the symbol in the original obarray, then it must've
         // been added since and must be removed from the current obarray.
         if (scon == NULL) {
            if (last) {
               rplacd(last, cdr(dcon));
            }
            else {
               setelement(obvec, i, cdr(dcon));
            }
         }

         // Must track the last dcon for symbol removal
         last = dcon;
      }
   }
}

#else

LOCAL LVAL copylist(LVAL from)
{
   LVAL nsym;
   if (from == NULL) {
      return NULL;
   }

   return cons(car(from), copylist(cdr(from)));
}

/* Make a copy of the obarray so that we can erase any
   changes the user makes to global variables */
LOCAL void nyx_copy_obarray()
{
   LVAL newarray;
   int i;

   // Create and set the new vector.
   newarray = newvector(HSIZE);
   setvalue(obarray, newarray);

   for (i = 0; i < HSIZE; i++) {
      LVAL from = getelement(nyx_obarray, i);
      if (from) {
         setelement(newarray, i, copylist(from));
      }
   }
}

#endif

void nyx_init()
{
   if (nyx_first_time) {
      char *argv[1];
      argv[0] = "nyquist";
      xlisp_main_init(1, argv);

      nyx_os_cb = NULL;
      nyx_output_cb = NULL;
      
      nyx_first_time = 0;

#if defined(NYX_FULL_COPY) && NYX_FULL_COPY
      // Save a copy of the original obarray's contents.
      nyx_save_obarray();
#else
      // Permanently protect the original obarray value.  This is needed since
      // it would be unreferenced in the new obarray and would be garbage
      // collected.  We want to keep it around so we can make copies of it to 
      // refresh the execution state.
      xlprot1(nyx_obarray);
      nyx_obarray = getvalue(obarray);
#endif
   }

#if !defined(NYX_FULL_COPY) || !NYX_FULL_COPY
   // Create a copy of the original obarray
   nyx_copy_obarray();
#endif

   // Keep nyx_result from being garbage-collected
   xlprot1(nyx_result);

#if defined(NYX_MEMORY_STATS) && NYX_MEMORY_STATS
   printf("\nnyx_init\n");
   xmem();
#endif
}

void nyx_cleanup()
{
   // Garbage-collect nyx_result
   xlpop();

#if defined(NYX_FULL_COPY) && NYX_FULL_COPY

   // Restore the original symbol values
   nyx_restore_obarray();

#else

   // Restore obarray to original state...but not the values
   setvalue(obarray, nyx_obarray);

#endif

   // Make sure the sound nodes can be garbage-collected.  Sounds are EXTERN
   // nodes whose value does not get copied during a full copy of the obarray.
   setvalue(xlenter("S"), NIL);

   // Free excess memory segments - does a gc()
   freesegs();

   // No longer need the callbacks
   nyx_output_cb = NULL;
   nyx_os_cb = NULL;

#if defined(NYX_MEMORY_STATS) && NYX_MEMORY_STATS
   printf("\nnyx_cleanup\n");
   xmem();
#endif
}

LOCAL void nyx_susp_fetch(register nyx_susp_type susp, snd_list_type snd_list)
{
   sample_block_type         out;
   sample_block_values_type  out_ptr;
   long                      n;
   int                       err;

   falloc_sample_block(out, "nyx_susp_fetch");
   out_ptr = out->samples;
   snd_list->block = out;

   n = max_sample_block_len;
   if (susp->susp.current + n > susp->len)
      n = susp->len - susp->susp.current;

   err = susp->callback(out_ptr, susp->channel,
                            susp->susp.current, n, 0, susp->userdata);
   if (err) {
      // The user canceled or some other error occurred, so we use
      // xlsignal() to jump back to our error handler.
      xlsignal(NULL, NULL);
      // never get here.
   }

   snd_list->block_len = (short)n;
   susp->susp.current += n;

   if (n == 0) {
      /* we didn't read anything, but can't return length zero, so
         convert snd_list to pointer to zero block */
      snd_list_terminate(snd_list);
   }
   else if (n < max_sample_block_len) {
      /* should free susp */
      snd_list_unref(snd_list->u.next);
      /* if something is in buffer, terminate by pointing to zero block */
      snd_list->u.next = zero_snd_list;
   }
}

LOCAL void nyx_susp_free(nyx_susp_type susp)
{
   ffree_generic(susp, sizeof(nyx_susp_node), "nyx_susp_free");
}

LOCAL void nyx_susp_print_tree(nyx_susp_type susp, int n)
{
}

void nyx_capture_output(nyx_output_callback callback, void *userdata)
{
   nyx_output_cb = callback;
   nyx_output_ud = userdata;
}

void nyx_set_audio_params(double rate, long len)
{
   double stretch_len = (len > 0 ? len / rate : 1.0);
   LVAL warp;

   /* Bind the sample rate to the "*sound-srate*" global */
   setvalue(xlenter("*SOUND-SRATE*"), cvflonum(rate));

   /* Bind selection len to "len" global */
   setvalue(xlenter("LEN"), cvflonum(len));

   /* Set the "*warp*" global based on the length of the audio */
   xlprot1(warp);
   warp = cons(cvflonum(0),                    /* time offset */
               cons(cvflonum(stretch_len),     /* time stretch */
                    cons(NULL,                 /* cont. time warp */
                         NULL)));
   setvalue(xlenter("*WARP*"), warp);
   xlpop();
}

void nyx_set_input_audio(nyx_audio_callback callback,
                         void *userdata,
                         int num_channels,
                         long len, double rate)
{
   sample_type      scale_factor = 1.0;
   time_type        t0 = 0.0;
   nyx_susp_type   *susp;
   sound_type      *snd;
   int              ch;

   nyx_set_audio_params(rate, len);

   susp = (nyx_susp_type *)malloc(num_channels * sizeof(nyx_susp_type));
   snd = (sound_type *)malloc(num_channels * sizeof(sound_type));

   for(ch=0; ch < num_channels; ch++) {
      falloc_generic(susp[ch], nyx_susp_node, "nyx_set_input_audio");

      susp[ch]->callback = callback;
      susp[ch]->userdata = userdata;
      susp[ch]->len = len;
      susp[ch]->channel = ch;

      susp[ch]->susp.fetch = nyx_susp_fetch;
      susp[ch]->susp.keep_fetch = NULL;
      susp[ch]->susp.free = nyx_susp_free;
      susp[ch]->susp.mark = NULL;
      susp[ch]->susp.print_tree = nyx_susp_print_tree;
      susp[ch]->susp.name = "nyx";
      susp[ch]->susp.toss_cnt = 0;
      susp[ch]->susp.current = 0;
      susp[ch]->susp.sr = rate;
      susp[ch]->susp.t0 = t0;
      susp[ch]->susp.log_stop_cnt = 0;
      
      snd[ch] = sound_create((snd_susp_type)susp[ch], t0, 
                             rate, 
                             scale_factor);
   }

   if (num_channels > 1) {
      LVAL array = newvector(num_channels);
      for(ch=0; ch<num_channels; ch++)
         setelement(array, ch, cvsound(snd[ch]));

      setvalue(xlenter("S"), array);
   }
   else {
      LVAL s = cvsound(snd[0]);

      setvalue(xlenter("S"), s);
   }
}

LOCAL int nyx_is_labels(LVAL expr)
{
   /* make sure that we have a list whose first element is a
      list of the form (time "label") */

   LVAL label;
   LVAL first;
   LVAL second;
   LVAL third;

   if (expr == NULL) {
      return 0;
   }

   while (expr != NULL) {
      if (!consp(expr))
         return 0;

      label = car(expr);

      if (!consp(label))
         return 0;

      first = car(label);
      if (!(floatp(first) || fixp(first)))
         return 0;

      if (!consp(cdr(label)))
         return 0;

      second = car(cdr(label));

      if (floatp(second) || fixp(second)) {
         if (!consp(cdr(cdr(label))))
            return 0;
         third = car(cdr(cdr(label)));
         if (!(stringp(third)))
            return 0;
      }
      else
         if (!(stringp(second)))
            return 0;

      expr = cdr(expr);
   }

   return 1;
}

nyx_rval nyx_get_type(LVAL expr)
{
   if (nyx_result_type != nyx_error) {
      return nyx_result_type;
   }

   nyx_result_type = nyx_error;

   if (expr==NULL) {
      return nyx_result_type;
   }

   switch(ntype(expr))
   {
      case FIXNUM:
         nyx_result_type = nyx_int;
      break;
         
      case FLONUM:
         nyx_result_type = nyx_double;
      break;

      case STRING:
         nyx_result_type = nyx_string;
      break;

      case VECTOR:
      {
         /* make sure it's a vector of sounds */
         int i;
         nyx_result_type = nyx_audio;
         for(i=0; i<getsize(expr); i++) {
            if (!soundp(getelement(expr, i))) {
               nyx_result_type = nyx_error;
               break;
            }
         }
      }
      break;

      case CONS:
      {
         /* see if it's a list of time/string pairs representing a
            label track */
         if (nyx_is_labels(expr))
            nyx_result_type = nyx_labels;
      }
      break;

      case EXTERN:
      {
         if (soundp(expr))
            nyx_result_type = nyx_audio;
      }
      break;
   } /* switch */

   return nyx_result_type;
}

nyx_rval nyx_eval_expression(const char *expr_string)
{
   LVAL expr = NULL;

#if defined(NYX_MEMORY_STATS) && NYX_MEMORY_STATS
   printf("\nnyx_eval_expression before\n");
   xmem();
#endif

   nyx_expr_string = expr_string;
   nyx_expr_len = strlen(nyx_expr_string);
   nyx_expr_pos = 0;

   nyx_result = NULL;
   nyx_result_type = nyx_error;

   xlprot1(expr);

   /* Setup a new context */
   xlbegin(&nyx_cntxt, CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL|CF_ERROR, s_true);

   /* setup the error return */
   if (setjmp(nyx_cntxt.c_jmpbuf)) {
      // If the script is cancelled or some other condition occurs that causes
      // the script to exit and return to this level, then we don't need to
      // restore the previous context.
      goto finish;
   }

   while(nyx_expr_pos < nyx_expr_len) {
      expr = NULL;

      /* read an expression */
      if (!xlread(getvalue(s_stdin), &expr, FALSE))
         break;

      #if 0
      /* save the input expression (so the user can refer to it
         as +, ++, or +++) */
      xlrdsave(expr);
      #endif
      
      /* evaluate the expression */
      nyx_result = xleval(expr);
   }

   xlflush();

   xltoplevel();

 finish:

   xlpop(); /* unprotect expr */

#if defined(NYX_MEMORY_STATS) && NYX_MEMORY_STATS
   printf("\nnyx_eval_expression after\n");
   xmem();
#endif

   return nyx_get_type(nyx_result);
}

int nyx_get_audio_num_channels()
{
   if (nyx_get_type(nyx_result) != nyx_audio)
      return 0;

   if (vectorp(nyx_result))
      return getsize(nyx_result);
   else
      return 1;
}

int nyx_get_audio(nyx_audio_callback callback, void *userdata)
{
   sample_block_type block;
   sound_type snd;
   sound_type *snds = NULL;
   float *buffer = NULL;
   long bufferlen = 0;
   long *totals = NULL;
   long *lens = NULL;
   long cnt;
   int result = 0;
   int num_channels;
   int ch, i;
   int success = FALSE;

   if (nyx_get_type(nyx_result) != nyx_audio)
      return success;

#if defined(NYX_MEMORY_STATS) && NYX_MEMORY_STATS
   printf("\nnyx_get_audio before\n");
   xmem();
#endif

   num_channels = nyx_get_audio_num_channels();

   snds = (sound_type *)malloc(num_channels * sizeof(sound_type));
   if (snds == NULL) {
      goto finish;
   }

   totals = (long *)malloc(num_channels * sizeof(long));
   if (totals == NULL) {
      goto finish;
   }

   lens = (long *)malloc(num_channels * sizeof(long));
   if (lens == NULL) {
      goto finish;
   }

   /* Setup a new context */
   xlbegin(&nyx_cntxt, CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL|CF_ERROR, s_true);

   /* setup the error return */
   if (setjmp(nyx_cntxt.c_jmpbuf)) {
      // If the script is cancelled or some other condition occurs that causes
      // the script to exit and return to this level, then we don't need to
      // restore the previous context.
      goto finish;
   }

   for(ch=0; ch<num_channels; ch++) {
      if (num_channels == 1)
         snd = getsound(nyx_result);
      else
         snd = getsound(getelement(nyx_result, ch));
      snds[ch] = snd;
      totals[ch] = 0;
      lens[ch] = snd_length(snd, snd->stop);
   }

   while(result==0) {
      for(ch=0; ch<num_channels; ch++) {
         snd = snds[ch];
         cnt = 0;
         block = snd->get_next(snd, &cnt);
         if (block == zero_block || cnt == 0) {
            result = -1;
            break;
         }

         /* copy the data to a temporary buffer and scale it
            by the appropriate scale factor */

         if (cnt > bufferlen) {
            if (buffer)
               free(buffer);

            buffer = (float *)malloc(cnt * sizeof(float));
            if (buffer == NULL) {
               goto finish;
            }

            bufferlen = cnt;
         }

         memcpy(buffer, block->samples, cnt * sizeof(float));

         for(i=0; i<cnt; i++)
            buffer[i] *= snd->scale;

         result = callback(buffer, ch,
                           totals[ch], cnt, lens[ch], userdata);

         if (result != 0) {
            // The user canceled or some other error occurred, so we use
            // xlsignal() to jump back to our error handler.
            xlsignal(NULL, NULL);
            // never get here.
         }

         totals[ch] += cnt;
      }
   }

   success = TRUE;

   xltoplevel();

 finish:

   gc();

   if (buffer) {
      free(buffer);
   }

   if (lens) {
      free(lens);
   }

   if (totals) {
      free(totals);
   }

   if (snds) {
      free(snds);
   }

#if defined(NYX_MEMORY_STATS) && NYX_MEMORY_STATS
   printf("\nnyx_get_audio after\n");
   xmem();
#endif

   return success;
}

int nyx_get_int()
{
   if (nyx_get_type(nyx_result) != nyx_int)
      return -1;

   return getfixnum(nyx_result);
}

double nyx_get_double()
{
   if (nyx_get_type(nyx_result) != nyx_double)
      return -1.0;

   return getflonum(nyx_result);
}

const char *nyx_get_string()
{
   if (nyx_get_type(nyx_result) != nyx_string)
      return NULL;

   return (const char *)getstring(nyx_result);
}

unsigned int nyx_get_num_labels()
{
   LVAL s = nyx_result;
   int count = 0;

   if (nyx_get_type(nyx_result) != nyx_labels)
      return 0;

   while(s) {
      count++;
      s = cdr(s);
   }

   return count;
}

void nyx_get_label(unsigned int index,
                   double *start_time,
                   double *end_time,
                   const char **label)
{
   LVAL s = nyx_result;
   LVAL label_expr;
   LVAL t0_expr;
   LVAL t1_expr;
   LVAL str_expr;

   if (nyx_get_type(nyx_result) != nyx_labels)
      return;

   while(index) {
      index--;
      s = cdr(s);
      if (s == NULL) {
         // index was larger than number of labels
         return;
      }
   }

   /* We either have (t0 "label") or (t0 t1 "label") */

   label_expr = car(s);
   t0_expr = car(label_expr);
   t1_expr = car(cdr(label_expr));
   if (stringp(t1_expr)) {
      str_expr = t1_expr;
      t1_expr = t0_expr;
   }
   else
      str_expr = car(cdr(cdr(label_expr)));

   if (floatp(t0_expr))
      *start_time = getflonum(t0_expr);
   else if (fixp(t0_expr))
      *start_time = (double)getfixnum(t0_expr);

   if (floatp(t1_expr))
      *end_time = getflonum(t1_expr);
   else if (fixp(t1_expr))
      *end_time = (double)getfixnum(t1_expr);

   *label = (const char *)getstring(str_expr);
}

const char *nyx_get_error_str()
{
   return NULL;
}

void nyx_set_os_callback(nyx_os_callback callback, void *userdata)
{
   nyx_os_cb = callback;
   nyx_os_ud = userdata;
}

void nyx_stop()
{
   xlflush();
   xltoplevel();
}

void nyx_break()
{
   xlflush();
   xlbreak("BREAK", s_unbound);
}

void nyx_continue()
{
   xlflush();
   xlcontinue();
}

int ostgetc()
{
   if (nyx_expr_pos < nyx_expr_len) {
      fflush(stdout);
      return (nyx_expr_string[nyx_expr_pos++]);
   }
   else if (nyx_expr_pos == nyx_expr_len) {
      /* Add whitespace at the end so that the parser
         knows that this is the end of the expression */
      nyx_expr_pos++;
      return '\n';
   }
   else
      return EOF;
}

/* osinit - initialize */
void osinit(char *banner)
{
}

/* osfinish - clean up before returning to the operating system */
void osfinish(void) 
{
}

/* oserror - print an error message */
void oserror(char *msg)
{
   printf("nyx error: %s\n", msg);
}

long osrand(long n)
{
   return (((int) rand()) % n);
}

/* cd ..
open - open an ascii file */
FILE *osaopen(name,mode) char *name,*mode;
{
   FILE *fp;
   fp = fopen(name,mode);
   return fp;
}

/* osbopen - open a binary file */
FILE *osbopen(char *name, char *mode)
{
   char bmode[10];
   FILE *fp;
   
   strncpy(bmode, mode, 8);
   strcat(bmode,"b");
   fp = fopen(name,bmode);
   return fp;
}

/* osclose - close a file */
int osclose(FILE *fp)
{
   return (fclose(fp));
}

/* osagetc - get a character from an ascii file */
int osagetc(FILE *fp)
{
   return (getc(fp));
}

/* osaputc - put a character to an ascii file */
int osaputc(int ch, FILE *fp)
{
   return (putc(ch,fp));
}

/* osoutflush - flush output to a file */
void osoutflush(FILE *fp) { fflush(fp); }

extern int dbgflg;

/* osbgetc - get a character from a binary file */
/* int osbgetc(fp) FILE *fp; {return (getc(fp));} */
#ifndef WIN32  // duplicated in winfun.c, per James Crook, 7/4/2003
	int osbgetc(FILE *fp)
	{
		return (getc(fp));
	}
#endif

/* osbputc - put a character to a binary file */
int osbputc(int ch, FILE *fp)
{
   return (putc(ch,fp));
}

/* ostputc - put a character to the terminal */
void ostputc(int ch)
{     
   oscheck();		/* check for control characters */
   
   if (nyx_output_cb)
      nyx_output_cb(ch, nyx_output_ud);
   else
      putchar(((char) ch));
}

/* ostoutflush - flush output buffer */
void ostoutflush()
{
   if (!nyx_output_cb)
      fflush(stdout);
}

/* osflush - flush the terminal input buffer */
void osflush(void)
{
}

/* oscheck - check for control characters during execution */
void oscheck(void)
{
   if (nyx_os_cb) {
      nyx_os_cb(nyx_os_ud);
   }
   /* if they hit control-c:
      xflush(); xltoplevel(); return;
   */
}

/* xsystem - execute a system command */
#ifndef WIN32  // duplicated in winfun.c, per James Crook, 7/4/2003
LVAL xsystem()
{
   if (moreargs()) {
      unsigned char *cmd;
      cmd = (unsigned char *)getstring(xlgastring());
      fprintf(stderr, "Will not execute system command: %s\n", cmd);
   }
   return s_true;
}
#endif

#ifndef WIN32
/* xsetdir -- set current directory of the process */
LVAL xsetdir()
{
   char *dir = (char *)getstring(xlgastring());
   int result;
   LVAL cwd = NULL;
   xllastarg();
   result = chdir(dir);
   if (result) {
      perror("SETDIR");
   }
   dir = getcwd(NULL, 1000);
   if (dir) {
       cwd = cvstring(dir);
       free(dir);
    }
   return cwd;
}
#endif

/* xgetkey - get a key from the keyboard */
#ifndef WIN32  // duplicated in winfun.c, per James Crook, 7/4/2003
	LVAL xgetkey() {xllastarg(); return (cvfixnum((FIXTYPE)getchar()));}
#endif

/* ossymbols - enter os specific symbols */
#ifndef WIN32  // duplicated in winfun.c, per James Crook, 7/4/2003
	void ossymbols(void) {}
#endif

/* xsetupconsole -- used to configure window in Win32 version */
#ifndef WIN32  // duplicated in winfun.c, per James Crook, 7/4/2003
	LVAL xsetupconsole() { return NULL; }
#endif

const char os_pathchar = '/';
const char os_sepchar = ':';

/* control-C handling */
void ctcinit()	{}

/* xechoenabled -- set/clear echo_enabled flag (unix only) */
LVAL xechoenabled() { return NULL; }

/* osdir_list_start -- open a directory listing */
int osdir_list_start(char *path) { return FALSE; }

/* osdir_list_next -- read the next entry from a directory */
char *osdir_list_next() { return NULL; }

/* osdir_list_finish -- close an open directory */
void osdir_list_finish() { return; }

#ifndef WIN32
/* xget_temp_path -- get a path to create temp files */
LVAL xget_temp_path()
{
   char *tmp = getenv("TMPDIR");
   if (!tmp || !*tmp) {
      tmp = getenv("TMP");
      if (!tmp || !*tmp) {
         tmp = "/tmp/";
      }
   }
   return cvstring(tmp);
}
#endif

#ifndef WIN32
/* xget_user -- get a string identifying the user, for use in file names */
LVAL xget_user()
{
   char *user = getenv("USER");
   if (!user || !*user) {
      user = getenv("USERNAME");
      if (!user || !*user) {
         errputstr("Warning: could not get user ID, using 'nyquist'\n");
         user = "nyquist";
      }
   }
   return cvstring(user);
}
#endif
