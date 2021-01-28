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
#else
#include <windows.h>
#include <direct.h>
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
LOCAL long                nyx_input_length = 0;
LOCAL char               *nyx_audio_name = NULL;

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
            nval = nyx_dup_value(cdr(val));
            nval = cons(nyx_dup_value(car(val)), nval);
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

         // Ignore *SCRATCH* since it's allowed to be updated
         if (strcmp(name, "*SCRATCH*") == 0) {
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
   LVAL sscratch = xlenter("*SCRATCH*"); // one-time lookup
   int i;

   // Scan all obarray vectors
   for (i = 0; i < HSIZE; i++) {
      LVAL last = NULL;
      LVAL dcon;

      // Scan all elements
      for (dcon = getelement(obvec, i); dcon; dcon = cdr(dcon)) {
         LVAL dsym = car(dcon);
         char *name = (char *)getstring(getpname(dsym));
         LVAL scon;

         // Ignore *OBARRAY* since setting it causes the input array to be
         // truncated.
         if (strcmp(name, "*OBARRAY*") == 0) {
            continue;
         }

         // Ignore *SCRATCH* since it's allowed to be updated
         if (strcmp(name, "*SCRATCH*") == 0) {
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

         // If we didn't find the symbol in the original obarray, then it
         // must've been added and must be removed from the current obarray.
         // Exception: if the new symbol is a property symbol of *scratch*,
         // then allow the symbol to stay; otherwise, property lookups will
         // fail.
         if (scon == NULL) {
            // check property list of scratch
            if (findprop(sscratch, dsym) == NIL) {
               if (last) {
                  rplacd(last, cdr(dcon));
               }
               else {
                  setelement(obvec, i, cdr(dcon));
               }
            } // otherwise, keep new property symbol
         }

         // Must track the last dcon for symbol removal
         last = dcon;
      }
   }
}

#else

LOCAL LVAL copylist(LVAL from)
{
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

      nyx_audio_name = NULL;
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
   setvalue(xlenter(nyx_get_audio_name()), NIL);

   // Free excess memory segments - does a gc()
   freesegs();

   // Free unused memory pools
   falloc_gc();

   // No longer need the callbacks
   nyx_output_cb = NULL;
   nyx_os_cb = NULL;

   // Reset vars
   nyx_input_length = 0;

   if (nyx_audio_name) {
      free(nyx_audio_name);
      nyx_audio_name = NULL;
   }

#if defined(NYX_MEMORY_STATS) && NYX_MEMORY_STATS
   printf("\nnyx_cleanup\n");
   xmem();
#endif
}

void nyx_set_xlisp_path(const char *path)
{
   set_xlisp_path(path);
}

LOCAL void nyx_susp_fetch(nyx_susp_type susp, snd_list_type snd_list)
{
   sample_block_type         out;
   sample_block_values_type  out_ptr;
   long                      n;
   int                       err;

   falloc_sample_block(out, "nyx_susp_fetch");
   out_ptr = out->samples;
   snd_list->block = out;

   n = max_sample_block_len;
   if (susp->susp.current + n > susp->len) {
      n = susp->len - susp->susp.current;
   }

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

char *nyx_get_audio_name()
{
   if (!nyx_audio_name) {
      nyx_audio_name = strdup("S");
   }

   return nyx_audio_name;
}

void nyx_set_audio_name(const char *name)
{
   if (nyx_audio_name) {
      free(nyx_audio_name);
      nyx_audio_name = NULL;
   }

   nyx_audio_name = strdup(name);
}

void nyx_set_audio_params(double rate, long len)
{
   LVAL flo;
   LVAL con;

   xlstkcheck(2);
   xlsave(flo);
   xlsave(con);

   /* Bind the sample rate to the "*sound-srate*" global */
   flo = cvflonum(rate);
   setvalue(xlenter("*DEFAULT-SOUND-SRATE*"), flo);
   setvalue(xlenter("*SOUND-SRATE*"), flo);

   /* Bind the control sample rate to "*control-srate*" globals */
   flo = cvflonum((double) rate / 20.0);
   setvalue(xlenter("*DEFAULT-CONTROL-SRATE*"), flo);
   setvalue(xlenter("*CONTROL-SRATE*"), flo);

   /* Bind selection len to "len" global */
   nyx_input_length = len;
   flo = cvflonum(len);
   setvalue(xlenter("LEN"), flo);

   /* Set the "*warp*" global based on the length of the audio */
   con = cons(NULL, NULL);
   flo = cvflonum(len > 0 ? (double) len / rate : 1.0);
   con = cons(flo, con);
   flo = cvflonum(0);
   con = cons(flo, con);
   setvalue(xlenter("*WARP*"), con);

   xlpopn(2);
}

void nyx_set_input_audio(nyx_audio_callback callback,
                         void *userdata,
                         int num_channels,
                         long len, double rate)
{
   LVAL val;
   int ch;

   nyx_set_audio_params(rate, len);

   if (num_channels > 1) {
      val = newvector(num_channels);
   }

   xlprot1(val);

   for (ch = 0; ch < num_channels; ch++) {
      nyx_susp_type susp;
      sound_type snd;

      falloc_generic(susp, nyx_susp_node, "nyx_set_input_audio");

      susp->callback = callback;
      susp->userdata = userdata;
      susp->len = len;
      susp->channel = ch;

      susp->susp.fetch = (snd_fetch_fn)nyx_susp_fetch;
      susp->susp.keep_fetch = NULL;
      susp->susp.free = (snd_free_fn)nyx_susp_free;
      susp->susp.mark = NULL;
      susp->susp.print_tree = (snd_print_tree_fn)nyx_susp_print_tree;
      susp->susp.name = "nyx";
      susp->susp.toss_cnt = 0;
      susp->susp.current = 0;
      susp->susp.sr = rate;
      susp->susp.t0 = 0.0;
      susp->susp.log_stop_cnt = 0;

      snd = sound_create((snd_susp_type) susp, 0.0, rate, 1.0);
      if (num_channels > 1) {
         setelement(val, ch, cvsound(snd));
      }
      else {
         val = cvsound(snd);
      }
   }

   setvalue(xlenter(nyx_get_audio_name()), val);

   xlpop();
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
      if (!consp(expr)) {
         return 0;
      }

      label = car(expr);

      if (!consp(label)) {
         return 0;
      }

      first = car(label);
      if (!(floatp(first) || fixp(first))) {
         return 0;
      }

      if (!consp(cdr(label))) {
         return 0;
      }

      second = car(cdr(label));

      if (floatp(second) || fixp(second)) {
         if (!consp(cdr(cdr(label)))) {
            return 0;
         }
         third = car(cdr(cdr(label)));
         if (!(stringp(third))) {
            return 0;
         }
      }
      else {
         if (!(stringp(second))) {
            return 0;
         }
      }

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

   if (expr == NULL) {
      return nyx_result_type;
   }

   switch (ntype(expr))
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
         for (i = 0; i < getsize(expr); i++) {
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
         if (nyx_is_labels(expr)) {
            nyx_result_type = nyx_labels;
         } else {
            nyx_result_type = nyx_list;
         }
      }
      break;

      case EXTERN:
      {
         if (soundp(expr)) {
            nyx_result_type = nyx_audio;
         }
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

   nyx_result = NULL;
   nyx_result_type = nyx_error;

   // Check argument
   if (!expr_string || !strlen(expr_string)) {
      return nyx_get_type(nyx_result);
   }

   nyx_expr_string = expr_string;
   nyx_expr_len = strlen(nyx_expr_string);
   nyx_expr_pos = 0;

   // Protect the expression from being garbage collected
   xlprot1(expr);

   // Setup a new context
   xlbegin(&nyx_cntxt, CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL|CF_ERROR, s_true);

   // Set the context jump destination
   if (_setjmp(nyx_cntxt.c_jmpbuf)) {
      // If the script is cancelled or some other condition occurs that causes
      // the script to exit and return to this level, then we don't need to
      // restore the previous context.
      goto finish;
   }

   while (nyx_expr_pos < nyx_expr_len) {
      expr = NULL;

      // Read an expression
      if (!xlread(getvalue(s_stdin), &expr, FALSE)) {
         break;
      }

      #if 0
      /* save the input expression (so the user can refer to it
         as +, ++, or +++) */
      xlrdsave(expr);
      #endif

      // Evaluate the expression
      nyx_result = xleval(expr);
   }

   // This will unwind the xlisp context and restore internals to a point just
   // before we issued our xlbegin() above.  This is important since the internal
   // xlisp stacks will contain pointers to invalid objects otherwise.
   //
   // Also note that execution will jump back up to the statement following the
   // _setjmp() above.
   xljump(&nyx_cntxt, CF_TOPLEVEL, NIL);
   // Never reached

 finish:

   xlflush();

   xlpop(); // unprotect expr

   setvalue(xlenter(nyx_get_audio_name()), NIL);

   gc();

#if defined(NYX_MEMORY_STATS) && NYX_MEMORY_STATS
   printf("\nnyx_eval_expression after\n");
   xmem();
#endif

   return nyx_get_type(nyx_result);
}

int nyx_get_audio_num_channels()
{
   if (nyx_get_type(nyx_result) != nyx_audio) {
      return 0;
   }

   if (vectorp(nyx_result)) {
      if (getsize(nyx_result) == 1) {
        return -1; // invalid number of channels in array
      } else {
        return getsize(nyx_result);
      }
   }

   return 1;
}

int nyx_get_audio(nyx_audio_callback callback, void *userdata)
{
   float *buffer = NULL;
   sound_type *snds = NULL;
   long *totals = NULL;
   long *lens = NULL;
   sound_type snd;
   int result = 0;
   int num_channels;
   int ch;

   // Any variable whose value is set between the _setjmp() and the "finish" label
   // and that is used after the "finish" label, must be marked volatile since
   // any routine outside of the current one that calls _longjmp() will cause values
   // cached in registers to be lost.
   volatile int success = FALSE;

   if (nyx_get_type(nyx_result) != nyx_audio) {
      return FALSE;
   }

#if defined(NYX_MEMORY_STATS) && NYX_MEMORY_STATS
   printf("\nnyx_get_audio before\n");
   xmem();
#endif

   num_channels = nyx_get_audio_num_channels();

   buffer = (sample_type *) malloc(max_sample_block_len * sizeof(sample_type));
   if (buffer == NULL) {
      goto finish;
   }

   snds = (sound_type *) malloc(num_channels * sizeof(sound_type));
   if (snds == NULL) {
      goto finish;
   }

   totals = (long *) malloc(num_channels * sizeof(long));
   if (totals == NULL) {
      goto finish;
   }

   lens = (long *) malloc(num_channels * sizeof(long));
   if (lens == NULL) {
      goto finish;
   }

   // Setup a new context
   xlbegin(&nyx_cntxt, CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL|CF_ERROR, s_true);

   // Set the context jump destination
   if (_setjmp(nyx_cntxt.c_jmpbuf)) {
      // If the script is cancelled or some other condition occurs that causes
      // the script to exit and return to this level, then we don't need to
      // restore the previous context.
      goto finish;
   }

   if (nyx_input_length == 0) {
      LVAL val = getvalue(xlenter("LEN"));
      if (val != s_unbound) {
         if (ntype(val) == FLONUM) {
            nyx_input_length = (long) getflonum(val);
         }
         else if (ntype(val) == FIXNUM) {
            nyx_input_length = (long) getfixnum(val);
         }
      }
   }

   for (ch = 0; ch < num_channels; ch++) {
      if (num_channels == 1) {
         snd = getsound(nyx_result);
      }
      else {
         snd = getsound(getelement(nyx_result, ch));
      }
      snds[ch] = snd;
      totals[ch] = 0;
      lens[ch] = nyx_input_length;
   }

   while (result == 0) {
      for (ch =0 ; ch < num_channels; ch++) {
         sample_block_type block;
         long cnt;
         int i;

         snd = snds[ch];

         cnt = 0;
         block = sound_get_next(snd, &cnt);
         if (block == zero_block || cnt == 0) {
            success = TRUE;
            result = -1;
            break;
         }

         // Copy and scale the samples
         for (i = 0; i < cnt; i++) {
            buffer[i] = block->samples[i] * snd->scale;
         }

         result = callback((float *)buffer, ch,
                           totals[ch], cnt, lens[ch] ? lens[ch] : cnt, userdata);

         if (result != 0) {
            result = -1;
            break;
         }

         totals[ch] += cnt;
      }
   }

   // This will unwind the xlisp context and restore internals to a point just
   // before we issued our xlbegin() above.  This is important since the internal
   // xlisp stacks will contain pointers to invalid objects otherwise.
   //
   // Also note that execution will jump back up to the statement following the
   // _setjmp() above.
   xljump(&nyx_cntxt, CF_TOPLEVEL, NIL);
   // Never reached

 finish:

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

   gc();

#if defined(NYX_MEMORY_STATS) && NYX_MEMORY_STATS
   printf("\nnyx_get_audio after\n");
   xmem();
#endif

   return success;
}

int nyx_get_int()
{
   if (nyx_get_type(nyx_result) != nyx_int) {
      return -1;
   }

   return getfixnum(nyx_result);
}

double nyx_get_double()
{
   if (nyx_get_type(nyx_result) != nyx_double) {
      return -1.0;
   }

   return getflonum(nyx_result);
}

const char *nyx_get_string()
{
   if (nyx_get_type(nyx_result) != nyx_string) {
      return NULL;
   }

   return (const char *)getstring(nyx_result);
}

unsigned int nyx_get_num_labels()
{
   LVAL s;
   int count = 0;

   if (nyx_get_type(nyx_result) != nyx_labels) {
      return 0;
   }

   for (s = nyx_result; s; s = cdr(s)) {
      count++;
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

   if (nyx_get_type(nyx_result) != nyx_labels) {
      return;
   }

   while (index) {
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
   else {
      str_expr = car(cdr(cdr(label_expr)));
   }

   if (floatp(t0_expr)) {
      *start_time = getflonum(t0_expr);
   }
   else if (fixp(t0_expr)) {
      *start_time = (double)getfixnum(t0_expr);
   }

   if (floatp(t1_expr)) {
      *end_time = getflonum(t1_expr);
   }
   else if (fixp(t1_expr)) {
      *end_time = (double)getfixnum(t1_expr);
   }

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

   return EOF;
}

/* osinit - initialize */
void osinit(const char *banner)
{
}

/* osfinish - clean up before returning to the operating system */
void osfinish(void)
{
}

/* oserror - print an error message */
void oserror(const char *msg)
{
   errputstr(msg);
}

long osrand(long n)
{
   return (((int) rand()) % n);
}

/* cd ..
open - open an ascii file */
FILE *osaopen(const char *name, const char *mode)
{
   return fopen(name, mode);
}

/* osbopen - open a binary file */
FILE *osbopen(const char *name, const char *mode)
{
   char bmode[10];

   strncpy(bmode, mode, 8);
   strcat(bmode, "b");

   return fopen(name,bmode);
}

/* osclose - close a file */
int osclose(FILE *fp)
{
   return fclose(fp);
}

/* osagetc - get a character from an ascii file */
int osagetc(FILE *fp)
{
   return getc(fp);
}

/* osaputc - put a character to an ascii file */
int osaputc(int ch, FILE *fp)
{
   return putc(ch,fp);
}

/* osoutflush - flush output to a file */
void osoutflush(FILE *fp)
{
   fflush(fp);
}

/* osbgetc - get a character from a binary file */
int osbgetc(FILE *fp)
{
   return getc(fp);
}

/* osbputc - put a character to a binary file */
int osbputc(int ch, FILE *fp)
{
   return putc(ch, fp);
}

/* ostputc - put a character to the terminal */
void ostputc(int ch)
{
   oscheck();		/* check for control characters */

   if (nyx_output_cb) {
      nyx_output_cb(ch, nyx_output_ud);
   }
   else {
      putchar((char) ch);
   }
}

/* ostoutflush - flush output buffer */
void ostoutflush()
{
   if (!nyx_output_cb) {
      fflush(stdout);
   }
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
LVAL xsystem()
{
   if (moreargs()) {
      unsigned char *cmd;
      cmd = (unsigned char *)getstring(xlgastring());
      fprintf(stderr, "Will not execute system command: %s\n", cmd);
   }
   return s_true;
}

/* xsetdir -- set current directory of the process */
LVAL xsetdir()
{
   char *dir = (char *)getstring(xlgastring());
   int result;
   LVAL cwd = NULL;
   int verbose = TRUE;

   if (moreargs()) {
      verbose = (xlgetarg() != NIL);
   }

   xllastarg();

   result = chdir(dir);
   if (result) {
      /* perror("SETDIR"); -- Nyquist uses SETDIR to search for directories
       * at startup, so failures are normal, and seeing error messages
       * could be confusing, so don't print them. The NULL return indicates
       * an error, but doesn't tell which one it is.
       * But now, SETDIR has a second verbose parameter that is nil when
       * searching for directories. -RBD
       */
      if (verbose) perror("Directory Setting Error");
      return NULL;
   }

   dir = getcwd(NULL, 1000);
   if (dir) {
      cwd = cvstring(dir);
      free(dir);
   }

   return cwd;
}

/* xgetkey - get a key from the keyboard */
LVAL xgetkey()
{
   xllastarg();
   return (cvfixnum((FIXTYPE)getchar()));
}

/* ossymbols - enter os specific symbols */
void ossymbols(void)
{
}

/* xsetupconsole -- used to configure window in Win32 version */
LVAL xsetupconsole()
{
   return NULL;
}

#if defined(WIN32)
const char os_pathchar = '\\';
const char os_sepchar = ',';
#else
const char os_pathchar = '/';
const char os_sepchar = ':';
#endif

/* control-C handling */
void ctcinit()
{
}

/* xechoenabled -- set/clear echo_enabled flag (unix only) */
LVAL xechoenabled()
{
   return NULL;
}

#if defined(WIN32)

static WIN32_FIND_DATA FindFileData;
static HANDLE hFind = INVALID_HANDLE_VALUE;
#define OSDIR_LIST_READY 0
#define OSDIR_LIST_STARTED 1
#define OSDIR_LIST_DONE 2
static int osdir_list_status = OSDIR_LIST_READY;
#define OSDIR_MAX_PATH 256
static char osdir_path[OSDIR_MAX_PATH];

// osdir_list_start -- prepare to list a directory
int osdir_list_start(const char *path)
{
   if (strlen(path) >= OSDIR_MAX_PATH - 2) {
      xlcerror("LISTDIR path too big", "return nil", NULL);
      return FALSE;
   }
   strcpy(osdir_path, path);
   strcat(osdir_path, "/*"); // make a pattern to match all files

   if (osdir_list_status != OSDIR_LIST_READY) {
      osdir_list_finish(); // close previously interrupted listing
   }

   hFind = FindFirstFile(osdir_path, &FindFileData); // get the "."
   if (hFind == INVALID_HANDLE_VALUE) {
      return FALSE;
   }
   if (FindNextFile(hFind, &FindFileData) == 0) {
      return FALSE; // get the ".."
   }

   osdir_list_status = OSDIR_LIST_STARTED;

   return TRUE;
}

/* osdir_list_next -- read the next entry from a directory */
const char *osdir_list_next()
{
   if (FindNextFile(hFind, &FindFileData) == 0) {
      osdir_list_status = OSDIR_LIST_DONE;
      return NULL;
   }
   return FindFileData.cFileName;
}

/* osdir_list_finish -- close an open directory */
void osdir_list_finish()
{
   if (osdir_list_status != OSDIR_LIST_READY) {
      FindClose(hFind);
   }
   osdir_list_status = OSDIR_LIST_READY;
}

#else

#include <dirent.h>
#define OSDIR_LIST_READY 0
#define OSDIR_LIST_STARTED 1
#define OSDIR_LIST_DONE 2
static int osdir_list_status = OSDIR_LIST_READY;
static DIR *osdir_dir;

/* osdir_list_start -- open a directory listing */
int osdir_list_start(const char *path)
{
   if (osdir_list_status != OSDIR_LIST_READY) {
      osdir_list_finish(); /* close current listing */
   }
   osdir_dir = opendir(path);
   if (!osdir_dir) {
      return FALSE;
   }
   osdir_list_status = OSDIR_LIST_STARTED;
   return TRUE;
}

/* osdir_list_next -- read the next entry from a directory */
const char *osdir_list_next()
{
   struct dirent *entry;

   if (osdir_list_status != OSDIR_LIST_STARTED) {
      return NULL;
   }

   entry = readdir(osdir_dir);
   if (!entry) {
      osdir_list_status = OSDIR_LIST_DONE;
      return NULL;
   }
   return entry->d_name;
}

/* osdir_list_finish -- close an open directory */
void osdir_list_finish()
{
    if (osdir_list_status != OSDIR_LIST_READY) {
        closedir(osdir_dir);
    }
    osdir_list_status = OSDIR_LIST_READY;
}

#endif

/* xget_temp_path -- get a path to create temp files */
LVAL xget_temp_path()
{
   char *tmp;

#if defined(WINDOWS)
   tmp = getenv("TEMP");
#else
   tmp = getenv("TMPDIR");
#endif

   if (!tmp || !*tmp) {
      tmp = getenv("TMP");
      if (!tmp || !*tmp) {
#if defined(WINDOWS)
         tmp = "/";
#else
         tmp = "/tmp/";
#endif
      }
   }

   return cvstring(tmp);
}

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

#if defined(WINDOWS)
/* get_xlisp_path -- return path to xlisp */
void get_xlisp_path(char *p, long p_max)
{
   char *paths = getenv("XLISPPATH");

   if (!paths || !*paths) {
      *p = 0;
      return;
   }

   strncpy(p, paths, p_max);
   p[p_max-1] = 0;
}
#endif

