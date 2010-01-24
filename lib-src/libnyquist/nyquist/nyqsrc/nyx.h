/**********************************************************************

  nyx.h

  Nyx: A very simple external interface to Nyquist

  Dominic Mazzoni

**********************************************************************/

#ifndef __NYX__
#define __NYX__

#ifdef __cplusplus
extern "C"
{
#endif /* __cplusplus */

   typedef enum {
      nyx_error,
      nyx_audio,
      nyx_int,
      nyx_double,
      nyx_string,
      nyx_labels
   } nyx_rval;
   
   void        nyx_init();
   void        nyx_cleanup();

   /* should return return 0 for success, -1 for error */
   typedef int (*nyx_audio_callback)(float *buffer,
                                     int channel,
                                     long start, long len,
                                     void *userdata);
   
   void        nyx_set_input_audio(nyx_audio_callback callback,
                                   void *userdata,
                                   int num_channels,
                                   long len, double rate);
   
   nyx_rval    nyx_eval_expression(const char *expr);
   
   int         nyx_get_audio_num_channels();
   void        nyx_get_audio(nyx_audio_callback callback,
                             void *userdata);

   int         nyx_get_int();
   double      nyx_get_double();
   const char *nyx_get_string();

   int         nyx_get_num_labels();
   void        nyx_get_label(int index,
                             double *time,
                             const char **label);

   const char *nyx_get_error_str();



#ifdef __cplusplus
}
#endif /* __cplusplus */
   
#endif /* __NYX__ */

