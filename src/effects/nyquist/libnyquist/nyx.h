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

   #define nyx_returns_start_and_end_time 1

   typedef enum {
      nyx_error,
      nyx_audio,
      nyx_int,
      nyx_double,
      nyx_string,
      nyx_labels,
      nyx_list
   } nyx_rval;

   void        nyx_init();
   void        nyx_cleanup();
   void        nyx_set_xlisp_path(const char *path);

   /* should return return 0 for success, -1 for error */
   typedef int (*nyx_audio_callback)(float *buffer,
                                     int channel,
                                     int64_t start, int64_t len,
                                     int64_t totlen,
                                     void *userdata);

   typedef void (*nyx_output_callback)(int c,
                                       void *userdata);

   typedef void (*nyx_os_callback)(void *userdata);

   /* Set to NULL to stop capturing output */
   void        nyx_capture_output(nyx_output_callback callback,
                                  void *userdata);

   /* Set to NULL to stop checking */
   void        nyx_set_os_callback(nyx_os_callback callback,
                                   void *userdata);

   void        nyx_stop();
   void        nyx_break();
   void        nyx_continue();

   void        nyx_set_audio_params(double rate, int64_t len);

   void        nyx_set_input_audio(nyx_audio_callback callback,
                                   void *userdata,
                                   int num_channels,
                                   int64_t len, double rate);

   char       *nyx_get_audio_name();
   void        nyx_set_audio_name(const char *name);

   nyx_rval    nyx_eval_expression(const char *expr);

   /** @brief Get the number of channels in the Nyquist audio object
    *
    * @return The positive integer number of audio channels in the
    * Nyquist audio object, 0 if not an audio object, -1 one if
    * Nyquist returns an array of samples (which we can't handle)
    */
   int         nyx_get_audio_num_channels();
   int         nyx_get_audio(nyx_audio_callback callback,
                             void *userdata);

   int         nyx_get_int();
   double      nyx_get_double();
   const char *nyx_get_string();

   unsigned int nyx_get_num_labels();
   void         nyx_get_label(unsigned int index,
                              double *start_time,
                              double *end_time,
                              const char **label);

   const char *nyx_get_error_str();



#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __NYX__ */

