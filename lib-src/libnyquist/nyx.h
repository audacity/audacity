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

   /* enabled experimental v5 functionality */
   #define EXPERIMENTAL_NYX_V5 1

   #define nyx_returns_start_and_end_time 1

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
   void        nyx_set_xlisp_path(const char *path);

   /* should return return 0 for success, -1 for error */
   typedef int (*nyx_audio_callback)(float *buffer,
                                     int channel,
                                     long start, long len,
                                     long totlen,
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

   void        nyx_set_audio_params(double rate, long len);

   void        nyx_set_input_audio(nyx_audio_callback callback,
                                   void *userdata,
                                   int num_channels,
                                   long len, double rate);

   char       *nyx_get_audio_name();
   void        nyx_set_audio_name(const char *name);

#if defined(EXPERIMENTAL_NYX_V5)
   /* should return return 0 for success, -1 for error */
   typedef int (*nyx_cmd_callback)(int argc,
                                   void *userdata);

   /* Set to NULL to stop checking */
   void        nyx_set_cmd_callback(const char *name, nyx_cmd_callback callback, void *userdata);
   void        nyx_define_function(const char *name, nyx_cmd_callback callback, void *userdata);
   void        nyx_set_property(const char *varname,
                                const char *propname,
                                nyx_rval type,
                                const void *value);

   /* used by CMD callback */
   void        nyx_add_symbol(const char *sym);

   void        nyx_fail(char *msg);
   nyx_rval    nyx_get_next_arg();
   void        nyx_last_arg();

   int         nyx_get_int_arg();
   int         nyx_get_int_arg_kw(const char *key, int *arg);

   long        nyx_get_long_arg();
   int         nyx_get_long_arg_kw(const char *key, long *arg);

   double      nyx_get_double_arg();
   int         nyx_get_double_arg_kw(const char *key, double *arg);

   const char  *nyx_get_string_arg();
   int         nyx_get_string_arg_kw(const char *key, const char **arg);

   const char  *nyx_get_symbol_arg();
   int         nyx_get_symbol_arg_kw(const char *key, const char **arg);

   typedef union
   {
      const char *str;
      double flo;
      int fix;
   } nyx_list;

   nyx_list    *nyx_get_list_arg();
   int         nyx_get_list_arg_kw(const char *key, nyx_list **arg);

   int         nyx_get_samples_arg(float **samples); /* must free() buffer */


   void        nyx_set_int_rval(int val);
   void        nyx_set_double_rval(double val);
   void        nyx_set_string_rval(char *val);
   void        nyx_set_samples_rval(float *samples, int count);
#endif

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

