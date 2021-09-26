/**********************************************************************

  Audacity: A Digital Audio Editor

  AVUtilFunctionLoader.cpp

  Dmitry Vedenko

**********************************************************************/

#include "AVUtilFunctionsLoader.h"

#include <wx/dynlib.h>

#include "AVUtilFunctions.h"
#include "impl/DynamicLibraryHelpers.h"


bool LoadAVUtilFunctions(
   const wxDynamicLibrary& lib, AVUtilFunctions& functions)
{
   RESOLVE(av_malloc);
   RESOLVE(av_free);
   RESOLVE(av_dict_free);
   RESOLVE(av_dict_get);
   RESOLVE(av_dict_set);
   RESOLVE(av_dict_copy);
   RESOLVE(av_get_bytes_per_sample);
   RESOLVE(av_log_set_callback);
   RESOLVE(av_log_default_callback);
   RESOLVE(av_fifo_alloc);
   RESOLVE(av_fifo_generic_read);
   RESOLVE(av_fifo_realloc2);
   RESOLVE(av_fifo_free);
   RESOLVE(av_fifo_size);
   RESOLVE(av_fifo_generic_write);
   RESOLVE(av_rescale_q);
   RESOLVE(av_frame_alloc);
   RESOLVE(av_frame_free);
   RESOLVE(av_samples_get_buffer_size);
   RESOLVE(av_get_default_channel_layout);
   RESOLVE(av_strerror);
   RESOLVE(av_get_channel_layout_nb_channels);

   return GetAVVersion(lib, "avutil_version", functions.AVUtilVersion);
}
