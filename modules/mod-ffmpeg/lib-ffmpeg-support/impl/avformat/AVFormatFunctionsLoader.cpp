/**********************************************************************

  Audacity: A Digital Audio Editor

  AVFormatFunctionsLoader.cpp

  Dmitry Vedenko

**********************************************************************/

#include "AVFormatFunctionsLoader.h"

#include <wx/dynlib.h>

#include "AVFormatFunctions.h"
#include "impl/DynamicLibraryHelpers.h"


bool LoadAVFormatFunctions(
   const wxDynamicLibrary& lib, AVFormatFunctions& functions)
{
   RESOLVE(avformat_find_stream_info);
   RESOLVE(av_read_frame);
   RESOLVE(av_seek_frame);
   RESOLVE(avformat_close_input);
   RESOLVE(avformat_write_header);
   RESOLVE(av_interleaved_write_frame);
   RESOLVE(avformat_new_stream);
   RESOLVE(avformat_alloc_context);
   RESOLVE(av_write_trailer);
   RESOLVE(av_codec_get_tag);
   RESOLVE(avformat_open_input);
   RESOLVE(avio_size);
   RESOLVE(avio_alloc_context);
   RESOLVE(av_guess_format);
   RESOLVE(avformat_free_context);
   
   GET_SYMBOL(av_oformat_next);
   GET_SYMBOL(av_register_all);
   GET_SYMBOL(avio_context_free);
   GET_SYMBOL(av_muxer_iterate);

   return GetAVVersion(lib, "avformat_version", functions.AVFormatVersion);
}
