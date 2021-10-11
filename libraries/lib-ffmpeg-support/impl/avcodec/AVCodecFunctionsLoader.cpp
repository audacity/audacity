/**********************************************************************

  Audacity: A Digital Audio Editor

  AVCodecFunctionsLoaders.cpp

  Dmitry Vedenko

**********************************************************************/

#include "AVCodecFunctionsLoader.h"

#include <wx/dynlib.h>

#include "AVCodecFunctions.h"
#include "impl/DynamicLibraryHelpers.h"


bool LoadAVCodecFunctions(
   const wxDynamicLibrary& lib, AVCodecFunctions& functions)
{
   RESOLVE(av_packet_ref);
   RESOLVE(av_packet_unref);
   RESOLVE(av_init_packet);
   RESOLVE(avcodec_find_encoder);
   RESOLVE(avcodec_find_encoder_by_name);
   RESOLVE(avcodec_find_decoder);
   RESOLVE(avcodec_get_name);
   RESOLVE(avcodec_open2);
   RESOLVE(avcodec_is_open);
   RESOLVE(avcodec_close);
   RESOLVE(avcodec_decode_audio4);
   RESOLVE(avcodec_encode_audio2);
   RESOLVE(avcodec_alloc_context3);
   RESOLVE(avcodec_register_all);
   RESOLVE(av_codec_next);
   RESOLVE(av_codec_is_encoder);
   RESOLVE(avcodec_fill_audio_frame);

   GET_SYMBOL(av_packet_alloc);
   GET_SYMBOL(av_packet_free);
   GET_SYMBOL(avcodec_free_context);

   return GetAVVersion(lib, "avcodec_version", functions.AVCodecVersion);
}
