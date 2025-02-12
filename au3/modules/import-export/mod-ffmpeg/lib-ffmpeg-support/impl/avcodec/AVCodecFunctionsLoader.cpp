/**********************************************************************

  Audacity: A Digital Audio Editor

  AVCodecFunctionsLoaders.cpp

  Dmitry Vedenko

**********************************************************************/

#include "AVCodecFunctionsLoader.h"

#include <wx/dynlib.h>

#include "modules/import-export/mod-ffmpeg/lib-ffmpeg-support/AVCodecFunctions.h"
#include "modules/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/DynamicLibraryHelpers.h"

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
    RESOLVE(avcodec_alloc_context3);
    RESOLVE(av_codec_is_encoder);
    RESOLVE(avcodec_fill_audio_frame);

    GET_SYMBOL(av_packet_alloc);
    GET_SYMBOL(av_packet_free);
    GET_SYMBOL(avcodec_free_context);
    GET_SYMBOL(avcodec_parameters_to_context);
    GET_SYMBOL(avcodec_parameters_from_context);
    // Missing in FFmpeg 59
    GET_SYMBOL(avcodec_decode_audio4);
    GET_SYMBOL(avcodec_encode_audio2);
    GET_SYMBOL(avcodec_register_all);
    GET_SYMBOL(av_codec_next);
    GET_SYMBOL(av_codec_iterate);
    // New decoding API
    GET_SYMBOL(avcodec_send_packet);
    GET_SYMBOL(avcodec_receive_frame);
    GET_SYMBOL(avcodec_send_frame);
    GET_SYMBOL(avcodec_receive_packet);

    return GetAVVersion(lib, "avcodec_version", functions.AVCodecVersion);
}
