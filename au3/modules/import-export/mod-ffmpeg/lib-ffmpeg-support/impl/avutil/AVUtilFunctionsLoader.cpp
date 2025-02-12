/**********************************************************************

  Audacity: A Digital Audio Editor

  AVUtilFunctionLoader.cpp

  Dmitry Vedenko

**********************************************************************/

#include "AVUtilFunctionsLoader.h"

#include <wx/dynlib.h>

#include "modules/import-export/mod-ffmpeg/lib-ffmpeg-support/AVUtilFunctions.h"
#include "modules/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/DynamicLibraryHelpers.h"

bool LoadAVUtilFunctions(
    const wxDynamicLibrary& lib, AVUtilFunctions& functions)
{
    RESOLVE(av_malloc);
    RESOLVE(av_free);
    RESOLVE(av_strdup);
    RESOLVE(av_dict_free);
    RESOLVE(av_dict_get);
    RESOLVE(av_dict_set);
    RESOLVE(av_dict_copy);
    RESOLVE(av_get_bytes_per_sample);
    RESOLVE(av_log_set_callback);
    RESOLVE(av_log_default_callback);

    RESOLVE(av_rescale_q);
    RESOLVE(av_frame_alloc);
    RESOLVE(av_frame_free);
    RESOLVE(av_samples_get_buffer_size);
    RESOLVE(av_strerror);

    GET_SYMBOL(av_get_default_channel_layout);
    GET_SYMBOL(av_channel_layout_default);

    return GetAVVersion(lib, "avutil_version", functions.AVUtilVersion);
}
