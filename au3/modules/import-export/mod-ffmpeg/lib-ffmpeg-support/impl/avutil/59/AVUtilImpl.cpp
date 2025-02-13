/**********************************************************************

  Audacity: A Digital Audio Editor

  AVUtilImpl.cpp

  Dmitry Vedenko

**********************************************************************/

extern "C"
{
#include "../../avutil/59/avconfig.h"
#include "../../ffmpeg-7.0.0-single-header.h"
}

#include <wx/log.h>

#include "modules/import-export/mod-ffmpeg/lib-ffmpeg-support/FFmpegFunctions.h"

#include "modules/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVChannelLayoutWrapper.h"
#include "modules/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVFrameWrapper.h"

#include "../../FFmpegAPIResolver.h"
#include "../../FFmpegLog.h"

namespace avutil_59 {
#include "../AVChannelLayoutWrapperImpl.inl"
#include "../AVFrameWrapperImpl.inl"
#include "../FFmpegLogImpl.inl"

void Register()
{
    FFmpegAPIResolver::Get().AddAVUtilFactories(59, {
            &CreateAVFrameWrapper,
            &CreateLogCallbackSetter,
            &CreateDefaultChannelLayout,
            &CreateLegacyChannelLayout,
            &CreateAVChannelLayout
        });
}

const bool registered = ([]() {
    FFmpegAPIResolver::Get().AddAVUtilFactories(59, {
            &CreateAVFrameWrapper,
            &CreateLogCallbackSetter,
            &CreateDefaultChannelLayout,
            &CreateLegacyChannelLayout,
            &CreateAVChannelLayout
        });

    return true;
})();
}
