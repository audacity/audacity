/**********************************************************************

  Audacity: A Digital Audio Editor

  AVUtilImpl.cpp

  Dmitry Vedenko

**********************************************************************/

extern "C"
{
#include "../../avutil/57/avconfig.h"
#include "../../ffmpeg-5.0.1-single-header.h"
}

#include <wx/log.h>

#include "modules/import-export/mod-ffmpeg/lib-ffmpeg-support/FFmpegFunctions.h"

#include "modules/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVChannelLayoutWrapper.h"
#include "modules/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVFrameWrapper.h"

#include "../../FFmpegAPIResolver.h"
#include "../../FFmpegLog.h"

namespace avutil_57 {
#include "../AVChannelLayoutWrapperImpl.inl"
#include "../AVFrameWrapperImpl.inl"
#include "../FFmpegLogImpl.inl"

void Register()
{
    FFmpegAPIResolver::Get().AddAVUtilFactories(57, {
            &CreateAVFrameWrapper,
            &CreateLogCallbackSetter,
            &CreateDefaultChannelLayout,
            &CreateLegacyChannelLayout,
            &CreateAVChannelLayout
        });
}

const bool registered = ([]() {
    FFmpegAPIResolver::Get().AddAVUtilFactories(57, {
            &CreateAVFrameWrapper,
            &CreateLogCallbackSetter,
            &CreateDefaultChannelLayout,
            &CreateLegacyChannelLayout,
            &CreateAVChannelLayout
        });

    return true;
})();
}
