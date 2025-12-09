/**********************************************************************

  Audacity: A Digital Audio Editor

  AVUtilImpl.cpp

  Dmitry Vedenko

**********************************************************************/

extern "C"
{
#include "../../avutil/60/avconfig.h"
#include "../../ffmpeg-8.0.0-single-header.h"
}

#include <wx/log.h>

#include "mod-ffmpeg/lib-ffmpeg-support/FFmpegFunctions.h"

#include "mod-ffmpeg/lib-ffmpeg-support/wrappers/AVChannelLayoutWrapper.h"
#include "mod-ffmpeg/lib-ffmpeg-support/wrappers/AVFrameWrapper.h"

#include "../../FFmpegAPIResolver.h"
#include "../../FFmpegLog.h"

namespace avutil_60 {
#include "../AVChannelLayoutWrapperImpl.inl"
#include "../AVFrameWrapperImpl.inl"
#include "../FFmpegLogImpl.inl"

void Register()
{
    FFmpegAPIResolver::Get().AddAVUtilFactories(60, {
            &CreateAVFrameWrapper,
            &CreateLogCallbackSetter,
            &CreateDefaultChannelLayout,
            &CreateLegacyChannelLayout,
            &CreateAVChannelLayout
        });
}

const bool registered = ([]() {
    FFmpegAPIResolver::Get().AddAVUtilFactories(60, {
            &CreateAVFrameWrapper,
            &CreateLogCallbackSetter,
            &CreateDefaultChannelLayout,
            &CreateLegacyChannelLayout,
            &CreateAVChannelLayout
        });

    return true;
})();
}
