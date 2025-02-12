/**********************************************************************

  Audacity: A Digital Audio Editor

  AVUtilImpl.cpp

  Dmitry Vedenko

**********************************************************************/

extern "C"
{
#include "../../avutil/55/avconfig.h"
#include "../../ffmpeg-3.4.8-single-header.h"
}

#include <wx/log.h>

#include "modules/import-export/mod-ffmpeg/lib-ffmpeg-support/FFmpegFunctions.h"

#include "modules/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVChannelLayoutWrapper.h"
#include "modules/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVFrameWrapper.h"

#include "../../FFmpegAPIResolver.h"
#include "../../FFmpegLog.h"

namespace avutil_55 {
#include "../AVChannelLayoutWrapperImpl.inl"
#include "../AVFrameWrapperImpl.inl"
#include "../FFmpegLogImpl.inl"

void Register()
{
    FFmpegAPIResolver::Get().AddAVUtilFactories(55, {
            &CreateAVFrameWrapper,
            &CreateLogCallbackSetter,
            &CreateDefaultChannelLayout,
            &CreateLegacyChannelLayout,
            &CreateAVChannelLayout
        });
}

const bool registered = ([]() {
    FFmpegAPIResolver::Get().AddAVUtilFactories(55, {
            &CreateAVFrameWrapper,
            &CreateLogCallbackSetter,
            &CreateDefaultChannelLayout,
            &CreateLegacyChannelLayout,
            &CreateAVChannelLayout
        });

    return true;
})();
}
