/**********************************************************************

  Audacity: A Digital Audio Editor

  AVFormatImpl.cpp

  Dmitry Vedenko

**********************************************************************/

extern "C"
{
#include "../../avutil/57/avconfig.h"
#include "../../ffmpeg-5.0.1-single-header.h"
}

#include <cstring>

#include "modules/import-export/mod-ffmpeg/lib-ffmpeg-support/FFmpegFunctions.h"

#include "modules/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVFormatContextWrapper.h"
#include "modules/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVInputFormatWrapper.h"
#include "modules/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVIOContextWrapper.h"
#include "modules/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVOutputFormatWrapper.h"
#include "modules/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVStreamWrapper.h"

#include "modules/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVCodecWrapper.h"

#include "../../FFmpegAPIResolver.h"

namespace avformat_59 {
#include "../AVFormatContextWrapperImpl.inl"
#include "../AVInputFormatWrapperImpl.inl"
#include "../AVIOContextWrapperImpl.inl"
#include "../AVOutputFormatWrapperImpl.inl"
#include "../AVStreamWrapperImpl.inl"

void Register()
{
    FFmpegAPIResolver::Get().AddAVFormatFactories(59, {
            &CreateAVFormatContextWrapper,
            &CreateAVInputFormatWrapper,
            &CreateAVIOContextWrapper,
            &CreateAVOutputFormatWrapper,
            &CreateAVStreamWrapper
        });
}

const bool registered = ([]() {
    FFmpegAPIResolver::Get().AddAVFormatFactories(59, {
            &CreateAVFormatContextWrapper,
            &CreateAVInputFormatWrapper,
            &CreateAVIOContextWrapper,
            &CreateAVOutputFormatWrapper,
            &CreateAVStreamWrapper,
        });

    return true;
})();
}
