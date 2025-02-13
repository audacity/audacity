#include <QtCore/qdebug.h>
#include <QtCore/qglobal.h>
extern "C" {
#include "../../avutil/59/avconfig.h"
#include "../../ffmpeg-7.0.0-single-header.h"
}

#include <cstring>
#include <numeric>

#include "float_cast.h"

#include "modules/import-export/mod-ffmpeg/lib-ffmpeg-support/FFmpegFunctions.h"

#include "modules/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVCodecContextWrapper.h"
#include "modules/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVCodecWrapper.h"
#include "modules/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVPacketWrapper.h"

#include "../../FFmpegAPIResolver.h"

namespace avcodec_61 {
#include "../AVCodecContextWrapperImpl.inl"
#include "../AVCodecWrapperImpl.inl"
#include "../AVPacketWrapperImpl.inl"

void Register()
{
    FFmpegAPIResolver::Get().AddAVCodecFactories(61, {
            &CreateAVCodecContextWrapper,
            &CreateAVCodecContextWrapperFromCodec,
            &CreateAVCodecWrapper,
            &CreateAVPacketWrapper,
        });
}

const bool registered = ([]() {
    qDebug() << "Registering FFmpeg AVFormat version 61";
    FFmpegAPIResolver::Get().AddAVCodecFactories(61, {
            &CreateAVCodecContextWrapper,
            &CreateAVCodecContextWrapperFromCodec,
            &CreateAVCodecWrapper,
            &CreateAVPacketWrapper,
        });

    return true;
})();
}

#include "modules/import-export/mod-ffmpeg/lib-ffmpeg-support/FFmpegTypes.h"
static_assert(
    AV_CODEC_FLAG_GLOBAL_HEADER == AUDACITY_AV_CODEC_FLAG_GLOBAL_HEADER
    && AV_CODEC_CAP_SMALL_LAST_FRAME == AUDACITY_AV_CODEC_CAP_SMALL_LAST_FRAME
    && AV_CODEC_FLAG_QSCALE == AUDACITY_AV_CODEC_FLAG_QSCALE
    ,
    "FFmpeg constants don't match"
    );
