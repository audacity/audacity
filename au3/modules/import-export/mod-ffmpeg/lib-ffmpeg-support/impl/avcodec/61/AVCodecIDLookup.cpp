extern "C"
{
#include "../../avutil/59/avconfig.h"
#include "../../ffmpeg-7.0.0-single-header.h"
}

#include <algorithm>

#include "AVCodecID.h"

#include "../../FFmpegAPIResolver.h"

#define AV_CODEC_ID_ESCAPE130_DEPRECATED AV_CODEC_ID_ESCAPE130
#define AV_CODEC_ID_G2M_DEPRECATED AV_CODEC_ID_G2M
#define AV_CODEC_ID_WEBP_DEPRECATED AV_CODEC_ID_WEBP
#define AV_CODEC_ID_HEVC_DEPRECATED AV_CODEC_ID_HEVC
#define AV_CODEC_ID_PCM_S24LE_PLANAR_DEPRECATED AV_CODEC_ID_PCM_S24LE_PLANAR
#define AV_CODEC_ID_PCM_S32LE_PLANAR_DEPRECATED AV_CODEC_ID_PCM_S32LE_PLANAR
#define AV_CODEC_ID_OPUS_DEPRECATED AV_CODEC_ID_OPUS
#define AV_CODEC_ID_TAK_DEPRECATED AV_CODEC_ID_TAK

#define AV_CODEC_ID_VIMA AV_CODEC_ID_ADPCM_VIMA

namespace avcodec_61
{
#include "../../AVCodecIDLookup.inl"

const bool registered = ([]() {
   FFmpegAPIResolver::Get().AddAVCodecIDResolver(61, {
      &GetAVCodeID,
      &GetAudacityCodecID
   });

   return true;
})();

}
