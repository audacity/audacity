/**********************************************************************

  Audacity: A Digital Audio Editor

  AVCodecIDLookup.cpp

  Dmitry Vedenko

**********************************************************************/

extern "C"
{
#include "../../avutil/52/avconfig.h"
#include "../../ffmpeg-2.3.6-single-header.h"
}

#include <algorithm>

#include "AVCodecID.h"

#include "../../FFmpegAPIResolver.h"

namespace avcodec_55
{
#include "../../AVCodecIDLookup.inl"

const bool registered = ([]() {
   FFmpegAPIResolver::Get().AddAVCodecIDResolver(55, {
      &GetAVCodeID,
      &GetAudacityCodecID
   });

   return true;
})();
}

