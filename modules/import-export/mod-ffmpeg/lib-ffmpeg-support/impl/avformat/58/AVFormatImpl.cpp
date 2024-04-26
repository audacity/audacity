/**********************************************************************

  Audacity: A Digital Audio Editor

  AVFormatImpl.cpp

  Dmitry Vedenko

**********************************************************************/

extern "C"
{
#include "../../avutil/56/avconfig.h"
#include "../../ffmpeg-4.2.4-single-header.h"
}

#include <cstring>

#include "FFmpegFunctions.h"

#include "wrappers/AVFormatContextWrapper.h"
#include "wrappers/AVInputFormatWrapper.h"
#include "wrappers/AVIOContextWrapper.h"
#include "wrappers/AVOutputFormatWrapper.h"
#include "wrappers/AVStreamWrapper.h"

#include "wrappers/AVCodecWrapper.h"

#include "../../FFmpegAPIResolver.h"

namespace avformat_58
{
#include "../AVFormatContextWrapperImpl.inl"
#include "../AVInputFormatWrapperImpl.inl"
#include "../AVIOContextWrapperImpl.inl"
#include "../AVOutputFormatWrapperImpl.inl"
#include "../AVStreamWrapperImpl.inl"

const bool registered = ([]() {
   FFmpegAPIResolver::Get().AddAVFormatFactories(58, {
      &CreateAVFormatContextWrapper,
      &CreateAVInputFormatWrapper,
      &CreateAVIOContextWrapper,
      &CreateAVOutputFormatWrapper,
      &CreateAVStreamWrapper,
   });

   return true;
})();
}
