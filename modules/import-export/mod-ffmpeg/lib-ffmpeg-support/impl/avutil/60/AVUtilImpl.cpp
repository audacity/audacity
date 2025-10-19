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

#include "FFmpegFunctions.h"

#include "wrappers/AVChannelLayoutWrapper.h"
#include "wrappers/AVFrameWrapper.h"

#include "../../FFmpegAPIResolver.h"
#include "../../FFmpegLog.h"

namespace avutil_60
{
#include "../AVChannelLayoutWrapperImpl.inl"
#include "../AVFrameWrapperImpl.inl"
#include "../FFmpegLogImpl.inl"

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
