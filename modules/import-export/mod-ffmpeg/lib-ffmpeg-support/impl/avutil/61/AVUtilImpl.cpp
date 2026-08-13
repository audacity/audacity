/**********************************************************************

  Audacity: A Digital Audio Editor

  AVUtilImpl.cpp

  Dmitry Vedenko

**********************************************************************/

extern "C"
{
#include "../../avutil/61/avconfig.h"
#include "../../ffmpeg-9.0.0-single-header.h"
}

#include <wx/log.h>

#include "FFmpegFunctions.h"

#include "wrappers/AVChannelLayoutWrapper.h"
#include "wrappers/AVFrameWrapper.h"

#include "../../FFmpegAPIResolver.h"
#include "../../FFmpegLog.h"

namespace avutil_61
{
#include "../AVChannelLayoutWrapperImpl.inl"
#include "../AVFrameWrapperImpl.inl"
#include "../FFmpegLogImpl.inl"

const bool registered = ([]() {
   FFmpegAPIResolver::Get().AddAVUtilFactories(61, {
      &CreateAVFrameWrapper,
      &CreateLogCallbackSetter,
      &CreateDefaultChannelLayout,
      &CreateLegacyChannelLayout,
      &CreateAVChannelLayout
   });

   return true;
})();
}
