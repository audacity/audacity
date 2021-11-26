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

#include "FFmpegFunctions.h"

#include "wrappers/AVFrameWrapper.h"

#include "../../FFmpegAPIResolver.h"
#include "../../FFmpegLog.h"

namespace avutil_55
{
#include "../AVFrameWrapperImpl.inl"
#include "../FFmpegLogImpl.inl"

const bool registered = ([]() {
   FFmpegAPIResolver::Get().AddAVUtilFactories(55, {
      &CreateAVFrameWrapper,
      &CreateLogCallbackSetter
   });

   return true;
})();
}
