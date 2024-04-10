/**********************************************************************

  Audacity: A Digital Audio Editor

  AVUtilImpl.cpp

  Dmitry Vedenko

**********************************************************************/

extern "C"
{
#include "../../avutil/52/avconfig.h"
#include "../../ffmpeg-2.3.6-single-header.h"
}

#include <wx/log.h>

#include "FFmpegFunctions.h"

#include "wrappers/AVFrameWrapper.h"

#include "../../FFmpegAPIResolver.h"
#include "../../FFmpegLog.h"

namespace avutil_52
{
#include "../AVFrameWrapperImpl.inl"
#include "../FFmpegLogImpl.inl"

const bool registered = ([]() {
   FFmpegAPIResolver::Get().AddAVUtilFactories(52, {
      &CreateAVFrameWrapper,
      &CreateLogCallbackSetter
   });

   return true;
})();
}
