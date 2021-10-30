/*!********************************************************************
*
 Audacity: A Digital Audio Editor

 @file WaveTrackUtils.cpp

 Vitaly Sverchinsky

 **********************************************************************/

#include "WaveTrackUtils.h"
#include "ViewInfo.h"
#include "../../../WaveClip.h"


bool WaveTrackUtils::IsClipSelected(const ViewInfo& viewInfo, const WaveClip& clip)
{
   return clip.GetPlayStartTime() == viewInfo.selectedRegion.t0() &&
      clip.GetPlayEndTime() == viewInfo.selectedRegion.t1();
}
