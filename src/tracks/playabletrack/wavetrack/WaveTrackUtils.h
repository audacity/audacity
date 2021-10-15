/*!********************************************************************
*
 Audacity: A Digital Audio Editor

 @file WaveTrackUtils.h

 Vitaly Sverchinsky

 @brief Contains some useful wave track external routines grouped into a single namespace

 **********************************************************************/

#pragma once

#include <algorithm>

class ViewInfo;
class WaveClip;

namespace WaveTrackUtils
{
   bool IsClipSelected(const ViewInfo& viewInfo, const WaveClip& waveClip);

   template<typename Iter>
   Iter SelectedClip(const ViewInfo& viewInfo, Iter begin, Iter end)
   {
      return std::find_if(begin, end,
        [&](auto& pClip) { return IsClipSelected(viewInfo, *pClip); });
   }

};
