/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveformCache.h

  Paul Licameli split from WaveClip.h

*******************************************************************/

#ifndef __AUDACITY_WAVEFORM_CACHE__
#define __AUDACITY_WAVEFORM_CACHE__

#include "WaveClip.h"

class WaveDisplay;
class WaveDataCache;
class WaveBitmapCache;

struct WaveClipWaveformCache final : WaveClipListener
{
   WaveClipWaveformCache( WaveClip& clip );
   ~WaveClipWaveformCache() override;

   std::shared_ptr<WaveDataCache> mWaveDataCache;
   std::shared_ptr<WaveBitmapCache> mWaveBitmapCache;

   static WaveClipWaveformCache &Get( const WaveClip &clip );

   void MarkChanged() override; // NOFAIL-GUARANTEE
   void Invalidate() override; // NOFAIL-GUARANTEE
};

#endif
