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

struct WaveClipWaveformCache final : WaveClipListener
{
   WaveClipWaveformCache();
   ~WaveClipWaveformCache() override;

   std::unique_ptr<WaveDataCache> mWaveDataCache;

   int mDirty { 0 };

   static WaveClipWaveformCache &Get( const WaveClip &clip );

   void MarkChanged() override; // NOFAIL-GUARANTEE
   void Invalidate() override; // NOFAIL-GUARANTEE

   ///Delete the wave cache - force redraw.  Thread-safe
   void Clear();

   /** Getting high-level data for screen display */
   bool GetWaveDisplay(const WaveClip &clip, WaveDisplay &display,
                       double t0, double pixelsPerSecond);
};

#endif
