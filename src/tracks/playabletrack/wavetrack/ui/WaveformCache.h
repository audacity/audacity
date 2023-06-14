/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveformCache.h

  Paul Licameli split from WaveClip.h

*******************************************************************/

#ifndef __AUDACITY_WAVEFORM_CACHE__
#define __AUDACITY_WAVEFORM_CACHE__

#include "WaveClip.h"

class WaveCache;

struct WaveClipWaveformCache final : WaveClipListener
{
   explicit WaveClipWaveformCache(size_t nChannels);
   ~WaveClipWaveformCache() override;

   // Cache of values for drawing the waveform
   std::vector<std::unique_ptr<WaveCache>> mWaveCaches;
   int mDirty { 0 };

   static WaveClipWaveformCache &Get( const WaveClip &clip );

   void MarkChanged() override; // NOFAIL-GUARANTEE
   void Invalidate() override; // NOFAIL-GUARANTEE

   ///Delete the wave cache - force redraw.  Thread-safe
   void Clear();

   /** Getting high-level data for screen display */
   bool GetWaveDisplay(const WaveClip &clip, size_t channel,
      WaveDisplay &display, double t0, double pixelsPerSecond);
};

#endif
