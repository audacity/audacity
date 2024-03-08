/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveformCache.h

  Paul Licameli split from WaveClip.h

*******************************************************************/

#ifndef __AUDACITY_WAVEFORM_CACHE__
#define __AUDACITY_WAVEFORM_CACHE__

#include "WaveClip.h"

class WaveCache;
class WaveChannelInterval;

// A bundle of arrays needed for drawing waveforms.  The object may or may not
// own the storage for those arrays.  If it does, it destroys them.
class WaveDisplay
{
public:
   int width;
   sampleCount *where;
   float *min, *max, *rms;

   std::vector<sampleCount> ownWhere;
   std::vector<float> ownMin, ownMax, ownRms;

public:
   WaveDisplay(int w)
      : width(w), where(0), min(0), max(0), rms(0)
   {
   }

   // Create "own" arrays.
   void Allocate()
   {
      ownWhere.resize(width + 1);
      ownMin.resize(width);
      ownMax.resize(width);
      ownRms.resize(width);

      where = &ownWhere[0];
      if (width > 0) {
         min = &ownMin[0];
         max = &ownMax[0];
         rms = &ownRms[0];
      }
      else {
         min = max = rms = 0;
      }
   }

   ~WaveDisplay()
   {
   }
};

struct WaveClipWaveformCache final : WaveClipListener
{
   explicit WaveClipWaveformCache(size_t nChannels);
   ~WaveClipWaveformCache() override;

   std::unique_ptr<WaveClipListener> Clone() const override;

   // Cache of values for drawing the waveform
   std::vector<std::unique_ptr<WaveCache>> mWaveCaches;
   int mDirty { 0 };

   static WaveClipWaveformCache &Get(const WaveChannelInterval &clip);

   void MarkChanged() override; // NOFAIL-GUARANTEE
   void Invalidate() override; // NOFAIL-GUARANTEE

   ///Delete the wave cache - force redraw.  Thread-safe
   void Clear();

   /** Getting high-level data for screen display */
   bool GetWaveDisplay(const WaveChannelInterval &clip,
      WaveDisplay &display, double t0, double pixelsPerSecond);

   void MakeStereo(WaveClipListener &&other, bool aligned) override;
   void SwapChannels() override;
   void Erase(size_t index) override;
};

#endif
