/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveformCache.h

  Paul Licameli split from WaveClip.h

*******************************************************************/

#ifndef __AUDACITY_WAVEFORM_CACHE__
#define __AUDACITY_WAVEFORM_CACHE__

#include "WaveClip.h"

class WaveDataCache;
class WaveClipPainter;

namespace graphics
{
class Painter;
}

std::shared_ptr<WaveDataCache> CreateWaveClipDataCache(const WaveClip& clip);

class WaveClipWaveformCache final
   : public WaveClipListener
   , public ClientData::Site<WaveClipWaveformCache>
{
public:
   WaveClipWaveformCache( WaveClip& clip );
   ~WaveClipWaveformCache() override;

   static WaveClipWaveformCache &Get( const WaveClip &clip );

   std::shared_ptr<WaveDataCache> GetDataCache() const;

   void MarkChanged() override; // NOFAIL-GUARANTEE
   void Invalidate() override; // NOFAIL-GUARANTEE
private:
   std::shared_ptr<WaveDataCache> mWaveDataCache;
};

#endif
