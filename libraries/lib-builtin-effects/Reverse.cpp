/**********************************************************************

  Audacity: A Digital Audio Editor

  Reverse.cpp

  Mark Phillips

  This class reverses the selected audio.

**********************************************************************/
#include "Reverse.h"
#include "EffectOutputTracks.h"
#include "LabelTrack.h"
#include "SyncLock.h"
#include "WaveTrack.h"
#include "WaveTrackUtilities.h"
#include <algorithm>
#include <cmath>

Reverse::Reverse()
{
}

Reverse::~Reverse()
{
}

const ComponentInterfaceSymbol Reverse::Symbol { XO("Reverse") };

ComponentInterfaceSymbol Reverse::GetSymbol() const
{
   return Symbol;
}

TranslatableString Reverse::GetDescription() const
{
   return XO("Reverses the selected audio");
}

// EffectDefinitionInterface implementation

EffectType Reverse::GetType() const
{
   return EffectTypeProcess;
}

bool Reverse::IsInteractive() const
{
   return false;
}

// Effect implementation

bool Reverse::Process(EffectInstance&, EffectSettings&)
{
   // all needed because Reverse should move the labels too
   EffectOutputTracks outputs {
      *mTracks, GetType(), { { mT0, mT1 } }, true, true
   };
   bool bGoodResult = true;
   int count = 0;

   auto trackRange =
      outputs.Get().Any() + &SyncLock::IsSelectedOrSyncLockSelectedP;
   trackRange.VisitWhile(
      bGoodResult,
      [&](WaveTrack& track) {
         const auto progress = [&](double fraction) {
            return !TrackProgress(count, fraction);
         };
         if (mT1 > mT0)
         {
            auto start = track.TimeToLongSamples(mT0);
            auto end = track.TimeToLongSamples(mT1);
            auto len = end - start;

            if (!WaveTrackUtilities::Reverse(track, start, len, progress))
               bGoodResult = false;
         }
         count += track.NChannels();
      },
      [&](LabelTrack& track) {
         track.ChangeLabelsOnReverse(mT0, mT1);
         count++;
      });

   if (bGoodResult)
      outputs.Commit();

   return bGoodResult;
}
