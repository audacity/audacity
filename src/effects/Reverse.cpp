/**********************************************************************

  Audacity: A Digital Audio Editor

  Reverse.cpp

  Mark Phillips

*******************************************************************//**

\class EffectReverse
\brief An Effect that reverses the selected audio.

*//********************************************************************/



#include "Reverse.h"
#include "EffectOutputTracks.h"
#include "LoadEffects.h"

#include <algorithm>
#include <math.h>

#include "LabelTrack.h"
#include "SyncLock.h"
#include "WaveClip.h"
#include "WaveTrack.h"

//
// EffectReverse
//

const ComponentInterfaceSymbol EffectReverse::Symbol
{ XO("Reverse") };

namespace{ BuiltinEffectsModule::Registration< EffectReverse > reg; }

EffectReverse::EffectReverse()
{
}

EffectReverse::~EffectReverse()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectReverse::GetSymbol() const
{
   return Symbol;
}

TranslatableString EffectReverse::GetDescription() const
{
   return XO("Reverses the selected audio");
}

// EffectDefinitionInterface implementation

EffectType EffectReverse::GetType() const
{
   return EffectTypeProcess;
}

bool EffectReverse::IsInteractive() const
{
   return false;
}

// Effect implementation

bool EffectReverse::Process(EffectInstance &, EffectSettings &)
{
   //all needed because Reverse should move the labels too
   EffectOutputTracks outputs {
      *mTracks, GetType(), { { mT0, mT1 } }, true, true
   };
   bool bGoodResult = true;
   int count = 0;

   auto trackRange =
      outputs.Get().Any() + &SyncLock::IsSelectedOrSyncLockSelected;
   trackRange.VisitWhile(bGoodResult,
      [&](WaveTrack &track) {
         const auto progress =
            [&](double fraction){ return !TrackProgress(count, fraction); };
         if (mT1 > mT0) {
            auto start = track.TimeToLongSamples(mT0);
            auto end = track.TimeToLongSamples(mT1);
            auto len = end - start;

            if (!track.Reverse(start, len, progress))
               bGoodResult = false;
         }
         count += track.NChannels();
      },
      [&](LabelTrack &track) {
         track.ChangeLabelsOnReverse(mT0, mT1);
         count++;
      }
   );

   if (bGoodResult)
      outputs.Commit();

   return bGoodResult;
}
