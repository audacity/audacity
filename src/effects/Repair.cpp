/**********************************************************************

  Audacity: A Digital Audio Editor

  Repair.cpp

  Dominic Mazzoni

*******************************************************************//**

\class EffectRepair
\brief Use the interpolation code to fill in damaged audio.
Damage can include pops, clicks, or clipping.  As long as the
damaged section is short and surrounded by lots of good audio,
it is usually quite successful.

This was formerly the PopClickRemoval effect, but it was
renamed and focused on the smaller subproblem of repairing
the audio, rather than actually finding the clicks.

*//*******************************************************************/
#include "Repair.h"

#include <math.h>

#include "AudacityMessageBox.h"
#include "EffectOutputTracks.h"
#include "EffectUIServices.h"
#include "InterpolateAudio.h"
#include "LoadEffects.h"
#include "WaveTrack.h"
#include "TimeStretching.h"

const ComponentInterfaceSymbol EffectRepair::Symbol
{ XO("Repair") };

namespace{ BuiltinEffectsModule::Registration< EffectRepair > reg; }

EffectRepair::EffectRepair()
{
}

EffectRepair::~EffectRepair()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectRepair::GetSymbol() const
{
   return Symbol;
}

TranslatableString EffectRepair::GetDescription() const
{
   return XO("Sets the peak amplitude of a one or more tracks");
}

// EffectDefinitionInterface implementation

EffectType EffectRepair::GetType() const
{
   return EffectTypeProcess;
}

bool EffectRepair::IsInteractive() const
{
   return false;
}

// Effect implementation

bool EffectRepair::Process(EffectInstance &, EffectSettings &)
{
   // This may be too much copying for EffectRepair. To support Cancel, may be
   // able to copy much less.
   // But for now, Cancel isn't supported without this.
   // Repair doesn't make sense for stretched clips, so don't pass a stretch
   // interval.
   EffectOutputTracks outputs { *mTracks, GetType(), {} };
   bool bGoodResult = true;

   int count = 0;
   for (auto track : outputs.Get().Selected<WaveTrack>()) {
      const double trackStart = track->GetStartTime();
      const double repair_t0 = std::max(mT0, trackStart);
      const double trackEnd = track->GetEndTime();
      const double repair_t1 = std::min(mT1, trackEnd);
      const double repair_deltat = repair_t1 - repair_t0;
      if (repair_deltat > 0) {  // selection is within track audio
         const auto repair0 = track->TimeToLongSamples(repair_t0);
         const auto repair1 = track->TimeToLongSamples(repair_t1);
         const auto repairLen = repair1 - repair0;
         if (TimeStretching::HasPitchOrSpeed(*track, repair_t0, repair_t1)) {
            EffectUIServices::DoMessageBox(*this,
               XO(
"The Repair effect cannot be applied within stretched or shrunk clips") );
            bGoodResult = false;
            break;
         }
         if (repairLen > 128) {
            EffectUIServices::DoMessageBox(*this,
               XO(
"The Repair effect is intended to be used on very short sections of damaged audio (up to 128 samples).\n\nZoom in and select a tiny fraction of a second to repair.") );
            bGoodResult = false;
            break;
         }

         const double rate = track->GetRate();
         const double spacing = std::max(repair_deltat * 2, 128. / rate);
         const double t0 = std::max(repair_t0 - spacing, trackStart);
         const double t1 = std::min(repair_t1 + spacing, trackEnd);

         const auto s0 = track->TimeToLongSamples(t0);
         const auto s1 = track->TimeToLongSamples(t1);
         // The difference is at most 2 * 128:
         const auto repairStart = (repair0 - s0).as_size_t();
         const auto len = s1 - s0;

         if (s0 == repair0 && s1 == repair1) {
            EffectUIServices::DoMessageBox(*this,
               XO(
"Repair works by using audio data outside the selection region.\n\nPlease select a region that has audio touching at least one side of it.\n\nThe more surrounding audio, the better it performs.") );
   ///            The Repair effect needs some data to go on.\n\nPlease select an area to repair with some audio on at least one side (the more the better).") );
            bGoodResult = false;
            break;
         }

         for (const auto pChannel : track->Channels())
            if (!ProcessOne(count++, *pChannel, s0,
               // len is at most 5 * 128.
               len.as_size_t(),
               repairStart,
               // repairLen is at most 128.
               repairLen.as_size_t())
            ) {
               bGoodResult = false;
               goto done;
            }
      }
   }
done:

   if (bGoodResult)
      outputs.Commit();

   return bGoodResult;
}

bool EffectRepair::ProcessOne(int count, WaveChannel &track,
   sampleCount start, size_t len, size_t repairStart, size_t repairLen)
{
   Floats buffer{ len };
   track.GetFloats(buffer.get(), start, len);
   InterpolateAudio(buffer.get(), len, repairStart, repairLen);
   if (!track.SetFloats(&buffer[repairStart],
      start + repairStart, repairLen,
      // little repairs shouldn't force dither on rendering:
      narrowestSampleFormat
   ))
      return false;
   return !TrackProgress(count, 1.0); // TrackProgress returns true on Cancel.
}

bool EffectRepair::NeedsDither() const
{
   return false;
}
