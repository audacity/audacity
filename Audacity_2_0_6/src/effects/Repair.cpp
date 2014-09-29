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


#include "../Audacity.h"

#include <math.h>

#include <wx/msgdlg.h>
#include <wx/intl.h>

#include "Repair.h"
#include "../WaveTrack.h"
#include "../InterpolateAudio.h"

EffectRepair::EffectRepair()
{
}

EffectRepair::~EffectRepair()
{
}

bool EffectRepair::PromptUser()
{
   return true;
}

bool EffectRepair::TransferParameters( Shuttle & WXUNUSED(shuttle) )
{
   //TODO: pop-click values.
//   shuttle.TransferInt("",,0);
   return true;
}

bool EffectRepair::Process()
{
   //v This may be too much copying for EffectRepair. To support Cancel, may be able to copy much less.
   //  But for now, Cancel isn't supported without this.
   this->CopyInputTracks(); // Set up mOutputTracks. //v This may be too much copying for EffectRepair.
   bool bGoodResult = true;

   SelectedTrackListOfKindIterator iter(Track::Wave, mOutputTracks);
   WaveTrack *track = (WaveTrack *) iter.First();
   int count = 0;
   while (track) {
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();
      double repair_t0 = mT0;
      double repair_t1 = mT1;
      repair_t0 = (repair_t0 < trackStart? trackStart: repair_t0);
      repair_t1 = (repair_t1 > trackEnd? trackEnd: repair_t1);
      if (repair_t0 < repair_t1) {  // selection is within track audio
         double rate = track->GetRate();
         double repair_deltat = repair_t1 - repair_t0;

         double spacing = repair_deltat * 2;

         if (spacing < 128. / rate)
            spacing = 128. / rate;

         double t0 = repair_t0 - spacing;
         double t1 = repair_t1 + spacing;

         t0 = t0 < trackStart? trackStart: t0;
         t1 = t1 > trackEnd? trackEnd: t1;

         repair_t0 = (repair_t0 < t0? t0: repair_t0);
         repair_t1 = (repair_t1 > t1? t1: repair_t1);

         sampleCount s0 = track->TimeToLongSamples(t0);
         sampleCount repair0 = track->TimeToLongSamples(repair_t0);
         sampleCount repair1 = track->TimeToLongSamples(repair_t1);
         sampleCount s1 = track->TimeToLongSamples(t1);

         sampleCount repairStart = (sampleCount)(repair0 - s0);
         sampleCount repairLen = (sampleCount)(repair1 - repair0);
         sampleCount len = (sampleCount)(s1 - s0);

         if (repairLen > 128) {
            ::wxMessageBox(_("The Repair effect is intended to be used on very short sections of damaged audio (up to 128 samples).\n\nZoom in and select a tiny fraction of a second to repair."));
            bGoodResult = false;
            break;
         }

         if (s0 == repair0 && s1 == repair1) {
            ::wxMessageBox(_("Repair works by using audio data outside the selection region.\n\nPlease select a region that has audio touching at least one side of it.\n\nThe more surrounding audio, the better it performs."));
   ///            The Repair effect needs some data to go on.\n\nPlease select an area to repair with some audio on at least one side (the more the better)."));
            bGoodResult = false;
            break;
         }

         if (!ProcessOne(count, track,
                         s0, len, repairStart, repairLen)) {
            bGoodResult = false;
            break;
         }
      }

      track = (WaveTrack *) iter.Next();
      count++;
   }

   this->ReplaceProcessedTracks(bGoodResult);
   return bGoodResult;
}

bool EffectRepair::ProcessOne(int count, WaveTrack * track,
                              sampleCount start,
                              sampleCount len,
                              sampleCount repairStart, sampleCount repairLen)
{
   float *buffer = new float[len];
   track->Get((samplePtr) buffer, floatSample, start, len);
   InterpolateAudio(buffer, len, repairStart, repairLen);
   track->Set((samplePtr)&buffer[repairStart], floatSample,
              start + repairStart, repairLen);
   delete[] buffer;
   return !TrackProgress(count, 1.0); // TrackProgress returns true on Cancel.
}
