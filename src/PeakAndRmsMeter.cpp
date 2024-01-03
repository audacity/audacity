/**********************************************************************

  Audacity: A Digital Audio Editor

  PeakAndRmsMeter.cpp

  Dominic Mazzoni
  Vaughan Johnson

  Paul Licameli split from MeterPanel.cpp

*********************************************************************/
#include "PeakAndRmsMeter.h"

#include <wx/string.h>

#include <math.h>

/* Updates to the meter are passed across via meter updates, each contained in
 * a MeterUpdateMsg object */
wxString MeterUpdateMsg::toString()
{
wxString output;  // somewhere to build up a string in
output = wxString::Format(wxT("Meter update msg: %i channels, %i samples\n"), \
      kMaxMeterBars, numFrames);
for (int i = 0; i<kMaxMeterBars; i++)
   {  // for each channel of the meters
   output += wxString::Format(wxT("%f peak, %f rms "), peak[i], rms[i]);
   if (clipping[i])
      output += wxString::Format(wxT("clipped "));
   else
      output += wxString::Format(wxT("no clip "));
   output += wxString::Format(wxT("%i head, %i tail\n"), headPeakCount[i], tailPeakCount[i]);
   }
return output;
}

wxString MeterUpdateMsg::toStringIfClipped()
{
   for (int i = 0; i<kMaxMeterBars; i++)
   {
      if (clipping[i] || (headPeakCount[i] > 0) || (tailPeakCount[i] > 0))
         return toString();
   }
   return wxT("");
}

//
// The MeterPanel passes itself messages via this queue so that it can
// communicate between the audio thread and the GUI thread.
// This class uses lock-free synchronization with atomics.
//

PeakAndRmsMeter::PeakAndRmsMeter(int dbRange, float decayRate)
   : mDecayRate{ decayRate }
   , mDBRange{ dbRange }
{}

PeakAndRmsMeter::~PeakAndRmsMeter() = default;

void PeakAndRmsMeter::Clear()
{
   mQueue.Clear();
}

void PeakAndRmsMeter::Reset(double sampleRate, bool resetClipping)
{
   mT = 0;
   mRate = sampleRate;
   for (int j = 0; j < kMaxMeterBars; j++)
      mStats[j].Reset(resetClipping);
   mQueue.Clear();
}

static float ToDB(float v, float range)
{
   double db;
   if (v > 0)
      db = LINEAR_TO_DB(fabs(v));
   else
      db = -999;
   return std::clamp((db + range) / range, 0.0, 1.0);
}

void PeakAndRmsMeter::Update(unsigned numChannels,
   unsigned long numFrames, const float *sampleData, bool interleaved)
{
   auto sptr = sampleData;
   const auto majorStep = (interleaved ? numChannels : 1);
   const auto minorStep = (interleaved ? 1 : numFrames);
   auto num = std::min(numChannels, mNumBars);
   MeterUpdateMsg msg;

   memset(&msg, 0, sizeof(msg));
   msg.numFrames = numFrames;

   for (size_t i = 0; i < numFrames; ++i, sptr += majorStep) {
      for (size_t j = 0; j < num; ++j) {
         const auto sample = sptr[j * minorStep];
         msg.peak[j] = std::max(msg.peak[j], fabs(sample));
         msg.rms[j] += sample * sample;

         // In addition to looking for mNumPeakSamplesToClip peaked
         // samples in a row, also send the number of peaked samples
         // at the head and tail, in case there's a run of peaked samples
         // that crosses block boundaries
         if (fabs(sample) >= MAX_AUDIO) {
            if (msg.headPeakCount[j] == i)
               ++msg.headPeakCount[j];
            ++msg.tailPeakCount[j];
            if (msg.tailPeakCount[j] > mNumPeakSamplesToClip)
               msg.clipping[j] = true;
         }
         else
            msg.tailPeakCount[j] = 0;
      }
   }
   for (unsigned int j = 0; j < mNumBars; ++j) {
      auto &rms = msg.rms[j];
      rms = sqrt(rms / numFrames);
   }

   mQueue.Put(msg);
}

void PeakAndRmsMeter::Poll()
{
   MeterUpdateMsg msg;
   unsigned numChanges = 0;

   // We shouldn't receive any events if the meter is disabled, but clear it to be safe
   if (mMeterDisabled) {
      mQueue.Clear();
      return;
   }


   // There may have been several update messages since the last
   // time we got to this function.  Catch up to real-time by
   // popping them off until there are none left.  It is necessary
   // to process all of them, otherwise we won't handle peaks and
   // peak-hold bars correctly.
   while (mQueue.Get(msg)) {
      ++numChanges;
      double deltaT = msg.numFrames / mRate;

      mT += deltaT;
      for (size_t j = 0; j < mNumBars; ++j) {
         auto &stats = mStats[j];
         if (mDB) {
            msg.peak[j] = ToDB(msg.peak[j], mDBRange);
            msg.rms[j] = ToDB(msg.rms[j], mDBRange);
         }

         if (mDecay) {
            if (mDB) {
               float decayAmount = mDecayRate * deltaT / mDBRange;
               stats.peak = std::max(msg.peak[j], stats.peak - decayAmount);
            }
            else {
               double decayAmount = mDecayRate * deltaT;
               double decayFactor = DB_TO_LINEAR(-decayAmount);
               stats.peak =
                  std::max<float>(msg.peak[j], stats.peak * decayFactor);
            }
         }
         else
            stats.peak = msg.peak[j];

         // This smooths out the RMS signal
         float smooth = pow(0.9, (double)msg.numFrames / 1024.0);
         stats.rms = stats.rms * smooth + msg.rms[j] * (1.0 - smooth);

         if (mT - stats.peakHoldTime > mPeakHoldDuration ||
             stats.peak > stats.peakHold) {
            stats.peakHold = stats.peak;
            stats.peakHoldTime = mT;
         }

         if (stats.peak > stats.peakPeakHold )
            stats.peakPeakHold = stats.peak;

         if (msg.clipping[j] ||
             stats.tailPeakCount + msg.headPeakCount[j] >=
             mNumPeakSamplesToClip){
            stats.clipping = true;
         }

         stats.tailPeakCount = msg.tailPeakCount[j];
         Receive(mT, msg);
      }
   } // while
}

void PeakAndRmsMeter::Receive(double, const MeterUpdateMsg &)
{
}

bool PeakAndRmsMeter::IsClipping() const
{
   for (int c = 0; c < mNumBars; c++)
      if (mStats[c].clipping)
         return true;
   return false;
}

int PeakAndRmsMeter::GetDBRange() const
{
   return mDB ? mDBRange : -1;
}

bool PeakAndRmsMeter::IsDisabled() const
{
   return mMeterDisabled != 0;
}
