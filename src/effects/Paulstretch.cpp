/**********************************************************************

  Audacity: A Digital Audio Editor

  Paulstretch.cpp

  Nasca Octavian Paul (Paul Nasca)
  Some GUI code was taken from the Echo effect

*******************************************************************//**

\class EffectPaulstretch
\brief An Extreme Time Stretch and Time Smear effect

*//*******************************************************************/

#include "../Audacity.h"
#include "Paulstretch.h"

#include <algorithm>

#include <math.h>
#include <float.h>

#include <wx/intl.h>
#include <wx/valgen.h>

#include "../ShuttleGui.h"
#include "../FFT.h"
#include "../widgets/valnum.h"
#include "../Prefs.h"

#include "../WaveTrack.h"

// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name    Type     Key                     Def      Min      Max      Scale
Param( Amount, float,   XO("Stretch Factor"),   10.0,    1.0,     FLT_MAX, 1   );
Param( Time,   float,   XO("Time Resolution"),  0.25f,   0.00099f,  FLT_MAX, 1   );

class PaulStretch
{
public:
   PaulStretch(float rap_, size_t in_bufsize_, float samplerate_);
   //in_bufsize is also a half of a FFT buffer (in samples)
   virtual ~PaulStretch();

   void process(float *smps,int nsmps);

   size_t get_nsamples();//how many samples are required to be added in the pool next time
   size_t get_nsamples_for_fill();//how many samples are required to be added for a complete buffer refill (at start of the song or after seek)

private:
   void process_spectrum(float *WXUNUSED(freq)) {};

   const float samplerate;
   const float rap;
   const size_t in_bufsize;

public:
   const size_t out_bufsize;
   float *const out_buf;

private:
   float *const old_out_smp_buf;

public:
   const size_t poolsize;//how many samples are inside the input_pool size (need to know how many samples to fill when seeking)

private:
   float *const in_pool;//de marimea in_bufsize

   double remained_samples;//how many fraction of samples has remained (0..1)

   float *const fft_smps;
   float *const fft_c;
   float *const fft_s;
   float *const fft_freq;
   float *const fft_tmp;
};

//
// EffectPaulstretch
//

BEGIN_EVENT_TABLE(EffectPaulstretch, wxEvtHandler)
    EVT_TEXT(wxID_ANY, EffectPaulstretch::OnText)
END_EVENT_TABLE()

EffectPaulstretch::EffectPaulstretch()
{
   mAmount = DEF_Amount;
   mTime_resolution = DEF_Time;

   SetLinearEffectFlag(true);
}

EffectPaulstretch::~EffectPaulstretch()
{
}

// IdentInterface implementation

wxString EffectPaulstretch::GetSymbol()
{
   return PAULSTRETCH_PLUGIN_SYMBOL;
}

wxString EffectPaulstretch::GetDescription()
{
   return XO("Use Paulstretch only for an extreme time-stretch or \"stasis\" effect");
}

// EffectIdentInterface implementation

EffectType EffectPaulstretch::GetType()
{
   return EffectTypeProcess;
}

// EffectClientInterface implementation

bool EffectPaulstretch::GetAutomationParameters(EffectAutomationParameters & parms)
{
   parms.WriteFloat(KEY_Amount, mAmount);
   parms.WriteFloat(KEY_Time, mTime_resolution);

   return true;
}

bool EffectPaulstretch::SetAutomationParameters(EffectAutomationParameters & parms)
{
   ReadAndVerifyFloat(Amount);
   ReadAndVerifyFloat(Time);

   mAmount = Amount;
   mTime_resolution = Time;

   return true;
}

// Effect implementation

double EffectPaulstretch::CalcPreviewInputLength(double previewLength)
{
   // FIXME: Preview is currently at the project rate, but should really be
   // at the track rate (bugs 1284 and 852).
   auto minDuration = GetBufferSize(mProjectRate) * 2 + 1;

   // Preview playback may need to be trimmed but this is the smallest selection that we can use.
   double minLength = std::max<double>(minDuration / mProjectRate, previewLength / mAmount);

   return minLength;
}


bool EffectPaulstretch::Process()
{
   CopyInputTracks();
   SelectedTrackListOfKindIterator iter(Track::Wave, mOutputTracks.get());
   WaveTrack *track = (WaveTrack *) iter.First();
   m_t1=mT1;
   int count=0;
   while (track) {
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();
      double t0 = mT0 < trackStart? trackStart: mT0;
      double t1 = mT1 > trackEnd? trackEnd: mT1;

      if (t1 > t0) {
         if (!ProcessOne(track, t0,t1,count))
            return false;
      }

      track = (WaveTrack *) iter.Next();
      count++;
   }
   mT1=m_t1;

   ReplaceProcessedTracks(true);

   return true;
}


void EffectPaulstretch::PopulateOrExchange(ShuttleGui & S)
{
   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      FloatingPointValidator<float> vldAmount(1, &mAmount);
      vldAmount.SetMin(MIN_Amount);

      /* i18n-hint: This is how many times longer the sound will be, e.g. applying
       * the effect to a 1-second sample, with the default Stretch Factor of 10.0
       * will give an (approximately) 10 second sound
       */
      S.AddTextBox(_("Stretch Factor:"), wxT(""), 10)->SetValidator(vldAmount);

      FloatingPointValidator<float> vldTime(3, &mTime_resolution, NUM_VAL_ONE_TRAILING_ZERO);
      vldTime.SetMin(MIN_Time);
      S.AddTextBox(_("Time Resolution (seconds):"), wxT(""), 10)->SetValidator(vldTime);
   }
   S.EndMultiColumn();
};

bool EffectPaulstretch::TransferDataToWindow()
{
   if (!mUIParent->TransferDataToWindow())
   {
      return false;
   }

   return true;
}

bool EffectPaulstretch::TransferDataFromWindow()
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }

   return true;
}

// EffectPaulstretch implementation

void EffectPaulstretch::OnText(wxCommandEvent & WXUNUSED(evt))
{
   EnableApply(mUIParent->TransferDataFromWindow());
}

size_t EffectPaulstretch::GetBufferSize(double rate)
{
   // Audacity's fft requires a power of 2
   float tmp = rate * mTime_resolution / 2.0;
   tmp = log(tmp) / log(2.0);
   tmp = pow(2.0, floor(tmp + 0.5));

   auto stmp = size_t(tmp);
   if (stmp != tmp)
      // overflow
      return 0;
   if (stmp >= 2 * stmp)
      // overflow
      return 0;

   return std::max<size_t>(stmp, 128);
}

bool EffectPaulstretch::ProcessOne(WaveTrack *track,double t0,double t1,int count)
{
   auto badAllocMessage = _("Requested value exceeds memory capacity.");

   const auto stretch_buf_size = GetBufferSize(track->GetRate());
   if (stretch_buf_size == 0) {
      ::wxMessageBox( badAllocMessage );
      return false;
   }

   double amount = this->mAmount;

   auto start = track->TimeToLongSamples(t0);
   auto end = track->TimeToLongSamples(t1);
   auto len = end - start;

   const auto minDuration = stretch_buf_size * 2 + 1;
   if (minDuration < stretch_buf_size) {
      // overflow!
      ::wxMessageBox( badAllocMessage );
      return false;
   }

   if (len < minDuration) {   //error because the selection is too short

      float maxTimeRes = log( len.as_double() ) / log(2.0);
      maxTimeRes = pow(2.0, floor(maxTimeRes) + 0.5);
      maxTimeRes = maxTimeRes / track->GetRate();

      if (this->IsPreviewing()) {
         double defaultPreviewLen;
         gPrefs->Read(wxT("/AudioIO/EffectsPreviewLen"), &defaultPreviewLen, 6.0);

         /* i18n-hint: 'Time Resolution' is the name of a control in the Paulstretch effect.*/
         if ((minDuration / mProjectRate) < defaultPreviewLen) {
            ::wxMessageBox (wxString::Format(_("Audio selection too short to preview.\n\n"
                                               "Try increasing the audio selection to at least %.1f seconds,\n"
                                               "or reducing the 'Time Resolution' to less than %.1f seconds."),
                                             (minDuration / track->GetRate()) + 0.05, // round up to 1/10 s.
                                             floor(maxTimeRes * 10.0) / 10.0),
                            GetName(), wxOK | wxICON_EXCLAMATION);
         }
         else {
            /* i18n-hint: 'Time Resolution' is the name of a control in the Paulstretch effect.*/
            ::wxMessageBox (wxString::Format(_("Unable to Preview.\n\n"
                                               "For the current audio selection, the maximum\n"
                                               "'Time Resolution' is %.1f seconds."),
                                             floor(maxTimeRes * 10.0) / 10.0),
                            GetName(), wxOK | wxICON_EXCLAMATION);
         }
      }
      else {
         /* i18n-hint: 'Time Resolution' is the name of a control in the Paulstretch effect.*/
         ::wxMessageBox (wxString::Format(_("The 'Time Resolution' is too long for the selection.\n\n"
                                            "Try increasing the audio selection to at least %.1f seconds,\n"
                                            "or reducing the 'Time Resolution' to less than %.1f seconds."),
                                          (minDuration / track->GetRate()) + 0.05, // round up to 1/10 s.
                                          floor(maxTimeRes * 10.0) / 10.0),
                         GetName(), wxOK | wxICON_EXCLAMATION);
      }

      return false;
   }


   auto dlen = len.as_double();
   double adjust_amount = dlen /
      (dlen - ((double)stretch_buf_size * 2.0));
   amount = 1.0 + (amount - 1.0) * adjust_amount;

   auto outputTrack = mFactory->NewWaveTrack(track->GetSampleFormat(),track->GetRate());

   try {
      // This encloses all the allocations of buffers, including those in
      // the constructor of the PaulStretch object

      PaulStretch stretch(amount, stretch_buf_size, track->GetRate());

      auto nget = stretch.get_nsamples_for_fill();

      auto bufsize = stretch.poolsize;
      float *buffer0 = new float[bufsize];
      float *bufferptr0 = buffer0;
      bool first_time = true;

      const auto fade_len = std::min<size_t>(100, bufsize / 2 - 1);
      float *fade_track_smps = new float[fade_len];
      decltype(len) s = 0;
      bool cancelled = false;

      while (s < len) {
         track->Get((samplePtr)bufferptr0, floatSample, start + s, nget);
         stretch.process(buffer0, nget);

         if (first_time) {
            stretch.process(buffer0, 0);
         };

         s += nget;

         if (first_time) {//blend the the start of the selection
            track->Get((samplePtr)fade_track_smps, floatSample, start, fade_len);
            first_time = false;
            for (int i = 0; i < fade_len; i++){
               float fi = (float)i / (float)fade_len;
               stretch.out_buf[i] =
                  stretch.out_buf[i] * fi + (1.0 - fi) * fade_track_smps[i];
            };
         };
         if (s >= len) {//blend the end of the selection
            track->Get((samplePtr)fade_track_smps,floatSample,end-fade_len,fade_len);
            for (int i = 0; i < fade_len; i++){
               float fi = (float)i / (float)fade_len;
               auto i2 = bufsize / 2 - 1 - i;
               stretch.out_buf[i2] =
                  stretch.out_buf[i2] * fi + (1.0 - fi) *
                  fade_track_smps[fade_len - 1 - i];
            };
         };

         outputTrack->Append((samplePtr)stretch.out_buf, floatSample,
                             stretch.out_bufsize);

         nget = stretch.get_nsamples();
         if (TrackProgress(count,
            s.as_double() / len.as_double()
         )) {
            cancelled=true;
            break;
         }
      }

      delete [] fade_track_smps;
      outputTrack->Flush();

      track->Clear(t0,t1);
      bool success = track->Paste(t0, outputTrack.get());
      if (!cancelled && success){
         m_t1 = mT0 + outputTrack->GetEndTime();
      }
      
      delete []buffer0;
      
      return !cancelled;
   }
   catch ( const std::bad_alloc& ) {
      ::wxMessageBox( badAllocMessage );
      return false;
   }
};

/*************************************************************/


PaulStretch::PaulStretch(float rap_, size_t in_bufsize_, float samplerate_ )
   : samplerate { samplerate_ }
   , rap { std::max(1.0f, rap_) }
   , in_bufsize { in_bufsize_ }
   , out_bufsize { std::max(size_t{ 8 }, in_bufsize) }
   , out_buf { new float[out_bufsize] }
   , old_out_smp_buf { new float[out_bufsize * 2] { 0.0f } }
   , poolsize { in_bufsize_ * 2 }
   , in_pool { new float[poolsize] { 0.0f } }
   , remained_samples { 0.0 }
   , fft_smps { new float[poolsize] { 0.0f } }
   , fft_s { new float[poolsize] { 0.0f } }
   , fft_c { new float[poolsize] { 0.0f } }
   , fft_freq { new float[poolsize] { 0.0f } }
   , fft_tmp { new float[poolsize] }
{
}

PaulStretch::~PaulStretch()
{
   delete [] out_buf;
   delete [] old_out_smp_buf;
   delete [] in_pool;
   delete [] fft_smps;
   delete [] fft_c;
   delete [] fft_s;
   delete [] fft_freq;
   delete [] fft_tmp;
}

void PaulStretch::process(float *smps,int nsmps)
{
   //add NEW samples to the pool
   if ((smps != NULL) && (nsmps != 0)) {
      if (nsmps > poolsize) {
         nsmps = poolsize;
      }
      int nleft = poolsize - nsmps;

      //move left the samples from the pool to make room for NEW samples
      for (int i = 0; i < nleft; i++)
         in_pool[i] = in_pool[i + nsmps];

      //add NEW samples to the pool
      for (int i = 0; i < nsmps; i++)
         in_pool[i + nleft] = smps[i];
   }

   //get the samples from the pool
   for (size_t i = 0; i < poolsize; i++)
      fft_smps[i] = in_pool[i];
   WindowFunc(eWinFuncHanning, poolsize, fft_smps);

   RealFFT(poolsize, fft_smps, fft_c, fft_s);

   for (size_t i = 0; i < poolsize / 2; i++)
      fft_freq[i] = sqrt(fft_c[i] * fft_c[i] + fft_s[i] * fft_s[i]);
   process_spectrum(fft_freq);


   //put randomize phases to frequencies and do a IFFT
   float inv_2p15_2pi = 1.0 / 16384.0 * (float)M_PI;
   for (size_t i = 1; i < poolsize / 2; i++) {
      unsigned int random = (rand()) & 0x7fff;
      float phase = random * inv_2p15_2pi;
      float s = fft_freq[i] * sin(phase);
      float c = fft_freq[i] * cos(phase);

      fft_c[i] = fft_c[poolsize - i] = c;

      fft_s[i] = s; fft_s[poolsize - i] = -s;
   }
   fft_c[0] = fft_s[0] = 0.0;
   fft_c[poolsize / 2] = fft_s[poolsize / 2] = 0.0;

   FFT(poolsize, true, fft_c, fft_s, fft_smps, fft_tmp);

   float max = 0.0, max2 = 0.0;
   for (size_t i = 0; i < poolsize; i++) {
      max = std::max(max, fabsf(fft_tmp[i]));
      max2 = std::max(max2, fabsf(fft_smps[i]));
   }


   //make the output buffer
   float tmp = 1.0 / (float) out_bufsize * M_PI;
   float hinv_sqrt2 = 0.853553390593f;//(1.0+1.0/sqrt(2))*0.5;

   float ampfactor = 1.0;
   if (rap < 1.0)
      ampfactor = rap * 0.707;
   else
      ampfactor = (out_bufsize / (float)poolsize) * 4.0;

   for (size_t i = 0; i < out_bufsize; i++) {
      float a = (0.5 + 0.5 * cos(i * tmp));
      float out = fft_smps[i + out_bufsize] * (1.0 - a) + old_out_smp_buf[i] * a;
      out_buf[i] =
         out * (hinv_sqrt2 - (1.0 - hinv_sqrt2) * cos(i * 2.0 * tmp)) *
         ampfactor;
   }

   //copy the current output buffer to old buffer
   for (size_t i = 0; i < out_bufsize * 2; i++)
      old_out_smp_buf[i] = fft_smps[i];
}

size_t PaulStretch::get_nsamples()
{
   double r = out_bufsize / rap;
   auto ri = (size_t)floor(r);
   double rf = r - floor(r);

   remained_samples += rf;
   if (remained_samples >= 1.0){
      ri += (size_t)floor(remained_samples);
      remained_samples = remained_samples - floor(remained_samples);
   }

   if (ri > poolsize) {
      ri = poolsize;
   }

   return ri;
}

size_t PaulStretch::get_nsamples_for_fill()
{
   return poolsize;
}
