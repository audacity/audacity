/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2012 Audacity Team.
   License: GPL v2.  See License.txt.

   Resample.cpp
   Dominic Mazzoni, Rob Sykes, Vaughan Johnson

******************************************************************//**

\class Resample
\brief Combined interface to libresample, libsamplerate, and libsoxr.

   This class abstracts the interface to two different variable-rate
   resampling libraries:

      libresample, written by Dominic Mazzoni based on Resample-1.7
      by Julius Smith.  LGPL.

      libsamplerate, written by Erik de Castro Lopo.  GPL.  The author
      of libsamplerate requests that you not distribute a binary version
      of Audacity that links to libsamplerate and also has plug-in support.

      Since Audacity always does resampling on mono streams that are
      contiguous in memory, this class doesn't support multiple channels
      or some of the other optional features of some of these resamplers.

   and the fixed-rate resampling library:

      libsoxr, written by Rob Sykes. LGPL.

*//*******************************************************************/


#include "Resample.h"

#include <wx/intl.h>

//v Currently unused.
//void Resample::SetFastMethod(int index)
//{
//   gPrefs->Write(GetFastMethodKey(), (long)index);
//   gPrefs->Flush();
//}
//
//void Resample::SetBestMethod(int index)
//{
//   gPrefs->Write(GetBestMethodKey(), (long)index);
//   gPrefs->Flush();
//}

// constant-rate resampler(s)
#ifdef USE_LIBSOXR

   #include <soxr.h>

   ConstRateResample::ConstRateResample(const bool useBestMethod, const double dFactor)
      : Resample()
   {
      soxr_quality_spec_t q_spec = soxr_quality_spec("\0\1\4\6"[mMethod], 0);
      mHandle = (void *)soxr_create(1, dFactor, 1, 0, 0, &q_spec, 0);
   }

   ConstRateResample::~ConstRateResample()
   {
      delete mHandle;
      mHandle = NULL;
      soxr_delete((soxr_t)mHandle);
   }

   //v Currently unused. 
   //wxString ConstRateResample::GetResamplingLibraryName()
   //{
   //   return _("Libsoxr by Rob Sykes");
   //}

   int ConstRateResample::GetNumMethods() { return 4; }

   static char const * const soxr_method_names[] = {
      "Quick & dirty", "Basic quality", "High quality", "Very high quality"
   };

   wxString ConstRateResample::GetMethodName(int index)
   {
      return wxString(wxString::FromAscii(soxr_method_names[index]));
   }

   const wxString ConstRateResample::GetFastMethodKey()
   {
      return wxT("/Quality/SampleRateConverter");
   }

   const wxString ConstRateResample::GetBestMethodKey()
   {
      return wxT("/Quality/HQSampleRateConverter");
   }

   int ConstRateResample::GetFastMethodDefault() {return 1;}
   int ConstRateResample::GetBestMethodDefault() {return 3;}

   int ConstRateResample::Process(double  factor,
                                  float  *inBuffer,
                                  int     inBufferLen,
                                  bool    lastFlag,
                                  int    *inBufferUsed,
                                  float  *outBuffer,
                                  int     outBufferLen)
   {
      size_t idone , odone;
      soxr_set_oi_ratio((soxr_t)mHandle, factor);
      soxr_process((soxr_t)mHandle,
            inBuffer , (size_t)(lastFlag? ~inBufferLen : inBufferLen), &idone,
            outBuffer, (size_t)                          outBufferLen, &odone);
      *inBufferUsed = (int)idone;
      return (int)odone;
   }

#else // no const-rate resampler
   ConstRateResample::ConstRateResample(const bool useBestMethod, const double dFactor)
      : Resample()
   {
   }

   ConstRateResample::~ConstRateResample()
   {
   }
#endif







//vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

#if USE_LIBRESAMPLE

#include "libresample.h"

bool Resample::ResamplingEnabled()
{
   return true;
}

//v Currently unused. 
//wxString Resample::GetResamplingLibraryName()
//{
//   return _("Libresample by Dominic Mazzoni and Julius Smith");
//}

int Resample::GetNumMethods()
{
   return 2;
}

wxString Resample::GetMethodName(int index)
{
   if (index == 1)
      return _("High-quality Sinc Interpolation");

   return _("Fast Sinc Interpolation");
}

const wxString Resample::GetFastMethodKey()
{
   return wxT("/Quality/LibresampleSampleRateConverter");
}

const wxString Resample::GetBestMethodKey()
{
   return wxT("/Quality/LibresampleHQSampleRateConverter");
}

int Resample::GetFastMethodDefault()
{
   return 0;
}

int Resample::GetBestMethodDefault()
{
   return 1;
}

Resample::Resample(bool useBestMethod, double minFactor, double maxFactor)
{
   if (useBestMethod)
      mMethod = GetBestMethod();
   else
      mMethod = GetFastMethod();

   mHandle = resample_open(mMethod, minFactor, maxFactor);
}

bool Resample::Ok()
{
   return (mHandle != NULL);
}

int Resample::Process(double  factor,
                      float  *inBuffer,
                      int     inBufferLen,
                      bool    lastFlag,
                      int    *inBufferUsed,
                      float  *outBuffer,
                      int     outBufferLen)
{
   return resample_process(mHandle, factor, inBuffer, inBufferLen,
									(int)lastFlag, inBufferUsed, outBuffer, outBufferLen);
}

Resample::~Resample()
{
   resample_close(mHandle);
}

#elif USE_LIBSAMPLERATE 

#include <samplerate.h>

bool Resample::ResamplingEnabled()
{
   return true;
}

//v Currently unused. 
//wxString Resample::GetResamplingLibraryName()
//{
//#ifdef USE_LIBSAMPLERATE
//   return _("Libsamplerate by Erik de Castro Lopo");
//#elif USE_LIBSOXR
//   return _("Libsoxr by Rob Sykes");
//#else
//   return _("Unknown");
//#endif
//}

int Resample::GetNumMethods()
{
   int i = 0;

   while(src_get_name(i))
      i++;

   return i;
}

wxString Resample::GetMethodName(int index)
{
   return wxString(wxString::FromAscii(src_get_name(index)));
}

const wxString Resample::GetFastMethodKey()
{
   return wxT("/Quality/SampleRateConverter");
}

const wxString Resample::GetBestMethodKey()
{
   return wxT("/Quality/HQSampleRateConverter");
}

int Resample::GetFastMethodDefault()
{
   return SRC_SINC_FASTEST;
}

int Resample::GetBestMethodDefault()
{
   return SRC_SINC_BEST_QUALITY;
}

Resample::Resample(bool useBestMethod, double minFactor, double maxFactor)
{
   if (!src_is_valid_ratio (minFactor) || !src_is_valid_ratio (maxFactor)) {
      fprintf(stderr, "libsamplerate supports only resampling factors between 1/SRC_MAX_RATIO and SRC_MAX_RATIO.\n");
      // FIX-ME: Audacity will hang after this if branch.
      mHandle = NULL;
      return;
   }

   if (useBestMethod)
      mMethod = GetBestMethod();
   else
      mMethod = GetFastMethod();

   int err;
   SRC_STATE *state = src_new(mMethod, 1, &err);
   mHandle = (void *)state;
   mInitial = true;
}

bool Resample::Ok()
{
   return (mHandle != NULL);
}

int Resample::Process(double  factor,
                      float  *inBuffer,
                      int     inBufferLen,
                      bool    lastFlag,
                      int    *inBufferUsed,
                      float  *outBuffer,
                      int     outBufferLen)
{
   if (mInitial) {
      src_set_ratio((SRC_STATE *)mHandle, factor);
      mInitial = false;
   }

   SRC_DATA data;

   data.data_in = inBuffer;
   data.data_out = outBuffer;
   data.input_frames = inBufferLen;
   data.output_frames = outBufferLen;
   data.input_frames_used = 0;
   data.output_frames_gen = 0;
   data.end_of_input = (int)lastFlag;
   data.src_ratio = factor;

   int err = src_process((SRC_STATE *)mHandle, &data);
   if (err) {
      wxFprintf(stderr, _("Libsamplerate error: %d\n"), err);
      return 0;
   }

   *inBufferUsed = (int)data.input_frames_used;
   return (int)data.output_frames_gen;
}

Resample::~Resample()
{
   src_delete((SRC_STATE *)mHandle);
}

#endif
