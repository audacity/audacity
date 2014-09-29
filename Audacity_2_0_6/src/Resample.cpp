/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2012 Audacity Team.
   License: GPL v2.  See License.txt.

   Resample.cpp
   Dominic Mazzoni, Rob Sykes, Vaughan Johnson

******************************************************************//**

\class Resample
\brief Combined interface to libresample, libsamplerate, and libsoxr.

   This class abstracts the interface to different resampling libraries:

      libresample, written by Dominic Mazzoni based on Resample-1.7
      by Julius Smith.  LGPL.

      libsamplerate, written by Erik de Castro Lopo.  GPL.  The author
      of libsamplerate requests that you not distribute a binary version
      of Audacity that links to libsamplerate and also has plug-in support.

      libsoxr, written by Rob Sykes. LGPL.

   Since Audacity always does resampling on mono streams that are
   contiguous in memory, this class doesn't support multiple channels
   or some of the other optional features of some of these resamplers.

*//*******************************************************************/


#include "Resample.h"

#if USE_LIBRESAMPLE

   #include "libresample.h"

   Resample::Resample(const bool useBestMethod, const double dMinFactor, const double dMaxFactor)
   {
      this->SetMethod(useBestMethod);
      mHandle = resample_open(mMethod, dMinFactor, dMaxFactor);
      if(mHandle == NULL) {
         fprintf(stderr, "libresample doesn't support range of factors %f to %f.\n", dMinFactor, dMaxFactor);
         // FIXME: Audacity will hang after this if branch.
         return;
      }
   }

   Resample::~Resample()
   {
      resample_close(mHandle);
      mHandle = NULL;
   }

   int Resample::GetNumMethods() { return 2; }

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

   int Resample::GetFastMethodDefault() { return 0; }
   int Resample::GetBestMethodDefault() { return 1; }

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

#elif USE_LIBSAMPLERATE

   #include <samplerate.h>

   Resample::Resample(const bool useBestMethod, const double dMinFactor, const double dMaxFactor)
   {
      this->SetMethod(useBestMethod);
      if (!src_is_valid_ratio (dMinFactor) || !src_is_valid_ratio (dMaxFactor)) {
         fprintf(stderr, "libsamplerate supports only resampling factors between 1/SRC_MAX_RATIO and SRC_MAX_RATIO.\n");
         // FIXME: Audacity will hang after this if branch.
         mHandle = NULL;
         return;
      }

      int err;
      SRC_STATE *state = src_new(mMethod, 1, &err);
      mHandle = (void *)state;
      mShouldReset = false;
      mSamplesLeft = 0;
   }

   Resample::~Resample()
   {
      src_delete((SRC_STATE *)mHandle);
      mHandle = NULL;
   }

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

   int Resample::Process(double  factor,
                                  float  *inBuffer,
                                  int     inBufferLen,
                                  bool    lastFlag,
                                  int    *inBufferUsed,
                                  float  *outBuffer,
                                  int     outBufferLen)
   {
      src_set_ratio((SRC_STATE *)mHandle, factor);

      if(mShouldReset) {
         if(inBufferLen > mSamplesLeft) {
            mShouldReset = false;
            src_reset((SRC_STATE *)mHandle);
         } else {
            mSamplesLeft -= inBufferLen;
         }
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

      if(lastFlag) {
         mShouldReset = true;
         mSamplesLeft = inBufferLen - (int)data.input_frames_used;
      }

      *inBufferUsed = (int)data.input_frames_used;
      return (int)data.output_frames_gen;
   }

#elif USE_LIBSOXR

   #include <soxr.h>

   Resample::Resample(const bool useBestMethod, const double dMinFactor, const double dMaxFactor)
   {
      this->SetMethod(useBestMethod);
      soxr_quality_spec_t q_spec;
      if (dMinFactor == dMaxFactor)
      {
         mbWantConstRateResampling = true; // constant rate resampling
         q_spec = soxr_quality_spec("\0\1\4\6"[mMethod], 0);
      }
      else
      {
         mbWantConstRateResampling = false; // variable rate resampling
         q_spec = soxr_quality_spec(SOXR_HQ, SOXR_VR);
      }
      mHandle = (void *)soxr_create(1, dMinFactor, 1, 0, 0, &q_spec, 0);
   }

   Resample::~Resample()
   {
      soxr_delete((soxr_t)mHandle);
      mHandle = NULL;
   }

   int Resample::GetNumMethods() { return 4; }

   wxString Resample::GetMethodName(int index)
   {
      static char const * const soxr_method_names[] = {
         "Low Quality (Fastest)", "Medium Quality", "High Quality", "Best Quality (Slowest)"
      };

      return wxString(wxString::FromAscii(soxr_method_names[index]));
   }

   const wxString Resample::GetFastMethodKey()
   {
      return wxT("/Quality/LibsoxrSampleRateConverter");
   }

   const wxString Resample::GetBestMethodKey()
   {
      return wxT("/Quality/LibsoxrHQSampleRateConverter");
   }

   int Resample::GetFastMethodDefault() {return 1;}
   int Resample::GetBestMethodDefault() {return 3;}

   int Resample::Process(double  factor,
                         float  *inBuffer,
                         int     inBufferLen,
                         bool    lastFlag,
                         int    *inBufferUsed,
                         float  *outBuffer,
                         int     outBufferLen)
   {
      size_t idone, odone;
      if (mbWantConstRateResampling)
      {
         soxr_process((soxr_t)mHandle,
               inBuffer , (size_t)(lastFlag? ~inBufferLen : inBufferLen), &idone,
               outBuffer, (size_t)                          outBufferLen, &odone);
      }
      else
      {
         soxr_set_io_ratio((soxr_t)mHandle, 1/factor, 0);

         inBufferLen = lastFlag? ~inBufferLen : inBufferLen;
         soxr_process((soxr_t)mHandle,
               inBuffer , (size_t)inBufferLen , &idone,
               outBuffer, (size_t)outBufferLen, &odone);
      }
      *inBufferUsed = (int)idone;
      return (int)odone;
   }
#endif
