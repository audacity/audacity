/**********************************************************************

  Audacity: A Digital Audio Editor

  Dominic Mazzoni

*******************************************************************//*!

\class Resample
\brief Combined interface to libresample and libsamplerate.

  This class abstracts the interface to two different resampling
  libraries:

    libresample, written by Dominic Mazzoni based on Resample-1.7
    by Julius Smith.  LGPL.

    libsamplerate, written by Erik de Castro Lopo.  GPL.  The author
    of libsamplerate requests that you not distribute a binary version
    of Audacity that links to libsamplerate and also has plug-in support.


*//*******************************************************************/


#include "Resample.h"
#include "Prefs.h"

#include <wx/intl.h>

int Resample::GetFastMethod()
{
   return gPrefs->Read(GetFastMethodKey(), GetFastMethodDefault());
}

int Resample::GetBestMethod()
{
   return gPrefs->Read(GetBestMethodKey(), GetBestMethodDefault());
}

void Resample::SetFastMethod(int index)
{
   gPrefs->Write(GetFastMethodKey(), (long)index);
}

void Resample::SetBestMethod(int index)
{
   gPrefs->Write(GetBestMethodKey(), (long)index);
}

#if USE_LIBRESAMPLE

#include "libresample.h"

bool Resample::ResamplingEnabled()
{
   return true;
}

wxString Resample::GetResamplingLibraryName()
{
   return _("Libresample by Dominic Mazzoni and Julius Smith");
}

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

wxString Resample::GetResamplingLibraryName()
{
   return _("Libsamplerate by Erik de Castro Lopo");
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

Resample::Resample(bool useBestMethod, double minFactor, double maxFactor)
{
   if (!src_is_valid_ratio (minFactor) || !src_is_valid_ratio (maxFactor)) {
      fprintf(stderr, "libsamplerate supports only resampling factors between 1/SRC_MAX_RATIO and SRC_MAX_RATIO.\n");
      // FIXME: Audacity will hang after this if branch.
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

#else // No resampling support

bool Resample::ResamplingEnabled()
{
   return false;
}

wxString Resample::GetResamplingLibraryName()
{
   return _("Resampling disabled.");
}

int Resample::GetNumMethods()
{
   return 1;
}

wxString Resample::GetMethodName(int index)
{
   return _("Resampling disabled.");
}

const wxString Resample::GetFastMethodKey()
{
   return wxT("/Quality/DisabledConverter");
}

const wxString Resample::GetBestMethodKey()
{
   return wxT("/Quality/DisabledConverter");
}

int Resample::GetFastMethodDefault()
{
   return 0;
}

int Resample::GetBestMethodDefault()
{
   return 0;
}

Resample::Resample(bool, double, double)
{
}

bool Resample::Ok()
{
   return false;
}

int Resample::Process(double  factor,
                      float  *inBuffer,
                      int     inBufferLen,
                      bool    lastFlag,
                      int    *inBufferUsed,
                      float  *outBuffer,
                      int     outBufferLen)
{
   int i;
   int len = inBufferLen;

   if (len > outBufferLen)
      len = outBufferLen;

   for(i=0; i<len; i++)
      outBuffer[i] = inBuffer[i];

   return len;
}

Resample::~Resample()
{
}

#endif






// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: ceb8c6d0-3df6-4b6f-b763-100fe856f6fb

