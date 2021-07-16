/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2012 Audacity Team.
   License: GPL v2.  See License.txt.

   Resample.cpp
   Dominic Mazzoni, Rob Sykes, Vaughan Johnson

******************************************************************//**

\class Resample
\brief Interface to libsoxr.

   This class abstracts the interface to different resampling libraries:

      libsoxr, written by Rob Sykes. LGPL.

   Since Audacity always does resampling on mono streams that are
   contiguous in memory, this class doesn't support multiple channels
   or some of the other optional features of some of these resamplers.

*//*******************************************************************/

#include "Resample.h"
#include "Prefs.h"
#include "Internat.h"
#include "ComponentInterface.h"

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
   mHandle.reset(soxr_create(1, dMinFactor, 1, 0, 0, &q_spec, 0));
}

Resample::~Resample()
{
}

//////////
static const std::initializer_list<EnumValueSymbol> methodNames{
   { wxT("LowQuality"), XO("Low Quality (Fastest)") },
   { wxT("MediumQuality"), XO("Medium Quality") },
   { wxT("HighQuality"), XO("High Quality") },
   { wxT("BestQuality"), XO("Best Quality (Slowest)") }
};

static auto intChoicesMethod = {
   0, 1, 2, 3
};

EnumSetting< int > Resample::FastMethodSetting{
   wxT("/Quality/LibsoxrSampleRateConverterChoice"),
   methodNames,
   1,  // Medium Quality

   // for migrating old preferences:
   intChoicesMethod,
   wxT("/Quality/LibsoxrSampleRateConverter")
};

EnumSetting< int > Resample::BestMethodSetting
{
   wxT("/Quality/LibsoxrHQSampleRateConverterChoice"),
   methodNames,
   3, // Best Quality,

   // for migrating old preferences:
   intChoicesMethod,
   wxT("/Quality/LibsoxrHQSampleRateConverter")
};

//////////
std::pair<size_t, size_t>
      Resample::Process(double  factor,
                        float  *inBuffer,
                        size_t  inBufferLen,
                        bool    lastFlag,
                        float  *outBuffer,
                        size_t  outBufferLen)
{
   size_t idone, odone;
   if (mbWantConstRateResampling)
   {
      soxr_process(mHandle.get(),
            inBuffer , (lastFlag? ~inBufferLen : inBufferLen), &idone,
            outBuffer,                           outBufferLen, &odone);
   }
   else
   {
      soxr_set_io_ratio(mHandle.get(), 1/factor, 0);

      inBufferLen = lastFlag? ~inBufferLen : inBufferLen;
      soxr_process(mHandle.get(),
            inBuffer , inBufferLen , &idone,
            outBuffer, outBufferLen, &odone);
   }
   return { idone, odone };
}

void Resample::SetMethod(const bool useBestMethod)
{
   if (useBestMethod)
      mMethod = BestMethodSetting.ReadEnum();
   else
      mMethod = FastMethodSetting.ReadEnum();
}
