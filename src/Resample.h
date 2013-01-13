/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2012 Audacity Team.
   License: GPL v2.  See License.txt.

   Resample.cpp
   Dominic Mazzoni, Rob Sykes, Vaughan Johnson

**********************************************************************/

#ifndef __AUDACITY_RESAMPLE_H__
#define __AUDACITY_RESAMPLE_H__

#include "Audacity.h"

#include <wx/intl.h>
#include <wx/string.h>

#include "Prefs.h"
#include "SampleFormat.h"

class Resample
{
 public:
   /// The first parameter lets you select either the best method or
   /// the fast method. 
   //v (The particular method used was previously set by
   /// SetFastMethod or SetBestMethod.)  
   Resample() 
   {
      mMethod = 0; 
      mHandle = NULL;
   };
   virtual ~Resample() {};

   /// Returns the name of the library used for resampling
   /// (long format, may include author name and version number).
   //v Currently unused. 
   // virtual wxString GetResamplingLibraryName() { return _("Resampling disabled."); };

   /// Resamplers may have more than one method, offering a
   /// tradeoff between speed and quality.  
   /// Audacity identifies two methods out of all of the choices:
   /// a Fast method intended for real-time audio I/O, and a Best
   /// method intended for mixing and exporting. 
   //v (These were previously saved
   // in the preferences when you call Set[Best,Fast]Method.
   // Now done with TieChoice() in QualityPrefs. 
   // Implemented in descendant classes, for class-specificity.)
   //static void SetFastMethod(int index);
   //static void SetBestMethod(int index);

   /** @brief Main processing function. Resamples from the input buffer to the 
    * output buffer.
    *
    * Reads samples from the input buffer, and writes samples to the output
    * buffer. Stops when either is exhaughsted, or we reach a convenient block
    * end, unless lastFlag is set to force emptying the input buffer.
    * The number of input samples used is returned in inBufferUsed, and the
    * number of output samples generated is the return value of the function. 
    * This function may do nothing if you don't pass a large enough output
    * buffer (i.e. there is no where to put a full block of output data)
    @param factor The scaling factor to resample by.
    @param inBuffer Buffer of input samples to be processed (mono)
    @param inBufferLen Length of the input buffer, in samples.
    @param lastFlag Flag to indicate this is the last lot of input samples and
    the buffer needs to be emptied out into the rate converter.
    @param inBufferUsed Number of samples from inBuffer that have been used 
    (unless lastFlag is true, we don't garuntee to process all the samples in
    the input this time, we may leave some for next time)
    @param outBuffer Buffer to write output (converted) samples to.
    @param outBufferLen How big outBuffer is.
    @return Number of output samples created by this call
   */
   virtual int Process(double  WXUNUSED(factor),
                        float  *inBuffer,
                        int     inBufferLen,
                        bool    WXUNUSED(lastFlag),
                        int    * WXUNUSED(inBufferUsed),
                        float  *outBuffer,
                        int     outBufferLen) 
   {
      // Base class method just copies data with no change. 
      int i;
      int len = inBufferLen;

      if (len > outBufferLen)
         len = outBufferLen;

      for(i=0; i<len; i++)
         outBuffer[i] = inBuffer[i];

      return len;
   };

 protected:
   int   mMethod; // resampler-specific enum for resampling method
   void* mHandle; // constant-rate or variable-rate resampler (XOR per instance)
#if USE_LIBSAMPLERATE 
   bool mShouldReset; // whether the resampler should be reset because lastFlag has been set previously
   int  mSamplesLeft; // number of samples left before a reset is needed
#endif
};

class ConstRateResample : public Resample
{
 public:
   ConstRateResample(const bool useBestMethod, const double dFactor);
   virtual ~ConstRateResample();

   // Override base class methods only if we actually have a sample rate conversion library.
   #if USE_LIBRESAMPLE || USE_LIBSAMPLERATE || USE_LIBSOXR
      static int GetNumMethods();
      static wxString GetMethodName(int index);

      static const wxString GetFastMethodKey();
      static const wxString GetBestMethodKey();
      static int GetFastMethodDefault();
      static int GetBestMethodDefault();

      virtual int Process(double  factor,
                           float  *inBuffer,
                           int     inBufferLen,
                           bool    lastFlag,
                           int    *inBufferUsed,
                           float  *outBuffer,
                           int     outBufferLen);
   #else
      static int GetNumMethods() { return 1; };
      static wxString GetMethodName(int WXUNUSED(index)) { return _("Resampling disabled."); };
      static const wxString GetFastMethodKey() { return wxT("/Quality/DisabledConverter"); };
      static const wxString GetBestMethodKey() { return wxT("/Quality/DisabledConverter"); };
      static int GetFastMethodDefault() { return 0; };
      static int GetBestMethodDefault() { return 0; };
   #endif

 protected:
   void SetMethod(const bool useBestMethod)
   {
      if (useBestMethod)
         mMethod = gPrefs->Read(GetBestMethodKey(), GetBestMethodDefault());
      else
         mMethod = gPrefs->Read(GetFastMethodKey(), GetFastMethodDefault());
   };
 private:
   bool  mInitial;
};

class VarRateResample : public Resample
{
 public:
   // dMinFactor and dMaxFactor specify the range of factors for variable-rate resampling.
   VarRateResample(const bool useBestMethod, const double dMinFactor, const double dMaxFactor);
   virtual ~VarRateResample();

   // Override base class methods only if we actually have a sample rate conversion library.
   #if USE_LIBRESAMPLE || USE_LIBSAMPLERATE || USE_LIBSOXR
      //vvv Note that we're not actually calling any of these Get* methods 
      // for var-rate, as the decision was to not allow QualityPrefs for it. 
      // However the menthods already existed for libresample and libsamplerate, 
      // so they're now implemented for all, in case we decide to provide prefs. 
      static int GetNumMethods();
      static wxString GetMethodName(int index);

      static const wxString GetFastMethodKey();
      static const wxString GetBestMethodKey();
      static int GetFastMethodDefault();
      static int GetBestMethodDefault();

      virtual int Process(double  factor,
                           float  *inBuffer,
                           int     inBufferLen,
                           bool    lastFlag,
                           int    *inBufferUsed,
                           float  *outBuffer,
                           int     outBufferLen);
   #else
      static int GetNumMethods() { return 1; };
      static wxString GetMethodName(int WXUNUSED(index)) { return _("Resampling disabled."); };
      static const wxString GetFastMethodKey() { return wxT("/Quality/DisabledConverter"); };
      static const wxString GetBestMethodKey() { return wxT("/Quality/DisabledConverter"); };
      static int GetFastMethodDefault() { return 0; };
      static int GetBestMethodDefault() { return 0; };
   #endif

 protected:
   void SetMethod(const bool useBestMethod)
   {
      if (useBestMethod)
         mMethod = gPrefs->Read(GetBestMethodKey(), GetBestMethodDefault());
      else
         mMethod = gPrefs->Read(GetFastMethodKey(), GetFastMethodDefault());
   };
};

#endif // __AUDACITY_RESAMPLE_H__
