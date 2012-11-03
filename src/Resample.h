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
   // minFactor and maxFactor
   /// specify the range of factors that will be used, if you plan
   /// to vary the factor over time.  Otherwise set minFactor and
   /// maxFactor to the same value for optimized performance.
   Resample() 
   { 
      mMethod = 0; 
      mHandle = NULL;
      mInitial = false;
   };
   virtual ~Resample() {};

   /// Returns the name of the library used for resampling
   /// (long format, may include author name and version number).
   //v Currently unused. 
   // virtual wxString GetResamplingLibraryName() { return _("Resampling disabled."); };

   /// Resamplers may have more than one method, offering a
   /// tradeoff between speed and quality.  This lets you query
   /// the various methods available.
   static int GetNumMethods() { return 1; };
   static wxString GetMethodName(int index) { return _("Resampling disabled."); };

   /// Audacity identifies two methods out of all of the choices:
   /// a Fast method intended for real-time audio I/O, and a Best
   /// method intended for mixing and exporting. 
   //v (These were previously saved
   /// in the preferences when you call Set[Best,Fast]Method.)
   int GetFastMethod() { return gPrefs->Read(GetFastMethodKey(), GetFastMethodDefault()); };
   int GetBestMethod() { return gPrefs->Read(GetBestMethodKey(), GetBestMethodDefault()); };
   //v Currently unused.
   //static void SetFastMethod(int index);
   //static void SetBestMethod(int index);

   static const wxString GetFastMethodKey() { return wxT("/Quality/DisabledConverter"); };
   static const wxString GetBestMethodKey() { return wxT("/Quality/DisabledConverter"); };
   static int GetFastMethodDefault() { return 0; };
   static int GetBestMethodDefault() { return 0; };

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
   virtual int Process(double  factor,
                        float  *inBuffer,
                        int     inBufferLen,
                        bool    lastFlag,
                        int    *inBufferUsed,
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
   bool  mInitial;
};

class ConstRateResample : public Resample
{
 public:
   ConstRateResample(const bool useBestMethod, const double dFactor);
   virtual ~ConstRateResample();

   // Override base class methods only if we actually have a const-rate library.
   #if USE_LIBSOXR
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
   #endif
};

#endif // __AUDACITY_RESAMPLE_H__
