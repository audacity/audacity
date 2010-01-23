/**********************************************************************

  Audacity: A Digital Audio Editor

  Dominic Mazzoni

  This class abstracts the interface to two different resampling
  libraries:

    libresample, written by Dominic Mazzoni based on Resample-1.7
    by Julius Smith.  LGPL.

    libsamplerate, written by Erik de Castro Lopo.  GPL.  The author
    of libsamplerate requests that you not distribute a binary version
    of Audacity that links to libsamplerate and also has plug-in support.

  Since Audacity always does resampling on mono streams that are
  contiguous in memory, this class doesn't support multiple channels
  or some of the other optional features of some of these resamplers.

**********************************************************************/

#ifndef __AUDACITY_RESAMPLE_H__
#define __AUDACITY_RESAMPLE_H__

#include "Audacity.h"

#include <wx/string.h>

#include "SampleFormat.h"

class Resample
{
 public:

   /// This will return true if Audacity is being compiled with
   /// resampling support.
   static bool ResamplingEnabled();

   /// Returns the name of the library used for resampling
   /// (long format, may include author name and version number).
   static wxString GetResamplingLibraryName();

   /// Resamplers may have more than one method, offering a
   /// tradeoff between speed and quality.  This lets you query
   /// the various methods available.
   static int GetNumMethods();
   static wxString GetMethodName(int index);

   /// Audacity identifies two methods out of all of the choices:
   /// a Fast method intended for real-time audio I/O, and a Best
   /// method intended for mixing and exporting.  These are saved
   /// in the preferences when you call Set[Best,Fast]Method.
   static int GetFastMethod();
   static int GetBestMethod();
   static void SetFastMethod(int index);
   static void SetBestMethod(int index);

   static const wxString GetFastMethodKey();
   static const wxString GetBestMethodKey();
   static int GetFastMethodDefault();
   static int GetBestMethodDefault();

   /// Constructor.
   /// The first parameter lets you select either the best method or
   /// the fast method - the particular method used was set by
   /// SetFastMethod or SetBestMethod, above.  minFactor and maxFactor
   /// specify the range of factors that will be used, if you plan
   /// to vary the factor over time.  Otherwise set minFactor and
   /// maxFactor to the same value for optimized performance.
   Resample(bool useBestMethod, double minFactor, double maxFactor);

   /// Returns true if the constructor succeeded.
   bool Ok();

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
   int Process(double  factor,
               float  *inBuffer,
               int     inBufferLen,
               bool    lastFlag,
               int    *inBufferUsed,
               float  *outBuffer,
               int     outBufferLen);

   // Destructor
   ~Resample();

 private:
   int   mMethod;
   void *mHandle;
   bool  mInitial;
};

#endif // __AUDACITY_RESAMPLE_H__

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: bffbe34c-3029-47dc-af4c-f83d9a26002c

