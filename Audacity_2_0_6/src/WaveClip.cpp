/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveClip.cpp

  ?? Dominic Mazzoni
  ?? Markus Meyer

*******************************************************************//**

\class WaveClip
\brief This allows multiple clips to be a part of one WaveTrack.

*//****************************************************************//**

\class WaveCache
\brief Cache used with WaveClip to cache wave information (for drawing).

*//****************************************************************//**

\class SpecCache
\brief Cache used with WaveClip to cache spectrum information (for
drawing).  Cache's the Spectrogram frequency samples.

*//*******************************************************************/

#include <math.h>
#include <vector>
#include <wx/log.h>

#include "Spectrum.h"
#include "Prefs.h"
#include "WaveClip.h"
#include "Envelope.h"
#include "Resample.h"
#include "Project.h"

#include <wx/listimpl.cpp>
WX_DEFINE_LIST(WaveClipList);

class WaveCache {
public:
   WaveCache(int cacheLen)
   {
      dirty = -1;
      start = -1.0;
      pps = 0.0;
      len = cacheLen;
      min = new float[len];
      max = new float[len];
      rms = new float[len];
      bl = new int[len];
      where = new sampleCount[len+1];
      numODPixels=0;
   }

   ~WaveCache()
   {
      delete[] min;
      delete[] max;
      delete[] rms;
      delete[] bl;
      delete[] where;

      ClearInvalidRegions();
   }

   int          dirty;
   sampleCount  len;
   double       start;
   double       pps;
   int          rate;
   sampleCount *where;
   float       *min;
   float       *max;
   float       *rms;
   int         *bl;
   int         numODPixels;

   class InvalidRegion
   {
   public:
     InvalidRegion(int s, int e):start(s),end(e){}
     //start and end pixel count.  (not samples)
     int start;
     int end;
   };


   //Thread safe call to add a new region to invalidate.  If it overlaps with other regions, it unions the them.
   void AddInvalidRegion(sampleCount sampleStart, sampleCount sampleEnd)
   {
      //use pps to figure out where we are.  (pixels per second)
      if(pps ==0)
         return;
      double samplesPerPixel = rate/pps;
      //rate is SR, start is first time of the waveform (in second) on cache
      long invalStart = (sampleStart - start*rate)/samplesPerPixel ;

      long invalEnd = (sampleEnd - start*rate)/samplesPerPixel +1; //we should cover the end..

      //if they are both off the cache boundary in the same direction, the cache is missed,
      //so we are safe, and don't need to track this one.
      if((invalStart<0 && invalEnd <0) || (invalStart>=len && invalEnd >= len))
         return;

      //in all other cases, we need to clip the boundries so they make sense with the cache.
      //for some reason, the cache is set up to access up to array[len], not array[len-1]
      if(invalStart <0)
         invalStart =0;
      else if(invalStart > len)
         invalStart = len;

      if(invalEnd <0)
         invalEnd =0;
      else if(invalEnd > len)
         invalEnd = len;


      mRegionsMutex.Lock();

      //look thru the region array for a place to insert.  We could make this more spiffy than a linear search
      //but right now it is not needed since there will usually only be one region (which grows) for OD loading.
      bool added=false;
      if(mRegions.size())
      {
         for(size_t i=0;i<mRegions.size();i++)
         {
            //if the regions intersect OR are pixel adjacent
            if(mRegions[i]->start <= invalEnd+1
               && mRegions[i]->end >= invalStart-1)
            {
               //take the union region
               if(mRegions[i]->start > invalStart)
                  mRegions[i]->start = invalStart;
               if(mRegions[i]->end < invalEnd)
                  mRegions[i]->end = invalEnd;
               added=true;
               break;
            }

            //this bit doesn't make sense because it assumes we add in order - now we go backwards after the initial OD finishes
//            //this array is sorted by start/end points and has no overlaps.   If we've passed all possible intersections, insert.  The array will remain sorted.
//            if(mRegions[i]->end < invalStart)
//            {
//               InvalidRegion* newRegion = new InvalidRegion(invalStart,invalEnd);
//               mRegions.insert(mRegions.begin()+i,newRegion);
//               break;
//            }
         }
      }

      if(!added)
      {
         InvalidRegion* newRegion = new InvalidRegion(invalStart,invalEnd);
         mRegions.insert(mRegions.begin(),newRegion);
      }


      //now we must go and patch up all the regions that overlap.  Overlapping regions will be adjacent.
      for(size_t i=1;i<mRegions.size();i++)
      {
         //if the regions intersect OR are pixel adjacent
         if(mRegions[i]->start <= mRegions[i-1]->end+1
            && mRegions[i]->end >= mRegions[i-1]->start-1)
         {
            //take the union region
            if(mRegions[i]->start > mRegions[i-1]->start)
               mRegions[i]->start = mRegions[i-1]->start;
            if(mRegions[i]->end < mRegions[i-1]->end)
               mRegions[i]->end = mRegions[i-1]->end;

            //now we must delete the previous region
            delete mRegions[i-1];
            mRegions.erase(mRegions.begin()+i-1);
               //musn't forget to reset cursor
               i--;
         }

         //if we are past the end of the region we added, we are past the area of regions that might be oversecting.
         if(mRegions[i]->start > invalEnd)
         {
            break;
         }
      }


      mRegionsMutex.Unlock();
   }

   //lock before calling these in a section.  unlock after finished.
   int GetNumInvalidRegions(){return mRegions.size();}
   int GetInvalidRegionStart(int i){return mRegions[i]->start;}
   int GetInvalidRegionEnd(int i){return mRegions[i]->end;}

   void LockInvalidRegions(){mRegionsMutex.Lock();}
   void UnlockInvalidRegions(){mRegionsMutex.Unlock();}

   void ClearInvalidRegions()
   {
      for(size_t i =0;i<mRegions.size();i++)
      {
         delete mRegions[i];
      }
      mRegions.clear();
   }


protected:
   std::vector<InvalidRegion*> mRegions;
      ODLock mRegionsMutex;

};

class SpecCache {
public:
   SpecCache(int cacheLen, int half, bool autocorrelation)
   {
      minFreqOld = -1;
      maxFreqOld = -1;
      gainOld = -1;
      rangeOld = -1;
      windowTypeOld = -1;
      windowSizeOld = -1;
      frequencyGainOld = false;
#ifdef EXPERIMENTAL_FFT_SKIP_POINTS
      fftSkipPointsOld = -1;
#endif //EXPERIMENTAL_FFT_SKIP_POINTS
      dirty = -1;
      start = -1.0;
      pps = 0.0;
      len = cacheLen;
      ac = autocorrelation;
      freq = new float[len*half];
      where = new sampleCount[len+1];
   }

   ~SpecCache()
   {
      delete[] freq;
      delete[] where;
   }

   int          minFreqOld;
   int          maxFreqOld;
   int          gainOld;
   int          rangeOld;
   int          windowTypeOld;
   int          windowSizeOld;
   int          frequencyGainOld;
#ifdef EXPERIMENTAL_FFT_SKIP_POINTS
   int          fftSkipPointsOld;
#endif //EXPERIMENTAL_FFT_SKIP_POINTS
   int          dirty;
   bool         ac;
   sampleCount  len;
   double       start;
   double       pps;
   sampleCount *where;
   float       *freq;
};

#ifdef EXPERIMENTAL_USE_REALFFTF
#include "FFT.h"
static void ComputeSpectrumUsingRealFFTf(float *buffer, HFFT hFFT, float *window, int len, float *out)
{
   int i;
   if(len > hFFT->Points*2)
      len = hFFT->Points*2;
   for(i=0; i<len; i++)
      buffer[i] *= window[i];
   for( ; i<(hFFT->Points*2); i++)
      buffer[i]=0; // zero pad as needed
   RealFFTf(buffer, hFFT);
   // Handle the (real-only) DC
   float power = buffer[0]*buffer[0];
   if(power <= 0)
      out[0] = -160.0;
   else
      out[0] = 10.0*log10(power);
   for(i=1;i<hFFT->Points;i++) {
      power = (buffer[hFFT->BitReversed[i]  ]*buffer[hFFT->BitReversed[i]  ])
            + (buffer[hFFT->BitReversed[i]+1]*buffer[hFFT->BitReversed[i]+1]);
      if(power <= 0)
         out[i] = -160.0;
      else
         out[i] = 10.0*log10f(power);
   }
}
#endif // EXPERIMENTAL_USE_REALFFTF

WaveClip::WaveClip(DirManager *projDirManager, sampleFormat format, int rate)
{
   mOffset = 0;
   mRate = rate;
   mSequence = new Sequence(projDirManager, format);
   mEnvelope = new Envelope();
   mWaveCache = new WaveCache(1);
#ifdef EXPERIMENTAL_USE_REALFFTF
   mWindowType = -1;
   mWindowSize = -1;
   hFFT = NULL;
   mWindow = NULL;
#endif
   mSpecCache = new SpecCache(1, 1, false);
   mSpecPxCache = new SpecPxCache(1);
   mAppendBuffer = NULL;
   mAppendBufferLen = 0;
   mDirty = 0;
   mIsPlaceholder = false;
}

WaveClip::WaveClip(WaveClip& orig, DirManager *projDirManager)
{
   // essentially a copy constructor - but you must pass in the
   // current project's DirManager, because we might be copying
   // from one project to another

   mOffset = orig.mOffset;
   mRate = orig.mRate;
   mSequence = new Sequence(*orig.mSequence, projDirManager);
   mEnvelope = new Envelope();
   mEnvelope->Paste(0.0, orig.mEnvelope);
   mEnvelope->SetOffset(orig.GetOffset());
   mEnvelope->SetTrackLen(((double)orig.mSequence->GetNumSamples()) / orig.mRate);
   mWaveCache = new WaveCache(1);
#ifdef EXPERIMENTAL_USE_REALFFTF
   mWindowType = -1;
   mWindowSize = -1;
   hFFT = NULL;
   mWindow = NULL;
#endif
   mSpecCache = new SpecCache(1, 1, false);
   mSpecPxCache = new SpecPxCache(1);

   for (WaveClipList::compatibility_iterator it=orig.mCutLines.GetFirst(); it; it=it->GetNext())
      mCutLines.Append(new WaveClip(*it->GetData(), projDirManager));

   mAppendBuffer = NULL;
   mAppendBufferLen = 0;
   mDirty = 0;
   mIsPlaceholder = orig.GetIsPlaceholder();
}

WaveClip::~WaveClip()
{
   delete mSequence;

   delete mEnvelope;
   mEnvelope = NULL;

   delete mWaveCache;
   delete mSpecCache;
   delete mSpecPxCache;
#ifdef EXPERIMENTAL_USE_REALFFTF
   if(hFFT != NULL)
      EndFFT(hFFT);
   if(mWindow != NULL)
      delete[] mWindow;
#endif

   if (mAppendBuffer)
      DeleteSamples(mAppendBuffer);

   mCutLines.DeleteContents(true);
   mCutLines.Clear();
}

void WaveClip::SetOffset(double offset)
{
    mOffset = offset;
    mEnvelope->SetOffset(mOffset);
}

bool WaveClip::GetSamples(samplePtr buffer, sampleFormat format,
                   sampleCount start, sampleCount len) const
{
   return mSequence->Get(buffer, format, start, len);
}

bool WaveClip::SetSamples(samplePtr buffer, sampleFormat format,
                   sampleCount start, sampleCount len)
{
   bool bResult = mSequence->Set(buffer, format, start, len);
   MarkChanged();
   return bResult;
}

double WaveClip::GetStartTime() const
{
   // JS: mOffset is the minimum value and it is returned; no clipping to 0
   return mOffset;
}

double WaveClip::GetEndTime() const
{
   sampleCount numSamples = mSequence->GetNumSamples();

   double maxLen = mOffset + double(numSamples+mAppendBufferLen)/mRate;
   // JS: calculated value is not the length;
   // it is a maximum value and can be negative; no clipping to 0

   return maxLen;
}

sampleCount WaveClip::GetStartSample() const
{
   return (sampleCount)floor(mOffset * mRate + 0.5);
}

sampleCount WaveClip::GetEndSample() const
{
   return GetStartSample() + mSequence->GetNumSamples();
}

bool WaveClip::WithinClip(double t) const
{
   sampleCount ts = (sampleCount)floor(t * mRate + 0.5);
   return ts > GetStartSample() && ts < GetEndSample() + mAppendBufferLen;
}

bool WaveClip::BeforeClip(double t) const
{
   sampleCount ts = (sampleCount)floor(t * mRate + 0.5);
   return ts <= GetStartSample();
}

bool WaveClip::AfterClip(double t) const
{
   sampleCount ts = (sampleCount)floor(t * mRate + 0.5);
   return ts >= GetEndSample() + mAppendBufferLen;
}

///Delete the wave cache - force redraw.  Thread-safe
void WaveClip::DeleteWaveCache()
{
   mWaveCacheMutex.Lock();
   if(mWaveCache!=NULL)
      delete mWaveCache;
   mWaveCache = new WaveCache(1);
   mWaveCacheMutex.Unlock();
}

///Adds an invalid region to the wavecache so it redraws that portion only.
void WaveClip::AddInvalidRegion(long startSample, long endSample)
{
   mWaveCacheMutex.Lock();
   if(mWaveCache!=NULL)
      mWaveCache->AddInvalidRegion(startSample,endSample);
   mWaveCacheMutex.Unlock();
}

//
// Getting high-level data from the track for screen display and
// clipping calculations
//

bool WaveClip::GetWaveDisplay(float *min, float *max, float *rms,int* bl,
                               sampleCount *where,
                               int numPixels, double t0,
                               double pixelsPerSecond, bool &isLoadingOD)
{
   mWaveCacheMutex.Lock();


   if (mWaveCache &&
       mWaveCache->dirty == mDirty &&
       mWaveCache->start == t0 &&
       mWaveCache->len >= numPixels &&
       mWaveCache->pps == pixelsPerSecond) {

      //check for invalid regions, and make the bottom if an else if.
      //invalid regions are kept in a sorted array.
      for(int i=0;i<mWaveCache->GetNumInvalidRegions();i++)
      {
         int invStart;
         invStart = mWaveCache->GetInvalidRegionStart(i);
         int invEnd;
         invEnd = mWaveCache->GetInvalidRegionEnd(i);

         int regionODPixels;
         regionODPixels =0;
         int regionODPixelsAfter;
         regionODPixelsAfter =0;
         //before check number of ODPixels
         for(int j=invStart;j<invEnd;j++)
         {
            if(mWaveCache->bl[j]<0)
               regionODPixels++;
         }
         mSequence->GetWaveDisplay(&mWaveCache->min[invStart],
                                        &mWaveCache->max[invStart],
                                        &mWaveCache->rms[invStart],
                                        &mWaveCache->bl[invStart],
                                        invEnd-invStart,
                                        &mWaveCache->where[invStart],
                                        mRate / pixelsPerSecond);
         //after check number of ODPixels
         for(int j=invStart;j<invEnd;j++)
         {
            if(mWaveCache->bl[j]<0)
               regionODPixelsAfter++;
         }
         //decrement the number of od pixels.
         mWaveCache->numODPixels -= (regionODPixels - regionODPixelsAfter);
      }
      mWaveCache->ClearInvalidRegions();


      memcpy(min, mWaveCache->min, numPixels*sizeof(float));
      memcpy(max, mWaveCache->max, numPixels*sizeof(float));
      memcpy(rms, mWaveCache->rms, numPixels*sizeof(float));
      memcpy(bl, mWaveCache->bl, numPixels*sizeof(int));
      memcpy(where, mWaveCache->where, (numPixels+1)*sizeof(sampleCount));
      isLoadingOD = mWaveCache->numODPixels>0;
      mWaveCacheMutex.Unlock();
      return true;
   }

   WaveCache *oldCache = mWaveCache;

   mWaveCache = new WaveCache(numPixels);
   mWaveCache->pps = pixelsPerSecond;
   mWaveCache->rate = mRate;
   mWaveCache->start = t0;
   double tstep = 1.0 / pixelsPerSecond;

   sampleCount x;

   for (x = 0; x < mWaveCache->len + 1; x++) {
      mWaveCache->where[x] =
         (sampleCount) floor(t0 * mRate +
                             ((double) x) * mRate * tstep + 0.5);
   }

   //mchinen: I think s0 - s1 represents the range of samples that we will need to look up.  likewise p0-p1 the number of pixels.
   sampleCount s0 = mWaveCache->where[0];
   sampleCount s1 = mWaveCache->where[mWaveCache->len];
   int p0 = 0;
   int p1 = mWaveCache->len;

   // Optimization: if the old cache is good and overlaps
   // with the current one, re-use as much of the cache as
   // possible
   if (oldCache->dirty == mDirty &&
       oldCache->pps == pixelsPerSecond &&
       oldCache->where[0] < mWaveCache->where[mWaveCache->len] &&
       oldCache->where[oldCache->len] > mWaveCache->where[0]) {

      //now we are assuming the entire range is covered by the old cache and reducing s1/s0 as we find out otherwise.
      s0 = mWaveCache->where[mWaveCache->len];  //mchinen:s0 is the min sample covered up to by the wave cache.  will shrink if old doen't overlap
      s1 = mWaveCache->where[0];  //mchinen - same, but the maximum sample covered.
      p0 = mWaveCache->len;
      p1 = 0;

      //check for invalid regions, and make the bottom if an else if.
      //invalid regions are keep in a sorted array.
      //TODO:integrate into below for loop so that we only load inval regions if
      //necessary.  (usually is the case, so no rush.)
      //also, we should be updating the NEW cache, but here we are patching the old one up.
      for(int i=0;i<oldCache->GetNumInvalidRegions();i++)
      {
         int invStart;
         invStart = oldCache->GetInvalidRegionStart(i);
         int invEnd;
         invEnd = oldCache->GetInvalidRegionEnd(i);
         mSequence->GetWaveDisplay(&oldCache->min[invStart],
                                        &oldCache->max[invStart],
                                        &oldCache->rms[invStart],
                                        &oldCache->bl[invStart],
                                        invEnd-invStart,
                                        &oldCache->where[invStart],
                                        mRate / pixelsPerSecond);
      }
      oldCache->ClearInvalidRegions();

      for (x = 0; x < mWaveCache->len; x++)
      {



         //below is regular cache access.
         if (mWaveCache->where[x] >= oldCache->where[0] &&
             mWaveCache->where[x] <= oldCache->where[oldCache->len - 1]) {

             //if we hit an invalid region, load it up.

            int ox =
                int ((double (oldCache->len) *
                      (mWaveCache->where[x] -
                       oldCache->where[0])) /(oldCache->where[oldCache->len] -
                                             oldCache->where[0]) + 0.5);

            mWaveCache->min[x] = oldCache->min[ox];
            mWaveCache->max[x] = oldCache->max[ox];
            mWaveCache->rms[x] = oldCache->rms[ox];
            mWaveCache->bl[x] = oldCache->bl[ox];
         } else {
            if (mWaveCache->where[x] < s0) {
               s0 = mWaveCache->where[x];
               p0 = x;
            }
            if (mWaveCache->where[x + 1] > s1) {
               s1 = mWaveCache->where[x + 1];
               p1 = x + 1;
            }
         }
      }
   }

   if (p1 > p0) {

      /* handle values in the append buffer */

      int numSamples = mSequence->GetNumSamples();
      int a;

      for(a=p0; a<p1; a++)
         if (mWaveCache->where[a+1] > numSamples)
            break;

      //compute the values that are outside the overlap from scratch.
      if (a < p1) {
         int i;

         sampleFormat seqFormat = mSequence->GetSampleFormat();
         bool didUpdate = false;
         for(i=a; i<p1; i++) {
            sampleCount left;
            left = mWaveCache->where[i] - numSamples;
            sampleCount right;
            right = mWaveCache->where[i+1] - numSamples;

            //wxCriticalSectionLocker locker(mAppendCriticalSection);

            if (left < 0)
               left = 0;
            if (right > mAppendBufferLen)
               right = mAppendBufferLen;

            if (right > left) {
               float *b;
               sampleCount len = right-left;
               sampleCount j;

               if (seqFormat == floatSample)
                  b = &((float *)mAppendBuffer)[left];
               else {
                  b = new float[len];
                  CopySamples(mAppendBuffer + left*SAMPLE_SIZE(seqFormat),
                              seqFormat,
                              (samplePtr)b, floatSample, len);
               }

               float max = b[0];
               float min = b[0];
               float sumsq = b[0] * b[0];

               for(j=1; j<len; j++) {
                  if (b[j] > max)
                     max = b[j];
                  if (b[j] < min)
                     min = b[j];
                  sumsq += b[j]*b[j];
               }

               mWaveCache->min[i] = min;
               mWaveCache->max[i] = max;
               mWaveCache->rms[i] = (float)sqrt(sumsq / len);
               mWaveCache->bl[i] = 1; //for now just fake it.

               if (seqFormat != floatSample)
                  delete[] b;

               didUpdate=true;
            }
         }

         // So that the sequence doesn't try to write any
         // of these values
         //mchinen: but only do this if we've updated pixels in the cache.
         if(didUpdate)
            p1 = a;
      }

      if (p1 > p0) {
         if (!mSequence->GetWaveDisplay(&mWaveCache->min[p0],
                                        &mWaveCache->max[p0],
                                        &mWaveCache->rms[p0],
                                        &mWaveCache->bl[p0],
                                        p1-p0,
                                        &mWaveCache->where[p0],
                                        mRate / pixelsPerSecond))
         {
            isLoadingOD=false;
            mWaveCacheMutex.Unlock();
            return false;
         }
      }
   }

   mWaveCache->dirty = mDirty;
   delete oldCache;

   memcpy(min, mWaveCache->min, numPixels*sizeof(float));
   memcpy(max, mWaveCache->max, numPixels*sizeof(float));
   memcpy(rms, mWaveCache->rms, numPixels*sizeof(float));
   memcpy(bl, mWaveCache->bl, numPixels*sizeof(int));
   memcpy(where, mWaveCache->where, (numPixels+1)*sizeof(sampleCount));

   //find the number of OD pixels - the only way to do this is by recounting since we've lost some old cache.
   mWaveCache->numODPixels = 0;
   for(int j=0;j<mWaveCache->len;j++)
      if(mWaveCache->bl[j]<0)
         mWaveCache->numODPixels++;

   isLoadingOD = mWaveCache->numODPixels>0;
   mWaveCacheMutex.Unlock();
   return true;
}

bool WaveClip::GetSpectrogram(float *freq, sampleCount *where,
                               int numPixels,
                               double t0, double pixelsPerSecond,
                               bool autocorrelation)
{
   int minFreq = gPrefs->Read(wxT("/Spectrum/MinFreq"), 0L);
   int maxFreq = gPrefs->Read(wxT("/Spectrum/MaxFreq"), 8000L);
   int range = gPrefs->Read(wxT("/Spectrum/Range"), 80L);
   int gain = gPrefs->Read(wxT("/Spectrum/Gain"), 20L);
   int frequencygain = gPrefs->Read(wxT("/Spectrum/FrequencyGain"), 0L);
   int windowType;
   int windowSize = gPrefs->Read(wxT("/Spectrum/FFTSize"), 256);
#ifdef EXPERIMENTAL_FFT_SKIP_POINTS
   int fftSkipPoints = gPrefs->Read(wxT("/Spectrum/FFTSkipPoints"), 0L);
   int fftSkipPoints1 = fftSkipPoints+1;
#endif //EXPERIMENTAL_FFT_SKIP_POINTS
   int half = windowSize/2;
   gPrefs->Read(wxT("/Spectrum/WindowType"), &windowType, 3);

#ifdef EXPERIMENTAL_USE_REALFFTF
   // Update the FFT and window if necessary
   if((mWindowType != windowType) || (mWindowSize != windowSize)
      || (hFFT == NULL) || (mWindow == NULL) || (mWindowSize != hFFT->Points*2) ) {
      mWindowType = windowType;
      mWindowSize = windowSize;
      if(hFFT != NULL)
         EndFFT(hFFT);
      hFFT = InitializeFFT(mWindowSize);
      if(mWindow != NULL) delete[] mWindow;
      // Create the requested window function
      mWindow = new float[mWindowSize];
      int i;
      for(i=0; i<windowSize; i++)
         mWindow[i]=1.0;
      WindowFunc(mWindowType, mWindowSize, mWindow);
      // Scale the window function to give 0dB spectrum for 0dB sine tone
      double ws=0;
      for(i=0; i<windowSize; i++)
         ws += mWindow[i];
      if(ws > 0) {
         ws = 2.0/ws;
         for(i=0; i<windowSize; i++)
            mWindow[i] *= ws;
      }
   }
#endif // EXPERIMENTAL_USE_REALFFTF

   if (mSpecCache &&
       mSpecCache->minFreqOld == minFreq &&
       mSpecCache->maxFreqOld == maxFreq &&
       mSpecCache->rangeOld == range &&
       mSpecCache->gainOld == gain &&
       mSpecCache->windowTypeOld == windowType &&
       mSpecCache->windowSizeOld == windowSize &&
       mSpecCache->frequencyGainOld == frequencygain &&
#ifdef EXPERIMENTAL_FFT_SKIP_POINTS
       mSpecCache->fftSkipPointsOld == fftSkipPoints &&
#endif //EXPERIMENTAL_FFT_SKIP_POINTS
       mSpecCache->dirty == mDirty &&
       mSpecCache->start == t0 &&
       mSpecCache->ac == autocorrelation &&
       mSpecCache->len >= numPixels &&
       mSpecCache->pps == pixelsPerSecond) {
      memcpy(freq, mSpecCache->freq, numPixels*half*sizeof(float));
      memcpy(where, mSpecCache->where, (numPixels+1)*sizeof(sampleCount));
      return false;  //hit cache completely
   }

   SpecCache *oldCache = mSpecCache;

   mSpecCache = new SpecCache(numPixels, half, autocorrelation);
   mSpecCache->pps = pixelsPerSecond;
   mSpecCache->start = t0;

   sampleCount x;

   bool *recalc = new bool[mSpecCache->len + 1];

   for (x = 0; x < mSpecCache->len + 1; x++) {
      recalc[x] = true;
      // purposely offset the display 1/2 bin to the left (as compared
      // to waveform display to properly center response of the FFT
      mSpecCache->where[x] =
         (sampleCount)floor((t0*mRate) + (x*mRate/pixelsPerSecond) + 1.);
   }

   // Optimization: if the old cache is good and overlaps
   // with the current one, re-use as much of the cache as
   // possible
   if (oldCache->dirty == mDirty &&
       oldCache->minFreqOld == minFreq &&
       oldCache->maxFreqOld == maxFreq &&
       oldCache->rangeOld == range &&
       oldCache->gainOld == gain &&
       oldCache->windowTypeOld == windowType &&
       oldCache->windowSizeOld == windowSize &&
       oldCache->frequencyGainOld == frequencygain &&
#ifdef EXPERIMENTAL_FFT_SKIP_POINTS
       oldCache->fftSkipPointsOld == fftSkipPoints &&
#endif //EXPERIMENTAL_FFT_SKIP_POINTS
       oldCache->pps == pixelsPerSecond &&
       oldCache->ac == autocorrelation &&
       oldCache->where[0] < mSpecCache->where[mSpecCache->len] &&
       oldCache->where[oldCache->len] > mSpecCache->where[0]) {

      for (x = 0; x < mSpecCache->len; x++)
         if (mSpecCache->where[x] >= oldCache->where[0] &&
             mSpecCache->where[x] <= oldCache->where[oldCache->len]) {

            int ox = (int) ((double (oldCache->len) *
                      (mSpecCache->where[x] - oldCache->where[0]))
                       / (oldCache->where[oldCache->len] -
                                             oldCache->where[0]) + 0.5);
            if (ox >= 0 && ox < oldCache->len &&
                mSpecCache->where[x] == oldCache->where[ox]) {

               for (sampleCount i = 0; i < (sampleCount)half; i++)
                  mSpecCache->freq[half * x + i] =
                     oldCache->freq[half * ox + i];

               recalc[x] = false;
            }
         }
   }

#ifdef EXPERIMENTAL_FFT_SKIP_POINTS
   float *buffer = new float[windowSize*fftSkipPoints1];
   mSpecCache->fftSkipPointsOld = fftSkipPoints;
#else //!EXPERIMENTAL_FFT_SKIP_POINTS
   float *buffer = new float[windowSize];
#endif //EXPERIMENTAL_FFT_SKIP_POINTS
   mSpecCache->minFreqOld = minFreq;
   mSpecCache->maxFreqOld = maxFreq;
   mSpecCache->gainOld = gain;
   mSpecCache->rangeOld = range;
   mSpecCache->windowTypeOld = windowType;
   mSpecCache->windowSizeOld = windowSize;
   mSpecCache->frequencyGainOld = frequencygain;

   float *gainfactor = NULL;
   if(frequencygain > 0) {
      // Compute a frequency-dependant gain factor
      // scaled such that 1000 Hz gets a gain of 0dB
      double factor = 0.001*(double)mRate/(double)windowSize;
      gainfactor = new float[half];
      for(x = 0; x < half; x++) {
         gainfactor[x] = frequencygain*log10(factor * x);
      }
   }

   for (x = 0; x < mSpecCache->len; x++)
      if (recalc[x]) {

         sampleCount start = mSpecCache->where[x];
         sampleCount len = windowSize;
         sampleCount i;

         if (start <= 0 || start >= mSequence->GetNumSamples()) {

            for (i = 0; i < (sampleCount)half; i++)
               mSpecCache->freq[half * x + i] = 0;

         }
         else
         {
            float *adj = buffer;
            start -= windowSize >> 1;

            if (start < 0) {
               for (i = start; i < 0; i++)
                  *adj++ = 0;
               len += start;
               start = 0;
            }
#ifdef EXPERIMENTAL_FFT_SKIP_POINTS
            if (start + len*fftSkipPoints1 > mSequence->GetNumSamples()) {
               int newlen = (mSequence->GetNumSamples() - start)/fftSkipPoints1;
               for (i = newlen*fftSkipPoints1; i < (sampleCount)len*fftSkipPoints1; i++)
#else //!EXPERIMENTAL_FFT_SKIP_POINTS
            if (start + len > mSequence->GetNumSamples()) {
               int newlen = mSequence->GetNumSamples() - start;
               for (i = newlen; i < (sampleCount)len; i++)
#endif //EXPERIMENTAL_FFT_SKIP_POINTS
                  adj[i] = 0;
               len = newlen;
            }

            if (len > 0)
#ifdef EXPERIMENTAL_FFT_SKIP_POINTS
               mSequence->Get((samplePtr)adj, floatSample, start, len*fftSkipPoints1);
            if (fftSkipPoints) {
               // TODO: (maybe) alternatively change Get to include skipping of points
               int j=0;
               for (int i=0; i < len; i++) {
                  adj[i]=adj[j];
                  j+=fftSkipPoints1;
               }
            }
#else //!EXPERIMENTAL_FFT_SKIP_POINTS
               mSequence->Get((samplePtr)adj, floatSample, start, len);
#endif //EXPERIMENTAL_FFT_SKIP_POINTS

#ifdef EXPERIMENTAL_USE_REALFFTF
            if(autocorrelation) {
               ComputeSpectrum(buffer, windowSize, windowSize,
                               mRate, &mSpecCache->freq[half * x],
                               autocorrelation, windowType);
            } else {
               ComputeSpectrumUsingRealFFTf(buffer, hFFT, mWindow, mWindowSize, &mSpecCache->freq[half * x]);
            }
#else  // EXPERIMENTAL_USE_REALFFTF
           ComputeSpectrum(buffer, windowSize, windowSize,
                           mRate, &mSpecCache->freq[half * x],
                           autocorrelation, windowType);
#endif // EXPERIMENTAL_USE_REALFFTF
           if(gainfactor) {
              // Apply a frequency-dependant gain factor
              for(i=0; i<half; i++)
                 mSpecCache->freq[half * x + i] += gainfactor[i];
           }
         }
      }

   if(gainfactor)
      delete[] gainfactor;
   delete[]buffer;
   delete[]recalc;
   delete oldCache;

   mSpecCache->dirty = mDirty;
   memcpy(freq, mSpecCache->freq, numPixels*half*sizeof(float));
   memcpy(where, mSpecCache->where, (numPixels+1)*sizeof(sampleCount));
   return true;
}

bool WaveClip::GetMinMax(float *min, float *max,
                          double t0, double t1)
{
   *min = float(0.0);   // harmless, but unused since Sequence::GetMinMax does not use these values
   *max = float(0.0);   // harmless, but unused since Sequence::GetMinMax does not use these values

   if (t0 > t1)
      return false;

   if (t0 == t1)
      return true;

   sampleCount s0, s1;

   TimeToSamplesClip(t0, &s0);
   TimeToSamplesClip(t1, &s1);

   return mSequence->GetMinMax(s0, s1-s0, min, max);
}

bool WaveClip::GetRMS(float *rms, double t0,
                          double t1)
{
   *rms = float(0.0);

   if (t0 > t1)
      return false;

   if (t0 == t1)
      return true;

   sampleCount s0, s1;

   TimeToSamplesClip(t0, &s0);
   TimeToSamplesClip(t1, &s1);

   return mSequence->GetRMS(s0, s1-s0, rms);
}

void WaveClip::ConvertToSampleFormat(sampleFormat format)
{
   bool bChanged;
   bool bResult = mSequence->ConvertToSampleFormat(format, &bChanged);
   if (bResult && bChanged)
      MarkChanged();
   wxASSERT(bResult); // TODO: Throw an actual error.
}

void WaveClip::UpdateEnvelopeTrackLen()
{
   mEnvelope->SetTrackLen(((double)mSequence->GetNumSamples()) / mRate);
}

void WaveClip::TimeToSamplesClip(double t0, sampleCount *s0) const
{
   if (t0 < mOffset)
      *s0 = 0;
   else if (t0 > mOffset + double(mSequence->GetNumSamples())/mRate)
      *s0 = mSequence->GetNumSamples();
   else
      *s0 = (sampleCount)floor(((t0 - mOffset) * mRate) + 0.5);
}

void WaveClip::ClearDisplayRect()
{
   mDisplayRect.x = mDisplayRect.y = -1;
   mDisplayRect.width = mDisplayRect.height = -1;
}

void WaveClip::SetDisplayRect(const wxRect& r)
{
   mDisplayRect = r;
}

void WaveClip::GetDisplayRect(wxRect* r)
{
   *r = mDisplayRect;
}

bool WaveClip::Append(samplePtr buffer, sampleFormat format,
                      sampleCount len, unsigned int stride /* = 1 */,
                      XMLWriter* blockFileLog /*=NULL*/)
{
   //wxLogDebug(wxT("Append: len=%i"), len);

   sampleCount maxBlockSize = mSequence->GetMaxBlockSize();
   sampleCount blockSize = mSequence->GetIdealAppendLen();
   sampleFormat seqFormat = mSequence->GetSampleFormat();

   if (!mAppendBuffer)
      mAppendBuffer = NewSamples(maxBlockSize, seqFormat);

   for(;;) {
      if (mAppendBufferLen >= blockSize) {
         bool success =
            mSequence->Append(mAppendBuffer, seqFormat, blockSize,
                              blockFileLog);
         if (!success)
            return false;
         memmove(mAppendBuffer,
                 mAppendBuffer + blockSize * SAMPLE_SIZE(seqFormat),
                 (mAppendBufferLen - blockSize) * SAMPLE_SIZE(seqFormat));
         mAppendBufferLen -= blockSize;
         blockSize = mSequence->GetIdealAppendLen();
      }

      if (len == 0)
         break;

      int toCopy = maxBlockSize - mAppendBufferLen;
      if (toCopy > len)
         toCopy = len;

      CopySamples(buffer, format,
                  mAppendBuffer + mAppendBufferLen * SAMPLE_SIZE(seqFormat),
                  seqFormat,
                  toCopy,
                  true, // high quality
                  stride);

      mAppendBufferLen += toCopy;
      buffer += toCopy * SAMPLE_SIZE(format) * stride;
      len -= toCopy;
   }

   UpdateEnvelopeTrackLen();
   MarkChanged();

   return true;
}

bool WaveClip::AppendAlias(wxString fName, sampleCount start,
                            sampleCount len, int channel,bool useOD)
{
   bool result = mSequence->AppendAlias(fName, start, len, channel,useOD);
   if (result)
   {
      UpdateEnvelopeTrackLen();
      MarkChanged();
   }
   return result;
}

bool WaveClip::AppendCoded(wxString fName, sampleCount start,
                            sampleCount len, int channel, int decodeType)
{
   bool result = mSequence->AppendCoded(fName, start, len, channel, decodeType);
   if (result)
   {
      UpdateEnvelopeTrackLen();
      MarkChanged();
   }
   return result;
}

bool WaveClip::Flush()
{
   //wxLogDebug(wxT("WaveClip::Flush"));
   //wxLogDebug(wxT("   mAppendBufferLen=%i"), mAppendBufferLen);
   //wxLogDebug(wxT("   previous sample count %i"), mSequence->GetNumSamples());

   bool success = true;
   if (mAppendBufferLen > 0) {
      success = mSequence->Append(mAppendBuffer, mSequence->GetSampleFormat(), mAppendBufferLen);
      if (success) {
         mAppendBufferLen = 0;
         UpdateEnvelopeTrackLen();
         MarkChanged();
      }
   }

   //wxLogDebug(wxT("now sample count %i"), mSequence->GetNumSamples());

   return success;
}

bool WaveClip::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   if (!wxStrcmp(tag, wxT("waveclip")))
   {
      double dblValue;
      while (*attrs)
      {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;

         if (!value)
            break;

         const wxString strValue = value;
         if (!wxStrcmp(attr, wxT("offset")))
         {
            if (!XMLValueChecker::IsGoodString(strValue) ||
                  !Internat::CompatibleToDouble(strValue, &dblValue))
               return false;
            SetOffset(dblValue);
         }
      }
      return true;
   }

   return false;
}

void WaveClip::HandleXMLEndTag(const wxChar *tag)
{
   if (!wxStrcmp(tag, wxT("waveclip")))
      UpdateEnvelopeTrackLen();
}

XMLTagHandler *WaveClip::HandleXMLChild(const wxChar *tag)
{
   if (!wxStrcmp(tag, wxT("sequence")))
      return mSequence;
   else if (!wxStrcmp(tag, wxT("envelope")))
      return mEnvelope;
   else if (!wxStrcmp(tag, wxT("waveclip")))
   {
      // Nested wave clips are cut lines
      WaveClip *newCutLine = new WaveClip(mSequence->GetDirManager(),
                                mSequence->GetSampleFormat(), mRate);
      mCutLines.Append(newCutLine);
      return newCutLine;
   } else
      return NULL;
}

void WaveClip::WriteXML(XMLWriter &xmlFile)
{
   xmlFile.StartTag(wxT("waveclip"));
   xmlFile.WriteAttr(wxT("offset"), mOffset, 8);

   mSequence->WriteXML(xmlFile);
   mEnvelope->WriteXML(xmlFile);

   for (WaveClipList::compatibility_iterator it=mCutLines.GetFirst(); it; it=it->GetNext())
      it->GetData()->WriteXML(xmlFile);

   xmlFile.EndTag(wxT("waveclip"));
}

bool WaveClip::CreateFromCopy(double t0, double t1, WaveClip* other)
{
   sampleCount s0, s1;

   other->TimeToSamplesClip(t0, &s0);
   other->TimeToSamplesClip(t1, &s1);

   Sequence* oldSequence = mSequence;
   mSequence = NULL;
   if (!other->mSequence->Copy(s0, s1, &mSequence))
   {
      mSequence = oldSequence;
      return false;
   }

   delete oldSequence;
   delete mEnvelope;
   mEnvelope = new Envelope();
   mEnvelope->CopyFrom(other->mEnvelope, (double)s0/mRate, (double)s1/mRate);

   MarkChanged();

   return true;
}

bool WaveClip::Paste(double t0, WaveClip* other)
{
   WaveClip* pastedClip;

   bool clipNeedsResampling = other->mRate != mRate;

   if (clipNeedsResampling)
   {
      // The other clip's rate is different to our's, so resample
      pastedClip = new WaveClip(*other, mSequence->GetDirManager());
      if (!pastedClip->Resample(mRate))
      {
         delete pastedClip;
         return false;
      }
   } else
   {
      // No resampling needed, just use original clip without making a copy
      pastedClip = other;
   }

   sampleCount s0;
   TimeToSamplesClip(t0, &s0);

   // Force sample formats to match.
   if (pastedClip->mSequence->GetSampleFormat() != mSequence->GetSampleFormat())
      pastedClip->ConvertToSampleFormat(mSequence->GetSampleFormat());

   bool result = false;
   if (mSequence->Paste(s0, pastedClip->mSequence))
   {
      MarkChanged();
      mEnvelope->Paste((double)s0/mRate + mOffset, pastedClip->mEnvelope);
      mEnvelope->RemoveUnneededPoints();
      OffsetCutLines(t0, pastedClip->GetEndTime() - pastedClip->GetStartTime());

      // Paste cut lines contained in pasted clip
      for (WaveClipList::compatibility_iterator it = pastedClip->mCutLines.GetFirst(); it; it=it->GetNext())
      {
         WaveClip* cutline = it->GetData();
         WaveClip* newCutLine = new WaveClip(*cutline,
                                             mSequence->GetDirManager());
         newCutLine->Offset(t0 - mOffset);
         mCutLines.Append(newCutLine);
      }

      result = true;
   }

   if (clipNeedsResampling)
   {
      // Clip was constructed as a copy, so delete it
      delete pastedClip;
   }

   return result;
}

bool WaveClip::InsertSilence(double t, double len)
{
   sampleCount s0;
   TimeToSamplesClip(t, &s0);
   sampleCount slen = (sampleCount)floor(len * mRate + 0.5);

   if (!GetSequence()->InsertSilence(s0, slen))
   {
      wxASSERT(false);
      return false;
   }
   OffsetCutLines(t, len);
   GetEnvelope()->InsertSpace(t, len);
   MarkChanged();

   return true;
}

bool WaveClip::Clear(double t0, double t1)
{
   sampleCount s0, s1;

   TimeToSamplesClip(t0, &s0);
   TimeToSamplesClip(t1, &s1);

   if (GetSequence()->Delete(s0, s1-s0))
   {
      // msmeyer
      //
      // Delete all cutlines that are within the given area, if any.
      //
      // Note that when cutlines are active, two functions are used:
      // Clear() and ClearAndAddCutLine(). ClearAndAddCutLine() is called
      // whenever the user directly calls a command that removes some audio, e.g.
      // "Cut" or "Clear" from the menu. This command takes care about recursive
      // preserving of cutlines within clips. Clear() is called when internal
      // operations want to remove audio. In the latter case, it is the right
      // thing to just remove all cutlines within the area.
      //
      double clip_t0 = t0;
      double clip_t1 = t1;
      if (clip_t0 < GetStartTime())
         clip_t0 = GetStartTime();
      if (clip_t1 > GetEndTime())
         clip_t1 = GetEndTime();

      WaveClipList::compatibility_iterator nextIt;

      for (WaveClipList::compatibility_iterator it = mCutLines.GetFirst(); it; it=nextIt)
      {
         nextIt = it->GetNext();
         WaveClip* clip = it->GetData();
         double cutlinePosition = mOffset + clip->GetOffset();
         if (cutlinePosition >= t0 && cutlinePosition <= t1)
         {
            // This cutline is within the area, delete it
            delete clip;
            mCutLines.DeleteNode(it);
         } else
         if (cutlinePosition >= t1)
         {
            clip->Offset(clip_t0-clip_t1);
         }
      }

      // Collapse envelope
      GetEnvelope()->CollapseRegion(t0, t1);
      if (t0 < GetStartTime())
         Offset(-(GetStartTime() - t0));

      MarkChanged();
      return true;
   }

   return false;
}

bool WaveClip::ClearAndAddCutLine(double t0, double t1)
{
   if (t0 > GetEndTime() || t1 < GetStartTime())
      return true; // time out of bounds

   WaveClip *newClip = new WaveClip(mSequence->GetDirManager(),
                                    mSequence->GetSampleFormat(),
                                    mRate);
   double clip_t0 = t0;
   double clip_t1 = t1;
   if (clip_t0 < GetStartTime())
      clip_t0 = GetStartTime();
   if (clip_t1 > GetEndTime())
      clip_t1 = GetEndTime();

   if (!newClip->CreateFromCopy(clip_t0, clip_t1, this))
      return false;
   newClip->SetOffset(clip_t0-mOffset);

   // Sort out cutlines that belong to the new cutline
   WaveClipList::compatibility_iterator nextIt;

   for (WaveClipList::compatibility_iterator it = mCutLines.GetFirst(); it; it=nextIt)
   {
      nextIt = it->GetNext();
      WaveClip* clip = it->GetData();
      double cutlinePosition = mOffset + clip->GetOffset();
      if (cutlinePosition >= t0 && cutlinePosition <= t1)
      {
         clip->SetOffset(cutlinePosition - newClip->GetOffset() - mOffset);
         newClip->mCutLines.Append(clip);
         mCutLines.DeleteNode(it);
      } else
      if (cutlinePosition >= t1)
      {
         clip->Offset(clip_t0-clip_t1);
      }
   }

   // Clear actual audio data
   sampleCount s0, s1;

   TimeToSamplesClip(t0, &s0);
   TimeToSamplesClip(t1, &s1);

   if (GetSequence()->Delete(s0, s1-s0))
   {
      // Collapse envelope
      GetEnvelope()->CollapseRegion(t0, t1);
      if (t0 < GetStartTime())
         Offset(-(GetStartTime() - t0));

      MarkChanged();

      mCutLines.Append(newClip);
      return true;
   } else
   {
      delete newClip;
      return false;
   }
}

bool WaveClip::FindCutLine(double cutLinePosition,
                           double* cutlineStart /* = NULL */,
                           double* cutlineEnd /* = NULL */)
{
   for (WaveClipList::compatibility_iterator it = mCutLines.GetFirst(); it; it=it->GetNext())
   {
      WaveClip* cutline = it->GetData();
      if (fabs(mOffset + cutline->GetOffset() - cutLinePosition) < 0.0001)
      {
         if (cutlineStart)
            *cutlineStart = mOffset+cutline->GetStartTime();
         if (cutlineEnd)
            *cutlineEnd = mOffset+cutline->GetEndTime();
         return true;
      }
   }

   return false;
}

bool WaveClip::ExpandCutLine(double cutLinePosition)
{
   for (WaveClipList::compatibility_iterator it = mCutLines.GetFirst(); it; it=it->GetNext())
   {
      WaveClip* cutline = it->GetData();
      if (fabs(mOffset + cutline->GetOffset() - cutLinePosition) < 0.0001)
      {
         if (!Paste(mOffset+cutline->GetOffset(), cutline))
            return false;
         delete cutline;
         mCutLines.DeleteNode(it);
         return true;
      }
   }

   return false;
}

bool WaveClip::RemoveCutLine(double cutLinePosition)
{
   for (WaveClipList::compatibility_iterator it = mCutLines.GetFirst(); it; it=it->GetNext())
   {
      if (fabs(mOffset + it->GetData()->GetOffset() - cutLinePosition) < 0.0001)
      {
         delete it->GetData();
         mCutLines.DeleteNode(it);
         return true;
      }
   }

   return false;
}

void WaveClip::RemoveAllCutLines()
{
   while (!mCutLines.IsEmpty())
   {
      WaveClipList::compatibility_iterator head = mCutLines.GetFirst();
      delete head->GetData();
      mCutLines.DeleteNode(head);
   }
}

void WaveClip::OffsetCutLines(double t0, double len)
{
   for (WaveClipList::compatibility_iterator it = mCutLines.GetFirst(); it; it=it->GetNext())
   {
      WaveClip* cutLine = it->GetData();
      if (mOffset + cutLine->GetOffset() >= t0)
         cutLine->Offset(len);
   }
}

void WaveClip::Lock()
{
   GetSequence()->Lock();
   for (WaveClipList::compatibility_iterator it = mCutLines.GetFirst(); it; it=it->GetNext())
      it->GetData()->Lock();
}

void WaveClip::CloseLock()
{
   GetSequence()->CloseLock();
   for (WaveClipList::compatibility_iterator it = mCutLines.GetFirst(); it; it=it->GetNext())
      it->GetData()->Lock();
}

void WaveClip::Unlock()
{
   GetSequence()->Unlock();
   for (WaveClipList::compatibility_iterator it = mCutLines.GetFirst(); it; it=it->GetNext())
      it->GetData()->Unlock();
}

void WaveClip::SetRate(int rate)
{
   mRate = rate;
   UpdateEnvelopeTrackLen();
   MarkChanged();
}

bool WaveClip::Resample(int rate, ProgressDialog *progress)
{
   if (rate == mRate)
      return true; // Nothing to do

   double factor = (double)rate / (double)mRate;
   ::Resample* resample = new ::Resample(true, factor, factor); // constant rate resampling

   int bufsize = 65536;
   float* inBuffer = new float[bufsize];
   float* outBuffer = new float[bufsize];
   sampleCount pos = 0;
   bool error = false;
   int outGenerated = 0;
   sampleCount numSamples = mSequence->GetNumSamples();

   Sequence* newSequence =
      new Sequence(mSequence->GetDirManager(), mSequence->GetSampleFormat());

   /**
    * We want to keep going as long as we have something to feed the resampler
    * with OR as long as the resampler spews out samples (which could continue
    * for a few iterations after we stop feeding it)
    */
   while (pos < numSamples || outGenerated > 0)
   {
      int inLen = numSamples - pos;
      if (inLen > bufsize)
         inLen = bufsize;

      bool isLast = ((pos + inLen) == numSamples);

      if (!mSequence->Get((samplePtr)inBuffer, floatSample, pos, inLen))
      {
         error = true;
         break;
      }

      int inBufferUsed = 0;
      outGenerated = resample->Process(factor, inBuffer, inLen, isLast,
                                           &inBufferUsed, outBuffer, bufsize);

      pos += inBufferUsed;

      if (outGenerated < 0)
      {
         error = true;
         break;
      }

      if (!newSequence->Append((samplePtr)outBuffer, floatSample,
                               outGenerated))
      {
         error = true;
         break;
      }

      if (progress)
      {
         int updateResult = progress->Update(pos, numSamples);
         error = (updateResult != eProgressSuccess);
         if (error)
         {
            break;
         }
      }
   }

   delete[] inBuffer;
   delete[] outBuffer;
   delete resample;

   if (error)
   {
      delete newSequence;
   } else
   {
      delete mSequence;
      mSequence = newSequence;
      mRate = rate;

      // Invalidate wave display cache
      if (mWaveCache)
      {
         delete mWaveCache;
         mWaveCache = NULL;
      }
      mWaveCache = new WaveCache(1);
      // Invalidate the spectrum display cache
      if (mSpecCache)
         delete mSpecCache;
      mSpecCache = new SpecCache(1, 1, false);
   }

   return !error;
}
