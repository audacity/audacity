/**********************************************************************

  Audacity: A Digital Audio Editor

  Mix.h

  Dominic Mazzoni
  Markus Meyer

**********************************************************************/

#ifndef __AUDACITY_MIX__
#define __AUDACITY_MIX__

#include "MemoryX.h"
#include <wx/string.h>
#include "SampleFormat.h"

class Resample;
class DirManager;
class TimeTrack;
class TrackFactory;
class TrackList;
class WaveTrack;
class WaveTrackConstArray;
class WaveTrackCache;

/** @brief Mixes together all input tracks, applying any envelopes, amplitude
 * gain, panning, and real-time effects in the process.
 *
 * Takes one or more tracks as input; of all the WaveTrack s that are selected,
 * it mixes them together, applying any envelopes, amplitude gain, panning, and
 * real-time effects in the process.  The resulting pair of tracks (stereo) are
 * "rendered" and have no effects, gain, panning, or envelopes. Other sorts of
 * tracks are ignored.
 * If the start and end times passed are the same this is taken as meaning
 * no explicit time range to process, and the whole occupied length of the
 * input tracks is processed.
 */
void MixAndRender(TrackList * tracks, TrackFactory *factory,
                  double rate, sampleFormat format,
                  double startTime, double endTime,
                  std::unique_ptr<WaveTrack> &uLeft, std::unique_ptr<WaveTrack> &uRight);

void MixBuffers(unsigned numChannels, int *channelFlags, float *gains,
                samplePtr src,
                samplePtr *dests, int len, bool interleaved);

class AUDACITY_DLL_API MixerSpec
{
   unsigned mNumTracks, mNumChannels, mMaxNumChannels;

   void Alloc();
   void Free();

   public:
   bool **mMap;

   MixerSpec( unsigned numTracks, unsigned maxNumChannels );
   MixerSpec( const MixerSpec &mixerSpec );
   virtual ~MixerSpec();

   bool SetNumChannels( unsigned numChannels );
   unsigned GetNumChannels() { return mNumChannels; }

   unsigned GetMaxNumChannels() { return mMaxNumChannels; }
   unsigned GetNumTracks() { return mNumTracks; }

   MixerSpec& operator=( const MixerSpec &mixerSpec );
};

class AUDACITY_DLL_API Mixer {
 public:

    // An argument to Mixer's constructor
    class WarpOptions
    {
    public:
       explicit WarpOptions(const TimeTrack *t)
          : timeTrack(t), minSpeed(0.0), maxSpeed(0.0)
       {}

       WarpOptions(double min, double max);

    private:
       friend class Mixer;
       const TimeTrack *timeTrack;
       double minSpeed, maxSpeed;
    };

    //
   // Constructor / Destructor
   //

   Mixer(const WaveTrackConstArray &inputTracks,
         const WarpOptions &warpOptions,
         double startTime, double stopTime,
         unsigned numOutChannels, size_t outBufferSize, bool outInterleaved,
         double outRate, sampleFormat outFormat,
         bool highQuality = true, MixerSpec *mixerSpec = NULL);

   virtual ~ Mixer();

   //
   // Setup
   //

   void ApplyTrackGains(bool apply = true); // True by default

   //
   // Processing
   //

   /// Process a maximum of 'maxSamples' samples and put them into
   /// a buffer which can be retrieved by calling GetBuffer().
   /// Returns number of output samples, or 0, if there are no
   /// more samples that must be processed.
   size_t Process(size_t maxSamples);

   /// Restart processing at beginning of buffer next time
   /// Process() is called.
   void Restart();

   /// Reposition processing to absolute time next time
   /// Process() is called.
   void Reposition(double t);

   // Used in scrubbing.
   void SetTimesAndSpeed(double t0, double t1, double speed);

   /// Current time in seconds (unwarped, i.e. always between startTime and stopTime)
   /// This value is not accurate, it's useful for progress bars and indicators, but nothing else.
   double MixGetCurrentTime();

   /// Retrieve the main buffer or the interleaved buffer
   samplePtr GetBuffer();

   /// Retrieve one of the non-interleaved buffers
   samplePtr GetBuffer(int channel);

 private:

   void Clear();
   size_t MixSameRate(int *channelFlags, WaveTrackCache &cache,
                           sampleCount *pos);

   size_t MixVariableRates(int *channelFlags, WaveTrackCache &cache,
                                sampleCount *pos, float *queue,
                                int *queueStart, int *queueLen,
                                Resample * pResample);

 private:
   // Input
   int              mNumInputTracks;
   WaveTrackCache  *mInputTrack;

   bool             mbVariableRates;
   const TimeTrack *mTimeTrack;
   sampleCount     *mSamplePos;
   bool             mApplyTrackGains;
   float           *mGains;
   double          *mEnvValues;
   double           mT0; // Start time
   double           mT1; // Stop time (none if mT0==mT1)
   double           mTime;  // Current time (renamed from mT to mTime for consistency with AudioIO - mT represented warped time there)
   Resample       **mResample;
   float          **mSampleQueue;
   int             *mQueueStart;
   int             *mQueueLen;
   size_t           mQueueMaxLen;
   size_t           mProcessLen;
   MixerSpec        *mMixerSpec;

   // Output
   size_t              mMaxOut;
   unsigned         mNumChannels;
   unsigned         mNumBuffers;
   size_t              mBufferSize;
   size_t              mInterleavedBufferSize;
   sampleFormat     mFormat;
   bool             mInterleaved;
   SampleBuffer    *mBuffer;
   SampleBuffer    *mTemp;
   float           *mFloatBuffer;
   double           mRate;
   double           mSpeed;
   bool             mHighQuality;
};

#endif

