/**********************************************************************

  Audacity: A Digital Audio Editor

  Mix.h

  Dominic Mazzoni
  Markus Meyer

**********************************************************************/

#ifndef __AUDACITY_MIX__
#define __AUDACITY_MIX__

#include <wx/string.h>

#include "SampleFormat.h"
#include "WaveTrack.h"
#include "TimeTrack.h"
#include "Resample.h"

class DirManager;

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
bool MixAndRender(TrackList * tracks, TrackFactory *factory,
                  double rate, sampleFormat format,
                  double startTime, double endTime,
                  WaveTrack **newLeft, WaveTrack **newRight);

void MixBuffers(int numChannels, int *channelFlags, float *gains,
                samplePtr src,
                samplePtr *dests, int len, bool interleaved);

class AUDACITY_DLL_API MixerSpec
{
   int mNumTracks, mNumChannels, mMaxNumChannels;

   void Alloc();
   void Free();

   public:
   bool **mMap;

   MixerSpec( int numTracks, int maxNumChannels );
   MixerSpec( const MixerSpec &mixerSpec );
   virtual ~MixerSpec();

   bool SetNumChannels( int numChannels );
   int GetNumChannels() { return mNumChannels; }

   int GetMaxNumChannels() { return mMaxNumChannels; }
   int GetNumTracks() { return mNumTracks; }

   MixerSpec& operator=( const MixerSpec &mixerSpec );
};

class AUDACITY_DLL_API Mixer {
 public:
   //
   // Constructor / Destructor
   //

   Mixer(int numInputTracks, WaveTrack **inputTracks,
         TimeTrack *timeTrack,
         double startTime, double stopTime,
         int numOutChannels, int outBufferSize, bool outInterleaved,
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
   sampleCount Process(sampleCount maxSamples);

   /// Restart processing at beginning of buffer next time
   /// Process() is called.
   void Restart();
   void Restart( double NewStart ){ mT0 = NewStart; Restart();}

   /// Reposition processing to absolute time next time
   /// Process() is called.
   void Reposition(double t);

   /// Current time in seconds (unwarped, i.e. always between startTime and stopTime)
   /// This value is not accurate, it's useful for progress bars and indicators, but nothing else.
   double MixGetCurrentTime();

   /// Retrieve the main buffer or the interleaved buffer
   samplePtr GetBuffer();

   /// Retrieve one of the non-interleaved buffers
   samplePtr GetBuffer(int channel);

 private:

   void Clear();
   sampleCount MixSameRate(int *channelFlags, WaveTrack *src,
                           sampleCount *pos);

   sampleCount MixVariableRates(int *channelFlags, WaveTrack *track,
                                sampleCount *pos, float *queue,
                                int *queueStart, int *queueLen,
                                Resample * pResample);

 private:
   // Input
   int              mNumInputTracks;
   WaveTrack      **mInputTrack;
   TimeTrack       *mTimeTrack;
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
   int              mQueueMaxLen;
   int              mProcessLen;
   MixerSpec        *mMixerSpec;

   // Output
   int              mMaxOut;
   int              mNumChannels;
   int              mNumBuffers;
   int              mBufferSize;
   int              mInterleavedBufferSize;
   sampleFormat     mFormat;
   bool             mInterleaved;
   samplePtr       *mBuffer;
   samplePtr       *mTemp;
   float           *mFloatBuffer;
   double           mRate;
   bool             mHighQuality;
};

#endif

