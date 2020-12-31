/*!********************************************************************

Audacity: A Digital Audio Editor

@file MIDIPlay.h
@brief Inject added MIDI playback capability into Audacity's audio engine
 
Paul Licameli split from AudIOBase.h

**********************************************************************/

#ifndef __AUDACITY_MIDI_PLAY__
#define __AUDACITY_MIDI_PLAY__

#include "AudioIOExt.h"
#include <condition_variable>
#include <optional>
#include "../lib-src/header-substitutes/allegro.h"

typedef void PmStream;
typedef int32_t PmTimestamp;
class Alg_event;
class Alg_iterator;
class NoteTrack;
using NoteTrackConstArray = std::vector < std::shared_ptr< const NoteTrack > >;

class AudioThread;

// This workaround makes pause and stop work when output is to GarageBand,
// which seems not to implement the notes-off message correctly.
#define AUDIO_IO_GB_MIDI_WORKAROUND

#include "NoteTrack.h"
#include "PlaybackSchedule.h"
#include <forward_list>

namespace {

struct MIDIPlay;

Alg_update gAllNotesOff; // special event for loop ending
// the fields of this event are never used, only the address is important

struct Iterator {
   Iterator(
      const PlaybackSchedule &schedule, MIDIPlay &midiPlay,
      NoteTrackConstArray &midiPlaybackTracks,
      double startTime, double offset, bool send );
   ~Iterator();

   void Prime(bool send, double startTime);

   double GetNextEventTime() const;

   // Compute nondecreasing real time stamps, accounting for pauses, but not the
   // synth latency.
   double UncorrectedMidiEventTime(double pauseTime);

   bool Unmuted(bool hasSolo) const;

   // Returns true after outputting all-notes-off
   bool OutputEvent(double pauseTime,
      /// when true, sendMidiState means send only updates, not note-ons,
      /// used to send state changes that precede the selected notes
      bool sendMidiState,
      bool hasSolo);
   void GetNextEvent();

   // These may update future ending behavior of an iterator that is being used
   // in another thread, so they use atomics to do that properly.
   void SetNotesOffTime(double notesOffTime)
   { mNotesOffTime.store(notesOffTime, std::memory_order_relaxed); }
   void SetSkipping()
   { mSkipping.store(true, std::memory_order_relaxed); }

   // And, the corresponding accessors.
   double GetNotesOffTime() const
   { return mNotesOffTime.load(std::memory_order_relaxed); }
   bool GetSkipping() const
   { return mSkipping.load(std::memory_order_relaxed); }

   const PlaybackSchedule &mPlaybackSchedule;
   MIDIPlay &mMIDIPlay;
   Alg_iterator it{ nullptr, false };
   /// The next event to play (or null)
   Alg_event    *mNextEvent = nullptr;

   /// Track of next event
   NoteTrack        *mNextEventTrack = nullptr;

   /// Is the next event a note-on?
   bool             mNextIsNoteOn = false;

   std::atomic<double> mNotesOffTime{ std::numeric_limits<double>::infinity() };
   std::atomic<bool> mSkipping{ false };

private:
   /// Real time at which the next event should be output, measured in seconds.
   /// Note that this could be a note's time+duration for note offs.
   double           mNextEventTime = 0;
};

struct MIDIPlay : AudioIOExt, NonInterferingBase
{
   explicit MIDIPlay(const PlaybackSchedule &schedule);
   ~MIDIPlay() override;

   void ForceShutdown(unsigned bit);
   bool Shutdown(unsigned bit);

   void Producer(std::pair<double, double> newTrackTimes,
      size_t nFrames) override;

   void Consumer(size_t nSamples, double rate, unsigned long pauseFrames,
      bool hasSolo) override;

   void Prime(double newTrackTime) override;

   double AudioTime(double rate) const
   { return mPlaybackSchedule.mT0 + mNumFrames / rate; }

   const PlaybackSchedule &mPlaybackSchedule;
   NoteTrackConstArray mMidiPlaybackTracks;

   /// mMidiStreamActive tells when mMidiStream is open for output and not in process of shutdown
   static std::atomic<bool> mMidiStreamActive;

   /// Zero when the queue of events is inactive
   static std::atomic<unsigned> mMidiOutputInProgress;

   // Use a cv when changing the above
   std::condition_variable mMidiOutputCompleteCV;
   std::mutex mMidiOutputCompleteMutex;

   PmStream        *mMidiStream = nullptr;
   int              mLastPmError = 0;

   /// Latency of MIDI synthesizer
   long             mSynthLatency = MIDISynthLatency_ms.GetDefault();

   // These fields are used to synchronize MIDI with audio:

   /// Number of frames output, including pauses
   long    mNumFrames = 0;
   /// total of backward jumps
   int     mMidiLoopPasses = 0;
   //
   inline double MidiLoopOffset() {
      return mMidiLoopPasses * (mPlaybackSchedule.mT1 - mPlaybackSchedule.mT0);
   }

   long    mAudioFramesPerBuffer = 0;
   /// Used by Midi process to record that pause has begun,
   /// so that AllNotesOff() is only delivered once
   bool    mMidiPaused = false;
   /// The largest timestamp written so far, used to delay
   /// stream closing until last message has been delivered
   PmTimestamp mMaxMidiTimestamp = 0;

   /// Offset from ideal sample computation time to system time,
   /// where "ideal" means when we would get the callback if there
   /// were no scheduling delays or computation time
   double mSystemMinusAudioTime = 0.0;
   /// audio output latency reported by PortAudio
   /// (initially; for Alsa, we adjust it to the largest "observed" value)
   double mAudioOutLatency = 0.0;

   // Next two are used to adjust the previous two, if
   // PortAudio does not provide the info (using ALSA):

   /// time of first callback
   /// used to find "observed" latency
   double mStartTime = 0.0;
   /// number of callbacks since stream start
   long mCallbackCount = 0;

   double mSystemMinusAudioTimePlusLatency = 0.0;

   std::optional<Iterator> mIterator;

#ifdef AUDIO_IO_GB_MIDI_WORKAROUND
   std::vector< std::pair< int, int > > mPendingNotesOff;
#endif

   //! @name Inter-thread queue
   //! @{

   struct Entry {
      size_t frames{ 0 };
   };

   //! Alignment size to avoid false sharing
   static constexpr size_t alignment = 64;
      //std::hardware_destructive_interference_size; // C++17

   //! Counter type for entries to-do, done, and cleaned up
   /*! If the counter type wraps during very long play, it won't matter, so long
    as the number of entries in transit in the queue never exceeds the
    largest value of the type. */
   using CounterType = size_t;

   //! Use a list container for its guarantees of non-invalidation of iterators
   using Queue = std::forward_list<Entry>;
   // The Queue object itself is changed only by main or producer thread
   Queue mEntries;

   //! Shared counter, updated by producer
   alignas(alignment) std::atomic<CounterType> mEntriesProduced{0};
   //! Shared counter, updated by consumer
   std::atomic<CounterType> mEntriesConsumed{0};
   //! @}

   //! @name For Producer's use only
   //! @{
   alignas(alignment) Queue::iterator mToProduce;
   CounterType mEntriesDestroyed{0};
   void ProduceCompleteEntry(Entry &entry, size_t frames);
   void PrepareMidiIterator(bool send, double startTime, double offset);
   //! @}

   //! @name For Consumer's use only
   //! @{
   alignas(alignment) Queue::const_iterator mToConsume;
   size_t mNextFrame = 0;
   size_t ConsumePartOfEntry(const Entry &entry);
   //! @}

   bool StartPortMidiStream(double rate);
   double PauseTime(double rate, unsigned long pauseFrames);
   void AllNotesOff(bool looping = false);

   /** \brief Compute the current PortMidi timestamp time.
    *
    * This is used by PortMidi to synchronize midi time to audio samples
    */
   PmTimestamp MidiTime();

   bool mUsingAlsa = false;

   static bool IsActive();
   bool IsOtherStreamActive() const override;

   void ComputeOtherTimings(double rate, bool paused,
      const PaStreamCallbackTimeInfo *timeInfo,
      unsigned long framesPerBuffer) override;
   void SignalOtherCompletion() override;
   unsigned CountOtherSoloTracks() const override;

   bool StartOtherStream(const TransportTracks &tracks,
      const PaStreamInfo* info, double startTime, double rate) override;
   void AbortOtherStream() override;
   void FillOtherBuffers(double rate, unsigned long pauseFrames,
      bool paused, bool hasSolo) override;
   void StopOtherStream() override;
   void DestroyOtherStream() override;

   AudioIODiagnostics Dump() const override;

   void ClearQueue();
};

}

#endif
