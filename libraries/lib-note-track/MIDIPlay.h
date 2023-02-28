/*!********************************************************************

Audacity: A Digital Audio Editor

@file MIDIPlay.h
@brief Inject added MIDI playback capability into Audacity's audio engine
 
Paul Licameli split from AudIOBase.h

**********************************************************************/

#ifndef __AUDACITY_MIDI_PLAY__
#define __AUDACITY_MIDI_PLAY__

#include "AudioIOExt.h"
#include <optional>
#include "../../lib-src/header-substitutes/allegro.h"

typedef void PmStream;
typedef int32_t PmTimestamp;
class Alg_event;
class Alg_iterator;
class NoteTrack;
using NoteTrackConstArray = std::vector < std::shared_ptr< const NoteTrack > >;

// This workaround makes pause and stop work when output is to GarageBand,
// which seems not to implement the notes-off message correctly.
#define AUDIO_IO_GB_MIDI_WORKAROUND

#include "NoteTrack.h"
#include "PlaybackSchedule.h"

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

   const PlaybackSchedule &mPlaybackSchedule;
   MIDIPlay &mMIDIPlay;
   Alg_iterator it{ nullptr, false };
   /// The next event to play (or null)
   Alg_event    *mNextEvent = nullptr;

   /// Track of next event
   NoteTrack        *mNextEventTrack = nullptr;

   /// Is the next event a note-on?
   bool             mNextIsNoteOn = false;

private:
   /// Real time at which the next event should be output, measured in seconds.
   /// Note that this could be a note's time+duration for note offs.
   double           mNextEventTime = 0;
};

struct MIDIPlay : AudioIOExt
{
   explicit MIDIPlay(const PlaybackSchedule &schedule);
   ~MIDIPlay() override;

   double AudioTime(double rate) const
   { return mPlaybackSchedule.mT0 + mNumFrames / rate; }

   const PlaybackSchedule &mPlaybackSchedule;
   NoteTrackConstArray mMidiPlaybackTracks;

   /// True when output reaches mT1
   static bool      mMidiOutputComplete;

   /// mMidiStreamActive tells when mMidiStream is open for output
   static bool      mMidiStreamActive;

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

   void PrepareMidiIterator(bool send, double startTime, double offset);
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

   AudioIODiagnostics Dump() const override;
};

}

#endif
