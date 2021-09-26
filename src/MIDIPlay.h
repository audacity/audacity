/*!********************************************************************

Audacity: A Digital Audio Editor

@file MIDIPlay.h
@brief Inject added MIDI playback capability into Audacity's audio engine
 
Paul Licameli split from AudIOBase.h

**********************************************************************/

#ifndef __AUDACITY_MIDI_PLAY__
#define __AUDACITY_MIDI_PLAY__

#include "AudioIOExt.h"
#include "NoteTrack.h"
#include <optional>
#include "../lib-src/header-substitutes/allegro.h"

typedef void PmStream;
typedef int32_t PmTimestamp;
class Alg_event;
class Alg_iterator;
using NoteTrackConstArray = std::vector < std::shared_ptr< const NoteTrack > >;

class AudioThread;

// This workaround makes pause and stop work when output is to GarageBand,
// which seems not to implement the notes-off message correctly.
#define AUDIO_IO_GB_MIDI_WORKAROUND

#include "PlaybackSchedule.h"

namespace {

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

   std::optional<Alg_iterator> mIterator;
   /// The next event to play (or null)
   Alg_event    *mNextEvent = nullptr;

#ifdef AUDIO_IO_GB_MIDI_WORKAROUND
   std::vector< std::pair< int, int > > mPendingNotesOff;
#endif

   /// Real time at which the next event should be output, measured in seconds.
   /// Note that this could be a note's time+duration for note offs.
   double           mNextEventTime = 0.0;
   /// Track of next event
   NoteTrack        *mNextEventTrack = nullptr;
   /// Is the next event a note-on?
   bool             mNextIsNoteOn = false;
   /// when true, mSendMidiState means send only updates, not note-on's,
   /// used to send state changes that precede the selected notes
   bool             mSendMidiState = false;

   void PrepareMidiIterator(bool send, double offset);
   bool StartPortMidiStream(double rate);

   // Compute nondecreasing real time stamps, accounting for pauses, but not the
   // synth latency.
   double UncorrectedMidiEventTime(double pauseTime);

   void OutputEvent(double pauseTime);
   void GetNextEvent();
   double PauseTime(double rate, unsigned long pauseFrames);
   void AllNotesOff(bool looping = false);

   /** \brief Compute the current PortMidi timestamp time.
    *
    * This is used by PortMidi to synchronize midi time to audio samples
    */
   PmTimestamp MidiTime();

   // Note: audio code solves the problem of soloing/muting tracks by scanning
   // all playback tracks on every call to the audio buffer fill routine.
   // We do the same for Midi, but it seems wasteful for at least two
   // threads to be frequently polling to update status. This could be
   // eliminated (also with a reduction in code I think) by updating mHasSolo
   // each time a solo button is activated or deactivated. For now, I'm
   // going to do this polling in the FillMidiBuffer routine to localize
   // changes for midi to the midi code, but I'm declaring the variable
   // here so possibly in the future, Audio code can use it too. -RBD
 private:
   bool  mHasSolo = false; // is any playback solo button pressed?
 public:
   bool SetHasSolo(bool hasSolo);
   bool GetHasSolo() { return mHasSolo; }

   bool mUsingAlsa = false;

   static bool IsActive();
   bool IsOtherStreamActive() const override;

   void ComputeOtherTimings(double rate,
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
