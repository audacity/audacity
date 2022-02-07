/**********************************************************************

Audacity: A Digital Audio Editor

@file MIDIPlay.cpp
@brief Inject added MIDI playback capability into Audacity's audio engine

Paul Licameli split from AudioIO.cpp and AudioIOBase.cpp

*//*****************************************************************//**

\class MIDIPlay
\brief Callbacks that AudioIO uses, to synchronize audio and MIDI playback

  \par EXPERIMENTAL_MIDI_OUT
  If EXPERIMENTAL_MIDI_OUT is defined, this class manages
  MIDI playback. It is decoupled from AudioIO by the abstract interface
  AudioIOExt. Some of its methods execute on the main thread and some on the
  low-latency PortAudio thread.

  \par MIDI With Audio
  When Audio and MIDI play simultaneously, MIDI synchronizes to Audio.
  This is necessary because the Audio sample clock is not the same
  hardware as the system time used to schedule MIDI messages. MIDI
  is synchronized to Audio because it is simple to pause or rush
  the dispatch of MIDI messages, but generally impossible to pause
  or rush synchronous audio samples (without distortion).

  \par
  MIDI output is driven by the low latency thread (PortAudio's callback)
  that also sends samples to the output device.  The relatively low
  latency to the output device allows Audacity to stop audio output
  quickly. We want the same behavior for MIDI, but there is not
  periodic callback from PortMidi (because MIDI is asynchronous).

  \par
  When Audio is running, MIDI is synchronized to Audio. Globals are set
  in the Audio callback (audacityAudioCallback) for use by a time
  function that reports milliseconds to PortMidi. (Details below.)

  \par MIDI Without Audio
  When Audio is not running, PortMidi uses its own millisecond timer
  since there is no audio to synchronize to. (Details below.)

  \par Implementation Notes and Details for MIDI
  When opening devices, successAudio and successMidi indicate errors
  if false, so normally both are true. Use playbackChannels,
  captureChannels and mMidiPlaybackTracks.empty() to determine if
  Audio or MIDI is actually in use.

  \par Audio Time
  Normally, the current time during playback is given by the variable
  mTime. mTime normally advances by frames / samplerate each time an
  audio buffer is output by the audio callback. However, Audacity has
  a speed control that can perform continuously variable time stretching
  on audio. This is achieved in two places: the playback "mixer" that
  generates the samples for output processes the audio according to
  the speed control. In a separate algorithm, the audio callback updates
  mTime by (frames / samplerate) * factor, where factor reflects the
  speed at mTime. This effectively integrates speed to get position.
  Negative speeds are allowed too, for instance in scrubbing.

  \par The Big Picture
@verbatim

Sample
Time (in seconds, = total_sample_count / sample_rate)
  ^
  |                                             /         /
  |             y=x-mSystemTimeMinusAudioTime /         /
  |                                         /     #   /
  |                                       /         /
  |                                     /   # <- callbacks (#) showing
  |                                   /#        /   lots of timing jitter.
  |       top line is "full buffer" /         /     Some are later,
  |                     condition /         /       indicating buffer is
  |                             /         /         getting low. Plot
  |                           /     #   /           shows sample time
  |                         /    #    /             (based on how many
  |                       /    #    /               samples previously
  |                     /         /                 *written*) vs. real
  |                   / #       /                   time.
  |                 /<------->/ audio latency
  |               /#       v/
  |             /         / bottom line is "empty buffer"
  |           /   #     /      condition = DAC output time =
  |         /         /
  |       /      # <-- rapid callbacks as buffer is filled
  |     /         /
0 +...+---------#---------------------------------------------------->
  0 ^ |         |                                            real time
    | |         first callback time
    | mSystemMinusAudioTime
    |
    Probably the actual real times shown in this graph are very large
    in practice (> 350,000 sec.), so the X "origin" might be when
    the computer was booted or 1970 or something.


@endverbatim

  To estimate the true DAC time (needed to synchronize MIDI), we need
  a mapping from track time to DAC time. The estimate is the theoretical
  time of the full buffer (top diagonal line) + audio latency. To
  estimate the top diagonal line, we "draw" the line to be at least
  as high as any sample time corresponding to a callback (#), and we
  slowly lower the line in case the sample clock is slow or the system
  clock is fast, preventing the estimated line from drifting too far
  from the actual callback observations. The line is occasionally
  "bumped" up by new callback observations, but continuously
  "lowered" at a very low rate.  All adjustment is accomplished
  by changing mSystemMinusAudioTime, shown here as the X-intercept.\n
    theoreticalFullBufferTime = realTime - mSystemMinusAudioTime\n
  To estimate audio latency, notice that the first callback happens on
  an empty buffer, but the buffer soon fills up. This will cause a rapid
  re-estimation of mSystemMinusAudioTime. (The first estimate of
  mSystemMinusAudioTime will simply be the real time of the first
  callback time.) By watching these changes, which happen within ms of
  starting, we can estimate the buffer size and thus audio latency.
  So, to map from track time to real time, we compute:\n
    DACoutputTime = trackTime + mSystemMinusAudioTime\n
  There are some additional details to avoid counting samples while
  paused or while waiting for initialization, MIDI latency, etc.
  Also, in the code, track time is measured with respect to the track
  origin, so there's an extra term to add (mT0) if you start somewhere
  in the middle of the track.
  Finally, when a callback occurs, you might expect there is room in
  the output buffer for the requested frames, so maybe the "full buffer"
  sample time should be based not on the first sample of the callback, but
  the last sample time + 1 sample. I suspect, at least on Linux, that the
  callback occurs as soon as the last callback completes, so the buffer is
  really full, and the callback thread is going to block waiting for space
  in the output buffer.

  \par Midi Time
  MIDI is not warped according to the speed control. This might be
  something that should be changed. (Editorial note: Wouldn't it
  make more sense to display audio at the correct time and allow
  users to stretch audio the way they can stretch MIDI?) For now,
  MIDI plays at 1 second per second, so it requires an unwarped clock.
  In fact, MIDI time synchronization requires a millisecond clock that
  does not pause. Note that mTime will stop progress when the Pause
  button is pressed, even though audio samples (zeros) continue to
  be output.

  \par
  Therefore, we define the following interface for MIDI timing:
  \li \c AudioTime() is the time based on all samples written so far, including zeros output during pauses. AudioTime() is based on the start location mT0, not zero.
  \li \c PauseTime() is the amount of time spent paused, based on a count of zero-padding samples output.
  \li \c MidiTime() is an estimate in milliseconds of the current audio output time + 1s. In other words, what audacity track time corresponds to the audio (plus pause insertions) at the DAC output?

  \par AudioTime() and PauseTime() computation
  AudioTime() is simply mT0 + mNumFrames / mRate.
  mNumFrames is incremented in each audio callback. Similarly, PauseTime()
  is pauseFrames / rate. pauseFrames is also incremented in
  each audio callback when a pause is in effect or audio output is ready to start.

  \par MidiTime() computation
  MidiTime() is computed based on information from PortAudio's callback,
  which estimates the system time at which the current audio buffer will
  be output. Consider the (unimplemented) function RealToTrack() that
  maps real audio write time to track time. If writeTime is the system
  time for the first sample of the current output buffer, and
  if we are in the callback, so AudioTime() also refers to the first sample
  of the buffer, then \n
  RealToTrack(writeTime) = AudioTime() - PauseTime()\n
  We want to know RealToTrack of the current time (when we are not in the
  callback, so we use this approximation for small d: \n
  RealToTrack(t + d) = RealToTrack(t) + d, or \n
  Letting t = writeTime and d = (systemTime - writeTime), we can
  substitute to get:\n
  RealToTrack(systemTime)
     = RealToTrack(writeTime) + systemTime - writeTime\n
     = AudioTime() - PauseTime() + (systemTime - writeTime) \n
  MidiTime() should include pause time, so that it increases smoothly,
  and audioLatency so that MidiTime() corresponds to the time of audio
  output rather than audio write times.  Also MidiTime() is offset by 1
  second to avoid negative time at startup, so add 1: \n
  MidiTime(systemTime) in seconds\n
     = RealToTrack(systemTime) + PauseTime() - audioLatency + 1 \n
     = AudioTime() + (systemTime - writeTime) - audioLatency + 1 \n
  (Note that audioLatency is called mAudioOutLatency in the code.)
  When we schedule a MIDI event with track time TT, we need
  to map TT to a PortMidi timestamp. The PortMidi timestamp is exactly
  MidiTime(systemTime) in ms units, and \n
     MidiTime(x) = RealToTrack(x) + PauseTime() + 1, so \n
     timestamp = TT + PauseTime() + 1 - midiLatency \n
  Note 1: The timestamp is incremented by the PortMidi stream latency
  (midiLatency) so we subtract midiLatency here for the timestamp
  passed to PortMidi. \n
  Note 2: Here, we're setting x to the time at which RealToTrack(x) = TT,
  so then MidiTime(x) is the desired timestamp. To be completely
  correct, we should assume that MidiTime(x + d) = MidiTime(x) + d,
  and consider that we compute MidiTime(systemTime) based on the
  *current* system time, but we really want the MidiTime(x) for some
  future time corresponding when MidiTime(x) = TT.)

  \par
  Also, we should assume PortMidi was opened with mMidiLatency, and that
  MIDI messages become sound with a delay of mSynthLatency. Therefore,
  the final timestamp calculation is: \n
     timestamp = TT + PauseTime() + 1 - (mMidiLatency + mSynthLatency) \n
  (All units here are seconds; some conversion is needed in the code.)

  \par
  The difference AudioTime() - PauseTime() is the time "cursor" for
  MIDI. When the speed control is used, MIDI and Audio will become
  unsynchronized. In particular, MIDI will not be synchronized with
  the visual cursor, which moves with scaled time reported in mTime.

  \par Timing in Linux
  It seems we cannot get much info from Linux. We can read the time
  when we get a callback, and we get a variable frame count (it changes
  from one callback to the next). Returning to the RealToTrack()
  equations above: \n
  RealToTrack(outputTime) = AudioTime() - PauseTime() - bufferDuration \n
  where outputTime should be PortAudio's estimate for the most recent output
  buffer, but at least on my Dell Latitude E7450, PortAudio is getting zero
  from ALSA, so we need to find a proxy for this.

  \par Estimating outputTime (Plan A, assuming double-buffered, fixed-size buffers, please skip to Plan B)
  One can expect the audio callback to happen as soon as there is room in
  the output for another block of samples, so we could just measure system
  time at the top of the callback. Then we could add the maximum delay
  buffered in the system. E.g. if there is simple double buffering and the
  callback is computing one of the buffers, the callback happens just as
  one of the buffers empties, meaning the other buffer is full, so we have
  exactly one buffer delay before the next computed sample is output.

  If computation falls behind a bit, the callback will be later, so the
  delay to play the next computed sample will be less. I think a reasonable
  way to estimate the actual output time is to assume that the computer is
  mostly keeping up and that *most* callbacks will occur immediately when
  there is space. Note that the most likely reason for the high-priority
  audio thread to fall behind is the callback itself, but the start of the
  callback should be pretty consistently keeping up.

  Also, we do not have to have a perfect estimate of the time. Suppose we
  estimate a linear mapping from sample count to system time by saying
  that the sample count maps to the system time at the most recent callback,
  and set the slope to 1% slower than real time (as if the sample clock is
  slow). Now, at each callback, if the callback seems to occur earlier than
  expected, we can adjust the mapping to be earlier. The earlier the
  callback, the more accurate it must be. On the other hand, if the callback
  is later than predicted, it must be a delayed callback (or else the
  sample clock is more than 1% slow, which is really a hardware problem.)
  How bad can this be? Assuming callbacks every 30ms (this seems to be what
  I'm observing in a default setup), you'll be a maximum of 1ms off even if
  2 out of 3 callbacks are late. This is pretty reasonable given that
  PortMIDI clock precision is 1ms. If buffers are larger and callback timing
  is more erratic, errors will be larger, but even a few ms error is
  probably OK.

  \par Estimating outputTime (Plan B, variable framesPerBuffer in callback, please skip to Plan C)
  ALSA is complicated because we get varying values of
  framesPerBuffer from callback to callback. Assume you get more frames
  when the callback is later (because there is more accumulated input to
  deliver and more more accumulated room in the output buffers). So take
  the current time and subtract the duration of the frame count in the
  current callback. This should be a time position that is relatively
  jitter free (because we estimated the lateness by frame count and
  subtracted that out). This time position intuitively represents the
  current ADC time, or if no input, the time of the tail of the output
  buffer. If we wanted DAC time, we'd have to add the total output
  buffer duration, which should be reported by PortAudio. (If PortAudio
  is wrong, we'll be systematically shifted in time by the error.)

  Since there is still bound to be jitter, we can smooth these estimates.
  First, we will assume a linear mapping from system time to audio time
  with slope = 1, so really it's just the offset we need.

  To improve the estimate, we get a new offset every callback, so we can
  create a "smooth" offset by using a simple regression model (also
  this could be seen as a first order filter). The following formula
  updates smooth_offset with a new offset estimate in the callback:
      smooth_offset = smooth_offset * 0.9 + new_offset_estimate * 0.1
  Since this is smooth, we'll have to be careful to give it a good initial
  value to avoid a long convergence.

  \par Estimating outputTime (Plan C)
  ALSA is complicated because we get varying values of
  framesPerBuffer from callback to callback. It seems there is a lot
  of variation in callback times and buffer space. One solution would
  be to go to fixed size double buffer, but Audacity seems to work
  better as is, so Plan C is to rely on one invariant which is that
  the output buffer cannot overflow, so there's a limit to how far
  ahead of the DAC time we can be writing samples into the
  buffer. Therefore, we'll assume that the audio clock runs slow by
  about 0.2% and we'll assume we're computing at that rate. If the
  actual output position is ever ahead of the computed position, we'll
  increase the computed position to the actual position. Thus whenever
  the buffer is less than near full, we'll stay ahead of DAC time,
  falling back at a rate of about 0.2% until eventually there's
  another near-full buffer callback that will push the time back ahead.

  \par Interaction between MIDI, Audio, and Pause
  When Pause is used, PauseTime() will increase at the same rate as
  AudioTime(), and no more events will be output. Because of the
  time advance of mAudioOutputLatency + latency and the
  fact that
  AudioTime() advances stepwise by mAudioBufferDuration, some extra MIDI
  might be output, but the same is true of audio: something like
  mAudioOutputLatency audio samples will be in the output buffer
  (with up to mAudioBufferDuration additional samples, depending on
  when the Pause takes effect). When playback is resumed, there will
  be a slight delay corresponding to the extra data previously sent.
  Again, the same is true of audio. Audio and MIDI will not pause and
  resume at exactly the same times, but their pause and resume times
  will be within the low tens of milliseconds, and the streams will
  be synchronized in any case. I.e. if audio pauses 10ms earlier than
  MIDI, it will resume 10ms earlier as well.

  \par PortMidi Latency Parameter
  PortMidi has a "latency" parameter that is added to all timestamps.
  This value must be greater than zero to enable timestamp-based timing,
  but serves no other function, so we will set it to 1. All timestamps
  must then be adjusted down by 1 before messages are sent. This
  adjustment is on top of all the calculations described above. It just
  seem too complicated to describe everything in complete detail in one
  place.

  \par Midi with a time track
  When a variable-speed time track is present, MIDI events are output
  with the times used by the time track (rather than the raw times).
  This ensures MIDI is synchronized with audio.

  \par Midi While Recording Only or Without Audio Playback
  To reduce duplicate code and to ensure recording is synchronised with
  MIDI, a portaudio stream will always be used, even when there is no
  actual audio output.  For recording, this ensures that the recorded
  audio will by synchronized with the MIDI (otherwise, it gets out-of-
  sync if played back with correct timing).

  \par NoteTrack PlayLooped
  When mPlayLooped is true, output is supposed to loop from mT0 to mT1.
  For NoteTracks, we interpret this to mean that any note-on or control
  change in the range mT0 <= t < mT1 is sent (notes that start before
  mT0 are not played even if they extend beyond mT0). Then, all notes
  are turned off. Events in the range mT0 <= t < mT1 are then repeated,
  offset by (mT1 - mT0), etc.  We do NOT go back to the beginning and
  play all control changes (update events) up to mT0, nor do we "undo"
  any state changes between mT0 and mT1.

  \par NoteTrack PlayLooped Implementation
  The mIterator object (an Alg_iterator) returns NULL when there are
  no more events scheduled before mT1. At mT1, we want to output
  all notes off messages, but the FillOtherBuffers() loop will exit
  if mNextEvent is NULL, so we create a "fake" mNextEvent for this
  special "event" of sending all notes off. After that, we destroy
  the iterator and use PrepareMidiIterator() to set up a NEW one.
  At each iteration, time must advance by (mT1 - mT0), so the
  accumulated complete loop time (in "unwarped," track time) is computed
  by MidiLoopOffset().

**********************************************************************/

#include "MIDIPlay.h"
#include "AudioIO.h"

#include "BasicUI.h"
#include "Prefs.h"
#include "portaudio.h"
#include <portmidi.h>
#include <porttime.h>
#include <thread>

#define ROUND(x) (int) ((x)+0.5)

class NoteTrack;
using NoteTrackConstArray = std::vector < std::shared_ptr< const NoteTrack > >;

namespace {

/*
 Adapt and rename the implementation of PaUtil_GetTime from commit
 c5d2c51bd6fe354d0ee1119ba932bfebd3ebfacc of portaudio
 */
#if defined( __APPLE__ )

#include <mach/mach_time.h>

/* Scaler to convert the result of mach_absolute_time to seconds */
static double machSecondsConversionScaler_ = 0.0;

/* Initialize it */
static struct InitializeTime { InitializeTime() {
   mach_timebase_info_data_t info;
   kern_return_t err = mach_timebase_info( &info );
   if( err == 0  )
       machSecondsConversionScaler_ = 1e-9 * (double) info.numer / (double) info.denom;
} } initializeTime;

static PaTime util_GetTime( void )
{
   return mach_absolute_time() * machSecondsConversionScaler_;
}

#elif defined( __WXMSW__ )

#include "profileapi.h"
#include "sysinfoapi.h"
#include "timeapi.h"

static int usePerformanceCounter_;
static double secondsPerTick_;

static struct InitializeTime { InitializeTime() {
    LARGE_INTEGER ticksPerSecond;

    if( QueryPerformanceFrequency( &ticksPerSecond ) != 0 )
    {
        usePerformanceCounter_ = 1;
        secondsPerTick_ = 1.0 / (double)ticksPerSecond.QuadPart;
    }
    else
    {
        usePerformanceCounter_ = 0;
    }
} } initializeTime;

static double util_GetTime( void )
{
    LARGE_INTEGER time;

    if( usePerformanceCounter_ )
    {
        /*
            Note: QueryPerformanceCounter has a known issue where it can skip forward
            by a few seconds (!) due to a hardware bug on some PCI-ISA bridge hardware.
            This is documented here:
            http://support.microsoft.com/default.aspx?scid=KB;EN-US;Q274323&

            The work-arounds are not very paletable and involve querying GetTickCount
            at every time step.

            Using rdtsc is not a good option on multi-core systems.

            For now we just use QueryPerformanceCounter(). It's good, most of the time.
        */
        QueryPerformanceCounter( &time );
        return time.QuadPart * secondsPerTick_;
    }
    else
    {
	#if defined(WINAPI_FAMILY) && (WINAPI_FAMILY == WINAPI_FAMILY_APP)
        return GetTickCount64() * .001;
	#else
        return timeGetTime() * .001;
	#endif
    }
}

#elif defined(HAVE_CLOCK_GETTIME)

#include <time.h>

static PaTime util_GetTime( void )
{
    struct timespec tp;
    clock_gettime(CLOCK_REALTIME, &tp);
    return (PaTime)(tp.tv_sec + tp.tv_nsec * 1e-9);
}

#else

#include <sys/time.h>

static PaTime util_GetTime( void )
{
    struct timeval tv;
    gettimeofday( &tv, NULL );
    return (PaTime) tv.tv_usec * 1e-6 + tv.tv_sec;
}

#endif

enum {
   // This is the least positive latency we can
   // specify to Pm_OpenOutput, 1 ms, which prevents immediate
   // scheduling of events:
   MIDI_MINIMAL_LATENCY_MS = 1
};

// return the system time as a double
static double streamStartTime = 0; // bias system time to small number

static double SystemTime(bool usingAlsa)
{
#ifdef __WXGTK__
   if (usingAlsa) {
      struct timespec now;
      // CLOCK_MONOTONIC_RAW is unaffected by NTP or adj-time
#ifdef FreeBSD
      clock_gettime(CLOCK_REALTIME, &now);
#else
      clock_gettime(CLOCK_MONOTONIC_RAW, &now);
#endif
      //return now.tv_sec + now.tv_nsec * 0.000000001;
      return (now.tv_sec + now.tv_nsec * 0.000000001) - streamStartTime;
   }
#else
   static_cast<void>(usingAlsa);//compiler food.
#endif

   return util_GetTime() - streamStartTime;
}

bool MIDIPlay::mMidiStreamActive = false;
bool MIDIPlay::mMidiOutputComplete = true;

AudioIOExt::RegisteredFactory sMIDIPlayFactory{
   [](const auto &playbackSchedule){
      return std::make_unique<MIDIPlay>(playbackSchedule);
   }
};

MIDIPlay::MIDIPlay(const PlaybackSchedule &schedule)
   : mPlaybackSchedule{ schedule }
{
#ifdef AUDIO_IO_GB_MIDI_WORKAROUND
   // Pre-allocate with a likely sufficient size, exceeding probable number of
   // channels
   mPendingNotesOff.reserve(64);
#endif

   PmError pmErr = Pm_Initialize();

   if (pmErr != pmNoError) {
      auto errStr =
              XO("There was an error initializing the midi i/o layer.\n");
      errStr += XO("You will not be able to play midi.\n\n");
      wxString pmErrStr = LAT1CTOWX(Pm_GetErrorText(pmErr));
      if (!pmErrStr.empty())
         errStr += XO("Error: %s").Format( pmErrStr );
      // XXX: we are in libaudacity, popping up dialogs not allowed!  A
      // long-term solution will probably involve exceptions
      using namespace BasicUI;
      ShowMessageBox(
         errStr,
         MessageBoxOptions{}
            .Caption(XO("Error Initializing Midi"))
            .ButtonStyle(Button::Ok)
            .IconStyle(Icon::Error));

      // Same logic for PortMidi as described above for PortAudio
   }
}

MIDIPlay::~MIDIPlay()
{
   Pm_Terminate();
}

bool MIDIPlay::StartOtherStream(const TransportTracks &tracks,
   const PaStreamInfo* info, double, double rate)
{
   mMidiPlaybackTracks.clear();
   for (const auto &pTrack : tracks.otherPlayableTracks) {
      pTrack->TypeSwitch( [&](const NoteTrack *pNoteTrack){
         mMidiPlaybackTracks.push_back(
            pNoteTrack->SharedPointer<const NoteTrack>());
      } );
   }

   streamStartTime = 0;
   streamStartTime = SystemTime(mUsingAlsa);

   mNumFrames = 0;
   // we want this initial value to be way high. It should be
   // sufficient to assume AudioTime is zero and therefore
   // mSystemMinusAudioTime is SystemTime(), but we'll add 1000s
   // for good measure. On the first callback, this should be
   // reduced to SystemTime() - mT0, and note that mT0 is always
   // positive.
   mSystemMinusAudioTimePlusLatency =
      mSystemMinusAudioTime = SystemTime(mUsingAlsa) + 1000;
   mAudioOutLatency = 0.0; // set when stream is opened
   mCallbackCount = 0;
   mAudioFramesPerBuffer = 0;

   // We use audio latency to estimate how far ahead of DACS we are writing
   if (info) {
      // this is an initial guess, but for PA/Linux/ALSA it's wrong and will be
      // updated with a better value:
      mAudioOutLatency = info->outputLatency;
      mSystemMinusAudioTimePlusLatency += mAudioOutLatency;
   }

   // TODO: it may be that midi out will not work unless audio in or out is
   // active -- this would be a bug and may require a change in the
   // logic here.

   bool successMidi = true;

   if(!mMidiPlaybackTracks.empty()){
      successMidi = StartPortMidiStream(rate);
   }

   // On the other hand, if MIDI cannot be opened, we will not complain
   // return successMidi;
   return true;
}

void MIDIPlay::AbortOtherStream()
{
   mMidiPlaybackTracks.clear();
}

PmTimestamp MidiTime(void *pInfo)
{
   return static_cast<MIDIPlay*>(pInfo)->MidiTime();
}

// Set up state to iterate NoteTrack events in sequence.
// Sends MIDI control changes up to the starting point mT0
// if send is true. Output is delayed by offset to facilitate
// looping (each iteration is delayed more).
void MIDIPlay::PrepareMidiIterator(bool send, double startTime, double offset)
{
   mIterator.emplace(mPlaybackSchedule, *this,
      mMidiPlaybackTracks, startTime, offset, send);
}

Iterator::Iterator(
   const PlaybackSchedule &schedule, MIDIPlay &midiPlay,
   NoteTrackConstArray &midiPlaybackTracks,
   double startTime, double offset, bool send )
   : mPlaybackSchedule{ schedule }
   , mMIDIPlay{ midiPlay }
{
   // instead of initializing with an Alg_seq, we use begin_seq()
   // below to add ALL Alg_seq's.
   // Iterator not yet initialized, must add each track...
   for (auto &t : midiPlaybackTracks) {
      Alg_seq_ptr seq = &t->GetSeq();
      // mark sequence tracks as "in use" since we're handing this
      // off to another thread and want to make sure nothing happens
      // to the data until playback finishes. This is just a sanity check.
      seq->set_in_use(true);
      const void *cookie = t.get();
      it.begin_seq(seq,
         // casting away const, but allegro just uses the pointer opaquely
         const_cast<void*>(cookie), t->GetOffset() + offset);
   }
   Prime(send, startTime + offset);
}

Iterator::~Iterator()
{
   it.end();
}

void Iterator::Prime(bool send, double startTime)
{
   GetNextEvent(); // prime the pump for FillOtherBuffers

   // Start MIDI from current cursor position
   while (mNextEvent &&
          GetNextEventTime() < startTime) {
      if (send)
         /*
          hasSolo argument doesn't matter because midiStateOnly is true.
          "Fast-forward" all update events from the start of track to the given
          play start time so the notes sound with correct timbre whenever
          turned on.
          */
         OutputEvent(0, true, false);
      GetNextEvent();
   }
}

double Iterator::GetNextEventTime() const
{
   if (mNextEvent == &gAllNotesOff)
      return mNextEventTime - ALG_EPS;
   return mNextEventTime;
}

bool MIDIPlay::StartPortMidiStream(double rate)
{
#ifdef __WXGTK__
   // Duplicating a bit of AudioIO::StartStream
   // Detect whether ALSA is the chosen host, and do the various involved MIDI
   // timing compensations only then.
   mUsingAlsa = (AudioIOHost.Read() == L"ALSA");
#endif

   int i;
   int nTracks = mMidiPlaybackTracks.size();
   // Only start MIDI stream if there is an open track
   if (nTracks == 0)
      return false;

   //wxPrintf("StartPortMidiStream: mT0 %g mTime %g\n",
   //       mT0, mTime);

   /* get midi playback device */
   PmDeviceID playbackDevice = Pm_GetDefaultOutputDeviceID();
   auto playbackDeviceName = MIDIPlaybackDevice.Read();
   mSynthLatency = MIDISynthLatency_ms.Read();
   if (wxStrcmp(playbackDeviceName, wxT("")) != 0) {
      for (i = 0; i < Pm_CountDevices(); i++) {
         const PmDeviceInfo *info = Pm_GetDeviceInfo(i);
         if (!info) continue;
         if (!info->output) continue;
         wxString interf = wxSafeConvertMB2WX(info->interf);
         wxString name = wxSafeConvertMB2WX(info->name);
         interf.Append(wxT(": ")).Append(name);
         if (wxStrcmp(interf, playbackDeviceName) == 0) {
            playbackDevice = i;
         }
      }
   } // (else playback device has Pm_GetDefaultOuputDeviceID())

   if (playbackDevice < 0)
      return false;

   /* open output device */
   mLastPmError = Pm_OpenOutput(&mMidiStream,
                                playbackDevice,
                                NULL,
                                0,
                                &::MidiTime,
                                this, // supplies pInfo argument to MidiTime
                                MIDI_MINIMAL_LATENCY_MS);
   if (mLastPmError == pmNoError) {
      mMidiStreamActive = true;
      mMidiPaused = false;
      mMidiLoopPasses = 0;
      mMidiOutputComplete = false;
      mMaxMidiTimestamp = 0;
      PrepareMidiIterator(true, mPlaybackSchedule.mT0, 0);

      // It is ok to call this now, but do not send timestamped midi
      // until after the first audio callback, which provides necessary
      // data for MidiTime().
      Pm_Synchronize(mMidiStream); // start using timestamps
   }
   return (mLastPmError == pmNoError);
}

void MIDIPlay::StopOtherStream()
{
   if (mMidiStream && mMidiStreamActive) {
      /* Stop Midi playback */
      mMidiStreamActive = false;

      mMidiOutputComplete = true;

      // now we can assume "ownership" of the mMidiStream
      // if output in progress, send all off, etc.
      AllNotesOff();
      // AllNotesOff() should be sufficient to stop everything, but
      // in Linux, if you Pm_Close() immediately, it looks like
      // messages are dropped. ALSA then seems to send All Sound Off
      // and Reset All Controllers messages, but not all synthesizers
      // respond to these messages. This is probably a bug in PortMidi
      // if the All Off messages do not get out, but for security,
      // delay a bit so that messages can be delivered before closing
      // the stream. Add 2ms of "padding" to avoid any rounding errors.
      while (mMaxMidiTimestamp + 2 > MidiTime()) {
         using namespace std::chrono;
         std::this_thread::sleep_for(1ms); // deliver the all-off messages
      }
      Pm_Close(mMidiStream);
      mMidiStream = NULL;
      mIterator.reset();

      // set in_use flags to false
      int nTracks = mMidiPlaybackTracks.size();
      for (int i = 0; i < nTracks; i++) {
         const auto t = mMidiPlaybackTracks[i].get();
         Alg_seq_ptr seq = &t->GetSeq();
         seq->set_in_use(false);
      }
   }

   mMidiPlaybackTracks.clear();
}

double Iterator::UncorrectedMidiEventTime(double pauseTime)
{
   double time;
   if (mPlaybackSchedule.mEnvelope)
      time =
         mPlaybackSchedule.RealDuration(
            GetNextEventTime() - mMIDIPlay.MidiLoopOffset())
         + mPlaybackSchedule.mT0 +
           (mMIDIPlay.mMidiLoopPasses * mPlaybackSchedule.mWarpedLength);
   else
      time = GetNextEventTime();

   return time + pauseTime;
}

bool Iterator::Unmuted(bool hasSolo) const
{
   int channel = (mNextEvent->chan) & 0xF; // must be in [0..15]
   if (!mNextEventTrack->IsVisibleChan(channel))
      return false;
   const bool channelIsMute = hasSolo
      ? !mNextEventTrack->GetSolo()
      : mNextEventTrack->GetMute();
   return !channelIsMute;
}

bool Iterator::OutputEvent(double pauseTime, bool midiStateOnly, bool hasSolo)
{
   int channel = (mNextEvent->chan) & 0xF; // must be in [0..15]
   int command = -1;
   int data1 = -1;
   int data2 = -1;

   double eventTime = UncorrectedMidiEventTime(pauseTime);

   // 0.0005 is for rounding
   double time = eventTime + 0.0005 -
                 (mMIDIPlay.mSynthLatency * 0.001);

   time += 1; // MidiTime() has a 1s offset
   // state changes have to go out without delay because the
   // midi stream time gets reset when playback starts, and
   // we don't want to leave any control changes scheduled for later
   if (time < 0 || midiStateOnly)
      time = 0;
   PmTimestamp timestamp = (PmTimestamp) (time * 1000); /* s to ms */

   // The special event gAllNotesOff means "end of playback, send
   // all notes off on all channels"
   if (mNextEvent == &gAllNotesOff) {
      bool looping = mPlaybackSchedule.GetPolicy().Looping(mPlaybackSchedule);
      mMIDIPlay.AllNotesOff(looping);
      return true;
   }

   // (RBD)
   // if mNextEvent's channel is visible, play it, visibility can
   // be updated while playing.
   
   // Be careful: if we have a note-off,
   // then we must not pay attention to the channel selection
   // or mute/solo buttons because we must turn the note off
   // even if the user changed something after the note began.

   // Note that because multiple tracks can output to the same
   // MIDI channels, it is not a good idea to send "All Notes Off"
   // when the user presses the mute button. We have no easy way
   // to know what notes are sounding on any given muted track, so
   // we'll just wait for the note-off events to happen.

   // (PRL)
   // Does that mean, try to get right results when playing to the same SET
   // of MIDI channels, but tracks play to different channels?  In the
   // case that two unrelated tracks try to merge events, for notes that
   // overlap in time, to the same channel -- then one track still turns off
   // a note that another turned on.  Maybe the prevention of this case belongs
   // to higher levels of the program, and should just be assumed here.

   // (RBD)
   // Also note that note-offs are only sent when we call
   // mIterator->request_note_off(), so notes that are not played
   // will not generate random note-offs. There is the interesting
   // case that if the playback is paused, all-notes-off WILL be sent
   // and if playback resumes, the pending note-off events WILL also
   // be sent (but if that is a problem, there would also be a problem
   // in the non-pause case.
   const bool sendIt = [&]{
      const bool isNote = mNextEvent->is_note();
      if (!(isNote && mNextIsNoteOn))
         // Regardless of channel visibility state,
         // always send note-off events,
         // and update events (program change, control change, pressure, bend)
         // in case the user changes the muting during play
         return true;
      return Unmuted(hasSolo);
   }();
   if (sendIt) {
      // Note event
      if (mNextEvent->is_note() && !midiStateOnly) {
         // Pitch and velocity
         data1 = mNextEvent->get_pitch();
         if (mNextIsNoteOn) {
            data2 = mNextEvent->get_loud(); // get velocity
            int offset = mNextEventTrack->GetVelocity();
            data2 += offset; // offset comes from per-track slider
            // clip velocity to insure a legal note-on value
            data2 = (data2 < 1 ? 1 : (data2 > 127 ? 127 : data2));
            // since we are going to play this note, we need to get a note_off
            it.request_note_off();

#ifdef AUDIO_IO_GB_MIDI_WORKAROUND
            mMIDIPlay.mPendingNotesOff.push_back(std::make_pair(channel, data1));
#endif
         }
         else {
            data2 = 0; // 0 velocity means "note off"
#ifdef AUDIO_IO_GB_MIDI_WORKAROUND
            auto end = mMIDIPlay.mPendingNotesOff.end();
            auto iter = std::find(
               mMIDIPlay.mPendingNotesOff.begin(), end, std::make_pair(channel, data1) );
            if (iter != end)
               mMIDIPlay.mPendingNotesOff.erase(iter);
#endif
         }
         command = 0x90; // MIDI NOTE ON (or OFF when velocity == 0)
      // Update event
      } else if (mNextEvent->is_update()) {
         // this code is based on allegrosmfwr.cpp -- it could be improved
         // by comparing attribute pointers instead of string compares
         Alg_update_ptr update = static_cast<Alg_update_ptr>(mNextEvent);
         const char *name = update->get_attribute();

         if (!strcmp(name, "programi")) {
            // Instrument change
            data1 = update->parameter.i;
            data2 = 0;
            command = 0xC0; // MIDI PROGRAM CHANGE
         } else if (!strncmp(name, "control", 7)) {
            // Controller change

            // The number of the controller being changed is embedded
            // in the parameter name.
            data1 = atoi(name + 7);
            // Allegro normalizes controller values
            data2 = ROUND(update->parameter.r * 127);
            command = 0xB0;
         } else if (!strcmp(name, "bendr")) {
            // Bend change

            // Reverse Allegro's post-processing of bend values
            int temp = ROUND(0x2000 * (update->parameter.r + 1));
            if (temp > 0x3fff) temp = 0x3fff; // 14 bits maximum
            if (temp < 0) temp = 0;
            data1 = temp & 0x7f; // low 7 bits
            data2 = temp >> 7;   // high 7 bits
            command = 0xE0; // MIDI PITCH BEND
         } else if (!strcmp(name, "pressurer")) {
            // Pressure change
            data1 = (int) (update->parameter.r * 127);
            if (update->get_identifier() < 0) {
               // Channel pressure
               data2 = 0;
               command = 0xD0; // MIDI CHANNEL PRESSURE
            } else {
               // Key pressure
               data2 = data1;
               data1 = update->get_identifier();
               command = 0xA0; // MIDI POLY PRESSURE
            }
         }
      }
      if (command != -1) {
         // keep track of greatest timestamp used
         if (timestamp > mMIDIPlay.mMaxMidiTimestamp) {
            mMIDIPlay.mMaxMidiTimestamp = timestamp;
         }
         Pm_WriteShort(mMIDIPlay.mMidiStream, timestamp,
                    Pm_Message((int) (command + channel),
                                  (long) data1, (long) data2));
         /* wxPrintf("Pm_WriteShort %lx (%p) @ %d, advance %d\n",
                Pm_Message((int) (command + channel),
                           (long) data1, (long) data2),
                           mNextEvent, timestamp, timestamp - Pt_Time()); */
      }
   }
   return false;
}

void Iterator::GetNextEvent()
{
   mNextEventTrack = nullptr; // clear it just to be safe
   // now get the next event and the track from which it came
   double nextOffset;
   auto midiLoopOffset = mMIDIPlay.MidiLoopOffset();
   mNextEvent = it.next(&mNextIsNoteOn,
      // Allegro retrieves the "cookie" for the event, which is a NoteTrack
      reinterpret_cast<void **>(&mNextEventTrack),
      &nextOffset, mPlaybackSchedule.mT1 + midiLoopOffset);

   mNextEventTime  = mPlaybackSchedule.mT1 + midiLoopOffset + 1;
   if (mNextEvent) {
      mNextEventTime = (mNextIsNoteOn ? mNextEvent->time :
                              mNextEvent->get_end_time()) + nextOffset;;
   }
   if (mNextEventTime > (mPlaybackSchedule.mT1 + midiLoopOffset)){ // terminate playback at mT1
      mNextEvent = &gAllNotesOff;
      mNextEventTime = mPlaybackSchedule.mT1 + midiLoopOffset;
      mNextIsNoteOn = true; // do not look at duration
   }
}

void MIDIPlay::FillOtherBuffers(
   double rate, unsigned long pauseFrames, bool paused, bool hasSolo)
{
   if (!mMidiStream)
      return;

   // If not paused, fill buffers.
   if (paused)
      return;

   // If we compute until GetNextEventTime() > current audio time,
   // we would have a built-in compute-ahead of mAudioOutLatency, and
   // it's probably good to compute MIDI when we compute audio (so when
   // we stop, both stop about the same time).
   double time = AudioTime(rate); // compute to here
   // But if mAudioOutLatency is very low, we might need some extra
   // compute-ahead to deal with mSynthLatency or even this thread.
   double actual_latency  = (MIDI_MINIMAL_LATENCY_MS + mSynthLatency) * 0.001;
   if (actual_latency > mAudioOutLatency) {
       time += actual_latency - mAudioOutLatency;
   }
   while (mIterator &&
          mIterator->mNextEvent &&
          mIterator->UncorrectedMidiEventTime(PauseTime(rate, pauseFrames)) < time) {
      if (mIterator->OutputEvent(PauseTime(rate, pauseFrames), false, hasSolo)) {
         if (mPlaybackSchedule.GetPolicy().Looping(mPlaybackSchedule)) {
            // jump back to beginning of loop
            ++mMidiLoopPasses;
            PrepareMidiIterator(false, mPlaybackSchedule.mT0, MidiLoopOffset());
         }
         else
            mIterator.reset();
      }
      else if (mIterator)
         mIterator->GetNextEvent();
   }
}

double MIDIPlay::PauseTime(double rate, unsigned long pauseFrames)
{
   return pauseFrames / rate;
}


// MidiTime() is an estimate in milliseconds of the current audio
// output (DAC) time + 1s. In other words, what audacity track time
// corresponds to the audio (including pause insertions) at the output?
//
PmTimestamp MIDIPlay::MidiTime()
{
   // note: the extra 0.0005 is for rounding. Round down by casting to
   // unsigned long, then convert to PmTimeStamp (currently signed)

   // PRL: the time correction is really Midi latency achieved by different
   // means than specifying it to Pm_OpenStream.  The use of the accumulated
   // sample count generated by the audio callback (in AudioTime()) might also
   // have the virtue of keeping the Midi output synched with audio.

   PmTimestamp ts;
   // subtract latency here because mSystemMinusAudioTime gets us
   // to the current *write* time, but we're writing ahead by audio output
   // latency (mAudioOutLatency).
   double now = SystemTime(mUsingAlsa);
   ts = (PmTimestamp) ((unsigned long)
         (1000 * (now + 1.0005 -
                  mSystemMinusAudioTimePlusLatency)));
   // wxPrintf("AudioIO::MidiTime() %d time %g sys-aud %g\n",
   //        ts, now, mSystemMinusAudioTime);
   return ts + MIDI_MINIMAL_LATENCY_MS;
}


void MIDIPlay::AllNotesOff(bool looping)
{
#ifdef __WXGTK__
   bool doDelay = !looping;
#else
   bool doDelay = false;
   static_cast<void>(looping);// compiler food.
#endif

   // to keep track of when MIDI should all be delivered,
   // update mMaxMidiTimestamp to now:
   PmTimestamp now = MidiTime();
   if (mMaxMidiTimestamp < now) {
       mMaxMidiTimestamp = now;
   }
#ifdef AUDIO_IO_GB_MIDI_WORKAROUND
   // PRL:
   // Send individual note-off messages for each note-on not yet paired.

   // RBD:
   // Even this did not work as planned. My guess is ALSA does not use
   // a "stable sort" for timed messages, so that when a note-off is
   // added later at the same time as a future note-on, the order is
   // not respected, and the note-off can go first, leaving a stuck note.
   // The workaround here is to use mMaxMidiTimestamp to ensure that
   // note-offs come at least 1ms later than any previous message

   // PRL:
   // I think we should do that only when stopping or pausing, not when looping
   // Note that on Linux, MIDI always uses ALSA, no matter whether portaudio
   // uses some other host api.

   mMaxMidiTimestamp += 1;
   for (const auto &pair : mPendingNotesOff) {
      Pm_WriteShort(mMidiStream,
                    (doDelay ? mMaxMidiTimestamp : 0),
                    Pm_Message(
         0x90 + pair.first, pair.second, 0));
      mMaxMidiTimestamp++; // allow 1ms per note-off
   }
   mPendingNotesOff.clear();

   // Proceed to do the usual messages too.
#endif

   for (int chan = 0; chan < 16; chan++) {
      Pm_WriteShort(mMidiStream,
                    (doDelay ? mMaxMidiTimestamp : 0),
                    Pm_Message(0xB0 + chan, 0x7B, 0));
      mMaxMidiTimestamp++; // allow 1ms per all-notes-off
   }
}

void MIDIPlay::ComputeOtherTimings(double rate, bool paused,
   const PaStreamCallbackTimeInfo *timeInfo,
   unsigned long framesPerBuffer
   )
{
   if (mCallbackCount++ == 0) {
       // This is effectively mSystemMinusAudioTime when the buffer is empty:
       mStartTime = SystemTime(mUsingAlsa) - mPlaybackSchedule.mT0;
       // later, mStartTime - mSystemMinusAudioTime will tell us latency
   }

   /* for Linux, estimate a smooth audio time as a slowly-changing
      offset from system time */
   // rnow is system time as a double to simplify math
   double rnow = SystemTime(mUsingAlsa);
   // anow is next-sample-to-be-computed audio time as a double
   double anow = AudioTime(rate);

   if (mUsingAlsa) {
      // timeInfo's fields are not all reliable.

      // enow is audio time estimated from our clock synchronization protocol,
      //   which produces mSystemMinusAudioTime. But we want the estimate
      //   to drift low, so we steadily increase mSystemMinusAudioTime to
      //   simulate a fast system clock or a slow audio clock. If anow > enow,
      //   we'll update mSystemMinusAudioTime to keep in sync. (You might think
      //   we could just use anow as the "truth", but it has a lot of jitter,
      //   so we are using enow to smooth out this jitter, in fact to < 1ms.)
      // Add worst-case clock drift using previous framesPerBuffer:
      const auto increase =
         mAudioFramesPerBuffer * 0.0002 / rate;
      mSystemMinusAudioTime += increase;
      mSystemMinusAudioTimePlusLatency += increase;
      double enow = rnow - mSystemMinusAudioTime;


      // now, use anow instead if it is ahead of enow
      if (anow > enow) {
         mSystemMinusAudioTime = rnow - anow;
         // Update our mAudioOutLatency estimate during the first 20 callbacks.
         // During this period, the buffer should fill. Once we have a good
         // estimate of mSystemMinusAudioTime (expected in fewer than 20 callbacks)
         // we want to stop the updating in case there is clock drift, which would
         // cause the mAudioOutLatency estimation to drift as well. The clock drift
         // in the first 20 callbacks should be negligible, however.
         if (mCallbackCount < 20) {
            mAudioOutLatency = mStartTime -
               mSystemMinusAudioTime;
         }
         mSystemMinusAudioTimePlusLatency =
            mSystemMinusAudioTime + mAudioOutLatency;
      }
   }
   else {
      // If not using Alsa, rely on timeInfo to have meaningful values that are
      // more precise than the output latency value reported at stream start.
      mSystemMinusAudioTime = rnow - anow;
      mSystemMinusAudioTimePlusLatency =
         mSystemMinusAudioTime +
            (timeInfo->outputBufferDacTime - timeInfo->currentTime);
   }

   mAudioFramesPerBuffer = framesPerBuffer;
   mNumFrames += framesPerBuffer;

   // Keep track of time paused.
   if (paused) {
      if (!mMidiPaused) {
         mMidiPaused = true;
         AllNotesOff(); // to avoid hanging notes during pause
      }
   }
   else if (mMidiPaused)
      mMidiPaused = false;
}

unsigned MIDIPlay::CountOtherSoloTracks() const
{
   return std::count_if(
      mMidiPlaybackTracks.begin(), mMidiPlaybackTracks.end(),
      [](const auto &pTrack){ return pTrack->GetSolo(); } );
}

void MIDIPlay::SignalOtherCompletion()
{
   mMidiOutputComplete = true;
}
}

bool MIDIPlay::IsActive()
{
   return ( mMidiStreamActive && !mMidiOutputComplete );
}

bool MIDIPlay::IsOtherStreamActive() const
{
   return IsActive();
}

AudioIODiagnostics MIDIPlay::Dump() const
{
   return {
      wxT("mididev.txt"),
      GetMIDIDeviceInfo(),
      wxT("MIDI Device Info")
   };
}

