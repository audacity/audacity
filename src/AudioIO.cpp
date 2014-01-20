/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIO.cpp

  Copyright 2000-2004:
  Dominic Mazzoni
  Joshua Haberman
  Markus Meyer
  Matt Brubeck

  This program is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

********************************************************************//**

\class AudioIO
\brief AudioIO uses the PortAudio library to play and record sound.

  Great care and attention to detail are necessary for understanding and
  modifying this system.  The code in this file is run from three
  different thread contexts: the UI thread, the disk thread (which
  this file creates and maintains; in the code, this is called the 
  Audio Thread), and the PortAudio callback thread.
  To highlight this deliniation, the file is divided into three parts
  based on what thread context each function is intended to run in.

  \par EXPERIMENTAL_MIDI_PLAYBACK
  If EXPERIMENTAL_MIDI_PLAYBACK is defined, this class also manages
  MIDI playback. The reason for putting MIDI here rather than in, say,
  class MidiIO, is that there is no high-level synchronization and
  transport architecture, so Audio and MIDI must be coupled in order 
  to start/stop/pause and synchronize them.

  \par MIDI With Audio
  When Audio and MIDI play simultaneously, MIDI synchronizes to Audio.
  This is necessary because the Audio sample clock is not the same
  hardware as the system time used to schedule MIDI messages. MIDI
  is synchronized to Audio because it is simple to pause or rush
  the dispatch of MIDI messages, but generally impossible to pause
  or rush synchronous audio samples (without distortion).

  \par
  MIDI output is driven by yet another thread. In principle, we could
  output timestamped MIDI data at the same time we fill audio buffers
  from disk, but audio buffers are filled far in advance of playback
  time, and there is a lower latency thread (PortAudio's callback) that
  actually sends samples to the output device. The relatively low
  latency to the output device allows Audacity to stop audio output
  quickly. We want the same behavior for MIDI, but there is not
  periodic callback from PortMidi (because MIDI is asynchronous), so 
  this function is performed by the MidiThread class.

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
  captureChannels and mMidiPlaybackTracks.IsEmpty() to determine if
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
  \li \c PauseTime() is the amount of time spent paused, based on a count of zero samples output.
  \li \c MidiTime() is an estimate in milliseconds of the current audio output time + 1s. In other words, what audacity track time corresponds to the audio (including pause insertions) at the output?

  \par AudioTime() and PauseTime() computation
  AudioTime() is simply mT0 + mNumFrames / mRate. 
  mNumFrames is incremented in each audio callback. Similarly, PauseTime()
  is mNumPauseFrames / mRate. mNumPauseFrames is also incremented in
  each audio callback when a pause is in effect.

  \par MidiTime() computation
  MidiTime() is computed based on information from PortAudio's callback,
  which estimates the system time at which the current audio buffer will
  be output. Consider the (unimplemented) function RealToTrack() that
  maps real time to track time. If outputTime is PortAudio's time 
  estimate for the most recent output buffer, then \n
  RealToTrack(outputTime) = AudioTime() - PauseTime() - bufferDuration \n
  We want to know RealToTrack of the current time, so we use this 
  approximation for small d: \n
  RealToTrack(t + d) = RealToTrack(t) + d \n
  Letting t = outputTime and d = (systemTime - outputTime), we can
  substitute to get:\n
  RealToTrack(systemTime) = AudioTime() - PauseTime() - bufferduration + (systemTime - outputTime) \n
  MidiTime() should include pause time, so add PauseTime() to both sides of
  the equation. Also MidiTime() is offset by 1 second to avoid negative
  time at startup, so add 1 to both sides:
  MidiTime() in seconds = RealToTrack(systemTime) + PauseTime() + 1 = \n
  AudioTime() - bufferduration + (systemTime - outputTime) + 1
  
  \par
  The difference AudioTime() - PauseTime() is the time "cursor" for 
  MIDI. When the speed control is used, MIDI and Audio will become 
  unsynchronized. In particular, MIDI will not be synchronized with 
  the visual cursor, which moves with scaled time reported in mTime.
  
  \par Midi Synchronization
  The goal of MIDI playback is to deliver MIDI messages synchronized to
  audio (assuming no speed variation for now). If a midi event has time
  tmidi, then the timestamp for that message should be \n
  timestamp (in seconds) = tmidi + PauseTime() + 1.0 - latency.\n
  (This is actually off by 1ms; see "PortMidi Latency Parameter" below for 
  more detail.)
  Notice the extra 1.0, added because MidiTime() is offset by 1s to avoid
  starting at a negative value. Also notice that we subtract latency.
  The user must set device latency using preferences. Some software
  synthesizers have very high latency (on the order of 100ms), so unless
  we lower timestamps and send messages early, the final output will not
  be synchronized.
  This timestamp is interpreted by PortMidi relative to MidiTime(), which
  is synchronized to audio output. So the only thing we need to do is
  output Midi messages shortly before they will be played with the correct
  timestamp. We will take "shortly before" to mean "at about the same time
  as corresponding audio". Based on this, output the event when 
  AudioTime() - PauseTime() > mtime - latency,
  adjusting the event time by adding PauseTime() + 1 - latency. 
  This gives at least mAudioOutputLatency for 
  the MIDI output to be generated (we want to generate MIDI output before
  the actual output time because events generated early are accurately timed 
  according to their timestamp). However, the MIDI thread sleeps for 
  MIDI_SLEEP in its polling loop, so the worst case is really 
  mAudioOutputLatency + MIDI_SLEEP. In case the audio output latency is 
  very low, we will output events when 
  AudioTime() + MIDI_SLEEP - PauseTime() > mtime - latency.

  \par Interaction between MIDI, Audio, and Pause
  When Pause is used, PauseTime() will increase at the same rate as
  AudioTime(), and no more events will be output. Because of the 
  time advance of mAudioOutputLatency + MIDI_SLEEP + latency and the 
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

  \par Midi While Recording Only
  All of the midi-to-audio synchronization is of course meaningless when
  audio is not playing. If only recording, there is the problem that
  synchronization is based on output time, but without audio output,
  there is no output time. This does not seem like a critical feature,
  so MIDI is not synchronized to audio without audio playback. The
  user can always play a track of silence while recording to synchronize.

  \par Midi Without Audio Playback
  When there is no audio playback, MIDI runs according to its own clock.
  The midi timestamp clock starts at approximately the same time as 
  audio recording (if any). A timestamp of 0 corresponds to mT0, the
  starting time in the Midi track(s). Thus the timestamp for an event
  at time tmidi should be: \n
  timestamp = tmidi - mT0 + PauseTime() - latency - 0.001\n
  Where latency is the synthesizer latency, and the extra 0.001 is the
  latency (1ms) that PortMidi adds to timestamps automatically.

  \par Midi Output Without Audio Playback
  Midi events should be written before their timestamp expires. Since
  the loop that checks for events to write pauses for MIDI_SLEEP, the
  events should be written at least MIDI_SLEEP early, and due to 
  other delays and computation, we want some extra time, so let's 
  allow 2*MIDI_SLEEP. Therefore, the write time should be when:\n
  tmidi - mT0 + PauseTime() - latency - 0.001 - 2 * MIDI_SLEEP < Pt_Time()\n,
  which can be rearranged to:\n
  tmidi < mT0 + Pt_Time() + MIDI_SLEEP + (MIDI_SLEEP + latency) - PauseTime\n
  which matches the code in AudioIO::FillMidiBuffers() after converting ms to
  s appropriately. (Note also that the 0.001 is dropped here -- it's not
  really important).

  \par The code for Midi Without Audio was developed by simply trying
  to play Midi alone and fixing everything that did not work. The
  "normal" AudioIO execution was full of assumptions about audio, so 
  there is no systematic design for running without audio, merely a
  number of "patches" to make it work. The expression 
  "mNumPlaybackChannels > 0" is used to detect whether audio playback
  is active, and "mNumFrames > 0" is used to indicate that playback
  of either Midi or Audio has actually started. (mNumFrames is 
  normally incremented by the audio callback, but if there is no
  audio playback or recording, it is set to 1 at the end of 
  initialization.

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
  all notes off messages, but the FillMidiBuffers() loop will exit
  if mNextEvent is NULL, so we create a "fake" mNextEvent for this
  special "event" of sending all notes off. After that, we destroy 
  the iterator and use PrepareMidiIterator() to set up a new one.
  At each iteration, time must advance by (mT1 - mT0), so the
  accumulated time is held in mMidiLoopOffset.

  \todo run through all functions called from audio and portaudio threads
  to verify they are thread-safe. Note that synchronization of the style:
  "A sets flag to signal B, B clears flag to acknowledge completion"
  is not thread safe in a general multiple-CPU context. For example,
  B can write to a buffer and set a completion flag. The flag write can
  occur before the buffer write due to out-of-order execution. Then A
  can see the flag and read the buffer before buffer writes complete.

*//****************************************************************//**

\class AudioThread
\brief Defined different on Mac and other platforms (on Mac it does not
use wxWidgets wxThread), this class sits in a thread loop reading and 
writing audio.

*//*******************************************************************/

#include "Audacity.h"
#include "float_cast.h"
#include "Experimental.h"

#include <math.h>
#include <stdlib.h>
#include <algorithm>

#ifdef __WXMSW__
#include <malloc.h>
#endif

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#if USE_PORTMIXER
#include "portmixer.h"
#endif

#include <wx/log.h>
#include <wx/textctrl.h>
#include <wx/msgdlg.h>
#include <wx/timer.h>
#include <wx/intl.h>
#include <wx/debug.h>
#include <wx/sstream.h>
#include <wx/txtstrm.h>

#include "AudacityApp.h"
#include "AudioIO.h"
#include "Mix.h"
#include "MixerBoard.h"
#include "Resample.h"
#include "RingBuffer.h"
#include "Prefs.h"
#include "Project.h"
#include "WaveTrack.h"

#include "toolbars/ControlToolBar.h"
#include "widgets/Meter.h"

#include "Experimental.h"

#ifdef EXPERIMENTAL_MIDI_OUT
   #define MIDI_SLEEP 10 /* milliseconds */
   #define ROUND(x) (int) ((x)+0.5)
   //#include <string.h>
   #include "portmidi.h"
   #include "common/pa_util.h"
   #include "NoteTrack.h"
#endif

#ifdef AUTOMATED_INPUT_LEVEL_ADJUSTMENT
   #define LOWER_BOUND 0.0
   #define UPPER_BOUND 1.0
#endif

using std::max;
using std::min;

AudioIO *gAudioIO;

// static
int AudioIO::mNextStreamToken = 0;
int AudioIO::mCachedPlaybackIndex = -1;
wxArrayLong AudioIO::mCachedPlaybackRates;
int AudioIO::mCachedCaptureIndex = -1;
wxArrayLong AudioIO::mCachedCaptureRates;
wxArrayLong AudioIO::mCachedSampleRates;
double AudioIO::mCachedBestRateIn = 0.0;
double AudioIO::mCachedBestRateOut;

const int AudioIO::StandardRates[] = {
   8000,
   11025,
   16000,
   22050,
   32000,
   44100,
   48000,
   88200,
   96000,
   176400,
   192000,
   352800,
   384000
};
const int AudioIO::NumStandardRates = sizeof(AudioIO::StandardRates) /
                                      sizeof(AudioIO::StandardRates[0]);
const int AudioIO::RatesToTry[] = {
   8000,
   9600,
   11025,
   12000,
   15000,
   16000,
   22050,
   24000,
   32000,
   44100,
   48000,
   88200,
   96000,
   176400,
   192000,
   352800,
   384000
};
const int AudioIO::NumRatesToTry = sizeof(AudioIO::RatesToTry) /
                                      sizeof(AudioIO::RatesToTry[0]);

int audacityAudioCallback(const void *inputBuffer, void *outputBuffer,
                          unsigned long framesPerBuffer,
                          const PaStreamCallbackTimeInfo *timeInfo,
                          PaStreamCallbackFlags statusFlags, void *userData );

#ifdef EXPERIMENTAL_MIDI_OUT
int compareTime( const void* a, const void* b );
#endif

//////////////////////////////////////////////////////////////////////
//
//     class AudioThread - declaration and glue code
//
//////////////////////////////////////////////////////////////////////

#ifdef __WXMAC__

// On Mac OS X, it's better not to use the wxThread class.
// We use our own implementation based on pthreads instead.

#include <pthread.h>
#include <time.h>

class AudioThread {
 public:
   typedef int ExitCode;
   AudioThread() { mDestroy = false; mThread = NULL; }
   virtual ExitCode Entry();
   void Create() {}
   void Delete() {
      mDestroy = true;
      pthread_join(mThread, NULL);
   }
   bool TestDestroy() { return mDestroy; }
   void Sleep(int ms) {
      struct timespec spec;
      spec.tv_sec = 0;
      spec.tv_nsec = ms * 1000 * 1000;
      nanosleep(&spec, NULL);
   }
   static void *callback(void *p) {
      AudioThread *th = (AudioThread *)p;
      return (void *)th->Entry();
   }
   void Run() {
      pthread_create(&mThread, NULL, callback, this);
   }
 private:
   bool mDestroy;
   pthread_t mThread;

};

#else

// The normal wxThread-derived AudioThread class for all other
// platforms:
class AudioThread : public wxThread {
 public:
   AudioThread():wxThread(wxTHREAD_JOINABLE) {}
   virtual ExitCode Entry();
};

#endif

#ifdef EXPERIMENTAL_MIDI_OUT
class MidiThread : public AudioThread {
 public:
   virtual ExitCode Entry();
};
#endif


//////////////////////////////////////////////////////////////////////
//
//     UI Thread Context
//
//////////////////////////////////////////////////////////////////////

void InitAudioIO()
{
   gAudioIO = new AudioIO();
   gAudioIO->mThread->Run();
#ifdef EXPERIMENTAL_MIDI_OUT
   gAudioIO->mMidiThread->Run();
#endif

   // Make sure device prefs are initialized
   if (gPrefs->Read(wxT("AudioIO/RecordingDevice"), wxT("")) == wxT("")) {
      int i = AudioIO::getRecordDevIndex();
      const PaDeviceInfo *info = Pa_GetDeviceInfo(i);
      if (info) {
         gPrefs->Write(wxT("/AudioIO/RecordingDevice"), DeviceName(info));
         gPrefs->Write(wxT("/AudioIO/Host"), HostName(info));
      }
   }

   if (gPrefs->Read(wxT("AudioIO/PlaybackDevice"), wxT("")) == wxT("")) {
      int i = AudioIO::getPlayDevIndex();
      const PaDeviceInfo *info = Pa_GetDeviceInfo(i);
      if (info) {
         gPrefs->Write(wxT("/AudioIO/PlaybackDevice"), DeviceName(info));
         gPrefs->Write(wxT("/AudioIO/Host"), HostName(info));
      }
   }

   gPrefs->Flush();
}

void DeinitAudioIO()
{
   delete gAudioIO;
}

wxString DeviceName(const PaDeviceInfo* info)
{
   wxString infoName(info->name, wxConvLocal);

   return infoName;
}

wxString HostName(const PaDeviceInfo* info)
{
   wxString hostapiName(Pa_GetHostApiInfo(info->hostApi)->name, wxConvLocal);

   return hostapiName;
}

bool AudioIO::ValidateDeviceNames(wxString play, wxString rec)
{
   const PaDeviceInfo *pInfo = Pa_GetDeviceInfo(AudioIO::getPlayDevIndex(play));
   const PaDeviceInfo *rInfo = Pa_GetDeviceInfo(AudioIO::getRecordDevIndex(rec));

   if (!pInfo || !rInfo || pInfo->hostApi != rInfo->hostApi) {
      return false;
   }

   return true;
}

AudioIO::AudioIO()
{
   mAudioThreadShouldCallFillBuffersOnce = false;
   mAudioThreadFillBuffersLoopRunning = false;
   mAudioThreadFillBuffersLoopActive = false;
   mPortStreamV19 = NULL;

#ifdef EXPERIMENTAL_MIDI_OUT
   mMidiStream = NULL;
   mMidiThreadFillBuffersLoopRunning = false;
   mMidiThreadFillBuffersLoopActive = false;
   mMidiStreamActive = false;
   mSendMidiState = false;
   mIterator = NULL;
   mMidiPlaySpeed = 1.0;

   mNumFrames = 0;
   mNumPauseFrames = 0;
#endif

#ifdef AUTOMATED_INPUT_LEVEL_ADJUSTMENT
   mAILAActive = false;
#endif
   mSilentBuf = NULL;
   mLastSilentBufSize = 0;

   mStreamToken = 0;
   mStopStreamCount = 0;

   mLastPaError = paNoError;

   mLastRecordingOffset = 0.0;
   mNumCaptureChannels = 0;
   mPaused = false;
   mPlayLooped = false;

   mListener = NULL;
   mUpdateMeters = false;
   mUpdatingMeters = false;

   PaError err = Pa_Initialize();

   if (err != paNoError) {
      wxString errStr = _("Could not find any audio devices.\n");
      errStr += _("You will not be able to play or record audio.\n\n");
      wxString paErrStr = LAT1CTOWX(Pa_GetErrorText(err));
      if (!paErrStr.IsEmpty())
         errStr += _("Error: ")+paErrStr;
      // XXX: we are in libaudacity, popping up dialogs not allowed!  A
      // long-term solution will probably involve exceptions
      wxMessageBox(errStr, _("Error Initializing Audio"), wxICON_ERROR|wxOK);

      // Since PortAudio is not initialized, all calls to PortAudio
      // functions will fail.  This will give reasonable behavior, since
      // the user will be able to do things not relating to audio i/o,
      // but any attempt to play or record will simply fail.
   }

#ifdef EXPERIMENTAL_MIDI_OUT
   PmError pmErr = Pm_Initialize();

   if (pmErr != pmNoError) {
      wxString errStr = 
              _("There was an error initializing the midi i/o layer.\n");
      errStr += _("You will not be able to play midi.\n\n");
      wxString pmErrStr = LAT1CTOWX(Pm_GetErrorText(pmErr));
      if (pmErrStr)
         errStr += _("Error: ") + pmErrStr;
      // XXX: we are in libaudacity, popping up dialogs not allowed!  A
      // long-term solution will probably involve exceptions
      wxMessageBox(errStr, _("Error Initializing Midi"), wxICON_ERROR|wxOK);

      // Same logic for PortMidi as described above for PortAudio
   }
   mMidiThread = new MidiThread();
   mMidiThread->Create();
#endif

   // Start thread
   mThread = new AudioThread();
   mThread->Create();

#if defined(USE_PORTMIXER)
   mPortMixer = NULL;
   mPreviousHWPlaythrough = -1.0;
   HandleDeviceChange();
#else
   mEmulateMixerOutputVol = true;
   mMixerOutputVol = 1.0;
   mInputMixerWorks = false;
#endif
}

AudioIO::~AudioIO()
{
#if defined(USE_PORTMIXER)
   if (mPortMixer) {
      #if __WXMAC__
      if (Px_SupportsPlaythrough(mPortMixer) && mPreviousHWPlaythrough >= 0.0)
         Px_SetPlaythrough(mPortMixer, mPreviousHWPlaythrough);
         mPreviousHWPlaythrough = -1.0;
      #endif
      Px_CloseMixer(mPortMixer);
      mPortMixer = NULL;
   }
#endif
   Pa_Terminate();

#ifdef EXPERIMENTAL_MIDI_OUT
   Pm_Terminate();
   mMidiThread->Delete();
#endif

   /* Delete is a "graceful" way to stop the thread.
      (Kill is the not-graceful way.) */
   wxTheApp->Yield();
   mThread->Delete();
   
   if(mSilentBuf) 
      DeleteSamples(mSilentBuf);

   delete mThread;
}

void AudioIO::SetMixer(int inputSource)
{
#if defined(USE_PORTMIXER)
   int oldRecordSource = Px_GetCurrentInputSource(mPortMixer);
   if ( inputSource != oldRecordSource )
         Px_SetCurrentInputSource(mPortMixer, inputSource);
#endif
}
void AudioIO::SetMixer(int inputSource, float recordVolume,
                       float playbackVolume)
{
   mMixerOutputVol = playbackVolume;

#if defined(USE_PORTMIXER)
   PxMixer *mixer = mPortMixer;

   if( mixer )
   {
      float oldRecordVolume = Px_GetInputVolume(mixer);
      float oldPlaybackVolume = Px_GetPCMOutputVolume(mixer);

      SetMixer(inputSource);
      if( oldRecordVolume != recordVolume )
         Px_SetInputVolume(mixer, recordVolume);
      if( oldPlaybackVolume != playbackVolume )
         Px_SetPCMOutputVolume(mixer, playbackVolume);

      return;
   }
#endif
}

void AudioIO::GetMixer(int *recordDevice, float *recordVolume,
                       float *playbackVolume)
{
#if defined(USE_PORTMIXER)

   PxMixer *mixer = mPortMixer;

   if( mixer )
   {
      *recordDevice = Px_GetCurrentInputSource(mixer);

      if (mInputMixerWorks)
         *recordVolume = Px_GetInputVolume(mixer);
      else
         *recordVolume = 1.0f;

      if (mEmulateMixerOutputVol)
         *playbackVolume = mMixerOutputVol;
      else
         *playbackVolume = Px_GetPCMOutputVolume(mixer);

      return;
   }

#endif

   *recordDevice = 0;
   *recordVolume = 1.0f;
   *playbackVolume = mMixerOutputVol;
}

bool AudioIO::InputMixerWorks()
{
   return mInputMixerWorks;
}

bool AudioIO::OutputMixerEmulated()
{
   return mEmulateMixerOutputVol;
}

wxArrayString AudioIO::GetInputSourceNames()
{
#if defined(USE_PORTMIXER)

   wxArrayString deviceNames;

   if( mPortMixer )
   {
      int numSources = Px_GetNumInputSources(mPortMixer);
      for( int source = 0; source < numSources; source++ )
         deviceNames.Add(wxString(Px_GetInputSourceName(mPortMixer, source), wxConvLocal));
   }
   else
   {
      wxLogDebug(wxT("AudioIO::GetInputSourceNames(): PortMixer not initialised!"));
   }

   return deviceNames;

#else

   wxArrayString blank;

   return blank;

#endif
}

void AudioIO::HandleDeviceChange()
{
   // This should not happen, but it would screw things up if it did.
   // Vaughan, 2010-10-08: But it *did* happen, due to a bug, and nobody 
   // caught it because this method just returned. Added wxASSERT().
   wxASSERT(!IsStreamActive()); 
   if (IsStreamActive())
      return;

   // get the selected record and playback devices
   int playDeviceNum = getPlayDevIndex();
   int recDeviceNum = getRecordDevIndex();

   // cache playback/capture rates
   mCachedPlaybackRates = GetSupportedPlaybackRates(playDeviceNum);
   mCachedCaptureRates = GetSupportedCaptureRates(recDeviceNum);
   mCachedSampleRates = GetSupportedSampleRates(playDeviceNum, recDeviceNum);
   mCachedPlaybackIndex = playDeviceNum;
   mCachedCaptureIndex = recDeviceNum;
   mCachedBestRateIn = 0.0;

#if defined(USE_PORTMIXER)

   // if we have a PortMixer object, close it down
   if (mPortMixer) {
      #if __WXMAC__
      // on the Mac we must make sure that we restore the hardware playthrough
      // state of the sound device to what it was before, because there isn't
      // a UI for this (!)
      if (Px_SupportsPlaythrough(mPortMixer) && mPreviousHWPlaythrough >= 0.0)
         Px_SetPlaythrough(mPortMixer, mPreviousHWPlaythrough);
         mPreviousHWPlaythrough = -1.0;
      #endif
      Px_CloseMixer(mPortMixer);
      mPortMixer = NULL;
   }

   // that might have given us no rates whatsoever, so we have to guess an
   // answer to do the next bit
   int numrates = mCachedSampleRates.GetCount();
   int highestSampleRate;
   if (numrates > 0)
   {
      highestSampleRate = mCachedSampleRates[numrates - 1];
   }
   else
   {  // we don't actually have any rates that work for Rec and Play. Guess one
      // to use for messing with the mixer, which doesn't actually do either
      highestSampleRate = 44100;
      // mCachedSampleRates is still empty, but it's not used again, so
      // can ignore
   }
   mInputMixerWorks = false;
   mEmulateMixerOutputVol = true;
   mMixerOutputVol = 1.0;

   int error;
   // This tries to open the device with the samplerate worked out above, which
   // will be the highest available for play and record on the device, or
   // 44.1kHz if the info cannot be fetched.

   PaStream *stream;

   PaStreamParameters playbackParameters;

   playbackParameters.device = playDeviceNum;
   playbackParameters.sampleFormat = paFloat32;
   playbackParameters.hostApiSpecificStreamInfo = NULL;
   playbackParameters.channelCount = 1;
   if (Pa_GetDeviceInfo(playDeviceNum))
      playbackParameters.suggestedLatency =
         Pa_GetDeviceInfo(playDeviceNum)->defaultLowOutputLatency;
   else
      playbackParameters.suggestedLatency = DEFAULT_LATENCY_CORRECTION/1000.0; 

   PaStreamParameters captureParameters;
 
   captureParameters.device = recDeviceNum;
   captureParameters.sampleFormat = paFloat32;;
   captureParameters.hostApiSpecificStreamInfo = NULL;
   captureParameters.channelCount = 1;
   if (Pa_GetDeviceInfo(recDeviceNum))
      captureParameters.suggestedLatency =
         Pa_GetDeviceInfo(recDeviceNum)->defaultLowInputLatency;
   else
      captureParameters.suggestedLatency = DEFAULT_LATENCY_CORRECTION/1000.0; 

   // try opening for record and playback
   error = Pa_OpenStream(&stream,
                         &captureParameters, &playbackParameters,
                         highestSampleRate, paFramesPerBufferUnspecified,
                         paClipOff | paDitherOff,
                         audacityAudioCallback, NULL);
   
   if (!error) {
      // Try portmixer for this stream
      mPortMixer = Px_OpenMixer(stream, 0);
      if (!mPortMixer) {
         Pa_CloseStream(stream);
         error = true;
      }
   }

   // if that failed, try just for record
   if( error ) {
      error = Pa_OpenStream(&stream,
                            &captureParameters, NULL,
                            highestSampleRate, paFramesPerBufferUnspecified,
                            paClipOff | paDitherOff,
                            audacityAudioCallback, NULL);

      if (!error) {
         mPortMixer = Px_OpenMixer(stream, 0);
         if (!mPortMixer) {
            Pa_CloseStream(stream);
            error = true;
         }
      }
   }

   // finally, try just for playback
   if ( error ) {
      error = Pa_OpenStream(&stream,
                            NULL, &playbackParameters,
                            highestSampleRate, paFramesPerBufferUnspecified,
                            paClipOff | paDitherOff,
                            audacityAudioCallback, NULL);
      
      if (!error) {
         mPortMixer = Px_OpenMixer(stream, 0);
         if (!mPortMixer) {
            Pa_CloseStream(stream);
            error = true;
         }
      }
   }

   // if it's still not working, give up
   if( error )
      return;

   // Set input source
#if USE_PORTMIXER
   int sourceIndex;
   if (gPrefs->Read(wxT("/AudioIO/RecordingSourceIndex"), &sourceIndex)) {
      if (sourceIndex >= 0) {
         //the current index of our source may be different because the stream
         //is a combination of two devices, so update it.
         sourceIndex = getRecordSourceIndex(mPortMixer);
         if (sourceIndex >= 0)
            SetMixer(sourceIndex);
      }
   }
#endif

   // Determine mixer capabilities - if it doesn't support control of output
   // signal level, we emulate it (by multiplying this value by all outgoing
   // samples)

   mMixerOutputVol = Px_GetPCMOutputVolume(mPortMixer);
   mEmulateMixerOutputVol = false;
   Px_SetPCMOutputVolume(mPortMixer, 0.0);
   if (Px_GetPCMOutputVolume(mPortMixer) > 0.1)
      mEmulateMixerOutputVol = true;
   Px_SetPCMOutputVolume(mPortMixer, 0.2f);
   if (Px_GetPCMOutputVolume(mPortMixer) < 0.1 ||
       Px_GetPCMOutputVolume(mPortMixer) > 0.3)
      mEmulateMixerOutputVol = true;
   Px_SetPCMOutputVolume(mPortMixer, mMixerOutputVol);

   float inputVol = Px_GetInputVolume(mPortMixer);
   mInputMixerWorks = true;   // assume it works unless proved wrong
   Px_SetInputVolume(mPortMixer, 0.0);
   if (Px_GetInputVolume(mPortMixer) > 0.1)
      mInputMixerWorks = false;  // can't set to zero
   Px_SetInputVolume(mPortMixer, 0.2f);
   if (Px_GetInputVolume(mPortMixer) < 0.1 ||
       Px_GetInputVolume(mPortMixer) > 0.3)
      mInputMixerWorks = false;  // can't set level accurately
   Px_SetInputVolume(mPortMixer, inputVol);

   Pa_CloseStream(stream);
 

   #if 0
   printf("PortMixer: Output: %s Input: %s\n",
          mEmulateMixerOutputVol? "emulated": "native",
          mInputMixerWorks? "hardware": "no control");
   #endif

   mMixerOutputVol = 1.0;

#endif   // USE_PORTMIXER
}

static PaSampleFormat AudacityToPortAudioSampleFormat(sampleFormat format)
{
   switch(format) {
   case int16Sample:
      return paInt16;
   case int24Sample:
      return paInt24;
   case floatSample:
   default:
      return paFloat32;
   }
}

bool AudioIO::StartPortAudioStream(double sampleRate,
                                   unsigned int numPlaybackChannels,
                                   unsigned int numCaptureChannels,
                                   sampleFormat captureFormat)
{
#ifdef EXPERIMENTAL_MIDI_OUT
   mNumFrames = 0;
   mNumPauseFrames = 0;
   mPauseTime = 0;
#endif
   mLastPaError = paNoError;
   // pick a rate to do the audio I/O at, from those available. The project
   // rate is suggested, but we may get something else if it isn't supported
   mRate = GetBestRate(numCaptureChannels > 0, numPlaybackChannels > 0, sampleRate);
   if (mListener) {
      // advertise the chosen I/O sample rate to the UI
      mListener->OnAudioIORate((int)mRate);
   }
   
   // Special case: Our 24-bit sample format is different from PortAudio's
   // 3-byte packed format. So just make PortAudio return float samples,
   // since we need float values anyway to apply the gain.
   // ANSWER-ME: So we *never* actually handle 24-bit?! This causes mCapture to be set to floatSample below. 
   if (captureFormat == int24Sample)
      captureFormat = floatSample;

   mNumPlaybackChannels = numPlaybackChannels;
   mNumCaptureChannels = numCaptureChannels;

   PaStreamParameters *playbackParameters = NULL;
   PaStreamParameters *captureParameters = NULL;
   
   double latencyDuration = DEFAULT_LATENCY_DURATION;
   gPrefs->Read(wxT("/AudioIO/LatencyDuration"), &latencyDuration);

   if( numPlaybackChannels > 0)
   {
      playbackParameters = new PaStreamParameters;
      // this sets the device index to whatever is "right" based on preferences,
      // then defaults
      playbackParameters->device = getPlayDevIndex();
     
      const PaDeviceInfo *playbackDeviceInfo;
      playbackDeviceInfo = Pa_GetDeviceInfo( playbackParameters->device );
      
      if( playbackDeviceInfo == NULL )
      {
         delete playbackParameters;
         return false;
      }
      
      // regardless of source formats, we always mix to float
      playbackParameters->sampleFormat = paFloat32;
      playbackParameters->hostApiSpecificStreamInfo = NULL;
      playbackParameters->channelCount = mNumPlaybackChannels;

      if (mSoftwarePlaythrough)
         playbackParameters->suggestedLatency =
            playbackDeviceInfo->defaultLowOutputLatency;
      else
         playbackParameters->suggestedLatency = latencyDuration/1000.0;
   }

   if( numCaptureChannels > 0)
   {
      mCaptureFormat = captureFormat;
      
      captureParameters = new PaStreamParameters;
      const PaDeviceInfo *captureDeviceInfo;
      // retrieve the index of the device set in the prefs, or a sensible
      // default if it isn't set/valid
      captureParameters->device = getRecordDevIndex();

      captureDeviceInfo = Pa_GetDeviceInfo( captureParameters->device );

      if( captureDeviceInfo == NULL )
      {
         delete captureParameters;
         delete playbackParameters;
         return false;
      }

      captureParameters->sampleFormat =
         AudacityToPortAudioSampleFormat(mCaptureFormat);

      captureParameters->hostApiSpecificStreamInfo = NULL;
      captureParameters->channelCount = mNumCaptureChannels;

      if (mSoftwarePlaythrough)
         captureParameters->suggestedLatency =
            captureDeviceInfo->defaultHighInputLatency;
      else
         captureParameters->suggestedLatency = latencyDuration/1000.0;
   }
#ifdef EXPERIMENTAL_MIDI_OUT
   if (numPlaybackChannels == 0 && numCaptureChannels == 0) 
      return true;
#endif

#ifdef USE_PORTMIXER
#ifdef __WXMSW__
   //mchinen nov 30 2010.  For some reason Pa_OpenStream resets the input volume on windows.
   //so cache and restore after it.
   //The actual problem is likely in portaudio's pa_win_wmme.c OpenStream().
   float oldRecordVolume = Px_GetInputVolume(mPortMixer);
#endif
#endif
   mLastPaError = Pa_OpenStream( &mPortStreamV19,
                                 captureParameters, playbackParameters,
                                 mRate, paFramesPerBufferUnspecified,
                                 paNoFlag,
                                 audacityAudioCallback, NULL );

#if USE_PORTMIXER
#ifdef __WXMSW__
   Px_SetInputVolume(mPortMixer, oldRecordVolume);
#endif
   if (mPortStreamV19 != NULL && mLastPaError == paNoError) {
      #ifdef __WXMAC__
      if (mPortMixer) {
         if (Px_SupportsPlaythrough(mPortMixer)) {
            bool playthrough;

            mPreviousHWPlaythrough = Px_GetPlaythrough(mPortMixer);

            gPrefs->Read(wxT("/AudioIO/Playthrough"), &playthrough, false);
            if (playthrough)
               Px_SetPlaythrough(mPortMixer, 1.0);
            else
               Px_SetPlaythrough(mPortMixer, 0.0);
         }
      }
      #endif
   }
#endif

   // these may be null, but deleting a null pointer should never crash.
   delete captureParameters;
   delete playbackParameters;

   return (mLastPaError == paNoError);
}

void AudioIO::StartMonitoring(double sampleRate)
{
   if ( mPortStreamV19 || mStreamToken )
      return;

   bool success;
   long captureChannels;
   sampleFormat captureFormat = (sampleFormat)
      gPrefs->Read(wxT("/SamplingRate/DefaultProjectSampleFormat"), floatSample);
   gPrefs->Read(wxT("/AudioIO/RecordChannels"), &captureChannels, 2L);
   gPrefs->Read(wxT("/AudioIO/SWPlaythrough"), &mSoftwarePlaythrough, false);
   int playbackChannels = 0;

   if (mSoftwarePlaythrough)
      playbackChannels = 2;

   success = StartPortAudioStream(sampleRate, (unsigned int)playbackChannels,
                                  (unsigned int)captureChannels,
                                  captureFormat);
   // TODO: Check return value of success.
   (void)success;

   // Now start the PortAudio stream!
   mLastPaError = Pa_StartStream( mPortStreamV19 );
}

int AudioIO::StartStream(WaveTrackArray playbackTracks,
                         WaveTrackArray captureTracks,
#ifdef EXPERIMENTAL_MIDI_OUT
                         NoteTrackArray midiPlaybackTracks,
#endif
                         TimeTrack *timeTrack, double sampleRate,
                         double t0, double t1,
                         AudioIOListener* listener,
                         bool playLooped /* = false */,
                         double cutPreviewGapStart /* = 0.0 */,
                         double cutPreviewGapLen /* = 0.0 */)
{
   if( IsBusy() )
      return 0;

   // We just want to set mStreamToken to -1 - this way avoids
   // an extremely rare but possible race condition, if two functions
   // somehow called StartStream at the same time...
   mStreamToken--;
   if (mStreamToken != -1)
      return 0;

   // TODO: we don't really need to close and reopen stream if the
   // format matches; however it's kind of tricky to keep it open...
   //
   //   if (sampleRate == mRate &&
   //       playbackChannels == mNumPlaybackChannels &&
   //       captureChannels == mNumCaptureChannels &&
   //       captureFormat == mCaptureFormat) {

   if (mPortStreamV19) {
      StopStream();
      while(mPortStreamV19)
         wxMilliSleep( 50 );            
   }

   gPrefs->Read(wxT("/AudioIO/SWPlaythrough"), &mSoftwarePlaythrough, false);
   gPrefs->Read(wxT("/AudioIO/SoundActivatedRecord"), &mPauseRec, false);
   int silenceLevelDB;
   gPrefs->Read(wxT("/AudioIO/SilenceLevel"), &silenceLevelDB, -50);
   int dBRange;
   dBRange = gPrefs->Read(wxT("/GUI/EnvdBRange"), ENV_DB_RANGE);
   if(silenceLevelDB < -dBRange)
   {
      silenceLevelDB = -dBRange + 3;   // meter range was made smaller than SilenceLevel
      gPrefs->Write(wxT("/GUI/EnvdBRange"), dBRange); // so set SilenceLevel reasonable
      gPrefs->Flush();
   }
   mSilenceLevel = (silenceLevelDB + dBRange)/(double)dBRange;  // meter goes -dBRange dB -> 0dB

   mTimeTrack = timeTrack;
   mListener = listener;
   mInputMeter = NULL;
   mOutputMeter = NULL;
   mRate    = sampleRate;
   mT0      = t0;
   mT1      = t1;
   mTime    = t0;
   mSeek    = 0;
   mLastRecordingOffset = 0;
   mPlaybackTracks = playbackTracks;
   mCaptureTracks  = captureTracks;
#ifdef EXPERIMENTAL_MIDI_OUT
   mMidiPlaybackTracks = midiPlaybackTracks;
#endif
   mPlayLooped = playLooped;
   mCutPreviewGapStart = cutPreviewGapStart;
   mCutPreviewGapLen = cutPreviewGapLen;
   mPlaybackBuffers = NULL;
   mPlaybackMixers = NULL;
   mCaptureBuffers = NULL;
   mResample = NULL;

   // with ComputeWarpedLength, it is now possible the calculate the warped length with 100% accuracy
   // (ignoring accumulated rounding errors during playback) which fixes the 'missing sound at the end' bug
   mWarpedTime = 0.0;
   if(mTimeTrack)
      mWarpedLength = mTimeTrack->ComputeWarpedLength(mT0, mT1);
   else
      mWarpedLength = mT1 - mT0;

   //
   // The RingBuffer sizes, and the max amount of the buffer to
   // fill at a time, both grow linearly with the number of
   // tracks.  This allows us to scale up to many tracks without
   // killing performance.
   //

   mPlaybackRingBufferSecs = 10.0;
   mMaxPlaybackSecsToCopy = 4.0;

   mCaptureRingBufferSecs = 4.5 + 0.5 * std::min(size_t(16), mCaptureTracks.GetCount());
   mMinCaptureSecsToCopy = 0.2 + 0.2 * std::min(size_t(16), mCaptureTracks.GetCount());

   unsigned int playbackChannels = 0;
   unsigned int captureChannels = 0;
   sampleFormat captureFormat = floatSample;

   if( playbackTracks.GetCount() > 0 )
      playbackChannels = 2;

   if (mSoftwarePlaythrough)
      playbackChannels = 2;

   if( captureTracks.GetCount() > 0 )
   {
      // For capture, every input channel gets its own track
      captureChannels = mCaptureTracks.GetCount();
      // I don't deal with the possibility of the capture tracks
      // having different sample formats, since it will never happen
      // with the current code.  This code wouldn't *break* if this
      // assumption was false, but it would be sub-optimal.  For example,
      // if the first track was 16-bit and the second track was 24-bit,
      // we would set the sound card to capture in 16 bits and the second
      // track wouldn't get the benefit of all 24 bits the card is capable
      // of.
      captureFormat = mCaptureTracks[0]->GetSampleFormat();
      
      // Tell project that we are about to start recording
      if (mListener)
         mListener->OnAudioIOStartRecording();
   }

   bool successAudio;

   successAudio = StartPortAudioStream(sampleRate, playbackChannels,
                                       captureChannels, captureFormat);
#ifdef EXPERIMENTAL_MIDI_OUT

   // TODO: it may be that midi out will not work unless audio in or out is
   // active -- this would be a bug and may require a change in the 
   // logic here.

   bool successMidi = true;

   if(!mMidiPlaybackTracks.IsEmpty()){
      successMidi = StartPortMidiStream();
   }

   // On the other hand, if MIDI cannot be opened, we will not complain
#endif

   if (!successAudio) {
      if (mListener && captureChannels > 0)
         mListener->OnAudioIOStopRecording();
      mStreamToken = 0;
      return 0;
   }

   //
   // The (audio) stream has been opened successfully (assuming we tried
   // to open it). We now proceed to 
   // allocate the memory structures the stream will need.
   //

   bool bDone;
   do
   {
      bDone = true; // assume success
      try
      {
         if( mNumPlaybackChannels > 0 ) {
            // Allocate output buffers.  For every output track we allocate
            // a ring buffer of five seconds
            sampleCount playbackBufferSize =
               (sampleCount)(mRate * mPlaybackRingBufferSecs + 0.5f);
            sampleCount playbackMixBufferSize = 
               (sampleCount)(mRate * mMaxPlaybackSecsToCopy + 0.5f);

            // In the extraordinarily rare case that we can't even afford 100 samples, just give up.
            if(playbackBufferSize < 100 || playbackMixBufferSize < 100)
            {
               StartStreamCleanup();
               wxMessageBox(_("Out of memory!"));
               return 0;
            }

            mPlaybackBuffers = new RingBuffer* [mPlaybackTracks.GetCount()];
            mPlaybackMixers  = new Mixer*      [mPlaybackTracks.GetCount()];

            // Set everything to zero in case we have to delete these due to a memory exception.
            memset(mPlaybackBuffers, 0, sizeof(RingBuffer*)*mPlaybackTracks.GetCount());
            memset(mPlaybackMixers, 0, sizeof(Mixer*)*mPlaybackTracks.GetCount());

            for( unsigned int i = 0; i < mPlaybackTracks.GetCount(); i++ )
            {
               mPlaybackBuffers[i] = new RingBuffer(floatSample, playbackBufferSize);

               // MB: use normal time for the end time, not warped time!
               mPlaybackMixers[i]  = new Mixer(1, &mPlaybackTracks[i],
                                               mTimeTrack, mT0, mT1, 1,
                                               playbackMixBufferSize, false,
                                               mRate, floatSample, false);
               mPlaybackMixers[i]->ApplyTrackGains(false);
            }
         }

         if( mNumCaptureChannels > 0 )
         {
            // Allocate input buffers.  For every input track we allocate
            // a ring buffer of five seconds
            sampleCount captureBufferSize =
               (sampleCount)(mRate * mCaptureRingBufferSecs + 0.5);

            // In the extraordinarily rare case that we can't even afford 100 samples, just give up.
            if(captureBufferSize < 100)
            {
               StartStreamCleanup();
               wxMessageBox(_("Out of memory!"));
               return 0;
            }

            mCaptureBuffers = new RingBuffer* [mCaptureTracks.GetCount()];
            mResample = new Resample* [mCaptureTracks.GetCount()];
            mFactor = sampleRate / mRate;

            // Set everything to zero in case we have to delete these due to a memory exception.
            memset(mCaptureBuffers, 0, sizeof(RingBuffer*)*mCaptureTracks.GetCount());
            memset(mResample, 0, sizeof(Resample*)*mCaptureTracks.GetCount());

            for( unsigned int i = 0; i < mCaptureTracks.GetCount(); i++ )
            {
               mCaptureBuffers[i] = new RingBuffer( mCaptureTracks[i]->GetSampleFormat(),
                                                    captureBufferSize );
               mResample[i] = new Resample(true, mFactor, mFactor); // constant rate resampling
            }
         }
      }
      catch(std::bad_alloc&)
      {
         // Oops!  Ran out of memory.  This is pretty rare, so we'll just
         // try deleting everything, halving our buffer size, and try again.
         StartStreamCleanup(true);
         mPlaybackRingBufferSecs *= 0.5;
         mMaxPlaybackSecsToCopy *= 0.5;
         mCaptureRingBufferSecs *= 0.5;
         mMinCaptureSecsToCopy *= 0.5;
         bDone = false;
       }
   } while(!bDone);

#ifdef AUTOMATED_INPUT_LEVEL_ADJUSTMENT
   AILASetStartTime();
#endif

   // We signal the audio thread to call FillBuffers, to prime the RingBuffers
   // so that they will have data in them when the stream starts.  Having the
   // audio thread call FillBuffers here makes the code more predictable, since
   // FillBuffers will ALWAYS get called from the Audio thread.
   mAudioThreadShouldCallFillBuffersOnce = true;

   while( mAudioThreadShouldCallFillBuffersOnce == true )
      wxMilliSleep( 50 );

#ifdef EXPERIMENTAL_MIDI_OUT
   // if no playback, reset the midi time to zero to roughly sync
   // with recording (or if recording is not going to happen, just
   // reset time now so that time stamps increase from zero
   Pt_Stop();
   Pt_Start(1, NULL, NULL);
#endif

   if(mNumPlaybackChannels > 0 || mNumCaptureChannels > 0) {

      // Now start the PortAudio stream!
      PaError err;
      err = Pa_StartStream( mPortStreamV19 );

      if( err != paNoError )
      {
         if (mListener && mNumCaptureChannels > 0)
            mListener->OnAudioIOStopRecording();
         StartStreamCleanup();
         wxMessageBox(LAT1CTOWX(Pa_GetErrorText(err)));
         return 0;
      }
   }

   mAudioThreadFillBuffersLoopRunning = true;
#ifdef EXPERIMENTAL_MIDI_OUT
   // If audio is not running, mNumFrames will not be incremented and
   // MIDI will hang waiting for it unless we do it here.
   if (mNumPlaybackChannels + mNumCaptureChannels == 0) {
      mNumFrames = 1;
   }
#endif

   // Enable warning popups for unfound aliased blockfiles.
   wxGetApp().SetMissingAliasedFileWarningShouldShow(true);

   //
   // Generate an unique value each time, to be returned to
   // clients accessing the AudioIO API, so they can query if
   // are the ones who have reserved AudioIO or not.
   //
   mStreamToken = (++mNextStreamToken);

   return mStreamToken;
}

void AudioIO::StartStreamCleanup(bool bOnlyBuffers)
{
   if(mPlaybackBuffers)
   {
      for( unsigned int i = 0; i < mPlaybackTracks.GetCount(); i++ )
         delete mPlaybackBuffers[i];
      delete [] mPlaybackBuffers;
      mPlaybackBuffers = NULL;
   }

   if(mPlaybackMixers)
   {
      for( unsigned int i = 0; i < mPlaybackTracks.GetCount(); i++ )
         delete mPlaybackMixers[i];
      delete [] mPlaybackMixers;
      mPlaybackMixers = NULL;
   }

   if(mCaptureBuffers)
   {
      for( unsigned int i = 0; i < mCaptureTracks.GetCount(); i++ )
         delete mCaptureBuffers[i];
      delete [] mCaptureBuffers;
      mCaptureBuffers = NULL;
   }

   if(mResample)
   {
      for( unsigned int i = 0; i < mCaptureTracks.GetCount(); i++ )
         delete mResample[i];
      delete [] mResample;
      mResample = NULL;
   }

   if(!bOnlyBuffers)
   {
      Pa_AbortStream( mPortStreamV19 );
      Pa_CloseStream( mPortStreamV19 );
      mPortStreamV19 = NULL;
      mStreamToken = 0;
   }
}

#ifdef EXPERIMENTAL_MIDI_OUT

PmTimestamp MidiTime(void *info)
{
   return gAudioIO->MidiTime();
}

// Set up state to iterate NoteTrack events in sequence.
// Sends MIDI control changes up to the starting point mT0
// if send is true. Output is delayed by offset to facilitate
// looping (each iteration is delayed more).
void AudioIO::PrepareMidiIterator(bool send, double offset)
{
   int i;
   int nTracks = mMidiPlaybackTracks.GetCount();
   // instead of initializing with an Alg_seq, we use begin_seq()
   // below to add ALL Alg_seq's.
   mIterator = new Alg_iterator(NULL, false);
   // Iterator not yet intialized, must add each track...
   for (i = 0; i < nTracks; i++) {
      NoteTrack *t = mMidiPlaybackTracks[i];
      Alg_seq_ptr seq = t->GetSequence();
      // mark sequence tracks as "in use" since we're handing this
      // off to another thread and want to make sure nothing happens
      // to the data until playback finishes. This is just a sanity check.
      seq->set_in_use(true);
      mIterator->begin_seq(seq, t, t->GetOffset() + offset);
   }
   GetNextEvent(); // prime the pump for FillMidiBuffers

   // Start MIDI from current cursor position
   mSendMidiState = true;
   while (mNextEvent && 
          mNextEventTime < mT0 + offset) {
      if (send) OutputEvent();
      GetNextEvent();
   }
   mSendMidiState = false;
}

bool AudioIO::StartPortMidiStream() 
{
   int i; 
   int nTracks = mMidiPlaybackTracks.GetCount();
   // Only start MIDI stream if there is an open track
   if (nTracks == 0)
      return false;

   mMidiLatency = 1; // arbitrary, but small
   //printf("StartPortMidiStream: mT0 %g mTime %g\n", 
   //       gAudioIO->mT0, gAudioIO->mTime);

   /* get midi playback device */
   PmDeviceID playbackDevice = Pm_GetDefaultOutputDeviceID();
   wxString playbackDeviceName = gPrefs->Read(wxT("/MidiIO/PlaybackDevice"), 
                                              wxT(""));
   mSynthLatency = gPrefs->Read(wxT("/MidiIO/SynthLatency"), 
                                DEFAULT_SYNTH_LATENCY);
   if (wxStrcmp(playbackDeviceName, wxT("")) != 0) {
      for (i = 0; i < Pm_CountDevices(); i++) {
         const PmDeviceInfo *info = Pm_GetDeviceInfo(i);
         if (!info) continue;
         if (!info->output) continue;
         wxString interf(info->interf, wxConvLocal);
         wxString name(info->name, wxConvLocal);
         interf.Append(wxT(": ")).Append(name);
        if (wxStrcmp(interf, playbackDeviceName) == 0) {
            playbackDevice = i;
         }
      }
   } // (else playback device has Pm_GetDefaultOuputDeviceID())

   /* open output device */
   mLastPmError = Pm_OpenOutput(&mMidiStream, 
                                playbackDevice, 
                                NULL,
                                0, 
                                &::MidiTime,
                                NULL, 
                                mMidiLatency);
   if (mLastPmError == pmNoError) {
      mMidiStreamActive = true;
      mPauseTime = 0;
      mMidiPaused = false;
      mMidiLoopOffset = 0;
      mMidiOutputComplete = false;
      PrepareMidiIterator();

      // It is ok to call this now, but do not send timestamped midi
      // until after the first audio callback, which provides necessary
      // data for MidiTime().
      Pm_Synchronize(mMidiStream); // start using timestamps
      // start midi output flowing (pending first audio callback)
      mMidiThreadFillBuffersLoopRunning = true;
   }
   return (mLastPmError == pmNoError);      
}
#endif

void AudioIO::SetMeters(Meter *inputMeter, Meter *outputMeter)
{
   mInputMeter = inputMeter;
   mOutputMeter = outputMeter;

   if (mInputMeter)
      mInputMeter->Reset(mRate, true);
   if (mOutputMeter)
      mOutputMeter->Reset(mRate, true);

   AudacityProject* pProj = GetActiveProject();
   MixerBoard* pMixerBoard = pProj->GetMixerBoard();
   if (pMixerBoard)
      pMixerBoard->ResetMeters(true);

   mUpdateMeters = true;
}

void AudioIO::StopStream()
{
   if( mPortStreamV19 == NULL 
#ifdef EXPERIMENTAL_MIDI_OUT
       && mMidiStream == NULL 
#endif
     )
      return;

   if( Pa_IsStreamStopped( mPortStreamV19 ) 
#ifdef EXPERIMENTAL_MIDI_OUT
       && !mMidiStreamActive 
#endif
     )
      return;

   // Avoid race condition by making sure this function only
   // gets called once at a time
   mStopStreamCount++; // <- note that this is not atomic, therefore has
                       // a race condition -RBD
   if (mStopStreamCount != 1)
      return;

   //
   // We got here in one of two ways:
   //
   // 1. The user clicked the stop button and we therefore want to stop
   //    as quickly as possible.  So we use AbortStream().  If this is
   //    the case the portaudio stream is still in the Running state
   //    (see PortAudio state machine docs).
   //
   // 2. The callback told PortAudio to stop the stream since it had
   //    reached the end of the selection.  The UI thread discovered
   //    this by noticing that AudioIO::IsActive() returned false.
   //    IsActive() (which calls Pa_GetStreamActive()) will not return
   //    false until all buffers have finished playing, so we can call
   //    AbortStream without losing any samples.  If this is the case
   //    we are in the "callback finished state" (see PortAudio state
   //    machine docs).
   //
   // The moral of the story: We can call AbortStream safely, without
   // losing samples.
   //
   // DMM: This doesn't seem to be true; it seems to be necessary to
   // call StopStream if the callback brought us here, and AbortStream
   // if the user brought us here.
   //

   mAudioThreadFillBuffersLoopRunning = false;

   // Audacity can deadlock if it tries to update meters while
   // we're stopping PortAudio (because the meter updating code
   // tries to grab a UI mutex while PortAudio tries to join a
   // pthread).  So we tell the callback to stop updating meters,
   // and wait until the callback has left this part of the code
   // if it was already there.
   mUpdateMeters = false;
   while(mUpdatingMeters) {
      ::wxSafeYield();
      wxMilliSleep( 50 );
   }

   // Turn off HW playthrough if PortMixer is being used

  #if defined(USE_PORTMIXER)
   if( mPortMixer ) {
      #if __WXMAC__
      if (Px_SupportsPlaythrough(mPortMixer) && mPreviousHWPlaythrough >= 0.0)
         Px_SetPlaythrough(mPortMixer, mPreviousHWPlaythrough);
         mPreviousHWPlaythrough = -1.0;
      #endif
   }
  #endif

   if (mPortStreamV19) {
      Pa_AbortStream( mPortStreamV19 );
      Pa_CloseStream( mPortStreamV19 );
      mPortStreamV19 = NULL;
   }


#ifdef EXPERIMENTAL_MIDI_OUT
   /* Stop Midi playback */
   if ( mMidiStream ) {
      mMidiStreamActive = false;
      mMidiThreadFillBuffersLoopRunning = false; // stop output to stream
      // but output is in another thread. Wait for output to stop...
      while (mMidiThreadFillBuffersLoopActive) {
         wxMilliSleep(1);
      }
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
      // the stream. It should take about 16ms to send All Off messages,
      // so this will add 24ms latency.
      wxMilliSleep(40); // deliver the all-off messages
      Pm_Close(mMidiStream);
      mMidiStream = NULL;
      mIterator->end();

      // set in_use flags to false
      int nTracks = mMidiPlaybackTracks.GetCount();
      for (int i = 0; i < nTracks; i++) {
         NoteTrack *t = mMidiPlaybackTracks[i];
         Alg_seq_ptr seq = t->GetSequence();
         seq->set_in_use(false);
      }

      delete mIterator;
      mIterator = NULL; // just in case someone tries to reference it
      mMidiPlaySpeed = 1.0;
   }
#endif

   // If there's no token, we were just monitoring, so we can
   // skip this next part...
   if (mStreamToken > 0) {
      // In either of the above cases, we want to make sure that any
      // capture data that made it into the PortAudio callback makes it
      // to the target WaveTrack.  To do this, we ask the audio thread to
      // call FillBuffers one last time (it normally would not do so since
      // Pa_GetStreamActive() would now return false
      mAudioThreadShouldCallFillBuffersOnce = true;

      while( mAudioThreadShouldCallFillBuffersOnce == true )
      {
         // LLL:  Experienced recursive yield here...once.
         wxGetApp().Yield(true); // Pass true for onlyIfNeeded to avoid recursive call error.
         wxMilliSleep( 50 );
      }

      //
      // Everything is taken care of.  Now, just free all the resources
      // we allocated in StartStream()
      //
      
      if( mPlaybackTracks.GetCount() > 0 )
      {
         for( unsigned int i = 0; i < mPlaybackTracks.GetCount(); i++ )
         {
            delete mPlaybackBuffers[i];
            delete mPlaybackMixers[i];
         }
         
         delete[] mPlaybackBuffers;
         delete[] mPlaybackMixers;
      }

      //
      // Offset all recorded tracks to account for latency
      //
      if( mCaptureTracks.GetCount() > 0 )
      {
         //
         // We only apply latency correction when we actually played back
         // tracks during the recording. If we did not play back tracks,
         // there's nothing we could be out of sync with. This also covers the
         // case that we do not apply latency correction when recording the
         // first track in a project.
         //
         double latencyCorrection = DEFAULT_LATENCY_CORRECTION;
         gPrefs->Read(wxT("/AudioIO/LatencyCorrection"), &latencyCorrection);
         
         double recordingOffset =
            mLastRecordingOffset + latencyCorrection / 1000.0;

         for( unsigned int i = 0; i < mCaptureTracks.GetCount(); i++ )
            {
               delete mCaptureBuffers[i];
               delete mResample[i];
               
               WaveTrack* track = mCaptureTracks[i];
               track->Flush();

               if (mPlaybackTracks.GetCount() > 0)
               {  // only do latency correction if some tracks are being played back
                  WaveTrackArray playbackTracks;
                  AudacityProject *p = GetActiveProject();
                  // we need to get this as mPlaybackTracks does not contain tracks being recorded into
                  playbackTracks = p->GetTracks()->GetWaveTrackArray(false);
                  bool appendRecord = false;
                  for( unsigned int j = 0; j < playbackTracks.GetCount(); j++)
                  {  // find if we are recording into an existing track (append-record)
                     WaveTrack* trackP = playbackTracks[j];
                     if( track == trackP )
                     {
                        if( track->GetStartTime() != mT0 )  // in a new track if these are equal
                        {
                           appendRecord = true;
                           break;
                        }
                     }
                  }
                  if( appendRecord )
                  {  // append-recording
                     bool bResult = true;
                     if (recordingOffset < 0)
                        bResult = track->Clear(mT0, mT0 - recordingOffset); // cut the latency out
                     else
                        bResult = track->InsertSilence(mT0, recordingOffset); // put silence in
                     wxASSERT(bResult); // TO DO: Actually handle this.
                  }
                  else
                  {  // recording into a new track
                     track->SetOffset(track->GetStartTime() + recordingOffset);
                     if(track->GetEndTime() < 0.)
                     {
                        wxMessageDialog m(NULL, _("Latency Correction setting has caused the recorded audio to be hidden before zero.\nAudacity has brought it back to start at zero.\nYou may have to use the Time Shift Tool (<---> or F5) to drag the track to the right place."),
                           _("Latency problem"), wxOK);
                        m.ShowModal();
                        track->SetOffset(0.);
                     }
                  }
               }
            }
         
         delete[] mCaptureBuffers;
         delete[] mResample;
      }
   }

   if (mInputMeter)
      mInputMeter->Reset(mRate, false);
   if (mOutputMeter)
      mOutputMeter->Reset(mRate, false);

   AudacityProject* pProj = GetActiveProject();
   MixerBoard* pMixerBoard = pProj->GetMixerBoard();
   if (pMixerBoard)
      pMixerBoard->ResetMeters(false);

   if (mListener && mNumCaptureChannels > 0)
      mListener->OnAudioIOStopRecording();
      
   //
   // Only set token to 0 after we're totally finished with everything
   //
   mStreamToken = 0;
   mStopStreamCount = 0;
}

void AudioIO::SetPaused(bool state)
{
   mPaused = state;
}

bool AudioIO::IsPaused()
{
   return mPaused;
}

bool AudioIO::IsBusy()
{
   if (mStreamToken != 0)
      return true;

   return false;
}

bool AudioIO::IsStreamActive()
{
   bool isActive = false;
   if( mPortStreamV19 )
      isActive = (Pa_IsStreamActive( mPortStreamV19 ) > 0);

#ifdef EXPERIMENTAL_MIDI_OUT
   if( mMidiStreamActive && !mMidiOutputComplete )
      isActive = true;
#endif
   return isActive;
}

bool AudioIO::IsStreamActive(int token)
{
   return (this->IsStreamActive() && this->IsAudioTokenActive(token));
}

bool AudioIO::IsAudioTokenActive(int token)
{
   return ( token > 0 && token == mStreamToken );
}

bool AudioIO::IsMonitoring()
{
   return ( mPortStreamV19 && mStreamToken==0 );
}

double AudioIO::NormalizeStreamTime(double absoluteTime) const
{
   // dmazzoni: This function is needed for two reasons:
   // One is for looped-play mode - this function makes sure that the
   // position indicator keeps wrapping around.  The other reason is
   // more subtle - it's because PortAudio can query the hardware for
   // the current stream time, and this query is not always accurate.
   // Sometimes it's a little behind or ahead, and so this function
   // makes sure that at least we clip it to the selection.
   //
   // msmeyer: There is also the possibility that we are using "cut preview"
   //          mode. In this case, we should jump over a defined "gap" in the
   //          audio.

   // msmeyer: Just to be sure, the returned stream time should
   //          never be smaller than the actual start time.
   if (absoluteTime < mT0)
      absoluteTime = mT0;

   if (absoluteTime > mT1)
      absoluteTime = mT1;
   
   if (mCutPreviewGapLen > 0)
   {
      // msmeyer: We're in cut preview mode, so if we are on the right
      // side of the gap, we jump over it.
      if (absoluteTime > mCutPreviewGapStart)
         absoluteTime += mCutPreviewGapLen;
   }

   return absoluteTime;
}

double AudioIO::GetStreamTime()
{
   if( !IsStreamActive() )
      return BAD_STREAM_TIME;

   return NormalizeStreamTime(mTime);
}


wxArrayLong AudioIO::GetSupportedPlaybackRates(int devIndex, double rate)
{
   if (devIndex == -1)
   {  // weren't given a device index, get the prefs / default one
      devIndex = getPlayDevIndex();
   }

   // Check if we can use the cached rates
   if (mCachedPlaybackIndex != -1 && devIndex == mCachedPlaybackIndex
         && (rate == 0.0 || mCachedPlaybackRates.Index(rate) != wxNOT_FOUND))
   {
      return mCachedPlaybackRates;
   }

   wxArrayLong supported;
   int irate = (int)rate;
   const PaDeviceInfo* devInfo = NULL;
   int i;

   devInfo = Pa_GetDeviceInfo(devIndex);
   
   if (!devInfo)
   {
      wxLogDebug(wxT("GetSupportedPlaybackRates() Could not get device info!"));
      return supported;
   }

   // LLL: Remove when a proper method of determining actual supported
   //      DirectSound rate is devised.
   const PaHostApiInfo* hostInfo = Pa_GetHostApiInfo(devInfo->hostApi);
   bool isDirectSound = (hostInfo && hostInfo->type == paDirectSound);

   PaStreamParameters pars;

   pars.device = devIndex;
   pars.channelCount = 1;
   pars.sampleFormat = paFloat32;
   pars.suggestedLatency = devInfo->defaultHighOutputLatency;
   pars.hostApiSpecificStreamInfo = NULL;
   
   for (i = 0; i < NumRatesToTry; i++)
   {
      // LLL: Remove when a proper method of determining actual supported
      //      DirectSound rate is devised.
      if (!(isDirectSound && RatesToTry[i] > 200000))
      if (Pa_IsFormatSupported(NULL, &pars, RatesToTry[i]) == 0)
         supported.Add(RatesToTry[i]);
   }

   if (irate != 0 && supported.Index(irate) == wxNOT_FOUND)
   {
      // LLL: Remove when a proper method of determining actual supported
      //      DirectSound rate is devised.
      if (!(isDirectSound && RatesToTry[i] > 200000))
      if (Pa_IsFormatSupported(NULL, &pars, irate) == 0)
         supported.Add(irate);
   }

   return supported;
}

wxArrayLong AudioIO::GetSupportedCaptureRates(int devIndex, double rate)
{
   if (devIndex == -1)
   {  // not given a device, look up in prefs / default
      devIndex = getRecordDevIndex();
   }

   // Check if we can use the cached rates
   if (mCachedCaptureIndex != -1 && devIndex == mCachedCaptureIndex
         && (rate == 0.0 || mCachedCaptureRates.Index(rate) != wxNOT_FOUND))
   {
      return mCachedCaptureRates;
   }

   wxArrayLong supported;
   int irate = (int)rate;
   const PaDeviceInfo* devInfo = NULL;
   int i;

   devInfo = Pa_GetDeviceInfo(devIndex);

   if (!devInfo)
   {
      wxLogDebug(wxT("GetSupportedCaptureRates() Could not get device info!"));
      return supported;
   }

   double latencyDuration = DEFAULT_LATENCY_DURATION;
   long recordChannels = 1;
   gPrefs->Read(wxT("/AudioIO/LatencyDuration"), &latencyDuration);
   gPrefs->Read(wxT("/AudioIO/RecordChannels"), &recordChannels);

   // LLL: Remove when a proper method of determining actual supported
   //      DirectSound rate is devised.
   const PaHostApiInfo* hostInfo = Pa_GetHostApiInfo(devInfo->hostApi);
   bool isDirectSound = (hostInfo && hostInfo->type == paDirectSound);

   PaStreamParameters pars;

   pars.device = devIndex;
   pars.channelCount = recordChannels;
   pars.sampleFormat = paFloat32;
   pars.suggestedLatency = latencyDuration / 1000.0;
   pars.hostApiSpecificStreamInfo = NULL;
   
   for (i = 0; i < NumRatesToTry; i++)
   {
      // LLL: Remove when a proper method of determining actual supported
      //      DirectSound rate is devised.
      if (!(isDirectSound && RatesToTry[i] > 200000))
      if (Pa_IsFormatSupported(&pars, NULL, RatesToTry[i]) == 0)
         supported.Add(RatesToTry[i]);
   }

   if (irate != 0 && supported.Index(irate) == wxNOT_FOUND)
   {
      // LLL: Remove when a proper method of determining actual supported
      //      DirectSound rate is devised.
      if (!(isDirectSound && RatesToTry[i] > 200000))
      if (Pa_IsFormatSupported(&pars, NULL, irate) == 0)
         supported.Add(irate);
   }

   return supported;
}

wxArrayLong AudioIO::GetSupportedSampleRates(int playDevice, int recDevice, double rate)
{
   // Not given device indices, look up prefs
   if (playDevice == -1) {
      playDevice = getPlayDevIndex();
   }
   if (recDevice == -1) {
      recDevice = getRecordDevIndex();
   }

   // Check if we can use the cached rates
   if (mCachedPlaybackIndex != -1 && mCachedCaptureIndex != -1 && 
         playDevice == mCachedPlaybackIndex &&
         recDevice == mCachedCaptureIndex &&
         (rate == 0.0 || mCachedSampleRates.Index(rate) != wxNOT_FOUND))
   {
      return mCachedSampleRates;
   }

   wxArrayLong playback = GetSupportedPlaybackRates(playDevice, rate);
   wxArrayLong capture = GetSupportedCaptureRates(recDevice, rate);
   int i;

   // Return only sample rates which are in both arrays
   wxArrayLong result;

   for (i = 0; i < (int)playback.GetCount(); i++)
      if (capture.Index(playback[i]) != wxNOT_FOUND)
         result.Add(playback[i]);

   // If this yields no results, use the default sample rates nevertheless
/*   if (result.IsEmpty())
   {
      for (i = 0; i < NumStandardRates; i++)
         result.Add(StandardRates[i]);
   }*/

   return result;
}

/** \todo: should this take into account PortAudio's value for 
 * PaDeviceInfo::defaultSampleRate? In principal this should let us work out
 * which rates are "real" and which resampled in the drivers, and so prefer
 * the real rates. */
int AudioIO::GetOptimalSupportedSampleRate()
{
   wxArrayLong rates = GetSupportedSampleRates();

   if (rates.Index(44100) != wxNOT_FOUND)
      return 44100;

   if (rates.Index(48000) != wxNOT_FOUND)
      return 48000;

   // if there are no supported rates, the next bit crashes. So check first,
   // and give them a "sensible" value if there are no valid values. They
   // will still get an error later, but with any luck may have changed
   // something by then. It's no worse than having an invalid default rate
   // stored in the preferences, which we don't check for
   if (rates.IsEmpty()) return 44100;

   return rates[rates.GetCount() - 1];
}

double AudioIO::GetBestRate(bool capturing, bool playing, double sampleRate)
{
   // Check if we can use the cached value
   if (mCachedBestRateIn != 0.0 && mCachedBestRateIn == sampleRate) {
      return mCachedBestRateOut;
   }

   // In order to cache the value, all early returns should instead set retval
   // and jump to finished
   double retval;

   wxArrayLong rates;
   if (capturing) wxLogDebug(wxT("AudioIO::GetBestRate() for capture"));
   if (playing) wxLogDebug(wxT("AudioIO::GetBestRate() for playback"));
   wxLogDebug(wxT("GetBestRate() suggested rate %.0lf Hz"), sampleRate);

   if (capturing && !playing) {
      rates = GetSupportedCaptureRates(-1, sampleRate);
   }
   else if (playing && !capturing) {
      rates = GetSupportedPlaybackRates(-1, sampleRate);
   }
   else {   // we assume capturing and playing - the alternative would be a 
            // bit odd
      rates = GetSupportedSampleRates(-1, -1, sampleRate);
   }
   /* rem rates is the array of hardware-supported sample rates (in the current
    * configuration), sampleRate is the Project Rate (desired sample rate) */
   long rate = (long)sampleRate;
   
   if (rates.Index(rate) != wxNOT_FOUND) {
      wxLogDebug(wxT("GetBestRate() Returning %.0ld Hz"), rate);
      retval = rate;
      goto finished;
      /* the easy case - the suggested rate (project rate) is in the list, and
       * we can just accept that and send back to the caller. This should be
       * the case for most users most of the time (all of the time on
       * Win MME as the OS does resampling) */
   }

   /* if we get here, there is a problem - the project rate isn't supported
    * on our hardware, so we can't us it. Need to come up with an alternative
    * rate to use. The process goes like this:
    * * If there are no rates to pick from, we're stuck and return 0 (error)
    * * If there are some rates, we pick the next one higher than the requested
    *   rate to use.
    * * If there aren't any higher, we use the highest available rate */

   if (rates.IsEmpty()) {
      /* we're stuck - there are no supported rates with this hardware. Error */
      wxLogDebug(wxT("GetBestRate() Error - no supported sample rates"));
      retval = 0.0;
      goto finished;
   }
   int i;
   for (i = 0; i < (int)rates.GetCount(); i++)  // for each supported rate
         {
         if (rates[i] > rate) {
            // supported rate is greater than requested rate
            wxLogDebug(wxT("GetBestRate() Returning next higher rate - %.0ld Hz"), rates[i]);
            retval = rates[i];
            goto finished;
         }
         }

   wxLogDebug(wxT("GetBestRate() Returning highest rate - %.0ld Hz"), rates[rates.GetCount() - 1]);
   retval = rates[rates.GetCount() - 1]; // the highest available rate
   goto finished;

finished:
   mCachedBestRateIn = sampleRate;
   mCachedBestRateOut = retval;
   return retval;
}      


//////////////////////////////////////////////////////////////////////
//
//     Audio Thread Context
//
//////////////////////////////////////////////////////////////////////

AudioThread::ExitCode AudioThread::Entry()
{
   while( !TestDestroy() )
   {
      // Set LoopActive outside the tests to avoid race condition
      gAudioIO->mAudioThreadFillBuffersLoopActive = true;
      if( gAudioIO->mAudioThreadShouldCallFillBuffersOnce )
      {
         gAudioIO->FillBuffers();
         gAudioIO->mAudioThreadShouldCallFillBuffersOnce = false;
      }
      else if( gAudioIO->mAudioThreadFillBuffersLoopRunning )
      {
         gAudioIO->FillBuffers();
      }
      gAudioIO->mAudioThreadFillBuffersLoopActive = false;

      Sleep(10);
   }

   return 0;
}


#ifdef EXPERIMENTAL_MIDI_OUT
MidiThread::ExitCode MidiThread::Entry()
{
   long pauseStart = 0;
   while( !TestDestroy() )
   {
      // Set LoopActive outside the tests to avoid race condition
      gAudioIO->mMidiThreadFillBuffersLoopActive = true;
      if( gAudioIO->mMidiThreadFillBuffersLoopRunning &&
          // mNumFrames signals at least one callback, needed for MidiTime()
          gAudioIO->mNumFrames > 0)
      {
         // Keep track of time paused. If not paused, fill buffers.
         if (gAudioIO->IsPaused()) {
            if (!gAudioIO->mMidiPaused) {
               gAudioIO->mMidiPaused = true;
               gAudioIO->AllNotesOff(); // to avoid hanging notes during pause
               pauseStart = MidiTime(NULL);
            }
         } else {
            if (gAudioIO->mMidiPaused) {
               gAudioIO->mMidiPaused = false;
               // note: mPauseTime ignored if audio is playing
               gAudioIO->mPauseTime += (MidiTime(NULL) - pauseStart);
            }

            gAudioIO->FillMidiBuffers();

            // test for end
            double realTime = gAudioIO->mT0 + gAudioIO->MidiTime() * 0.001 - 
                               gAudioIO->PauseTime();
            if (gAudioIO->mNumPlaybackChannels != 0) {
               realTime -= 1; // with audio, MidiTime() runs ahead 1s
            }
            // The TrackPanel::OnTimer() method updates the time position 
            // indicator every 200ms, so it tends to not advance the 
            // indicator to the end of the selection (mT1) but instead stop
            // up to 200ms before the end. At this point, output is shut
            // down and the indicator is removed, but for a brief time, the
            // indicator is clearly stopped before reaching mT1. To avoid
            // this, we do not set mMidiOutputComplete until we are actually
            // 0.22s beyond mT1 (even though we stop playing at mT1. This
            // gives OnTimer() time to wake up and draw the final time 
            // position at mT1 before shutting down the stream.
            double timeAtSpeed = (realTime - gAudioIO->mT0) * 
                                 gAudioIO->mMidiPlaySpeed + gAudioIO->mT0;

            gAudioIO->mMidiOutputComplete = 
               (!gAudioIO->mPlayLooped && timeAtSpeed >= gAudioIO->mT1 + 0.220);
            // !gAudioIO->mNextEvent);
         }
      }
      gAudioIO->mMidiThreadFillBuffersLoopActive = false;
      Sleep(MIDI_SLEEP);
   }
   return 0;
}
#endif

int AudioIO::GetCommonlyAvailPlayback()
{
   int commonlyAvail = mPlaybackBuffers[0]->AvailForPut();
   unsigned int i;

   for( i = 1; i < mPlaybackTracks.GetCount(); i++ )
   {
      int thisBlockAvail = mPlaybackBuffers[i]->AvailForPut();

      if( thisBlockAvail < commonlyAvail )
         commonlyAvail = thisBlockAvail;
   }

   return commonlyAvail;
}

int AudioIO::GetCommonlyAvailCapture()
{
   int commonlyAvail = mCaptureBuffers[0]->AvailForGet();
   unsigned int i;

   for( i = 1; i < mCaptureTracks.GetCount(); i++ )
   {
      int avail = mCaptureBuffers[i]->AvailForGet();
      if( avail < commonlyAvail )
         commonlyAvail = avail;
   }

   return commonlyAvail;
}

#if USE_PORTMIXER
int AudioIO::getRecordSourceIndex(PxMixer *portMixer)
{
   int i;
   wxString sourceName = gPrefs->Read(wxT("/AudioIO/RecordingSource"), wxT(""));
   int numSources = Px_GetNumInputSources(portMixer);
   for (i = 0; i < numSources; i++) {
      if (sourceName == wxString(Px_GetInputSourceName(portMixer, i), wxConvLocal))
         return i;
   }
   return -1;
}
#endif

int AudioIO::getPlayDevIndex(wxString devName)
{
   // if we don't get given a device, look up the preferences
   if (devName.IsEmpty())
   {
      devName = gPrefs->Read(wxT("/AudioIO/PlaybackDevice"), wxT(""));
   }

   wxString hostName = gPrefs->Read(wxT("/AudioIO/Host"), wxT(""));
   PaHostApiIndex hostCnt = Pa_GetHostApiCount();
   PaHostApiIndex hostNum;
   for (hostNum = 0; hostNum < hostCnt; hostNum++)
   {
      const PaHostApiInfo *hinfo = Pa_GetHostApiInfo(hostNum);
      if (hinfo && wxString(hinfo->name, wxConvLocal) == hostName)
      {
         for (PaDeviceIndex hostDevice = 0; hostDevice < hinfo->deviceCount; hostDevice++)
         {
            PaDeviceIndex deviceNum = Pa_HostApiDeviceIndexToDeviceIndex(hostNum, hostDevice);

            const PaDeviceInfo *dinfo = Pa_GetDeviceInfo(deviceNum);
            if (dinfo && DeviceName(dinfo) == devName && dinfo->maxOutputChannels > 0 )
            {
               // this device name matches the stored one, and works.
               // So we say this is the answer and return it
               return deviceNum;
            }
         }

         // The device wasn't found so use the default for this host.
         // LL:  At this point, preferences and active no longer match.
         return hinfo->defaultOutputDevice;
      }
   }

   // The host wasn't found, so use the default output device.
   PaDeviceIndex deviceNum = Pa_GetDefaultOutputDevice();

   // Sometimes PortAudio returns -1 if it cannot find a suitable default
   // device, so we just use the first one available
   //
   // LL:  At this point, preferences and active no longer match
   //
   //      And I can't imagine how far we'll get specifying an "invalid" index later
   //      on...are we certain "0" even exists?
   if (deviceNum < 0) {
      wxASSERT(false);
      deviceNum = 0;
   }

   return deviceNum;
}

int AudioIO::getRecordDevIndex(wxString devName)
{
   // if we don't get given a device, look up the preferences
   if (devName.IsEmpty())
   {
      devName = gPrefs->Read(wxT("/AudioIO/RecordingDevice"), wxT(""));
   }

   wxString hostName = gPrefs->Read(wxT("/AudioIO/Host"), wxT(""));
   PaHostApiIndex hostCnt = Pa_GetHostApiCount();
   PaHostApiIndex hostNum;
   for (hostNum = 0; hostNum < hostCnt; hostNum++)
   {
      const PaHostApiInfo *hinfo = Pa_GetHostApiInfo(hostNum);
      if (hinfo && wxString(hinfo->name, wxConvLocal) == hostName)
      {
         for (PaDeviceIndex hostDevice = 0; hostDevice < hinfo->deviceCount; hostDevice++)
         {
            PaDeviceIndex deviceNum = Pa_HostApiDeviceIndexToDeviceIndex(hostNum, hostDevice);

            const PaDeviceInfo *dinfo = Pa_GetDeviceInfo(deviceNum);
            if (dinfo && DeviceName(dinfo) == devName && dinfo->maxInputChannels > 0 )
            {
               // this device name matches the stored one, and works.
               // So we say this is the answer and return it
               return deviceNum;
            }
         }

         // The device wasn't found so use the default for this host.
         // LL:  At this point, preferences and active no longer match.
         return hinfo->defaultInputDevice;
      }
   }

   // The host wasn't found, so use the default input device.
   PaDeviceIndex deviceNum = Pa_GetDefaultInputDevice();

   // Sometimes PortAudio returns -1 if it cannot find a suitable default
   // device, so we just use the first one available
   //
   // LL:  At this point, preferences and active no longer match
   //
   //      And I can't imagine how far we'll get specifying an "invalid" index later
   //      on...are we certain "0" even exists?
   if (deviceNum < 0) {
      wxASSERT(false);
      deviceNum = 0;
   }

   return deviceNum;
}

wxString AudioIO::GetDeviceInfo()
{
   wxStringOutputStream o;
   wxTextOutputStream s(o, wxEOL_UNIX);
   wxString e(wxT("\n"));

   if (IsStreamActive()) {
      return wxT("Stream is active ... unable to gather information.");
   }


   int recDeviceNum = Pa_GetDefaultInputDevice();
   int playDeviceNum = Pa_GetDefaultOutputDevice();

   int cnt = Pa_GetDeviceCount();

   wxLogDebug(wxT("Portaudio reports %d audio devices"),cnt);

   s << wxT("==============================") << e;
   s << wxT("Default capture device number: ") << recDeviceNum << e;
   s << wxT("Default playback device number: ") << playDeviceNum << e;

   wxString recDevice = gPrefs->Read(wxT("/AudioIO/RecordingDevice"), wxT(""));
   wxString playDevice = gPrefs->Read(wxT("/AudioIO/PlaybackDevice"), wxT(""));
   int j;

   // This gets info on all available audio devices (input and output)
   if (cnt <= 0) {
      s << wxT("No devices found\n");
      return o.GetString();
   }

   const PaDeviceInfo* info;
     
   for (j = 0; j < cnt; j++) {
      s << wxT("==============================") << e;

      info = Pa_GetDeviceInfo(j);
      if (!info) {
         s << wxT("Device info unavailable for: ") << j << wxT("\n");
         continue;
      }

      wxString name = DeviceName(info);
      s << wxT("Device ID: ") << j << e;
      s << wxT("Device name: ") << name << e;
      s << wxT("Host name: ") << HostName(info) << e;
      s << wxT("Input channels: ") << info->maxInputChannels << e;
      s << wxT("Output channels: ") << info->maxOutputChannels << e;
      s << wxT("Low Input Latency: ") << info->defaultLowInputLatency << e;
      s << wxT("Low Output Latency: ") << info->defaultLowOutputLatency << e;
      s << wxT("High Input Latency: ") << info->defaultHighInputLatency << e;
      s << wxT("High Output Latency: ") << info->defaultHighOutputLatency << e;

      wxArrayLong rates = GetSupportedPlaybackRates(j, 0.0);

      s << wxT("Supported Rates:") << e;
      for (int k = 0; k < (int) rates.GetCount(); k++) {
         s << wxT("    ") << (int)rates[k] << e;
      }

      if (name == playDevice && info->maxOutputChannels > 0)
         playDeviceNum = j;

      if (name == recDevice && info->maxInputChannels > 0)
         recDeviceNum = j;

      // Sometimes PortAudio returns -1 if it cannot find a suitable default
      // device, so we just use the first one available
      if (recDeviceNum < 0 && info->maxInputChannels > 0){
         recDeviceNum = j;
      }
      if (playDeviceNum < 0 && info->maxOutputChannels > 0){
         playDeviceNum = j;
      }
   }

   bool haveRecDevice = (recDeviceNum >= 0);
   bool havePlayDevice = (playDeviceNum >= 0);

   s << wxT("==============================") << e;
   if(haveRecDevice){
      s << wxT("Selected capture device: ") << recDeviceNum << wxT(" - ") << recDevice << e;
   }else{
      s << wxT("No capture device found.") << e;
   }
   if(havePlayDevice){
      s << wxT("Selected playback device: ") << playDeviceNum << wxT(" - ") << playDevice << e;
   }else{
      s << wxT("No playback device found.") << e;
   }   

   wxArrayLong supportedSampleRates;

   if(havePlayDevice && haveRecDevice){
      supportedSampleRates = GetSupportedSampleRates(playDeviceNum, recDeviceNum);

      s << wxT("Supported Rates:") << e;
      for (int k = 0; k < (int) supportedSampleRates.GetCount(); k++) {
         s << wxT("    ") << (int)supportedSampleRates[k] << e;
      }
   }else{
      s << wxT("Cannot check mutual sample rates without both devices.") << e;
      return o.GetString();
   }

#if defined(USE_PORTMIXER)
   if (supportedSampleRates.GetCount() > 0)
      {
      int highestSampleRate = supportedSampleRates[supportedSampleRates.GetCount() - 1];
      bool EmulateMixerInputVol = true;
      bool EmulateMixerOutputVol = true;
      float MixerInputVol = 1.0;
      float MixerOutputVol = 1.0;

      int error;

      PaStream *stream;
   
      PaStreamParameters playbackParameters;

      playbackParameters.device = playDeviceNum;
      playbackParameters.sampleFormat = paFloat32;
      playbackParameters.hostApiSpecificStreamInfo = NULL;
      playbackParameters.channelCount = 1;
      if (Pa_GetDeviceInfo(playDeviceNum)){
         playbackParameters.suggestedLatency =
            Pa_GetDeviceInfo(playDeviceNum)->defaultLowOutputLatency;
      }
      else{
         playbackParameters.suggestedLatency = DEFAULT_LATENCY_CORRECTION/1000.0; 
      }

      PaStreamParameters captureParameters;
 
      captureParameters.device = recDeviceNum;
      captureParameters.sampleFormat = paFloat32;;
      captureParameters.hostApiSpecificStreamInfo = NULL;
      captureParameters.channelCount = 1;
      if (Pa_GetDeviceInfo(recDeviceNum)){
         captureParameters.suggestedLatency =
            Pa_GetDeviceInfo(recDeviceNum)->defaultLowInputLatency;
      }else{
         captureParameters.suggestedLatency = DEFAULT_LATENCY_CORRECTION/1000.0; 
      }

      error = Pa_OpenStream(&stream,
                         &captureParameters, &playbackParameters,
                         highestSampleRate, paFramesPerBufferUnspecified,
                         paClipOff | paDitherOff,
                         audacityAudioCallback, NULL);

      if (error) {
         error = Pa_OpenStream(&stream,
                            &captureParameters, NULL,
                            highestSampleRate, paFramesPerBufferUnspecified,
                            paClipOff | paDitherOff,
                            audacityAudioCallback, NULL);
      }

      if (error) {
         s << wxT("Recieved ") << error << wxT(" while opening devices") << e;
         return o.GetString();
      }

      PxMixer *PortMixer = Px_OpenMixer(stream, 0);

      if (!PortMixer) {
         s << wxT("Unable to open Portmixer") << e;
         Pa_CloseStream(stream);
         return o.GetString();
      }

      s << wxT("==============================") << e;
      s << wxT("Available mixers:") << e;

      cnt = Px_GetNumMixers(stream);
      for (int i = 0; i < cnt; i++) {
         wxString name(Px_GetMixerName(stream, i), wxConvLocal);
         s << i << wxT(" - ") << name << e;
      }

      s << wxT("==============================") << e;
      s << wxT("Available capture sources:") << e;
      cnt = Px_GetNumInputSources(PortMixer);
      for (int i = 0; i < cnt; i++) {
         wxString name(Px_GetInputSourceName(PortMixer, i), wxConvLocal);
         s << i << wxT(" - ") << name << e;
      }

      s << wxT("==============================") << e;
      s << wxT("Available playback volumes:") << e;
      cnt = Px_GetNumOutputVolumes(PortMixer);
      for (int i = 0; i < cnt; i++) {
         wxString name(Px_GetOutputVolumeName(PortMixer, i), wxConvLocal);
         s << i << wxT(" - ") << name << e;
      }

      // Determine mixer capabilities - it it doesn't support either
      // input or output, we emulate them (by multiplying this value
      // by all incoming/outgoing samples)

      MixerOutputVol = Px_GetPCMOutputVolume(PortMixer);
      EmulateMixerOutputVol = false;
      Px_SetPCMOutputVolume(PortMixer, 0.0);
      if (Px_GetPCMOutputVolume(PortMixer) > 0.1)
         EmulateMixerOutputVol = true;
      Px_SetPCMOutputVolume(PortMixer, 0.2f);
      if (Px_GetPCMOutputVolume(PortMixer) < 0.1 ||
          Px_GetPCMOutputVolume(PortMixer) > 0.3)
         EmulateMixerOutputVol = true;
      Px_SetPCMOutputVolume(PortMixer, MixerOutputVol);

      MixerInputVol = Px_GetInputVolume(PortMixer);
      EmulateMixerInputVol = false;
      Px_SetInputVolume(PortMixer, 0.0);
      if (Px_GetInputVolume(PortMixer) > 0.1)
         EmulateMixerInputVol = true;
      Px_SetInputVolume(PortMixer, 0.2f);
      if (Px_GetInputVolume(PortMixer) < 0.1 ||
          Px_GetInputVolume(PortMixer) > 0.3)
         EmulateMixerInputVol = true;
      Px_SetInputVolume(PortMixer, MixerInputVol);
   
      Pa_CloseStream(stream);
   
      s << wxT("==============================") << e;
      s << wxT("Capture volume is ") << (EmulateMixerInputVol? wxT("emulated"): wxT("native")) << e;
      s << wxT("Playback volume is ") << (EmulateMixerOutputVol? wxT("emulated"): wxT("native")) << e;
   
      Px_CloseMixer(PortMixer);

      }  //end of massive if statement if a valid sample rate has been found
#endif
   return o.GetString();
}

// This method is the data gateway between the audio thread (which
// communicates with the disk) and the PortAudio callback thread
// (which communicates with the audio device).
void AudioIO::FillBuffers()
{
   unsigned int i;

   if( mPlaybackTracks.GetCount() > 0 )
   {
      // Though extremely unlikely, it is possible that some buffers
      // will have more samples available than others.  This could happen
      // if we hit this code during the PortAudio callback.  To keep
      // things simple, we only write as much data as is vacant in
      // ALL buffers, and advance the global time by that much.
      // MB: subtract a few samples because the code below has rounding errors
      int commonlyAvail = GetCommonlyAvailPlayback() - 10;

      //
      // Determine how much this will globally advance playback time
      //
      double secsAvail = commonlyAvail / mRate;

      //
      // Don't fill the buffers at all unless we can do the
      // full mMaxPlaybackSecsToCopy.  This improves performance
      // by not always trying to process tiny chunks, eating the
      // CPU unnecessarily.
      //
      // The exception is if we're at the end of the selected
      // region - then we should just fill the buffer.
      //
      if (secsAvail >= mMaxPlaybackSecsToCopy ||
          (!mPlayLooped && (secsAvail > 0 && mWarpedTime+secsAvail >= mWarpedLength)))
      {
         // Limit maximum buffer size (increases performance)
         if (secsAvail > mMaxPlaybackSecsToCopy)
            secsAvail = mMaxPlaybackSecsToCopy;

         double deltat; // this is warped time

         // msmeyer: When playing a very short selection in looped
         // mode, the selection must be copied to the buffer multiple
         // times, to ensure, that the buffer has a reasonable size
         // This is the purpose of this loop.
         do {
            deltat = secsAvail;
            if( mWarpedTime + deltat > mWarpedLength )
            {
               deltat = mWarpedLength - mWarpedTime;
               mWarpedTime = mWarpedLength;
               if( deltat < 0.0 ) // this should never happen
                  deltat = 0.0;
            }
            else
            {
               mWarpedTime += deltat;
            }
            
            secsAvail -= deltat;

            for( i = 0; i < mPlaybackTracks.GetCount(); i++ )
            {
               // The mixer here isn't actually mixing: it's just doing
               // resampling, format conversion, and possibly time track
               // warping
               int processed = 0;
               samplePtr warpedSamples;
               //don't do anything if we have no length.  In particular, Process() will fail an wxAssert
               //that causes a crash since this is not the GUI thread and wxASSERT is a GUI call.
               if(deltat > 0.0)
               {
                  processed = mPlaybackMixers[i]->Process(lrint(deltat * mRate));
                  warpedSamples = mPlaybackMixers[i]->GetBuffer();
                  mPlaybackBuffers[i]->Put(warpedSamples, floatSample, processed);
               }
               //if looping and processed is less than the full chunk/block/buffer that gets pulled from
               //other longer tracks, then we still need to advance the ring buffers or
               //we'll trip up on ourselves when we start them back up again.
               //if not looping we never start them up again, so its okay to not do anything
               if(processed < lrint(deltat * mRate) && mPlayLooped)
               {
                  if(mLastSilentBufSize < lrint(deltat * mRate))
                  {
                     //delete old if necessary
                     if(mSilentBuf) 
                        DeleteSamples(mSilentBuf);
                     mLastSilentBufSize=lrint(deltat * mRate);
                     mSilentBuf = NewSamples(mLastSilentBufSize, floatSample);
                     ClearSamples(mSilentBuf, floatSample, 0, mLastSilentBufSize);
                  }
                  mPlaybackBuffers[i]->Put(mSilentBuf, floatSample, lrint(deltat * mRate) - processed);
               }
            }

            // msmeyer: If playing looped, check if we are at the end of the buffer
            // and if yes, restart from the beginning.
            if (mPlayLooped && mWarpedTime >= mWarpedLength)
            {
               for (i = 0; i < mPlaybackTracks.GetCount(); i++)
                  mPlaybackMixers[i]->Restart();
               mWarpedTime = 0.0;
            }

         } while (mPlayLooped && secsAvail > 0 && deltat > 0);
      }
   }  // end of playback buffering

   if( mCaptureTracks.GetCount() > 0 ) // start record buffering
   {
      int commonlyAvail = GetCommonlyAvailCapture();

      //
      // Determine how much this will add to captured tracks
      //
      double deltat = commonlyAvail / mRate;

      if (mAudioThreadShouldCallFillBuffersOnce ||
          deltat >= mMinCaptureSecsToCopy)
      {
         // Append captured samples to the end of the WaveTracks.
         // The WaveTracks have their own buffering for efficiency.
         XMLStringWriter blockFileLog;
         int numChannels = mCaptureTracks.GetCount();
         
         for( i = 0; (int)i < numChannels; i++ )
         {
            int avail = commonlyAvail;
            sampleFormat trackFormat = mCaptureTracks[i]->GetSampleFormat();

            XMLStringWriter appendLog;

            if( mFactor == 1.0 )
            {
               samplePtr temp = NewSamples(avail, trackFormat);
               mCaptureBuffers[i]->Get   (temp, trackFormat, avail);
               mCaptureTracks[i]-> Append(temp, trackFormat, avail, 1, 
                                          &appendLog);
               DeleteSamples(temp);
            }
            else
            {
               int size = lrint(avail * mFactor);
               samplePtr temp1 = NewSamples(avail, floatSample);
               samplePtr temp2 = NewSamples(size, floatSample);
               mCaptureBuffers[i]->Get(temp1, floatSample, avail);
               /* we are re-sampling on the fly. The last resampling call
                * must flush any samples left in the rate conversion buffer
                * so that they get recorded
                */
               size = mResample[i]->Process(mFactor, (float *)temp1, avail, !IsStreamActive(),
                                            &size, (float *)temp2, size);
               mCaptureTracks[i]-> Append(temp2, floatSample, size, 1, 
                                          &appendLog);
               DeleteSamples(temp1);
               DeleteSamples(temp2);
            }

            if (!appendLog.IsEmpty())
            {
               blockFileLog.StartTag(wxT("recordingrecovery"));
               blockFileLog.WriteAttr(wxT("channel"), (int)i);
               blockFileLog.WriteAttr(wxT("numchannels"), numChannels);
               blockFileLog.WriteSubTree(appendLog);
               blockFileLog.EndTag(wxT("recordingrecovery"));
            }
         }
         
         if (mListener && !blockFileLog.IsEmpty())
            mListener->OnAudioIONewBlockFiles(blockFileLog);
      }
   }  // end of record buffering
}

void AudioIO::SetListener(AudioIOListener* listener)
{
   if (IsBusy())
      return;

   mListener = listener;
}

#ifdef EXPERIMENTAL_MIDI_OUT

static Alg_update gAllNotesOff; // special event for loop ending
// the fields of this event are never used, only the address is important

void AudioIO::OutputEvent()
{
   int channel = (mNextEvent->chan) & 0xF; // must be in [0..15]
   int command = -1;
   int data1 = -1;
   int data2 = -1;
   // 0.0005 is for rounding
   double eventTime = (mNextEventTime - mT0) / mMidiPlaySpeed + mT0;
   double time = eventTime + PauseTime() + 0.0005 - 
                 ((mMidiLatency + mSynthLatency) * 0.001);

   if (mNumPlaybackChannels > 0) { // is there audio playback?
      time += 1; // MidiTime() has a 1s offset
   } else {
      time -= mT0; // Midi is not synced to audio
   }
   // state changes have to go out without delay because the 
   // midi stream time gets reset when playback starts, and
   // we don't want to leave any control changes scheduled for later
   if (time < 0 || mSendMidiState) time = 0;
   PmTimestamp timestamp = (PmTimestamp) (time * 1000); /* s to ms */

   // The special event gAllNotesOffEvent means "end of playback, send
   // all notes off on all channels"
   if (mNextEvent == &gAllNotesOff) {
      AllNotesOff();
      if (mPlayLooped) {
         // jump back to beginning of loop
         mMidiLoopOffset += (mT1 - mT0);
         PrepareMidiIterator(false, mMidiLoopOffset);
      } else {
         mNextEvent = NULL;
      }
      return;
   }

   // if mNextEvent's channel is visible, play it, visibility can
   // be updated while playing. Be careful: if we have a note-off,
   // then we must not pay attention to the channel selection
   // or mute/solo buttons because we must turn the note off 
   // even if the user changed something after the note began
   // Note that because multiple tracks can output to the same
   // MIDI channels, it is not a good idea to send "All Notes Off"
   // when the user presses the mute button. We have no easy way
   // to know what notes are sounding on any given muted track, so
   // we'll just wait for the note-off events to happen.
   // Also note that note-offs are only sent when we call 
   // mIterator->request_note_off(), so notes that are not played
   // will note generate random note-offs. There is the interesting
   // case that if the playback is paused, all-notes-off WILL be sent
   // and if playback resumes, the pending note-off events WILL also
   // be sent (but if that is a problem, there would also be a problem
   // in the non-pause case.
   if (((mNextEventTrack->GetVisibleChannels() & (1 << channel)) && 
        // only play if note is not muted:
        !((mHasSolo || mNextEventTrack->GetMute()) && 
          !mNextEventTrack->GetSolo())) ||
       (mNextEvent->is_note() && !mNextIsNoteOn)) { 
      // Note event
      if (mNextEvent->is_note() && !mSendMidiState) {
         // Pitch and velocity
         data1 = mNextEvent->get_pitch();
         if (mNextIsNoteOn) {
            data2 = mNextEvent->get_loud(); // get velocity
            int offset = mNextEventTrack->GetGain();
            data2 += offset; // offset comes from per-track slider
            // clip velocity to insure a legal note-on value
            data2 = (data2 < 0 ? 1 : (data2 > 127 ? 127 : data2));
            // since we are going to play this note, we need to get a note_off
            mIterator->request_note_off();
         } else data2 = 0; // 0 velocity means "note off"
         command = 0x90; // MIDI NOTE ON (or OFF when velocity == 0)
      // Update event
      } else if (mNextEvent->is_update()) {
         // this code is based on allegrosmfwr.cpp -- it could be improved
         // by comparing attribute pointers instead of string compares
         Alg_update_ptr update = (Alg_update_ptr) mNextEvent;
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
         Pm_WriteShort(mMidiStream, timestamp, 
                    Pm_Message((int) (command + channel), 
                                  (long) data1, (long) data2));
         /* printf("Pm_WriteShort %lx (%p) @ %d, advance %d\n", 
                Pm_Message((int) (command + channel), 
                           (long) data1, (long) data2), 
                           mNextEvent, timestamp, timestamp - Pt_Time()); */
      }
   }
}

void AudioIO::GetNextEvent()
{
   mNextEventTrack = NULL; // clear it just to be safe
   // now get the next event and the track from which it came
   double nextOffset;
   if (!mIterator) {
        mNextEvent = NULL;
        return;
   }
   mNextEvent = mIterator->next(&mNextIsNoteOn,
                                (void **) &mNextEventTrack,
                                &nextOffset, mT1 + mMidiLoopOffset);
   if (mNextEvent) {
      mNextEventTime = (mNextIsNoteOn ? mNextEvent->time : 
                              mNextEvent->get_end_time()) + nextOffset;;
   } else { // terminate playback at mT1
      mNextEvent = &gAllNotesOff;
      mNextEventTime = mT1 + mMidiLoopOffset - ALG_EPS;
      mNextIsNoteOn = true; // do not look at duration
      mIterator->end();
      delete mIterator;
      mIterator = NULL; // debugging aid
   }
}


bool AudioIO::SetHasSolo(bool hasSolo)
{
   mHasSolo = hasSolo;
   return mHasSolo;
}


void AudioIO::FillMidiBuffers()
{
   bool hasSolo = false;
   int numPlaybackTracks = gAudioIO->mPlaybackTracks.GetCount();
   int t;
   for(t = 0; t < numPlaybackTracks; t++ )
      if( gAudioIO->mPlaybackTracks[t]->GetSolo() ) {
         hasSolo = true;
         break;
      }
   int numMidiPlaybackTracks = gAudioIO->mMidiPlaybackTracks.GetCount();
   for(t = 0; t < numMidiPlaybackTracks; t++ )
      if( gAudioIO->mMidiPlaybackTracks[t]->GetSolo() ) {
         hasSolo = true;
         break;
      }
   SetHasSolo(hasSolo);
   // Compute the current track time differently depending upon
   // whether audio playback is in effect:
   double time;
   if (mNumPlaybackChannels > 0) {
      time = AudioTime() - PauseTime();
   } else {
      time = mT0 + Pt_Time() * 0.001 - PauseTime();
      double timeAtSpeed = (time - mT0) * mMidiPlaySpeed + mT0;
      if (mNumCaptureChannels <= 0) {
         // no audio callback, so move the time cursor here:
         double trackTime = timeAtSpeed - mMidiLoopOffset;
         //printf("mTime set. mT0 %g Pt_Time() %gs PauseTime %g\n",
         //       mT0, Pt_Time() * 0.001, PauseTime());
         // Since loop offset is incremented when we fill the
         // buffer, the cursor tends to jump back to mT0 early.
         // Therefore, if we are in loop mode, and if mTime < mT0,
         // we must not be at the end of the loop yet.
         if (mPlayLooped && trackTime < mT0) {
            trackTime += (mT1 - mT0);
         }
         // mTime is shared with another thread so we stored
         // intermediate values in trackTime. Do the update
         // atomically now that we have the final value:
         mTime = trackTime;
      }
      // advance time so that midi messages are written a little early, 
      // timestamps will insure accurate output timing. This is an "extra"
      // MIDI_SLEEP interval; another is added below to compensate for the
      // fact that we need to output messages that will become due while
      // we are sleeping.
      time += MIDI_SLEEP * 0.001;
   }
   while (mNextEvent && 
          (mNextEventTime - mT0) / mMidiPlaySpeed + mT0 < time +
                           ((MIDI_SLEEP + mSynthLatency) * 0.001)) {
      OutputEvent();
      GetNextEvent();
   }
}

double AudioIO::PauseTime()
{
   if (mNumPlaybackChannels > 0) {
      return mNumPauseFrames / mRate; 
   } else {
      return mPauseTime * 0.001;
   }
}


PmTimestamp AudioIO::MidiTime()
{
   if (mNumPlaybackChannels > 0) {
      //printf("AudioIO:MidiTime: PaUtil_GetTime() %g mAudioCallbackOutputTime %g time - outputTime %g\n",
      //        PaUtil_GetTime(), mAudioCallbackOutputTime, PaUtil_GetTime() - mAudioCallbackOutputTime);
      // note: the extra 0.0005 is for rounding. Round down by casting to
      // unsigned long, then convert to PmTimeStamp (currently signed)
      return (PmTimestamp) ((unsigned long) (1000 * (AudioTime() + 1.0005 - 
                              mAudioFramesPerBuffer / mRate + 
                              PaUtil_GetTime() - mAudioCallbackOutputTime)));
   } else {
      return Pt_Time();
   }
}

void AudioIO::AllNotesOff()
{
   for (int chan = 0; chan < 16; chan++) {
      Pm_WriteShort(mMidiStream, 0, Pm_Message(0xB0 + chan, 0x7B, 0));
   }
}

#endif

// Automated Input Level Adjustment - Automatically tries to find an acceptable input volume
#ifdef AUTOMATED_INPUT_LEVEL_ADJUSTMENT
void AudioIO::AILAInitialize() {
   gPrefs->Read(wxT("/AudioIO/AutomatedInputLevelAdjustment"), &mAILAActive,         false);
   gPrefs->Read(wxT("/AudioIO/TargetPeak"),            &mAILAGoalPoint,      AILA_DEF_TARGET_PEAK);
   gPrefs->Read(wxT("/AudioIO/DeltaPeakVolume"),       &mAILAGoalDelta,      AILA_DEF_DELTA_PEAK);
   gPrefs->Read(wxT("/AudioIO/AnalysisTime"),          &mAILAAnalysisTime,   AILA_DEF_ANALYSIS_TIME);
   gPrefs->Read(wxT("/AudioIO/NumberAnalysis"),        &mAILATotalAnalysis,  AILA_DEF_NUMBER_ANALYSIS);
   mAILAGoalDelta         /= 100.0;
   mAILAGoalPoint         /= 100.0; 
   mAILAAnalysisTime      /= 1000.0;
   mAILAMax                = 0.0;
   mAILALastStartTime      = max(0.0, mT0);
   mAILAClipped            = false;
   mAILAAnalysisCounter    = 0;
   mAILAChangeFactor       = 1.0;
   mAILALastChangeType     = 0;
   mAILATopLevel           = 1.0;
   mAILAAnalysisEndTime    = -1.0;
}

void AudioIO::AILADisable() {
   mAILAActive = false;
}

bool AudioIO::AILAIsActive() {
   return mAILAActive;
}

void AudioIO::AILASetStartTime() {
   mAILAAbsolutStartTime = Pa_GetStreamTime(mPortStreamV19);
   printf("START TIME %f\n\n", mAILAAbsolutStartTime);
}

double AudioIO::AILAGetLastDecisionTime() {
   return mAILAAnalysisEndTime;
}

void AudioIO::AILAProcess(double maxPeak) {
   AudacityProject *proj = GetActiveProject();
   if (proj && mAILAActive) {
      if (mInputMeter->IsClipping()) {
         mAILAClipped = true;
         printf("clipped");
      }
      
      mAILAMax = max(mAILAMax, maxPeak);
   
      if ((mAILATotalAnalysis == 0 || mAILAAnalysisCounter < mAILATotalAnalysis) && mTime - mAILALastStartTime >= mAILAAnalysisTime) {
         putchar('\n');
         mAILAMax = mInputMeter->ToLinearIfDB(mAILAMax);
         double iv = (double) Px_GetInputVolume(mPortMixer);
         unsigned short changetype = 0; //0 - no change, 1 - increase change, 2 - decrease change
         printf("mAILAAnalysisCounter:%d\n", mAILAAnalysisCounter);
         printf("\tmAILAClipped:%d\n", mAILAClipped);
         printf("\tmAILAMax (linear):%f\n", mAILAMax);
         printf("\tmAILAGoalPoint:%f\n", mAILAGoalPoint);
         printf("\tmAILAGoalDelta:%f\n", mAILAGoalDelta);
         printf("\tiv:%f\n", iv);
         printf("\tmAILAChangeFactor:%f\n", mAILAChangeFactor);
         if (mAILAClipped || mAILAMax > mAILAGoalPoint + mAILAGoalDelta) {
            printf("too high:\n");
            mAILATopLevel = min(mAILATopLevel, iv);
            printf("\tmAILATopLevel:%f\n", mAILATopLevel);
            //if clipped or too high
            if (iv <= LOWER_BOUND) {
               //we can't improve it more now
               if (mAILATotalAnalysis != 0) {
                  mAILAActive = false;
                  proj->TP_DisplayStatusMessage(_("Automated Input Level Adjustment stopped. It was not possible to optimize it more. Still too high."));
               }
               printf("\talready min vol:%f\n", iv);
            }
            else {
               float vol = (float) max(LOWER_BOUND, iv+(mAILAGoalPoint-mAILAMax)*mAILAChangeFactor);
               Px_SetInputVolume(mPortMixer, vol);
               wxString msg;
               msg.Printf(_("Automated Input Level Adjustment decreased the volume to %f."), vol);
               proj->TP_DisplayStatusMessage(msg);
               changetype = 1;
               printf("\tnew vol:%f\n", vol);
               float check = Px_GetInputVolume(mPortMixer);
               printf("\tverified %f\n", check);
            }
         }
         else if ( mAILAMax < mAILAGoalPoint - mAILAGoalDelta ) {
            //if too low
            printf("too low:\n"); 
            if (iv >= UPPER_BOUND || iv + 0.005 > mAILATopLevel) { //condition for too low volumes and/or variable volumes that cause mAILATopLevel to decrease too much
               //we can't improve it more
               if (mAILATotalAnalysis != 0) {
                  mAILAActive = false;
                  proj->TP_DisplayStatusMessage(_("Automated Input Level Adjustment stopped. It was not possible to optimize it more. Still too low."));
               }
               printf("\talready max vol:%f\n", iv);
            }
            else {
               float vol = (float) min(UPPER_BOUND, iv+(mAILAGoalPoint-mAILAMax)*mAILAChangeFactor);
               if (vol > mAILATopLevel) {
                  vol = (iv + mAILATopLevel)/2.0;
                  printf("\tTruncated vol:%f\n", vol);
               }
               Px_SetInputVolume(mPortMixer, vol);
               wxString msg;
               msg.Printf(_("Automated Input Level Adjustment increased the volume to %.2f."), vol);
               proj->TP_DisplayStatusMessage(msg);
               changetype = 2;
               printf("\tnew vol:%f\n", vol);
               float check = Px_GetInputVolume(mPortMixer);
               printf("\tverified %f\n", check);
            }
         }

         mAILAAnalysisCounter++;
         //const PaStreamInfo* info = Pa_GetStreamInfo(mPortStreamV19);
         //double latency = 0.0;
         //if (info)
         //   latency = info->inputLatency;
         //mAILAAnalysisEndTime = mTime+latency;
         mAILAAnalysisEndTime = Pa_GetStreamTime(mPortStreamV19) - mAILAAbsolutStartTime;
         mAILAMax             = 0;
         printf("\tA decision was made @ %f\n", mAILAAnalysisEndTime);
         mAILAClipped         = false;  
         mAILALastStartTime   = mTime;
         
         if (changetype == 0)
            mAILAChangeFactor *= 0.8; //time factor
         else if (mAILALastChangeType == changetype)
            mAILAChangeFactor *= 1.1; //concordance factor
         else
            mAILAChangeFactor *= 0.7; //discordance factor
         mAILALastChangeType = changetype;
         putchar('\n');
      }

      if (mAILAActive && mAILATotalAnalysis != 0 && mAILAAnalysisCounter >= mAILATotalAnalysis) {
         mAILAActive = false;
         if (mAILAMax > mAILAGoalPoint + mAILAGoalDelta)
            proj->TP_DisplayStatusMessage(_("Automated Input Level Adjustment stopped. The total number of analysis has been exceeded without finding an acceptable volume. Still too high."));
         else if (mAILAMax < mAILAGoalPoint - mAILAGoalDelta)
            proj->TP_DisplayStatusMessage(_("Automated Input Level Adjustment stopped. The total number of analysis has been exceeded without finding an acceptable volume. Still too low."));
         else {
            wxString msg;
            msg.Printf(_("Automated Input Level Adjustment stopped. %.2f seems an acceptable volume."), Px_GetInputVolume(mPortMixer));
            proj->TP_DisplayStatusMessage(msg);
         }
      }
   }
}
#endif

//////////////////////////////////////////////////////////////////////
//
//    PortAudio callback thread context
//
//////////////////////////////////////////////////////////////////////

#define MAX(a,b) ((a) > (b) ? (a) : (b))

static void DoSoftwarePlaythrough(const void *inputBuffer,
                                  sampleFormat inputFormat,
                                  int inputChannels,
                                  float *outputBuffer,
                                  int len)
{
   for (int i=0; i < inputChannels; i++) {
      samplePtr inputPtr = ((samplePtr)inputBuffer) + (i * SAMPLE_SIZE(inputFormat));
      samplePtr outputPtr = ((samplePtr)outputBuffer) + (i * SAMPLE_SIZE(floatSample));

      CopySamples(inputPtr, inputFormat,
                  (samplePtr)outputPtr, floatSample,
                  len, true, inputChannels, 2);
   }

   // One mono input channel goes to both output channels...
   if (inputChannels == 1)
      for (int i=0; i < len; i++)
         outputBuffer[2*i + 1] = outputBuffer[2*i];
}

int audacityAudioCallback(const void *inputBuffer, void *outputBuffer,
                          unsigned long framesPerBuffer,
                          const PaStreamCallbackTimeInfo * WXUNUSED(timeInfo),
                          const PaStreamCallbackFlags WXUNUSED(statusFlags), void * WXUNUSED(userData) )
{
   int numPlaybackChannels = gAudioIO->mNumPlaybackChannels;
   int numPlaybackTracks = gAudioIO->mPlaybackTracks.GetCount();
   int numCaptureChannels = gAudioIO->mNumCaptureChannels;
   int callbackReturn = paContinue;
   void *tempBuffer = alloca(framesPerBuffer*sizeof(float)*
                             MAX(numCaptureChannels,numPlaybackChannels));
   float *tempFloats = (float*)tempBuffer;

   // output meter may need samples untouched by volume emulation
   float *outputMeterFloats;
   outputMeterFloats =
      (outputBuffer && gAudioIO->mEmulateMixerOutputVol &&
                       gAudioIO->mMixerOutputVol != 1.0) ?
         (float *)alloca(framesPerBuffer*numPlaybackChannels * sizeof(float)) :
         (float *)outputBuffer;

#ifdef EXPERIMENTAL_MIDI_OUT
   /* GSW: Save timeInfo in case MidiPlayback needs it */
   gAudioIO->mAudioCallbackOutputTime = timeInfo->outputBufferDacTime;
   // printf("in callback, mAudioCallbackOutputTime %g\n", gAudioIO->mAudioCallbackOutputTime); //DBG
   gAudioIO->mAudioFramesPerBuffer = framesPerBuffer;
   if(gAudioIO->IsPaused())
      gAudioIO->mNumPauseFrames += framesPerBuffer;
   gAudioIO->mNumFrames += framesPerBuffer;
#endif

   unsigned int i;
   int t;

   /* Send data to recording VU meter if applicable */

   if (gAudioIO->mInputMeter &&
         !gAudioIO->mInputMeter->IsMeterDisabled() &&
         inputBuffer) {
      // get here if meters are actually live , and being updated
      /* It's critical that we don't update the meters while StopStream is
       * trying to stop PortAudio, otherwise it can lead to a freeze.  We use
       * two variables to synchronize:
       *   mUpdatingMeters tells StopStream when the callback is about to enter
       *     the code where it might update the meters, and
       *   mUpdateMeters is how the rest of the code tells the callback when it
       *     is allowed to actually do the updating.
       * Note that mUpdatingMeters must be set first to avoid a race condition.
       */
      gAudioIO->mUpdatingMeters = true;
      if (gAudioIO->mUpdateMeters) {
         if (gAudioIO->mCaptureFormat == floatSample)
            gAudioIO->mInputMeter->UpdateDisplay(numCaptureChannels,
                                                 framesPerBuffer,
                                                 (float *)inputBuffer);
         else {
            CopySamples((samplePtr)inputBuffer, gAudioIO->mCaptureFormat,
                        (samplePtr)tempFloats, floatSample,
                        framesPerBuffer * numCaptureChannels);
            gAudioIO->mInputMeter->UpdateDisplay(numCaptureChannels,
                                                 framesPerBuffer,
                                                 tempFloats);
         }
      }
      gAudioIO->mUpdatingMeters = false;
   }  // end recording VU meter update

   // Stop recording if 'silence' is detected
   if(gAudioIO->mPauseRec && inputBuffer && gAudioIO->mInputMeter) {
      if(gAudioIO->mInputMeter->GetMaxPeak() < gAudioIO->mSilenceLevel ) {
         if(!gAudioIO->IsPaused()) {
            AudacityProject *p = GetActiveProject();
            wxCommandEvent dummyEvt;
            p->GetControlToolBar()->OnPause(dummyEvt);
         }
      }
      else {
         if(gAudioIO->IsPaused()) {
            AudacityProject *p = GetActiveProject();
            wxCommandEvent dummyEvt;
            p->GetControlToolBar()->OnPause(dummyEvt);
         }
      }
   }
   if( gAudioIO->mPaused )
   {
      if (outputBuffer && numPlaybackChannels > 0)
      {
         ClearSamples((samplePtr)outputBuffer, floatSample,
                      0, framesPerBuffer * numPlaybackChannels);

         if (inputBuffer && gAudioIO->mSoftwarePlaythrough) {
            DoSoftwarePlaythrough(inputBuffer, gAudioIO->mCaptureFormat,
                                  numCaptureChannels,
                                  (float *)outputBuffer, (int)framesPerBuffer);
         }
      }

      return paContinue;
   }

   if (gAudioIO->mStreamToken > 0)
   {
      //
      // Mix and copy to PortAudio's output buffer
      //
      
      if( outputBuffer && (numPlaybackChannels > 0) )
      {
         bool cut = false;
         bool linkFlag = false;
         
         float *outputFloats = (float *)outputBuffer;
         for( i = 0; i < framesPerBuffer*numPlaybackChannels; i++)
            outputFloats[i] = 0.0;

         if (inputBuffer && gAudioIO->mSoftwarePlaythrough) {
            DoSoftwarePlaythrough(inputBuffer, gAudioIO->mCaptureFormat,
                                  numCaptureChannels,
                                  (float *)outputBuffer, (int)framesPerBuffer);
         }

         // Copy the results to outputMeterFloats if necessary
         if (outputMeterFloats != outputFloats) {
            for (i = 0; i < framesPerBuffer*numPlaybackChannels; ++i) {
               outputMeterFloats[i] = outputFloats[i];
            }
         }

         if (gAudioIO->mSeek)
         {
            // Pause audio thread and wait for it to finish
            gAudioIO->mAudioThreadFillBuffersLoopRunning = false;
            while( gAudioIO->mAudioThreadFillBuffersLoopActive == true )
            {
               wxMilliSleep( 50 );
            }

            // Calculate the new time position
            gAudioIO->mTime += gAudioIO->mSeek;
            if (gAudioIO->mTime < gAudioIO->mT0)
                gAudioIO->mTime = gAudioIO->mT0;
            else if (gAudioIO->mTime > gAudioIO->mT1)
                gAudioIO->mTime = gAudioIO->mT1;
            gAudioIO->mSeek = 0.0;
            
            // Reset mixer positions and flush buffers for all tracks
            if(gAudioIO->mTimeTrack)
               gAudioIO->mWarpedTime = gAudioIO->mTimeTrack->ComputeWarpedLength(gAudioIO->mT0, gAudioIO->mTime);
            else
               gAudioIO->mWarpedTime = gAudioIO->mTime - gAudioIO->mT0;
            for (i = 0; i < (unsigned int)numPlaybackTracks; i++)
            {
               gAudioIO->mPlaybackMixers[i]->Reposition(gAudioIO->mTime);
               gAudioIO->mPlaybackBuffers[i]->Discard(gAudioIO->mPlaybackBuffers[i]->AvailForGet());
            }

            // Reload the ring buffers
            gAudioIO->mAudioThreadShouldCallFillBuffersOnce = true;
            while( gAudioIO->mAudioThreadShouldCallFillBuffersOnce == true )
            {
               wxMilliSleep( 50 );
            }

            // Reenable the audio thread
            gAudioIO->mAudioThreadFillBuffersLoopRunning = true;
               
            return paContinue;
         }

         int numSolo = 0;
         for( t = 0; t < numPlaybackTracks; t++ )
            if( gAudioIO->mPlaybackTracks[t]->GetSolo() )
               numSolo++;
#ifdef EXPERIMENTAL_MIDI_OUT
         int numMidiPlaybackTracks = gAudioIO->mMidiPlaybackTracks.GetCount();
         for( t = 0; t < numMidiPlaybackTracks; t++ )
            if( gAudioIO->mMidiPlaybackTracks[t]->GetSolo() )
               numSolo++;
#endif         
         for( t = 0; t < numPlaybackTracks; t++)
         {
            WaveTrack *vt = gAudioIO->mPlaybackTracks[t];
            
            if (linkFlag)
               linkFlag = false;
            else {
               cut = false;
               
               // Cut if somebody else is soloing
               if (numSolo>0 && !vt->GetSolo())
                  cut = true;
               
               // Cut if we're muted (unless we're soloing)
               if (vt->GetMute() && !vt->GetSolo())
                  cut = true;
               
               linkFlag = vt->GetLinked();
            }
            
#define ORIGINAL_DO_NOT_PLAY_ALL_MUTED_TRACKS_TO_END
#ifdef ORIGINAL_DO_NOT_PLAY_ALL_MUTED_TRACKS_TO_END
            // this is original code prior to r10680 -RBD
            if (cut)
               {
                  gAudioIO->mPlaybackBuffers[t]->Discard(framesPerBuffer);
                  continue;
               }

            unsigned int len = (unsigned int)
               gAudioIO->mPlaybackBuffers[t]->Get((samplePtr)tempFloats,
                                                  floatSample,
                                                  (int)framesPerBuffer);
#else
            // This code was reorganized so that if all audio tracks
            // are muted, we still return paComplete when the end of
            // a selection is reached.
            // Vaughan, 2011-10-20: Further comments from Roger, by off-list email:
            //    ...something to do with what it means to mute all audio tracks. E.g. if you
            // mute all and play, does the playback terminate immediately or play
            // silence? If it terminates immediately, does that terminate any MIDI
            // playback that might also be going on? ...Maybe muted audio tracks + MIDI,
            // the playback would NEVER terminate. ...I think the #else part is probably preferable...
            unsigned int len;
            if (cut)
            {
               len = (unsigned int) 
                  gAudioIO->mPlaybackBuffers[t]->Discard(framesPerBuffer);
            } else 
            {
               len = (unsigned int)
                  gAudioIO->mPlaybackBuffers[t]->Get((samplePtr)tempFloats,
                                                     floatSample,
                                                     (int)framesPerBuffer);
            }
#endif
            // If our buffer is empty and the time indicator is past
            // the end, then we've actually finished playing the entire
            // selection.
            // msmeyer: We never finish if we are playing looped
            if (len == 0 && gAudioIO->mTime >= gAudioIO->mT1 &&
                !gAudioIO->mPlayLooped)
            {
               callbackReturn = paComplete;
            }
#ifndef ORIGINAL_DO_NOT_PLAY_ALL_MUTED_TRACKS_TO_END
            if (cut) // no samples to process, they've been discarded
               continue;
#endif

            if (vt->GetChannel() == Track::LeftChannel ||
                vt->GetChannel() == Track::MonoChannel)
            {
               float gain = vt->GetChannelGain(0);

               // Output volume emulation: possibly copy meter samples, then
               // apply volume, then copy to the output buffer
               if (outputMeterFloats != outputFloats)
                  for (i = 0; i < len; ++i)
                     outputMeterFloats[numPlaybackChannels*i] +=
                        gain*tempFloats[i];
               
               if (gAudioIO->mEmulateMixerOutputVol)
                  gain *= gAudioIO->mMixerOutputVol;
               
               for(i=0; i<len; i++)
                  outputFloats[numPlaybackChannels*i] += gain*tempFloats[i];
            }
            
            if (vt->GetChannel() == Track::RightChannel ||
                vt->GetChannel() == Track::MonoChannel)
            {
               float gain = vt->GetChannelGain(1);
               
               // Output volume emulation (as above)
               if (outputMeterFloats != outputFloats)
                  for (i = 0; i < len; ++i)
                     outputMeterFloats[numPlaybackChannels*i+1] +=
                        gain*tempFloats[i];

               if (gAudioIO->mEmulateMixerOutputVol)
                  gain *= gAudioIO->mMixerOutputVol;
               
               for(i=0; i<len; i++)
                  outputFloats[numPlaybackChannels*i+1] += gain*tempFloats[i];
            }
         }
         
         //
         // Clip output to [-1.0,+1.0] range (msmeyer)
         //
         for( i = 0; i < framesPerBuffer*numPlaybackChannels; i++)
         {
            float f = outputFloats[i];
            if (f > 1.0)
               outputFloats[i] = 1.0;
            else if (f < -1.0)
               outputFloats[i] = -1.0;
         }

         // Same for meter output
         if (outputMeterFloats != outputFloats)
         {
            for (i = 0; i < framesPerBuffer*numPlaybackChannels; ++i)
            {
               float f = outputMeterFloats[i];
               if (f > 1.0)
                  outputMeterFloats[i] = 1.0;
               else if (f < -1.0)
                  outputMeterFloats[i] = -1.0;
            }
         }
      }

      //
      // Copy from PortAudio to our input buffers.
      //
      
      if( inputBuffer && (numCaptureChannels > 0) )
      {
         unsigned int len = framesPerBuffer;
         for( t = 0; t < numCaptureChannels; t++) {
            unsigned int avail =
               (unsigned int)gAudioIO->mCaptureBuffers[t]->AvailForPut();
            if (avail < len)
               len = avail;
         }
         
         if (len < framesPerBuffer)
         {
            gAudioIO->mLostSamples += (framesPerBuffer - len);
            wxPrintf(wxT("lost %d samples\n"), (int)(framesPerBuffer - len));
         }

         if (len > 0) {
            for( t = 0; t < numCaptureChannels; t++) {
               
               // dmazzoni:
               // Un-interleave.  Ugly special-case code required because the
               // capture channels could be in three different sample formats;
               // it'd be nice to be able to call CopySamples, but it can't
               // handle multiplying by the gain and then clipping.  Bummer.

               switch(gAudioIO->mCaptureFormat) {
               case floatSample: {
                  float *inputFloats = (float *)inputBuffer;
                  for( i = 0; i < len; i++)
                     tempFloats[i] =
                        inputFloats[numCaptureChannels*i+t];
               } break;
               case int24Sample:
                  // We should never get here. Audacity's int24Sample format
                  // is different from PortAudio's sample format and so we
                  // make PortAudio return float samples when recording in
                  // 24-bit samples.
                  wxASSERT(false);
                  break;
               case int16Sample: {
                  short *inputShorts = (short *)inputBuffer;
                  short *tempShorts = (short *)tempBuffer;
                  for( i = 0; i < len; i++) {
                     float tmp = inputShorts[numCaptureChannels*i+t];
                     if (tmp > 32767)
                        tmp = 32767;
                     if (tmp < -32768)
                        tmp = -32768;
                     tempShorts[i] = (short)(tmp);
                  }
               } break;
               } // switch
               
               gAudioIO->mCaptureBuffers[t]->Put((samplePtr)tempBuffer,
                                                 gAudioIO->mCaptureFormat,
                                                 len);
            }
         }
      }

      // Update the current time position
      if (gAudioIO->mTimeTrack) {
         // MB: this is why SolveWarpedLength is needed :)
         gAudioIO->mTime = gAudioIO->mTimeTrack->SolveWarpedLength(gAudioIO->mTime, framesPerBuffer / gAudioIO->mRate);
      } else {
         gAudioIO->mTime += framesPerBuffer / gAudioIO->mRate;
      }

      // Wrap to start if looping
      if (gAudioIO->mPlayLooped && gAudioIO->mTime >= gAudioIO->mT1)
      {
         // LL:  This is not exactly right, but I'm at my wits end trying to
         //      figure it out.  Feel free to fix it.  :-)
         // MB: it's much easier than you think, mTime isn't warped at all!
         gAudioIO->mTime -= gAudioIO->mT1 - gAudioIO->mT0;
      }

      // Record the reported latency from PortAudio.
      // TODO: Don't recalculate this with every callback?

      // 01/21/2009:  Disabled until a better solution presents itself.
     #if 0
      // As of 06/17/2006, portaudio v19 returns inputBufferAdcTime set to
      // zero.  It is being worked on, but for now we just can't do much
      // but follow the leader.
      //
      // 08/27/2006: too inconsistent for now...just leave it a zero.
      //
      // 04/16/2008: Looks like si->inputLatency comes back with something useful though.
      // This rearranged logic uses si->inputLatency, but if PortAudio fixes inputBufferAdcTime, 
      // this code won't have to be modified to use it. 
      // Also avoids setting mLastRecordingOffset except when simultaneously playing and recording.
      //
      if (numCaptureChannels > 0 && numPlaybackChannels > 0) // simultaneously playing and recording
      {
         if (timeInfo->inputBufferAdcTime > 0)
            gAudioIO->mLastRecordingOffset = timeInfo->inputBufferAdcTime - timeInfo->outputBufferDacTime;
         else if (gAudioIO->mLastRecordingOffset == 0.0) 
         {
            const PaStreamInfo* si = Pa_GetStreamInfo( gAudioIO->mPortStreamV19 );
            gAudioIO->mLastRecordingOffset = -si->inputLatency;
         }
      }
     #endif
   } // if mStreamToken > 0
   else {
      // No tracks to play, but we should clear the output, and
      // possibly do software playthrough...
      
      if( outputBuffer && (numPlaybackChannels > 0) ) {
         float *outputFloats = (float *)outputBuffer;
         for( i = 0; i < framesPerBuffer*numPlaybackChannels; i++)
            outputFloats[i] = 0.0;
         
         if (inputBuffer && gAudioIO->mSoftwarePlaythrough) {
            DoSoftwarePlaythrough(inputBuffer, gAudioIO->mCaptureFormat,
                                  numCaptureChannels,
                                  (float *)outputBuffer, (int)framesPerBuffer);
         }

         // Copy the results to outputMeterFloats if necessary
         if (outputMeterFloats != outputFloats) {
            for (i = 0; i < framesPerBuffer*numPlaybackChannels; ++i) {
               outputMeterFloats[i] = outputFloats[i];
            }
         }
      }

   }
   /* Send data to playback VU meter if applicable */
   if (gAudioIO->mOutputMeter && 
      !gAudioIO->mOutputMeter->IsMeterDisabled() &&
      outputMeterFloats) {
      // Get here if playback meter is live 
      /* It's critical that we don't update the meters while StopStream is
       * trying to stop PortAudio, otherwise it can lead to a freeze.  We use
       * two variables to synchronize:
       *  mUpdatingMeters tells StopStream when the callback is about to enter
       *    the code where it might update the meters, and 
       *  mUpdateMeters is how the rest of the code tells the callback when it
       *    is allowed to actually do the updating.
       * Note that mUpdatingMeters must be set first to avoid a race condition.
       */
      gAudioIO->mUpdatingMeters = true;
      if (gAudioIO->mUpdateMeters) {
         gAudioIO->mOutputMeter->UpdateDisplay(numPlaybackChannels,
                                               framesPerBuffer,
                                               outputMeterFloats);

         //v Vaughan, 2011-02-25: Moved this update back to TrackPanel::OnTimer() 
         //    as it helps with playback issues reported by Bill and noted on Bug 258.
         //    The problem there occurs if Software Playthrough is on. 
         //    Could conditionally do the update here if Software Playthrough is off, 
         //    and in TrackPanel::OnTimer() if Software Playthrough is on, but not now. 
         //AudacityProject* pProj = GetActiveProject();
         //MixerBoard* pMixerBoard = pProj->GetMixerBoard();
         //if (pMixerBoard)
         //   pMixerBoard->UpdateMeters(gAudioIO->GetStreamTime(), 
         //                              (pProj->mLastPlayMode == loopedPlay));
      }
      gAudioIO->mUpdatingMeters = false;
   }  // end playback VU meter update

   return callbackReturn;
}

#ifdef EXPERIMENTAL_MIDI_OUT
int compareTime( const void* a, const void* b )
{
   return( (int)((*(PmEvent*)a).timestamp - (*(PmEvent*)b).timestamp ) );
}
#endif

