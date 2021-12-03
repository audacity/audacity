/**********************************************************************

  Audacity: A Digital Audio Editor

  SoundTouchEffect.h

  Dominic Mazzoni, Vaughan Johnson

  This abstract class contains all of the common code for an
  effect that uses SoundTouch to do its processing (ChangeTempo
  and ChangePitch).

**********************************************************************/



#if USE_SOUNDTOUCH

#ifndef __AUDACITY_EFFECT_SOUNDTOUCH__
#define __AUDACITY_EFFECT_SOUNDTOUCH__

#include "Effect.h"

// forward declaration of a class defined in SoundTouch.h
// which is not included here
namespace soundtouch { class SoundTouch; }


class TimeWarper;
class NoteTrack;
class WaveTrack;

class EffectSoundTouch /* not final */ : public Effect
{
public:
   
   // Effect implementation

   void End() override;

   // EffectSoundTouch implementation

#ifdef USE_MIDI
   double mSemitones; // pitch change for NoteTracks
   EffectSoundTouch();
#endif
   ~EffectSoundTouch() override;

protected:
   // Effect implementation

   using InitFunction = std::function< void(soundtouch::SoundTouch *soundtouch) >;
   bool ProcessWithTimeWarper(InitFunction initer,
                              const TimeWarper &warper,
                              bool preserveLength);

   std::unique_ptr<soundtouch::SoundTouch> mSoundTouch;
   double mCurT0;
   double mCurT1;

private:
   bool ProcessLabelTrack(LabelTrack *track, const TimeWarper &warper);
#ifdef USE_MIDI
   bool ProcessNoteTrack(NoteTrack *track, const TimeWarper &warper);
#endif
   bool ProcessOne(
      WaveTrack * t, sampleCount start, sampleCount end,
      const TimeWarper &warper);
   bool ProcessStereo(WaveTrack* leftTrack, WaveTrack* rightTrack,
                     sampleCount start, sampleCount end,
                      const TimeWarper &warper);
   bool ProcessStereoResults(const size_t outputCount,
                              WaveTrack* outputLeftTrack,
                              WaveTrack* outputRightTrack);
   void Finalize(WaveTrack* orig, WaveTrack* out, const TimeWarper &warper);

   bool   mPreserveLength;

   int    mCurTrackNum;

   double m_maxNewLength;
};

#endif

#endif
