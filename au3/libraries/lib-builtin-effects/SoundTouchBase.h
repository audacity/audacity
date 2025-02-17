/**********************************************************************

  Audacity: A Digital Audio Editor

  SoundTouchBase.h

  Dominic Mazzoni, Vaughan Johnson

  This abstract class contains all of the common code for an
  effect that uses SoundTouch to do its processing (ChangeTempo
  and ChangePitch).

**********************************************************************/

#if USE_SOUNDTOUCH

#ifndef __AUDACITY_EFFECT_SOUNDTOUCH__
#define __AUDACITY_EFFECT_SOUNDTOUCH__

#include "StatefulEffect.h"

// forward declaration of a class defined in SoundTouch.h
// which is not included here
namespace soundtouch {
class SoundTouch;
}

class TimeWarper;
class LabelTrack;
class NoteTrack;
class WaveChannel;
class WaveTrack;

class BUILTIN_EFFECTS_API SoundTouchBase /* not final */ : public StatefulEffect
{
public:

    // SoundTouchBase implementation

#ifdef USE_MIDI
    double mSemitones; // pitch change for NoteTracks
    SoundTouchBase();
#endif
    ~SoundTouchBase() override;

protected:
    // Effect implementation

    using InitFunction = std::function< void (soundtouch::SoundTouch* soundtouch) >;
    bool ProcessWithTimeWarper(InitFunction initer, const TimeWarper& warper, bool preserveLength);

private:
    bool ProcessLabelTrack(LabelTrack* track, const TimeWarper& warper);
#ifdef USE_MIDI
    bool ProcessNoteTrack(NoteTrack* track, const TimeWarper& warper);
#endif
    bool ProcessOne(soundtouch::SoundTouch* pSoundTouch, WaveChannel& orig, WaveTrack& out, sampleCount start, sampleCount end,
                    const TimeWarper& warper);
    bool ProcessStereo(soundtouch::SoundTouch* pSoundTouch, WaveTrack& orig, WaveTrack& out, sampleCount start, sampleCount end,
                       const TimeWarper& warper);
    bool ProcessStereoResults(soundtouch::SoundTouch* pSoundTouch, const size_t outputCount, WaveChannel& outputLeftTrack,
                              WaveChannel& outputRightTrack);
    /*!
     @pre `out.NChannels() == orig.NChannels()`
     */
    void Finalize(WaveTrack& orig, WaveTrack& out, const TimeWarper& warper);

    bool mPreserveLength;

    int mCurTrackNum;

    double m_maxNewLength;
};

#endif

#endif
