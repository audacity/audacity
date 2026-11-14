/**********************************************************************

Audacity: A Digital Audio Editor

WaveformScale.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_WAVEFORM_SETTINGS__
#define __AUDACITY_WAVEFORM_SETTINGS__

#include "ClientData.h" // to inherit
#include "Prefs.h"

class wxRect;

class WaveChannel;
class WaveTrack;

class AUDACITY_DLL_API WaveformScale : public ClientData::Cloneable<>
{
public:
    //! Mutative access to attachment even if the track argument is const
    static WaveformScale& Get(const WaveTrack& track);

    /*!
     @copydoc Get(const WaveTrack &)
     */
    static WaveformScale& Get(const WaveChannel& channel);

    ~WaveformScale() override;
    PointerType Clone() const override;

    int ZeroLevelYCoordinate(wxRect rect) const;

    void GetDisplayBounds(float& min, float& max) const
    { min = mDisplayMin; max = mDisplayMax; }

    void SetDisplayBounds(float min, float max)
    { mDisplayMin = min; mDisplayMax = max; }

    float GetLastScaleType() const { return mLastScaleType; }
    void SetLastScaleType(int type) { mLastScaleType = type; }

    int GetLastDBRange() const { return mLastdBRange; }
    void SetLastDBRange(int range) { mLastdBRange = range; }

private:
    float mDisplayMin = -1.0f, mDisplayMax = 1.0f;
    int mLastScaleType = -1;
    int mLastdBRange = -1;
};

#endif
