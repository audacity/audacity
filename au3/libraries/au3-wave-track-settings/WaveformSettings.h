/**********************************************************************

Audacity: A Digital Audio Editor

WaveformSettings.h

Paul Licameli

**********************************************************************/

#pragma once

#include "ClientData.h" // to inherit
#include "Prefs.h"

class wxRect;

class EnumValueSymbols;
class WaveChannel;
class WaveTrack;

class WAVE_TRACK_SETTINGS_API WaveformSettings : public PrefsListener, public ClientData::Cloneable<>
{
public:
    //! Create waveform settings for the track on demand
    //! Mutative access to attachment even if the track argument is const
    static WaveformSettings& Get(const WaveTrack& track);

    /*!
     @copydoc Get(const WaveTrack &);
     */
    static WaveformSettings& Get(const WaveChannel& channel);

    //! Guarantee independence of settings, then assign
    static void Set(
        WaveChannel& channel, std::unique_ptr<WaveformSettings> pSettings);

    // Singleton for settings that are not per-track
    class WAVE_TRACK_SETTINGS_API Globals
    {
    public:
        static Globals& Get();
        void SavePrefs();

    private:
        Globals();
        void LoadPrefs();
    };

    static WaveformSettings& defaults();

    WaveformSettings();
    WaveformSettings(const WaveformSettings& other);
    WaveformSettings& operator=(const WaveformSettings& other);
    ~WaveformSettings() override;

    PointerType Clone() const override;

    bool IsDefault() const
    {
        return this == &defaults();
    }

    bool Validate(bool quiet);
    void LoadPrefs();
    void SavePrefs();
    void Update();

    void UpdatePrefs() override;

    void ConvertToEnumeratedDBRange();
    void ConvertToActualDBRange();
    void NextLowerDBRange();
    void NextHigherDBRange();

    typedef int ScaleType;
    enum ScaleTypeValues : int {
        stLinearAmp,
        stLogarithmicDb,
        stLinearDb,

        stNumScaleTypes,
    };

    static const EnumValueSymbols& GetScaleNames();
    static void GetRangeChoices(
        TranslatableStrings* pChoices, wxArrayStringEx* pCodes, int* pDefaultRangeIndex = nullptr);

    static const wxString waveformScaleKey;
    static const wxString dbLogValueString;
    static const wxString dbLinValueString;
    static EnumSetting<ScaleTypeValues> waveformScaleSetting;

    ScaleType scaleType;
    int dBRange;

    // Convenience
    bool isLinear() const { return scaleType == stLinearAmp || scaleType == stLinearDb; }
};
