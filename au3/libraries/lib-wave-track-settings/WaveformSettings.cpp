/**********************************************************************

Audacity: A Digital Audio Editor

WaveformSettings.cpp

Paul Licameli

*******************************************************************//**

\class WaveformSettings
\brief Waveform settings, either for one track or as defaults.

*//*******************************************************************/

#include "WaveformSettings.h"
#include "WaveTrack.h"

#include "Decibels.h"

#include "Prefs.h"
#include <algorithm>

const wxString WaveformSettings::waveformScaleKey
    =wxT("/GUI/DefaultWaveformScaleChoice");
const wxString WaveformSettings::dbLogValueString = wxT("dB");
const wxString WaveformSettings::dbLinValueString = wxT("dBLin");

EnumSetting<WaveformSettings::ScaleTypeValues>
WaveformSettings::waveformScaleSetting {
    waveformScaleKey,
    {
        { wxT("Linear"), XO("Linear (amp)") },
        { dbLogValueString, XO("Logarithmic (dB)") },
        { dbLinValueString, XO("Linear (dB)") },
    },

    0,   // linear

    {
        WaveformSettings::stLinearAmp,
        WaveformSettings::stLogarithmicDb,
        WaveformSettings::stLinearDb,
    }
};

WaveformSettings::Globals::Globals()
{
    LoadPrefs();
}

void WaveformSettings::Globals::SavePrefs()
{
}

void WaveformSettings::Globals::LoadPrefs()
{
}

WaveformSettings::Globals
& WaveformSettings::Globals::Get()
{
    static Globals instance;
    return instance;
}

static const ChannelGroup::Attachments::RegisteredFactory
    key1{ [](auto&) {
        return std::make_unique<WaveformSettings>(WaveformSettings::defaults());
    } };

WaveformSettings& WaveformSettings::Get(const WaveTrack& track)
{
    auto& mutTrack = const_cast<WaveTrack&>(track);
    return mutTrack.Attachments::Get<WaveformSettings>(key1);
}

WaveformSettings& WaveformSettings::Get(const WaveChannel& channel)
{
    return Get(channel.GetTrack());
}

void WaveformSettings::Set(
    WaveChannel& channel, std::unique_ptr<WaveformSettings> pSettings)
{
    channel.GetTrack().Attachments::Assign(key1, move(pSettings));
}

WaveformSettings::WaveformSettings()
{
    LoadPrefs();
}

WaveformSettings::WaveformSettings(const WaveformSettings& other)
    : scaleType(other.scaleType)
    , dBRange(other.dBRange)
{
}

WaveformSettings& WaveformSettings::operator=(const WaveformSettings& other)
{
    if (this != &other) {
        scaleType = other.scaleType;
        dBRange = other.dBRange;
    }
    return *this;
}

WaveformSettings& WaveformSettings::defaults()
{
    static WaveformSettings instance;
    return instance;
}

bool WaveformSettings::Validate(bool /* quiet */)
{
    scaleType = ScaleType(
        std::max(0, std::min((int)(stNumScaleTypes) - 1, (int)(scaleType)))
        );

    ConvertToEnumeratedDBRange();
    ConvertToActualDBRange();

    return true;
}

void WaveformSettings::LoadPrefs()
{
    scaleType = waveformScaleSetting.ReadEnum();

    dBRange = DecibelScaleCutoff.Read();

    // Enforce legal values
    Validate(true);

    Update();
}

void WaveformSettings::SavePrefs()
{
}

void WaveformSettings::Update()
{
}

// This is a temporary hack until WaveformSettings gets fully integrated
void WaveformSettings::UpdatePrefs()
{
    if (scaleType == defaults().scaleType) {
        scaleType = waveformScaleSetting.ReadEnum();
    }

    if (dBRange == defaults().dBRange) {
        dBRange = DecibelScaleCutoff.Read();
    }

    // Enforce legal values
    Validate(true);
}

void WaveformSettings::ConvertToEnumeratedDBRange()
{
    // Assumes the codes are in ascending sequence.
    wxArrayStringEx codes;
    WaveformSettings::GetRangeChoices(nullptr, &codes);
    int ii = 0;
    for (int nn = codes.size(); ii < nn; ++ii) {
        long value = 0;
        codes[ii].ToLong(&value);
        if (dBRange < value) {
            break;
        }
    }
    dBRange = std::max(0, ii - 1);
}

void WaveformSettings::ConvertToActualDBRange()
{
    wxArrayStringEx codes;
    WaveformSettings::GetRangeChoices(nullptr, &codes);
    long value = 0;
    codes[std::max(0, std::min((int)(codes.size()) - 1, dBRange))]
    .ToLong(&value);
    dBRange = (int)(value);
}

void WaveformSettings::NextLowerDBRange()
{
    ConvertToEnumeratedDBRange();
    ++dBRange;
    ConvertToActualDBRange();
}

void WaveformSettings::NextHigherDBRange()
{
    ConvertToEnumeratedDBRange();
    --dBRange;
    ConvertToActualDBRange();
}

//static
const EnumValueSymbols& WaveformSettings::GetScaleNames()
{
    static const EnumValueSymbols result{
        // Keep in correspondence with ScaleTypeValues:
        { wxT("Linear"), XO("Linear (amp)") },
        { wxT("dB"), XO("Logarithmic (dB)") },
        { wxT("LinearDB"), XO("Linear (dB)") },
    };
    return result;
}

void WaveformSettings::GetRangeChoices(
    TranslatableStrings* pChoices, wxArrayStringEx* pCodes,
    int* pDefaultRangeIndex)
{
    static const wxArrayStringEx sCodes = {
        wxT("36"), wxT("48"), wxT("60"),  wxT("72"),
        wxT("84"), wxT("96"), wxT("120"), wxT("145"),
    };
    if (pCodes) {
        *pCodes = sCodes;
    }

    static const std::initializer_list<TranslatableString> sChoices = {
        XO("-36 dB (shallow range for high-amplitude editing)"),
        XO("-48 dB (PCM range of 8 bit samples)"),
        XO("-60 dB (PCM range of 10 bit samples)"),
        XO("-72 dB (PCM range of 12 bit samples)"),
        XO("-84 dB (PCM range of 14 bit samples)"),
        XO("-96 dB (PCM range of 16 bit samples)"),
        XO("-120 dB (approximate limit of human hearing)"),
        XO("-145 dB (PCM range of 24 bit samples)"),
    };

    if (pChoices) {
        *pChoices = sChoices;
    }

    if (pDefaultRangeIndex) {
        *pDefaultRangeIndex = 2; // 60 == ENV_DB_RANGE
    }
}

WaveformSettings::~WaveformSettings()
{
}

auto WaveformSettings::Clone() const -> PointerType
{
    return std::make_unique<WaveformSettings>(*this);
}
