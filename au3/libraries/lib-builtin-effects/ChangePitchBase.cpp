#ifdef USE_SOUNDTOUCH

#include "ChangePitchBase.h"
#include "ShuttleAutomation.h"
#include "Spectrum.h"
#include "TimeWarper.h"
#include "WaveTrack.h"
#include <wx/math.h>
#include "PitchName.h"

#if USE_SBSMS
#include "SBSMSBase.h"
#endif

// Soundtouch defines these as well, which are also in generated configmac.h
// and configunix.h, so get rid of them before including,
// to avoid compiler warnings, and be sure to do this
// after all other #includes, to avoid any mischief that might result
// from doing the un-definitions before seeing any wx headers.
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION
#undef PACKAGE_BUGREPORT
#undef PACKAGE
#undef VERSION
#include "SoundTouch.h"

// Soundtouch is not reasonable below -99% or above 3000%.

const EffectParameterMethods& ChangePitchBase::Parameters() const
{
    static CapturedParameters<
        ChangePitchBase,
        // Vaughan, 2013-06: Long lost to history, I don't see why
        // m_dPercentChange was chosen to be shuttled. Only m_dSemitonesChange is
        // used in Process(). PRL 2022: but that is so only when USE_SBSMS is not
        // defined
        Percentage, UseSBSMS>
    parameters {
        [](ChangePitchBase&, EffectSettings&, ChangePitchBase& e,
           bool updating) {
            if (updating) {
                e.Calc_SemitonesChange_fromPercentChange();
            }
            return true;
        },
    };
    return parameters;
}

const ComponentInterfaceSymbol ChangePitchBase::Symbol { XO("Change Pitch") };

ChangePitchBase::ChangePitchBase()
{
    // mUseSBSMS always defaults to false and its value is used only if USE_SBSMS
    // is defined
    Parameters().Reset(*this);

    m_dSemitonesChange = 0.0;
    m_dStartFrequency = 0.0; // 0.0 => uninitialized
    m_bLoopDetect = false;

    SetLinearEffectFlag(true);
}

ChangePitchBase::~ChangePitchBase()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol ChangePitchBase::GetSymbol() const
{
    return Symbol;
}

TranslatableString ChangePitchBase::GetDescription() const
{
    return XO("Changes the pitch of a track without changing its tempo");
}

ManualPageID ChangePitchBase::ManualPage() const
{
    return L"Change_Pitch";
}

// EffectDefinitionInterface implementation

EffectType ChangePitchBase::GetType() const
{
    return EffectTypeProcess;
}

OptionalMessage
ChangePitchBase::LoadFactoryDefaults(EffectSettings& settings) const
{
    // To do: externalize state so const_cast isn't needed
    return const_cast<ChangePitchBase&>(*this).DoLoadFactoryDefaults(settings);
}

OptionalMessage ChangePitchBase::DoLoadFactoryDefaults(EffectSettings& settings)
{
    DeduceFrequencies();

    return Effect::LoadFactoryDefaults(settings);
}

// Effect implementation

bool ChangePitchBase::Process(EffectInstance&, EffectSettings& settings)
{
#if USE_SBSMS
    if (mUseSBSMS) {
        double pitchRatio = 1.0 + m_dPercentChange / 100.0;
        SBSMSBase proxy;
        proxy.mProxyEffectName = XO("High Quality Pitch Change");
        proxy.setParameters(1.0, pitchRatio);
        //! Already processing; don't make a dialog
        return Delegate(proxy, settings);
    } else
#endif
    {
        // Macros save m_dPercentChange and not m_dSemitonesChange, so we must
        // ensure that m_dSemitonesChange is set.
        Calc_SemitonesChange_fromPercentChange();

        auto initer = [&](soundtouch::SoundTouch* soundtouch) {
            soundtouch->setPitchSemiTones((float)(m_dSemitonesChange));
        };
        IdentityTimeWarper warper;
#ifdef USE_MIDI
        // Pitch shifting note tracks is currently only supported by
        // SoundTouchBase and non-real-time-preview effects require an audio track
        // selection.
        //
        // Note: m_dSemitonesChange is private to ChangePitch because it only
        // needs to pass it along to mSoundTouch (above). I added mSemitones
        // to SoundTouchBase (the super class) to convey this value
        // to process Note tracks. This approach minimizes changes to existing
        // code, but it would be cleaner to change all m_dSemitonesChange to
        // mSemitones, make mSemitones exist with or without USE_MIDI, and
        // eliminate the next line:
        mSemitones = m_dSemitonesChange;
#endif
        return SoundTouchBase::ProcessWithTimeWarper(initer, warper, true);
    }
}

bool ChangePitchBase::CheckWhetherSkipEffect(const EffectSettings&) const
{
    return m_dPercentChange == 0.0;
}

// ChangePitchBase implementation

// Deduce m_FromFrequency from the samples at the beginning of
// the selection. Then set some other params accordingly.
void ChangePitchBase::DeduceFrequencies()
{
    auto FirstTrack = [&]() -> const WaveTrack* {
        if (IsBatchProcessing() || !inputTracks()) {
            return nullptr;
        }
        return *(inputTracks()->Selected<const WaveTrack>()).first;
    };

    m_dStartFrequency = 261.265; // Middle C.

    // As a neat trick, attempt to get the frequency of the note at the
    // beginning of the selection.
    auto track = FirstTrack();
    if (track) {
        double rate = track->GetRate();

        // Auto-size window -- high sample rates require larger windowSize.
        // Aim for around 2048 samples at 44.1 kHz (good down to about 100 Hz).
        // To detect single notes, analysis period should be about 0.2 seconds.
        // windowSize must be a power of 2.
        const size_t windowSize
            =// windowSize < 256 too inaccurate
              std::max(
                  256, wxRound(pow(2.0, floor((log(rate / 20.0) / log(2.0)) + 0.5))));

        // we want about 0.2 seconds to catch the first note.
        // number of windows rounded to nearest integer >= 1.
        const unsigned numWindows
            =std::max(1, wxRound((double)(rate / (5.0f * windowSize))));

        double trackStart = track->GetStartTime();
        double t0 = mT0 < trackStart ? trackStart : mT0;
        auto start = track->TimeToLongSamples(t0);

        auto analyzeSize = windowSize * numWindows;
        Floats buffer { analyzeSize };

        Floats freq { windowSize / 2 };
        Floats freqa { windowSize / 2, true };

        // Always used only the left channel for this deduction of initial pitch
        (*track->Channels().begin())->GetFloats(buffer.get(), start, analyzeSize);
        for (unsigned i = 0; i < numWindows; i++) {
            ComputeSpectrum(
                buffer.get() + i * windowSize, windowSize, windowSize, freq.get(),
                true);
            for (size_t j = 0; j < windowSize / 2; j++) {
                freqa[j] += freq[j];
            }
        }
        size_t argmax = 0;
        for (size_t j = 1; j < windowSize / 2; j++) {
            if (freqa[j] > freqa[argmax]) {
                argmax = j;
            }
        }

        auto lag = (windowSize / 2 - 1) - argmax;
        m_dStartFrequency = rate / lag;
    }

    double dFromMIDInote = FreqToMIDInote(m_dStartFrequency);
    double dToMIDInote = dFromMIDInote + m_dSemitonesChange;
    m_nFromPitch = PitchIndex(dFromMIDInote);
    m_nFromOctave = PitchOctave(dFromMIDInote);
    m_nToPitch = PitchIndex(dToMIDInote);
    m_nToOctave = PitchOctave(dToMIDInote);

    m_FromFrequency = m_dStartFrequency;
    // Calc_PercentChange();  // This will reset m_dPercentChange
    Calc_ToFrequency();
}

// calculations

void ChangePitchBase::Calc_ToPitch()
{
    int nSemitonesChange
        =(int)(m_dSemitonesChange + ((m_dSemitonesChange < 0.0) ? -0.5 : 0.5));
    m_nToPitch = (m_nFromPitch + nSemitonesChange) % 12;
    if (m_nToPitch < 0) {
        m_nToPitch += 12;
    }
}

void ChangePitchBase::Calc_ToOctave()
{
    m_nToOctave = PitchOctave(FreqToMIDInote(m_ToFrequency));
}

void ChangePitchBase::Calc_SemitonesChange_fromPitches()
{
    m_dSemitonesChange = PitchToMIDInote(m_nToPitch, m_nToOctave)
                         - PitchToMIDInote(m_nFromPitch, m_nFromOctave);
}

void ChangePitchBase::Calc_SemitonesChange_fromPercentChange()
{
    // Use m_dPercentChange rather than m_FromFrequency & m_ToFrequency, because
    // they start out uninitialized, but m_dPercentChange is always valid.
    m_dSemitonesChange
        =(12.0 * log((100.0 + m_dPercentChange) / 100.0)) / log(2.0);
}

void ChangePitchBase::Calc_ToFrequency()
{
    m_ToFrequency = (m_FromFrequency * (100.0 + m_dPercentChange)) / 100.0;
}

void ChangePitchBase::Calc_PercentChange()
{
    m_dPercentChange = 100.0 * (pow(2.0, (m_dSemitonesChange / 12.0)) - 1.0);
}

#endif // USE_SOUNDTOUCH
