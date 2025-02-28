/**********************************************************************

  Audacity: A Digital Audio Editor

  NoiseReductionBase.cpp

  Dominic Mazzoni

  detailed rewriting by
  Paul Licameli

*******************************************************************//**

\class NoiseReductionBase
\brief A two-pass effect to reduce background noise.

  The first pass is done over just noise.  For each windowed sample
  of the sound, we take a FFT and then statistics are tabulated for
  each frequency band.

  During the noise reduction phase, we start by setting a gain control
  for each frequency band such that if the sound has exceeded the
  previously-determined threshold, the gain is set to 0 dB, otherwise
  the gain is set lower (e.g. -18 dB), to suppress the noise.
  Then time-smoothing is applied so that the gain for each frequency
  band moves slowly, and then frequency-smoothing is applied so that a
  single frequency is never suppressed or boosted in isolation.
  Lookahead is employed; this effect is not designed for real-time
  but if it were, there would be a significant delay.

  The gain controls are applied to the complex FFT of the signal,
  and then the inverse FFT is applied.  A Hann window may be
  applied (depending on the advanced window types setting), and then
  the output signal is then pieced together using overlap/add.

*//********************************************************************/
#include "NoiseReductionBase.h"
#include "BasicUI.h"
#include "EffectOutputTracks.h"
#include "FFT.h"
#include "TrackSpectrumTransformer.h"
#include "WaveTrack.h"
#include <algorithm>
#include <cmath>

// SPECTRAL_SELECTION not to affect this effect for now, as there might be no
// indication that it does. [Discussed and agreed for v2.1 by Steve, Paul,
// Bill].
#undef SPECTRAL_EDIT_NOISE_REDUCTION

typedef std::vector<float> FloatVector;

// Define both of these to make the radio button three-way
#define RESIDUE_CHOICE
//#define ISOLATE_CHOICE

// Define for Attack and release controls.
// #define ATTACK_AND_RELEASE

// Define to expose other advanced, experimental dialog controls
//#define ADVANCED_SETTINGS

// Define to make the old statistical methods an available choice
//#define OLD_METHOD_AVAILABLE

namespace {
enum DiscriminationMethod : size_t
{
    DM_MEDIAN,
    DM_SECOND_GREATEST,
    DM_OLD_METHOD,

    DM_N_METHODS,
    DM_DEFAULT_METHOD = DM_SECOND_GREATEST,
};

const struct DiscriminationMethodInfo
{
    const TranslatableString name;
} discriminationMethodInfo[DM_N_METHODS] = {
    // Experimental only, don't need translations
    { XO("Median") },
    { XO("Second greatest") },
    { XO("Old") },
};

// magic number used only in the old statistics
// and the old discrimination
const float minSignalTime = 0.05f;

enum WindowTypes : unsigned
{
    WT_RECTANGULAR_HANN = 0, // 2.0.6 behavior, requires 1/2 step
    WT_HANN_RECTANGULAR,    // requires 1/2 step
    WT_HANN_HANN,           // requires 1/4 step
    WT_BLACKMAN_HANN,       // requires 1/4 step
    WT_HAMMING_RECTANGULAR, // requires 1/2 step
    WT_HAMMING_HANN,        // requires 1/4 step
    // WT_HAMMING_INV_HAMMING, // requires 1/2 step

    WT_N_WINDOW_TYPES,
    WT_DEFAULT_WINDOW_TYPES = WT_HANN_HANN
};

const struct WindowTypesInfo
{
    const TranslatableString name;
    unsigned minSteps;
} windowTypesInfo[WT_N_WINDOW_TYPES] = {
    // Experimental only, don't need translations
    { Verbatim("none, Hann (2.0.6 behavior)"), 2 },
    /* i18n-hint: Hann is a proper name */
    { Verbatim("Hann, none"), 2 },
    /* i18n-hint: Hann is a proper name */
    { Verbatim("Hann, Hann (default)"), 4 },
    /* i18n-hint: Hann and Blackman are proper names */
    { Verbatim("Blackman, Hann"), 4 },
    /* i18n-hint: Hamming is a proper name */
    { Verbatim("Hamming, none"), 2 },
    /* i18n-hint: Hamming and Hann area proper names */
    { Verbatim("Hamming, Hann"), 4 },
    /* i18n-hint: Hamming is a proper name */
    // { XO("Hamming, Reciprocal Hamming"),    2, }, // output window is special
};

enum
{
    DEFAULT_WINDOW_SIZE_CHOICE = 8, // corresponds to 2048
    DEFAULT_STEPS_PER_WINDOW_CHOICE
        =1 // corresponds to 4, minimum for WT_HANN_HANN
};
} // namespace

//----------------------------------------------------------------------------
// NoiseReductionBase::Statistics
//----------------------------------------------------------------------------

class NoiseReductionBase::Statistics
{
public:
    Statistics(size_t spectrumSize, double rate, int windowTypes)
        : mRate{rate}
        , mWindowSize{(spectrumSize - 1) * 2}
        , mWindowTypes{windowTypes}
        , mTotalWindows{0}
        , mTrackWindows{0}
        , mSums(spectrumSize)
        , mMeans(spectrumSize)
#ifdef OLD_METHOD_AVAILABLE
        , mNoiseThreshold(spectrumSize)
#endif
    {
    }

    // Noise profile statistics follow

    double mRate; // Rate of profile track(s) -- processed tracks must match
    size_t mWindowSize;
    int mWindowTypes;

    unsigned mTotalWindows;
    unsigned mTrackWindows;
    FloatVector mSums;
    FloatVector mMeans;

#ifdef OLD_METHOD_AVAILABLE
    // Old statistics:
    FloatVector mNoiseThreshold;
#endif
};

//----------------------------------------------------------------------------
// NoiseReductionBase::Settings
//----------------------------------------------------------------------------

NoiseReductionBase::Settings::Settings()
    : mDoProfile{true}
{
    PrefsIO(true);
}

struct MyTransformer : TrackSpectrumTransformer
{
    MyTransformer(
        NoiseReductionBase::Worker& worker, WaveChannel* pOutputTrack,
        bool needsOutput, eWindowFunctions inWindowType,
        eWindowFunctions outWindowType, size_t windowSize,
        unsigned stepsPerWindow, bool leadingPadding, bool trailingPadding)
        : TrackSpectrumTransformer{pOutputTrack,   needsOutput,
                                   inWindowType,   outWindowType,
                                   windowSize,     stepsPerWindow,
                                   leadingPadding, trailingPadding}
        , mWorker{worker}
    {
    }

    struct MyWindow : public Window
    {
        explicit MyWindow(size_t windowSize)
            : Window{windowSize}
            , mSpectrums(windowSize / 2 + 1)
            , mGains(windowSize / 2 + 1)
        {
        }

        ~MyWindow() override;

        FloatVector mSpectrums;
        FloatVector mGains;
    };

    MyWindow& NthWindow(int nn)
    {
        return static_cast<MyWindow&>(Nth(nn));
    }

    std::unique_ptr<Window> NewWindow(size_t windowSize) override;
    bool DoStart() override;
    bool DoFinish() override;

    NoiseReductionBase::Worker& mWorker;
};

//----------------------------------------------------------------------------
// NoiseReductionBase::Worker
//----------------------------------------------------------------------------

// This object holds information needed only during effect calculation
class NoiseReductionBase::Worker final
{
public:
    typedef NoiseReductionBase::Settings Settings;
    typedef NoiseReductionBase::Statistics Statistics;

    Worker(
        NoiseReductionBase& effect, const Settings& settings, Statistics& statistics
#ifdef SPECTRAL_EDIT_NOISE_REDUCTION
        , double f0, double f1
#endif
        );
    ~Worker();

    bool Process(
        eWindowFunctions inWindowType, eWindowFunctions outWindowType, TrackList& tracks, double mT0, double mT1);

    static bool Processor(SpectrumTransformer& transformer);

    void ApplyFreqSmoothing(FloatVector& gains);
    void GatherStatistics(MyTransformer& transformer);
    inline bool
    Classify(MyTransformer& transformer, unsigned nWindows, int band);
    void ReduceNoise(MyTransformer& transformer);
    void FinishTrackStatistics();

    const bool mDoProfile;

    NoiseReductionBase& mEffect;
    const Settings& mSettings;
    Statistics& mStatistics;

    FloatVector mFreqSmoothingScratch;
    const size_t mFreqSmoothingBins;
    // When spectral selection limits the affected band:
    size_t mBinLow; // inclusive lower bound
    size_t mBinHigh; // exclusive upper bound

    const int mNoiseReductionChoice;
    const int mMethod;
    const double mNewSensitivity;

    float mOneBlockAttack;
    float mOneBlockRelease;
    float mNoiseAttenFactor;
    float mOldSensitivityFactor;

    unsigned mNWindowsToExamine;
    unsigned mCenter;
    unsigned mHistoryLen;

    // Following are for progress indicator only:
    unsigned mProgressTrackCount = 0;
    sampleCount mLen = 0;
    sampleCount mProgressWindowCount = 0;
};

const ComponentInterfaceSymbol NoiseReductionBase::Symbol { XO(
                                                                "Noise Reduction") };

NoiseReductionBase::NoiseReductionBase()
    : mSettings(std::make_unique<NoiseReductionBase::Settings>())
{
}

NoiseReductionBase::~NoiseReductionBase()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol NoiseReductionBase::GetSymbol() const
{
    return Symbol;
}

TranslatableString NoiseReductionBase::GetDescription() const
{
    return XO("Removes background noise such as fans, tape noise, or hums");
}

// EffectDefinitionInterface implementation

EffectType NoiseReductionBase::GetType() const
{
    return EffectTypeProcess;
}

namespace {
template<typename StructureType, typename FieldType> struct PrefsTableEntry
{
    typedef FieldType(StructureType::* MemberPointer);

    MemberPointer field;
    const wxChar* name;
    FieldType defaultValue;
};

template<typename StructureType, typename FieldType>
void readPrefs(
    StructureType* structure, const wxString& prefix,
    const PrefsTableEntry<StructureType, FieldType>* fields, size_t numFields)
{
    for (size_t ii = 0; ii < numFields; ++ii) {
        const PrefsTableEntry<StructureType, FieldType>& entry = fields[ii];
        gPrefs->Read(
            prefix + entry.name, &(structure->*(entry.field)), entry.defaultValue);
    }
}

template<typename StructureType, typename FieldType>
void writePrefs(
    const StructureType* structure, const wxString& prefix,
    const PrefsTableEntry<StructureType, FieldType>* fields, size_t numFields)
{
    for (size_t ii = 0; ii < numFields; ++ii) {
        const PrefsTableEntry<StructureType, FieldType>& entry = fields[ii];
        gPrefs->Write(prefix + entry.name, structure->*(entry.field));
    }
}
} // namespace

bool NoiseReductionBase::Settings::PrefsIO(bool read)
{
    static const double DEFAULT_OLD_SENSITIVITY = 0.0;

    static const PrefsTableEntry<Settings, double> doubleTable[] = {
        { &Settings::mNewSensitivity, wxT("Sensitivity"), 6.0 },
        { &Settings::mNoiseGain, wxT("Gain"), 6.0 },
        { &Settings::mAttackTime, wxT("AttackTime"), 0.02 },
        { &Settings::mReleaseTime, wxT("ReleaseTime"), 0.10 },
        { &Settings::mFreqSmoothingBands, wxT("FreqSmoothing"), 6.0 },

        // Advanced settings
        { &Settings::mOldSensitivity, wxT("OldSensitivity"),
          DEFAULT_OLD_SENSITIVITY },
    };
    static auto doubleTableSize = sizeof(doubleTable) / sizeof(doubleTable[0]);

    static const PrefsTableEntry<Settings, int> intTable[] = {
        { &Settings::mNoiseReductionChoice, wxT("ReductionChoice"),
          NRC_REDUCE_NOISE },

        // Advanced settings
        { &Settings::mWindowTypes, wxT("WindowTypes"), WT_DEFAULT_WINDOW_TYPES },
        { &Settings::mWindowSizeChoice, wxT("WindowSize"),
          DEFAULT_WINDOW_SIZE_CHOICE },
        { &Settings::mStepsPerWindowChoice, wxT("StepsPerWindow"),
          DEFAULT_STEPS_PER_WINDOW_CHOICE },
        { &Settings::mMethod, wxT("Method"), DM_DEFAULT_METHOD },
    };
    static auto intTableSize = sizeof(intTable) / sizeof(intTable[0]);

    static const wxString prefix(wxT("/Effects/NoiseReduction/"));

    if (read) {
        readPrefs(this, prefix, doubleTable, doubleTableSize);
        readPrefs(this, prefix, intTable, intTableSize);

        // Ignore preferences for unavailable options.
#if !(defined(RESIDUE_CHOICE) || defined(ISOLATE_CHOICE))
        mNoiseReductionChoice == NRC_REDUCE_NOISE;
#elif !(defined(RESIDUE_CHOICE))
        if (mNoiseReductionChoice == NRC_LEAVE_RESIDUE) {
            mNoiseReductionChoice = NRC_ISOLATE_NOISE;
        }
#elif !(defined(ISOLATE_CHOICE))
        if (mNoiseReductionChoice == NRC_ISOLATE_NOISE) {
            mNoiseReductionChoice = NRC_LEAVE_RESIDUE;
        }
#endif

#ifndef ADVANCED_SETTINGS
        // Initialize all hidden advanced settings to defaults.
        mWindowTypes = WT_DEFAULT_WINDOW_TYPES;
        mWindowSizeChoice = DEFAULT_WINDOW_SIZE_CHOICE;
        mStepsPerWindowChoice = DEFAULT_STEPS_PER_WINDOW_CHOICE;
        mMethod = DM_DEFAULT_METHOD;
        mOldSensitivity = DEFAULT_OLD_SENSITIVITY;
#endif

#ifndef OLD_METHOD_AVAILABLE
        if (mMethod == DM_OLD_METHOD) {
            mMethod = DM_DEFAULT_METHOD;
        }
#endif

        return true;
    } else {
        writePrefs(this, prefix, doubleTable, doubleTableSize);
        writePrefs(this, prefix, intTable, intTableSize);
        return gPrefs->Flush();
    }
}

bool NoiseReductionBase::Settings::Validate(NoiseReductionBase* effect) const
{
    using namespace BasicUI;
    if (StepsPerWindow() < windowTypesInfo[mWindowTypes].minSteps) {
        ShowMessageBox(XO("Steps per block are too few for the window types."));
        return false;
    }

    if (StepsPerWindow() > WindowSize()) {
        ShowMessageBox(XO("Steps per block cannot exceed the window size."));
        return false;
    }

    if (mMethod == DM_MEDIAN && StepsPerWindow() > 4) {
        ShowMessageBox(XO(
                           "Median method is not implemented for more than four steps per window."));
        return false;
    }

    return true;
}

auto MyTransformer::NewWindow(size_t windowSize) -> std::unique_ptr<Window>
{
    return std::make_unique<MyWindow>(windowSize);
}

MyTransformer::MyWindow::~MyWindow()
{
}

bool NoiseReductionBase::Process(EffectInstance&, EffectSettings&)
{
    // This same code will either reduce noise or profile it

    EffectOutputTracks outputs { *mTracks, GetType(), { { mT0, mT1 } } };

    auto track = *(outputs.Get().Selected<const WaveTrack>()).begin();
    if (!track) {
        return false;
    }

    const auto stepsPerWindow = mSettings->StepsPerWindow();
    const auto stepSize = mSettings->WindowSize() / stepsPerWindow;

    // Initialize statistics if gathering them, or check for mismatched
    // (advanced) settings if reducing noise.
    if (mSettings->mDoProfile) {
        const auto spectrumSize = mSettings->SpectrumSize();
        mStatistics = std::make_unique<Statistics>(
            spectrumSize, track->GetRate(), mSettings->mWindowTypes);
    } else if (mStatistics->mWindowSize != mSettings->WindowSize()) {
        // possible only with advanced settings
        BasicUI::ShowMessageBox(
            XO("You must specify the same window size for steps 1 and 2."));
        return false;
    } else if (mStatistics->mWindowTypes != mSettings->mWindowTypes) {
        // A warning only
        BasicUI::ShowMessageBox(
            XO("Warning: window types are not the same as for profiling."));
    }

    eWindowFunctions inWindowType, outWindowType;
    switch (mSettings->mWindowTypes) {
    case WT_RECTANGULAR_HANN:
        inWindowType = eWinFuncRectangular;
        outWindowType = eWinFuncHann;
        break;
    case WT_HANN_RECTANGULAR:
        inWindowType = eWinFuncHann;
        outWindowType = eWinFuncRectangular;
        break;
    case WT_BLACKMAN_HANN:
        inWindowType = eWinFuncBlackman;
        outWindowType = eWinFuncHann;
        break;
    case WT_HAMMING_RECTANGULAR:
        inWindowType = eWinFuncHamming;
        outWindowType = eWinFuncRectangular;
        break;
    case WT_HAMMING_HANN:
        inWindowType = eWinFuncHamming;
        outWindowType = eWinFuncHann;
        break;
    default:
        wxASSERT(false);
        [[fallthrough]];
    case WT_HANN_HANN:
        inWindowType = outWindowType = eWinFuncHann;
        break;
    }
    Worker worker { *this, *mSettings, *mStatistics
#ifdef SPECTRAL_EDIT_NOISE_REDUCTION
                    ,
                    mF0, mF1
#endif
    };
    bool bGoodResult
        =worker.Process(inWindowType, outWindowType, outputs.Get(), mT0, mT1);
    const auto wasProfile = mSettings->mDoProfile;
    if (mSettings->mDoProfile) {
        if (bGoodResult) {
            mSettings->mDoProfile
                =false; // So that "repeat last effect" will reduce noise
        } else {
            mStatistics.reset(); // So that profiling must be done again before
        }
        // noise reduction
    }
    if (bGoodResult && !wasProfile) {
        outputs.Commit();
    }

    return bGoodResult;
}

NoiseReductionBase::Worker::~Worker()
{
}

bool NoiseReductionBase::Worker::Process(
    eWindowFunctions inWindowType, eWindowFunctions outWindowType,
    TrackList& tracks, double inT0, double inT1)
{
    mProgressTrackCount = 0;
    for (auto track : tracks.Selected<WaveTrack>()) {
        mProgressWindowCount = 0;
        if (track->GetRate() != mStatistics.mRate) {
            if (mDoProfile) {
                BasicUI::ShowMessageBox(
                    XO("All noise profile data must have the same sample rate."));
            } else {
                BasicUI::ShowMessageBox(XO(
                                            "The sample rate of the noise profile must match that of the sound to be processed."));
            }
            return false;
        }

        double trackStart = track->GetStartTime();
        double trackEnd = track->GetEndTime();
        double t0 = std::max(trackStart, inT0);
        double t1 = std::min(trackEnd, inT1);

        if (t1 > t0) {
            auto start = track->TimeToLongSamples(t0);
            auto end = track->TimeToLongSamples(t1);
            const auto len = end - start;
            mLen = len;
            const auto extra
                =(mSettings.StepsPerWindow() - 1) * mSettings.SpectrumSize();
            // Adjust denominator for presence or absence of padding,
            // which makes the number of windows visited either more or less
            // than the number of window steps in the data.
            if (mDoProfile) {
                mLen -= extra;
            } else {
                mLen += extra;
            }

            auto t0 = track->LongSamplesToTime(start);
            auto tLen = track->LongSamplesToTime(len);
            std::optional<WaveTrack::Holder> ppTempTrack;
            std::optional<ChannelGroup::ChannelIterator<WaveChannel> > pIter;
            WaveTrack* pFirstTrack {};
            if (!mSettings.mDoProfile) {
                ppTempTrack.emplace(track->EmptyCopy());
                pFirstTrack = ppTempTrack->get();
                pIter.emplace(pFirstTrack->Channels().begin());
            }
            for (const auto pChannel : track->Channels()) {
                auto pOutputTrack = pIter ? *(*pIter)++ : nullptr;
                MyTransformer transformer { *this,
                                            pOutputTrack.get(),
                                            !mSettings.mDoProfile,
                                            inWindowType,
                                            outWindowType,
                                            mSettings.WindowSize(),
                                            mSettings.StepsPerWindow(),
                                            !mSettings.mDoProfile,
                                            !mSettings.mDoProfile };
                if (!transformer.Process(
                        Processor, *pChannel, mHistoryLen, start, len)) {
                    return false;
                }
                ++mProgressTrackCount;
            }
            if (ppTempTrack) {
                TrackSpectrumTransformer::PostProcess(*pFirstTrack, len);
                constexpr auto preserveSplits = true;
                constexpr auto merge = true;
                track->ClearAndPaste(
                    t0, t0 + tLen, **ppTempTrack, preserveSplits, merge);
            }
        }
    }

    if (mDoProfile) {
        if (mStatistics.mTotalWindows == 0) {
            BasicUI::ShowMessageBox(XO("Selected noise profile is too short."));
            return false;
        }
    }

    return true;
}

void NoiseReductionBase::Worker::ApplyFreqSmoothing(FloatVector& gains)
{
    // Given an array of gain mutipliers, average them
    // GEOMETRICALLY.  Don't multiply and take nth root --
    // that may quickly cause underflows.  Instead, average the logs.

    if (mFreqSmoothingBins == 0) {
        return;
    }

    const auto spectrumSize = mSettings.SpectrumSize();

    {
        auto pScratch = mFreqSmoothingScratch.data();
        std::fill(pScratch, pScratch + spectrumSize, 0.0f);
    }

    for (size_t ii = 0; ii < spectrumSize; ++ii) {
        gains[ii] = log(gains[ii]);
    }

    // ii must be signed
    for (int ii = 0; ii < (int)spectrumSize; ++ii) {
        const int j0 = std::max(0, ii - (int)mFreqSmoothingBins);
        const int j1 = std::min(spectrumSize - 1, ii + mFreqSmoothingBins);
        for (int jj = j0; jj <= j1; ++jj) {
            mFreqSmoothingScratch[ii] += gains[jj];
        }
        mFreqSmoothingScratch[ii] /= (j1 - j0 + 1);
    }

    for (size_t ii = 0; ii < spectrumSize; ++ii) {
        gains[ii] = exp(mFreqSmoothingScratch[ii]);
    }
}

NoiseReductionBase::Worker::Worker(
    NoiseReductionBase& effect, const Settings& settings, Statistics& statistics
#ifdef SPECTRAL_EDIT_NOISE_REDUCTION
    ,
    double f0, double f1
#endif
    )
    : mDoProfile{settings.mDoProfile}

    , mEffect{effect}
    , mSettings{settings}
    , mStatistics{statistics}

    , mFreqSmoothingScratch(mSettings.SpectrumSize())
    , mFreqSmoothingBins{size_t(std::max(0.0, settings.mFreqSmoothingBands))}
    , mBinLow{0}
    , mBinHigh{mSettings.SpectrumSize()}

    , mNoiseReductionChoice{settings.mNoiseReductionChoice}
    , mMethod{settings.mMethod}

    // Sensitivity setting is a base 10 log, turn it into a natural log
    , mNewSensitivity{settings.mNewSensitivity * log(10.0)}
{
    const auto sampleRate = mStatistics.mRate;

#ifdef SPECTRAL_EDIT_NOISE_REDUCTION
    {
        // mBinLow is inclusive, mBinHigh is exclusive, of
        // the range of frequencies to affect.  Include any
        // bin that partly overlaps the selected range of frequencies.
        const double bin = sampleRate / mWindowSize;
        if (f0 >= 0.0) {
            mBinLow = floor(f0 / bin);
        }
        if (f1 >= 0.0) {
            mBinHigh = ceil(f1 / bin);
        }
    }
#endif

    const double noiseGain = -settings.mNoiseGain;
    const unsigned nAttackBlocks
        =1 + (int)(settings.mAttackTime * sampleRate / mSettings.StepSize());
    const unsigned nReleaseBlocks
        =1 + (int)(settings.mReleaseTime * sampleRate / mSettings.StepSize());
    // Applies to amplitudes, divide by 20:
    mNoiseAttenFactor = DB_TO_LINEAR(noiseGain);
    // Apply to gain factors which apply to amplitudes, divide by 20:
    mOneBlockAttack = DB_TO_LINEAR(noiseGain / nAttackBlocks);
    mOneBlockRelease = DB_TO_LINEAR(noiseGain / nReleaseBlocks);
    // Applies to power, divide by 10:
    mOldSensitivityFactor = pow(10.0, settings.mOldSensitivity / 10.0);

    mNWindowsToExamine
        =(mMethod == DM_OLD_METHOD)
          ? std::max(2, (int)(minSignalTime * sampleRate / mSettings.StepSize()))
          : 1 + mSettings.StepsPerWindow();

    mCenter = mNWindowsToExamine / 2;
    wxASSERT(mCenter >= 1); // release depends on this assumption

    if (mDoProfile)
#ifdef OLD_METHOD_AVAILABLE
    {
        mHistoryLen = mNWindowsToExamine;
    }
#else
    {
        mHistoryLen = 1;
    }
#endif
    else {
        // Allow long enough queue for sufficient inspection of the middle
        // and for attack processing
        // See ReduceNoise()
        mHistoryLen = std::max(mNWindowsToExamine, mCenter + nAttackBlocks);
    }
}

bool MyTransformer::DoStart()
{
    for (size_t ii = 0, nn = TotalQueueSize(); ii < nn; ++ii) {
        MyWindow& record = NthWindow(ii);
        std::fill(record.mSpectrums.begin(), record.mSpectrums.end(), 0.0);
        std::fill(
            record.mGains.begin(), record.mGains.end(), mWorker.mNoiseAttenFactor);
    }
    return TrackSpectrumTransformer::DoStart();
}

bool NoiseReductionBase::Worker::Processor(SpectrumTransformer& trans)
{
    auto& transformer = static_cast<MyTransformer&>(trans);
    auto& worker = transformer.mWorker;
    // Compute power spectrum in the newest window
    {
        auto& record = transformer.NthWindow(0);
        float* pSpectrum = &record.mSpectrums[0];
        const double dc = record.mRealFFTs[0];
        *pSpectrum++ = dc * dc;
        float* pReal = &record.mRealFFTs[1], * pImag = &record.mImagFFTs[1];
        for (size_t nn = worker.mSettings.SpectrumSize() - 2; nn--;) {
            const double re = *pReal++, im = *pImag++;
            *pSpectrum++ = re * re + im * im;
        }
        const double nyquist = record.mImagFFTs[0];
        *pSpectrum = nyquist * nyquist;
    }

    if (worker.mDoProfile) {
        worker.GatherStatistics(transformer);
    } else {
        worker.ReduceNoise(transformer);
    }

    // Update the Progress meter, let user cancel
    return !worker.mEffect.TrackProgress(
        worker.mProgressTrackCount,
        std::min(
            1.0, ((++worker.mProgressWindowCount).as_double()
                  * worker.mSettings.StepSize())
            / worker.mLen.as_double()));
}

void NoiseReductionBase::Worker::FinishTrackStatistics()
{
    const auto windows = mStatistics.mTrackWindows;

    // Combine averages in case of multiple profile tracks.
    if (windows) {
        const auto multiplier = mStatistics.mTotalWindows;
        const auto denom = windows + multiplier;
        for (size_t ii = 0, nn = mStatistics.mMeans.size(); ii < nn; ++ii) {
            auto& mean = mStatistics.mMeans[ii];
            auto& sum = mStatistics.mSums[ii];
            mean = (mean * multiplier + sum) / denom;
            // Reset for next track
            sum = 0;
        }
        // Reset for next track
        mStatistics.mTrackWindows = 0;
        mStatistics.mTotalWindows = denom;
    }
}

void NoiseReductionBase::Worker::GatherStatistics(MyTransformer& transformer)
{
    ++mStatistics.mTrackWindows;

    {
        // NEW statistics
        auto pPower = transformer.NthWindow(0).mSpectrums.data();
        auto pSum = mStatistics.mSums.data();
        for (size_t jj = 0; jj < mSettings.SpectrumSize(); ++jj) {
            *pSum++ += *pPower++;
        }
    }

#ifdef OLD_METHOD_AVAILABLE
    // The noise threshold for each frequency is the maximum
    // level achieved at that frequency for a minimum of
    // mMinSignalBlocks blocks in a row - the max of a min.

    auto finish = mHistoryLen;

    {
        // old statistics
        auto pPower = NthWindow(0).mSpectrums.data();
        auto pThreshold = mStatistics.mNoiseThreshold.data();
        for (size_t jj = 0; jj < mSpectrumSize; ++jj) {
            float min = *pPower++;
            for (unsigned ii = 1; ii < finish; ++ii) {
                min = std::min(min, NthWindow(ii).mSpectrums[jj]);
            }
            *pThreshold = std::max(*pThreshold, min);
            ++pThreshold;
        }
    }
#endif
}

// Return true iff the given band of the "center" window looks like noise.
// Examine the band in a few neighboring windows to decide.
inline bool NoiseReductionBase::Worker::Classify(
    MyTransformer& transformer, unsigned nWindows, int band)
{
    switch (mMethod) {
#ifdef OLD_METHOD_AVAILABLE
    case DM_OLD_METHOD:
    {
        float min = NthWindow(0).mSpectrums[band];
        for (unsigned ii = 1; ii < nWindows; ++ii) {
            min = std::min(min, NthWindow(ii).mSpectrums[band]);
        }
        return min <= mOldSensitivityFactor * mStatistics.mNoiseThreshold[band];
    }
#endif
    // New methods suppose an exponential distribution of power values
    // in the noise; NEW sensitivity (which is nonnegative) is meant to be
    // the negative of a log of probability (so the log is nonpositive)
    // that noise strays above the threshold.  Call that probability
    // 1 - F.  The quantile function of an exponential distribution is
    // - log (1 - F) * mean.  Thus simply multiply mean by sensitivity
    // to get the threshold.
    case DM_MEDIAN:
        // This method examines the window and all other windows
        // whose centers lie on or between its boundaries, and takes a median, to
        // avoid being fooled by up and down excursions into
        // either the mistake of classifying noise as not noise
        // (leaving a musical noise chime), or the opposite
        // (distorting the signal with a drop out).
        if (nWindows <= 3) {
            // No different from second greatest.
            goto secondGreatest;
        } else if (nWindows <= 5) {
            float greatest = 0.0, second = 0.0, third = 0.0;
            for (unsigned ii = 0; ii < nWindows; ++ii) {
                const float power = transformer.NthWindow(ii).mSpectrums[band];
                if (power >= greatest) {
                    third = second, second = greatest, greatest = power;
                } else if (power >= second) {
                    third = second, second = power;
                } else if (power >= third) {
                    third = power;
                }
            }
            return third <= mNewSensitivity * mStatistics.mMeans[band];
        } else {
            // not implemented
            wxASSERT(false);
            return true;
        }
secondGreatest:
    case DM_SECOND_GREATEST:
    {
        // This method just throws out the high outlier.  It
        // should be less prone to distortions and more prone to
        // chimes.
        float greatest = 0.0, second = 0.0;
        for (unsigned ii = 0; ii < nWindows; ++ii) {
            const float power = transformer.NthWindow(ii).mSpectrums[band];
            if (power >= greatest) {
                second = greatest, greatest = power;
            } else if (power >= second) {
                second = power;
            }
        }
        return second <= mNewSensitivity * mStatistics.mMeans[band];
    }
    default:
        wxASSERT(false);
        return true;
    }
}

void NoiseReductionBase::Worker::ReduceNoise(MyTransformer& transformer)
{
    auto historyLen = transformer.CurrentQueueSize();
    auto nWindows = std::min<unsigned>(mNWindowsToExamine, historyLen);

    const auto spectrumSize = mSettings.SpectrumSize();

    if (mNoiseReductionChoice != NRC_ISOLATE_NOISE) {
        auto& record = transformer.NthWindow(0);
        // Default all gains to the reduction factor,
        // until we decide to raise some of them later
        float* pGain = &record.mGains[0];
        std::fill(pGain, pGain + spectrumSize, mNoiseAttenFactor);
    }

    // Raise the gain for elements in the center of the sliding history
    // or, if isolating noise, zero out the non-noise
    if (nWindows > mCenter) {
        auto pGain = transformer.NthWindow(mCenter).mGains.data();
        if (mNoiseReductionChoice == NRC_ISOLATE_NOISE) {
            // All above or below the selected frequency range is non-noise
            std::fill(pGain, pGain + mBinLow, 0.0f);
            std::fill(pGain + mBinHigh, pGain + spectrumSize, 0.0f);
            pGain += mBinLow;
            for (size_t jj = mBinLow; jj < mBinHigh; ++jj) {
                const bool isNoise = Classify(transformer, nWindows, jj);
                *pGain++ = isNoise ? 1.0 : 0.0;
            }
        } else {
            // All above or below the selected frequency range is non-noise
            std::fill(pGain, pGain + mBinLow, 1.0f);
            std::fill(pGain + mBinHigh, pGain + spectrumSize, 1.0f);
            pGain += mBinLow;
            for (size_t jj = mBinLow; jj < mBinHigh; ++jj) {
                const bool isNoise = Classify(transformer, nWindows, jj);
                if (!isNoise) {
                    *pGain = 1.0;
                }
                ++pGain;
            }
        }
    }

    if (mNoiseReductionChoice != NRC_ISOLATE_NOISE) {
        // In each direction, define an exponential decay of gain from the
        // center; make actual gains the maximum of mNoiseAttenFactor, and
        // the decay curve, and their prior values.

        // First, the attack, which goes backward in time, which is,
        // toward higher indices in the queue.
        for (size_t jj = 0; jj < spectrumSize; ++jj) {
            for (unsigned ii = mCenter + 1; ii < historyLen; ++ii) {
                const float minimum = std::max(
                    mNoiseAttenFactor,
                    transformer.NthWindow(ii - 1).mGains[jj] * mOneBlockAttack);
                float& gain = transformer.NthWindow(ii).mGains[jj];
                if (gain < minimum) {
                    gain = minimum;
                } else {
                    // We can stop now, our attack curve is intersecting
                    // the release curve of some window previously processed.
                    break;
                }
            }
        }

        // Now, release.  We need only look one window ahead.  This part will
        // be visited again when we examine the next window, and
        // carry the decay further.
        {
            auto pNextGain = transformer.NthWindow(mCenter - 1).mGains.data();
            auto pThisGain = transformer.NthWindow(mCenter).mGains.data();
            for (auto nn = mSettings.SpectrumSize(); nn--;) {
                *pNextGain = std::max(
                    *pNextGain,
                    std::max(mNoiseAttenFactor, *pThisGain++ *mOneBlockRelease));
                ++pNextGain;
            }
        }
    }

    if (transformer.QueueIsFull()) {
        auto& record = transformer.NthWindow(historyLen - 1); // end of the queue
        const auto last = mSettings.SpectrumSize() - 1;

        if (mNoiseReductionChoice != NRC_ISOLATE_NOISE) {
            // Apply frequency smoothing to output gain
            // Gains are not less than mNoiseAttenFactor
            ApplyFreqSmoothing(record.mGains);
        }

        // Apply gain to FFT
        {
            const float* pGain = &record.mGains[1];
            float* pReal = &record.mRealFFTs[1];
            float* pImag = &record.mImagFFTs[1];
            auto nn = mSettings.SpectrumSize() - 2;
            if (mNoiseReductionChoice == NRC_LEAVE_RESIDUE) {
                for (; nn--;) {
                    // Subtract the gain we would otherwise apply from 1, and
                    // negate that to flip the phase.
                    const double gain = *pGain++ - 1.0;
                    *pReal++ *= gain;
                    *pImag++ *= gain;
                }
                record.mRealFFTs[0] *= (record.mGains[0] - 1.0);
                // The Fs/2 component is stored as the imaginary part of the DC
                // component
                record.mImagFFTs[0] *= (record.mGains[last] - 1.0);
            } else {
                for (; nn--;) {
                    const double gain = *pGain++;
                    *pReal++ *= gain;
                    *pImag++ *= gain;
                }
                record.mRealFFTs[0] *= record.mGains[0];
                // The Fs/2 component is stored as the imaginary part of the DC
                // component
                record.mImagFFTs[0] *= record.mGains[last];
            }
        }
    }
}

bool MyTransformer::DoFinish()
{
    if (mWorker.mDoProfile) {
        mWorker.FinishTrackStatistics();
    }
    return TrackSpectrumTransformer::DoFinish();
}
