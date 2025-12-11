/*
 * Audacity: A Digital Audio Editor
 */
#include "au3spectrogramsettings.h"

#include "au3-basic-ui/BasicUI.h"
#include "au3-fft/FFT.h"
#include "au3-screen-geometry/NumberScale.h"
#include "au3-wave-track/WaveTrack.h"

#include <algorithm>
#include <cmath>

namespace au::spectrogram {
namespace {
static const AttachedTrackObjects::RegisteredFactory key1{
    [](::Track&) -> std::shared_ptr<Au3SpectrogramSettings> {
        return std::make_shared<Au3SpectrogramSettings>();
    }
};
}

const Au3SpectrogramSettings& Au3SpectrogramSettings::Get(const WaveTrack& track)
{
    return const_cast<WaveTrack&>(track).AttachedTrackObjects::Get<Au3SpectrogramSettings>(key1);
}

Au3SpectrogramSettings& Au3SpectrogramSettings::Get(WaveTrack& track)
{
    return track.AttachedTrackObjects::Get<Au3SpectrogramSettings>(key1);
}

Au3SpectrogramSettings::Au3SpectrogramSettings(const Au3SpectrogramSettings& other)
    : minFreq(other.minFreq)
    , maxFreq(other.maxFreq)
    , range(other.range)
    , gain(other.gain)
    , frequencyGain(other.frequencyGain)
    , windowType(other.windowType)
    , windowSize(other.windowSize)
    , zeroPaddingFactor(other.zeroPaddingFactor)
    , colorScheme(other.colorScheme)
    , scaleType(other.scaleType)
    , spectralSelection(other.spectralSelection)
    , algorithm(other.algorithm)

    // Do not copy these!
    , hFFT{}
    , window{}
    , tWindow{}
    , dWindow{}
{
}

Au3SpectrogramSettings& Au3SpectrogramSettings::operator=(const Au3SpectrogramSettings& other)
{
    if (this != &other) {
        minFreq = other.minFreq;
        maxFreq = other.maxFreq;
        range = other.range;
        gain = other.gain;
        frequencyGain = other.frequencyGain;
        windowType = other.windowType;
        windowSize = other.windowSize;
        zeroPaddingFactor = other.zeroPaddingFactor;
        colorScheme = other.colorScheme;
        scaleType = other.scaleType;
        spectralSelection = other.spectralSelection;
        algorithm = other.algorithm;

        // Invalidate the caches
        DestroyWindows();
    }
    return *this;
}

//static
const EnumValueSymbols& Au3SpectrogramSettings::GetScaleNames()
{
    static const EnumValueSymbols result{
        // Keep in correspondence with enum Au3SpectrogramSettings::ScaleType:
        XO("Linear"),
        XO("Logarithmic"),
        /* i18n-hint: The name of a frequency scale in psychoacoustics */
        XO("Mel"),
        /* i18n-hint: The name of a frequency scale in psychoacoustics, named for Heinrich Barkhausen */
        XO("Bark"),
        /* i18n-hint: The name of a frequency scale in psychoacoustics, abbreviates Equivalent Rectangular Bandwidth */
        XO("ERB"),
        /* i18n-hint: Time units, that is Period = 1 / Frequency */
        XO("Period"),
    };
    return result;
}

//static
const EnumValueSymbols& Au3SpectrogramSettings::GetColorSchemeNames()
{
    static const EnumValueSymbols result{
        // Keep in correspondence with enum Au3SpectrogramSettings::ColorScheme:
        /* i18n-hint: New color scheme for spectrograms, Roseus is proper name of the color scheme */
        { wxT("SpecColorNew"),     XC("Color (Roseus)",    "spectrum prefs") },
        /* i18n-hint: Classic color scheme(from theme) for spectrograms */
        { wxT("SpecColorTheme"),   XC("Color (classic)",   "spectrum prefs") },
        /* i18n-hint: Grayscale color scheme for spectrograms */
        { wxT("SpecGrayscale"),    XC("Grayscale",         "spectrum prefs") },
        /* i18n-hint: Inverse grayscale color scheme for spectrograms */
        { wxT("SpecInvGrayscale"), XC("Inverse grayscale", "spectrum prefs") },
    };

    wxASSERT(csNumColorScheme == result.size());

    return result;
}

//static
const TranslatableStrings& Au3SpectrogramSettings::GetAlgorithmNames()
{
    static const TranslatableStrings results{
        // Keep in correspondence with enum Au3SpectrogramSettings::Algorithm:
        XO("Frequencies"),
        /* i18n-hint: the Reassignment algorithm for spectrograms */
        XO("Reassignment"),
        /* i18n-hint: EAC abbreviates "Enhanced Autocorrelation" */
        XO("Pitch (EAC)"),
    };
    return results;
}

bool Au3SpectrogramSettings::Validate(bool quiet)
{
    if (!quiet
        && maxFreq < 100) {
        BasicUI::ShowMessageBox(XO("Maximum frequency must be 100 Hz or above"));
        return false;
    } else {
        maxFreq = std::max(100, maxFreq);
    }

    if (!quiet
        && minFreq < 0) {
        BasicUI::ShowMessageBox(XO("Minimum frequency must be at least 0 Hz"));
        return false;
    } else {
        minFreq = std::max(0, minFreq);
    }

    if (!quiet
        && maxFreq <= minFreq) {
        BasicUI::ShowMessageBox(XO(
                                    "Minimum frequency must be less than maximum frequency"));
        return false;
    } else {
        maxFreq = std::max(1 + minFreq, maxFreq);
    }

    if (!quiet
        && range <= 0) {
        BasicUI::ShowMessageBox(XO("The range must be at least 1 dB"));
        return false;
    } else {
        range = std::max(1, range);
    }

    if (!quiet
        && frequencyGain < 0) {
        BasicUI::ShowMessageBox(XO("The frequency gain cannot be negative"));
        return false;
    } else if (!quiet
               && frequencyGain > 60) {
        BasicUI::ShowMessageBox(XO(
                                    "The frequency gain must be no more than 60 dB/dec"));
        return false;
    } else {
        frequencyGain
            =std::max(0, std::min(60, frequencyGain));
    }

    // The rest are controlled by drop-down menus so they can't go wrong
    // in the Preferences dialog, but we also come here after reading fom saved
    // preference files, which could be or from future versions.  Validate quietly.
    windowType
        =std::max(0, std::min(NumWindowFuncs() - 1, windowType));
    scaleType
        =ScaleType(std::max(0,
                            std::min((int)(Au3SpectrogramSettings::stNumScaleTypes)-1,
                                     (int)(scaleType))));
    colorScheme = ColorScheme(
        std::max(0, std::min<int>(csNumColorScheme - 1, colorScheme))
        );
    algorithm = Algorithm(
        std::max(0, std::min((int)(algNumAlgorithms) - 1, (int)(algorithm)))
        );
    ConvertToEnumeratedWindowSizes();
    ConvertToActualWindowSizes();

    return true;
}

void Au3SpectrogramSettings::InvalidateCaches()
{
    DestroyWindows();
}

Au3SpectrogramSettings::~Au3SpectrogramSettings()
{
    DestroyWindows();
}

void Au3SpectrogramSettings::CopyTo(::Track& track) const
{
    auto& specSettings = Au3SpectrogramSettings::Get(static_cast<WaveTrack&>(track));
    specSettings = *this;
}

void Au3SpectrogramSettings::WriteXMLAttributes(XMLWriter& writer) const
{
    writer.WriteAttr("syncWithGlobalSettings", syncWithGlobalSettings);
    writer.WriteAttr("minFreq", minFreq);
    writer.WriteAttr("maxFreq", maxFreq);
    writer.WriteAttr("range", range);
    writer.WriteAttr("gain", gain);
    writer.WriteAttr("frequencyGain", frequencyGain);
    writer.WriteAttr("windowType", windowType);
    writer.WriteAttr("windowSize", windowSize);
    writer.WriteAttr("zeroPaddingFactor", zeroPaddingFactor);
    writer.WriteAttr("colorScheme", static_cast<int>(colorScheme));
    writer.WriteAttr("scaleType", static_cast<int>(scaleType));
    writer.WriteAttr("spectralSelection", spectralSelection);
    writer.WriteAttr("algorithm", static_cast<int>(algorithm));
}

bool Au3SpectrogramSettings::HandleXMLAttribute(const std::string_view& attr, const XMLAttributeValueView& valueView)
{
    int nValue;
    if (attr == "syncWithGlobalSettings" && valueView.TryGet(nValue)) {
        syncWithGlobalSettings = (nValue != 0);
        return true;
    } else if (attr == "minFreq" && valueView.TryGet(nValue)) {
        minFreq = nValue;
        return true;
    } else if (attr == "maxFreq" && valueView.TryGet(nValue)) {
        maxFreq = nValue;
        return true;
    } else if (attr == "range" && valueView.TryGet(nValue)) {
        range = nValue;
        return true;
    } else if (attr == "gain" && valueView.TryGet(nValue)) {
        gain = nValue;
        return true;
    } else if (attr == "frequencyGain" && valueView.TryGet(nValue)) {
        frequencyGain = nValue;
        return true;
    } else if (attr == "windowType" && valueView.TryGet(nValue)) {
        windowType = nValue;
        return true;
    } else if (attr == "windowSize" && valueView.TryGet(nValue)) {
        windowSize = nValue;
        return true;
    } else if (attr == "zeroPaddingFactor" && valueView.TryGet(nValue)) {
        zeroPaddingFactor = nValue;
        return true;
    } else if (attr == "colorScheme" && valueView.TryGet(nValue)) {
        colorScheme = ColorScheme(nValue);
        return true;
    } else if (attr == "scaleType" && valueView.TryGet(nValue)) {
        scaleType = ScaleType(nValue);
        return true;
    } else if (attr == "spectralSelection" && valueView.TryGet(nValue)) {
        spectralSelection = (nValue != 0);
        return true;
    } else if (attr == "algorithm" && valueView.TryGet(nValue)) {
        algorithm = Algorithm(nValue);
        return true;
    }
    return false;
}

void Au3SpectrogramSettings::DestroyWindows()
{
    hFFT.reset();
    window.reset();
    dWindow.reset();
    tWindow.reset();
}

namespace {
enum {
    WINDOW, TWINDOW, DWINDOW
};
void RecreateWindow(
    Floats& window, int which, size_t fftLen,
    size_t padding, int windowType, size_t windowSize, double& scale)
{
    // Create the requested window function
    window = Floats{ fftLen };
    size_t ii;

    const bool extra = padding > 0;
    wxASSERT(windowSize % 2 == 0);
    if (extra) {
        // For windows that do not go to 0 at the edges, this improves symmetry
        ++windowSize;
    }
    const size_t endOfWindow = padding + windowSize;
    // Left and right padding
    for (ii = 0; ii < padding; ++ii) {
        window[ii] = 0.0;
        window[fftLen - ii - 1] = 0.0;
    }
    // Default rectangular window in the middle
    for (; ii < endOfWindow; ++ii) {
        window[ii] = 1.0;
    }
    // Overwrite middle as needed
    switch (which) {
    case WINDOW:
        NewWindowFunc(windowType, windowSize, extra, window.get() + padding);
        break;
    case TWINDOW:
        NewWindowFunc(windowType, windowSize, extra, window.get() + padding);
        {
            for (int jj = padding, multiplier = -(int)windowSize / 2; jj < (int)endOfWindow; ++jj, ++multiplier) {
                window[jj] *= multiplier;
            }
        }
        break;
    case DWINDOW:
        DerivativeOfWindowFunc(windowType, windowSize, extra, window.get() + padding);
        break;
    default:
        wxASSERT(false);
    }
    // Scale the window function to give 0dB spectrum for 0dB sine tone
    if (which == WINDOW) {
        scale = 0.0;
        for (ii = padding; ii < endOfWindow; ++ii) {
            scale += window[ii];
        }
        if (scale > 0) {
            scale = 2.0 / scale;
        }
    }
    for (ii = padding; ii < endOfWindow; ++ii) {
        window[ii] *= scale;
    }
}
}

void Au3SpectrogramSettings::CacheWindows()
{
    if (hFFT == NULL || window == NULL || (algorithm == algReassignment && (tWindow == NULL || dWindow == NULL))) {
        double scale;
        auto factor = ZeroPaddingFactor();
        const auto fftLen = WindowSize() * factor;
        const auto padding = (WindowSize() * (factor - 1)) / 2;

        hFFT = GetFFT(fftLen);
        RecreateWindow(window, WINDOW, fftLen, padding, windowType, windowSize, scale);
        if (algorithm == algReassignment) {
            RecreateWindow(tWindow, TWINDOW, fftLen, padding, windowType, windowSize, scale);
            RecreateWindow(dWindow, DWINDOW, fftLen, padding, windowType, windowSize, scale);
        }
    }
}

namespace {
constexpr auto isPowerOfTwo(int x) -> bool
{
    return (x != 0) && ((x & (x - 1)) == 0);
}

static_assert(isPowerOfTwo(3) == false);
static_assert(isPowerOfTwo(4) == true);
}

void Au3SpectrogramSettings::SetWindowSize(int size)
{
    assert(isPowerOfTwo(size));
    windowSize = size;
    InvalidateCaches();
}

void Au3SpectrogramSettings::ConvertToEnumeratedWindowSizes()
{
    unsigned size;
    int logarithm;

    logarithm = -LogMinWindowSize;
    size = unsigned(windowSize);
    while (size > 1) {
        size >>= 1, ++logarithm;
    }
    windowSize = std::max(0, std::min(NumWindowSizes - 1, logarithm));

    // Choices for zero padding begin at 1
    logarithm = 0;
    size = unsigned(zeroPaddingFactor);
    while (zeroPaddingFactor > 1) {
        zeroPaddingFactor >>= 1, ++logarithm;
    }
    zeroPaddingFactor = std::max(0,
                                 std::min(LogMaxWindowSize - (windowSize + LogMinWindowSize),
                                          logarithm
                                          ));
}

void Au3SpectrogramSettings::ConvertToActualWindowSizes()
{
    windowSize = 1 << (windowSize + LogMinWindowSize);
    zeroPaddingFactor = 1 << zeroPaddingFactor;
}

float Au3SpectrogramSettings::findBin(float frequency, float binUnit) const
{
    float linearBin = frequency / binUnit;
    if (linearBin < 0) {
        return -1;
    } else {
        return linearBin;
    }
}

size_t Au3SpectrogramSettings::GetFFTLength() const
{
//#ifndef EXPERIMENTAL_ZERO_PADDED_SPECTROGRAMS
    // return windowSize;
//#else
    return windowSize * ((algorithm != algPitchEAC) ? zeroPaddingFactor : 1);
//#endif
}

size_t Au3SpectrogramSettings::NBins() const
{
    // Omit the Nyquist frequency bin
    return GetFFTLength() / 2;
}

NumberScale Au3SpectrogramSettings::GetScale(float minFreqIn, float maxFreqIn) const
{
    NumberScaleType type = nstLinear;

    // Don't assume the correspondence of the enums will remain direct in the future.
    // Do this switch.
    switch (scaleType) {
    default:
        wxASSERT(false);
    case stLinear:
        type = nstLinear;
        break;
    case stLogarithmic:
        type = nstLogarithmic;
        break;
    case stMel:
        type = nstMel;
        break;
    case stBark:
        type = nstBark;
        break;
    case stErb:
        type = nstErb;
        break;
    case stPeriod:
        type = nstPeriod;
        break;
    }

    return NumberScale(type, minFreqIn, maxFreqIn);
}

bool Au3SpectrogramSettings::SpectralSelectionEnabled() const
{
    return spectralSelection;
}

static const ChannelGroup::Attachments::RegisteredFactory
    key2{ [](auto&) { return std::make_unique<SpectrogramBounds>(); } };

SpectrogramBounds& SpectrogramBounds::Get(WaveTrack& track)
{
    return track.Attachments::Get<SpectrogramBounds>(key2);
}

const SpectrogramBounds& SpectrogramBounds::Get(
    const WaveTrack& track)
{
    return Get(const_cast<WaveTrack&>(track));
}

SpectrogramBounds::~SpectrogramBounds() = default;

auto SpectrogramBounds::Clone() const -> PointerType
{
    return std::make_unique<SpectrogramBounds>(*this);
}

void SpectrogramBounds::GetBounds(
    const WaveTrack& wt, float& min, float& max) const
{
    const double rate = wt.GetRate();

    const auto& settings = Au3SpectrogramSettings::Get(wt);
    const auto type = settings.scaleType;

    const float top = (rate / 2.);

    float bottom;
    if (type == Au3SpectrogramSettings::stLinear) {
        bottom = 0.0f;
    } else if (type == Au3SpectrogramSettings::stPeriod) {
        // special case
        const auto half = settings.GetFFTLength() / 2;
        // EAC returns no data for below this frequency:
        const float bin2 = rate / half;
        bottom = bin2;
    } else {
        // logarithmic, etc.
        bottom = 1.0f;
    }

    {
        float spectrumMax = mSpectrumMax;
        if (spectrumMax < 0) {
            spectrumMax = settings.maxFreq;
        }
        if (spectrumMax < 0) {
            max = top;
        } else {
            max = std::clamp(spectrumMax, bottom, top);
        }
    }

    {
        float spectrumMin = mSpectrumMin;
        if (spectrumMin < 0) {
            spectrumMin = settings.minFreq;
        }
        if (spectrumMin < 0) {
            min = std::max(bottom, top / 1000.0f);
        } else {
            min = std::clamp(spectrumMin, bottom, top);
        }
    }
}
}
