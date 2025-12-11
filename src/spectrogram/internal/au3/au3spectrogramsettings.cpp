/*
 * Audacity: A Digital Audio Editor
 */
#include "au3spectrogramsettings.h"

#include "iglobalspectrogramconfiguration.h"

#include "au3-basic-ui/BasicUI.h"
#include "au3-fft/FFT.h"
#include "au3-screen-geometry/NumberScale.h"
#include "au3-wave-track/WaveTrack.h"

#include "framework/global/log.h"
#include "internal/au3/au3spectrogramutils.h"

#include <algorithm>
#include <cmath>

namespace au::spectrogram {
namespace {
static std::weak_ptr<IGlobalSpectrogramConfiguration> sGlobalConfiguration;

static const AttachedTrackObjects::RegisteredFactory key1{
    [](::Track&) -> std::shared_ptr<Au3SpectrogramSettings> {
        auto settings = std::make_shared<Au3SpectrogramSettings>();
        const auto globalConfig = sGlobalConfiguration.lock();
        IF_ASSERT_FAILED(globalConfig) {
            return settings;
        }

        const SpectrogramSettings globalSettings = globalConfig->allSettings();
        settings->range = globalSettings.colorRangeDb;
        settings->gain = globalSettings.colorGainDb;
        settings->frequencyGain = globalSettings.colorHighBoostDbPerDec;
        settings->SetWindowType(toAu3WindowType(globalSettings.windowType));
        settings->SetWindowSize(1 << globalSettings.winSizeLog2);
        settings->SetZeroPaddingFactor(globalSettings.zeroPaddingFactor);
        settings->colorScheme = toAu3ColorScheme(globalSettings.colorScheme);
        settings->scaleType = toAu3Scale(globalSettings.scale);
        settings->spectralSelectionEnabled = globalSettings.spectralSelectionEnabled;
        settings->algorithm = toAu3Algorithm(globalSettings.algorithm);

        return settings;
    }
};
}

void Au3SpectrogramSettings::setGlobalSpectrogramConfiguration(std::weak_ptr<IGlobalSpectrogramConfiguration> globalConfig)
{
    sGlobalConfiguration = globalConfig;
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
    , m_windowType(other.m_windowType)
    , m_windowSize(other.m_windowSize)
    , m_zeroPaddingFactor(other.m_zeroPaddingFactor)
    , colorScheme(other.colorScheme)
    , scaleType(other.scaleType)
    , spectralSelectionEnabled(other.spectralSelectionEnabled)
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
        m_windowType = other.m_windowType;
        m_windowSize = other.m_windowSize;
        m_zeroPaddingFactor = other.m_zeroPaddingFactor;
        colorScheme = other.colorScheme;
        scaleType = other.scaleType;
        spectralSelectionEnabled = other.spectralSelectionEnabled;
        algorithm = other.algorithm;

        // Invalidate the caches
        DestroyWindows();
    }
    return *this;
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
    writer.WriteAttr("m_windowType", m_windowType);
    writer.WriteAttr("m_windowSize", m_windowSize);
    writer.WriteAttr("zeroPaddingFactor", m_zeroPaddingFactor);
    writer.WriteAttr("colorScheme", static_cast<int>(colorScheme));
    writer.WriteAttr("scaleType", static_cast<int>(scaleType));
    writer.WriteAttr("spectralSelectionEnabled", spectralSelectionEnabled);
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
    } else if (attr == "m_windowType" && valueView.TryGet(nValue)) {
        m_windowType = nValue;
        return true;
    } else if (attr == "m_windowSize" && valueView.TryGet(nValue)) {
        m_windowSize = nValue;
        return true;
    } else if (attr == "zeroPaddingFactor" && valueView.TryGet(nValue)) {
        m_zeroPaddingFactor = nValue;
        return true;
    } else if (attr == "colorScheme" && valueView.TryGet(nValue)) {
        colorScheme = ColorScheme(nValue);
        return true;
    } else if (attr == "scaleType" && valueView.TryGet(nValue)) {
        scaleType = ScaleType(nValue);
        return true;
    } else if (attr == "spectralSelectionEnabled" && valueView.TryGet(nValue)) {
        spectralSelectionEnabled = (nValue != 0);
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
    size_t padding, int m_windowType, size_t m_windowSize, double& scale)
{
    // Create the requested window function
    window = Floats{ fftLen };
    size_t ii;

    const bool extra = padding > 0;
    wxASSERT(m_windowSize % 2 == 0);
    if (extra) {
        // For windows that do not go to 0 at the edges, this improves symmetry
        ++m_windowSize;
    }
    const size_t endOfWindow = padding + m_windowSize;
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
        NewWindowFunc(m_windowType, m_windowSize, extra, window.get() + padding);
        break;
    case TWINDOW:
        NewWindowFunc(m_windowType, m_windowSize, extra, window.get() + padding);
        {
            for (int jj = padding, multiplier = -(int)m_windowSize / 2; jj < (int)endOfWindow; ++jj, ++multiplier) {
                window[jj] *= multiplier;
            }
        }
        break;
    case DWINDOW:
        DerivativeOfWindowFunc(m_windowType, m_windowSize, extra, window.get() + padding);
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
        RecreateWindow(window, WINDOW, fftLen, padding, m_windowType, m_windowSize, scale);
        if (algorithm == algReassignment) {
            RecreateWindow(tWindow, TWINDOW, fftLen, padding, m_windowType, m_windowSize, scale);
            RecreateWindow(dWindow, DWINDOW, fftLen, padding, m_windowType, m_windowSize, scale);
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
    m_windowSize = size;
    DestroyWindows();
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
    return m_windowSize * ((algorithm != algPitchEAC) ? m_zeroPaddingFactor : 1);
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

void Au3SpectrogramSettings::SetZeroPaddingFactor(int factor)
{
    assert(isPowerOfTwo(factor));
    m_zeroPaddingFactor = factor;
    DestroyWindows();
}

void Au3SpectrogramSettings::SetWindowType(int type)
{
    m_windowType = type;
    DestroyWindows();
}
size_t Au3SpectrogramSettings::ZeroPaddingFactor() const {
  return algorithm == algPitchEAC ? 1 : m_zeroPaddingFactor;
}
} // namespace au::spectrogram
