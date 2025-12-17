/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "iglobalspectrogramconfiguration.h"
#include "spectrogramtypes.h"

#include "au3-math/SampleFormat.h"
#include "au3-fft/RealFFTf.h"
#include "au3-track/TrackAttachment.h"

class EnumValueSymbols;
struct FFTParam;
class NumberScale;
class SpectrumPrefs;
class wxArrayStringEx;
class WaveChannel;
class WaveTrack;

namespace au::spectrogram {
class Au3SpectrogramSettings : public TrackAttachment
{
public:
    static void setGlobalSpectrogramConfiguration(std::weak_ptr<IGlobalSpectrogramConfiguration> globalConfig);

    static const Au3SpectrogramSettings& Get(const WaveTrack&);
    static Au3SpectrogramSettings& Get(WaveTrack&);

    Au3SpectrogramSettings() = default;
    Au3SpectrogramSettings(const Au3SpectrogramSettings& other);
    Au3SpectrogramSettings& operator=(const Au3SpectrogramSettings& other);
    ~Au3SpectrogramSettings();

    void CacheWindows();
    float findBin(float frequency, float binUnit) const;
    NumberScale GetScale(float minFreq, float maxFreq) const;

public:
    bool syncWithGlobalSettings = true;
    int range = 0;
    int gain = 0;
    int frequencyGain = 0;
    SpectrogramColorScheme colorScheme = static_cast<SpectrogramColorScheme>(0);
    SpectrogramScale scaleType = static_cast<SpectrogramScale>(0);
    SpectrogramAlgorithm algorithm = static_cast<SpectrogramAlgorithm>(0);
    // For now. When rulers are implemented for spectrogram view, this may need some refactoring.
    int minFreq = 1;
    int maxFreq = 20000;

    SpectrogramWindowType WindowType() const { return m_windowType; }
    void SetWindowType(SpectrogramWindowType type);

    int WindowSize() const { return m_windowSize; }
    void SetWindowSize(int size);

    size_t ZeroPaddingFactor() const;
    void SetZeroPaddingFactor(int factor);

    size_t GetFFTLength() const;
    size_t NBins() const;

    // Variables used for computing the spectrum
    HFFT hFFT;
    Floats window;

    // Two other windows for computing reassigned spectrogram
    Floats tWindow;        // Window times time parameter
    Floats dWindow;        // Derivative of window

private:
    enum {
        LogMinWindowSize = 3,
        LogMaxWindowSize = 15,

        NumWindowSizes = LogMaxWindowSize - LogMinWindowSize + 1,
    };

private:
    void DestroyWindows();

    void CopyTo(::Track& track) const override;
    void WriteXMLAttributes(XMLWriter& writer) const override;
    bool HandleXMLAttribute(const std::string_view& attr, const XMLAttributeValueView& valueVuew) override;

private:
    // Changing any of these destroys the windows
    SpectrogramWindowType m_windowType = static_cast<SpectrogramWindowType>(0);
    int m_windowSize = 0;
    int m_zeroPaddingFactor = 0;
};

class SpectrogramBounds : public ClientData::Cloneable<>
{
public:

    //! Get either the global default settings, or the track's own if previously created
    static SpectrogramBounds& Get(WaveTrack& track);

    //! @copydoc Get(WaveTrack&)
    static const SpectrogramBounds& Get(const WaveTrack& track);

    ~SpectrogramBounds() override;
    PointerType Clone() const override;

    void GetBounds(const WaveTrack& track, float& min, float& max) const;

    void SetBounds(float min, float max)
    { mSpectrumMin = min, mSpectrumMax = max; }

private:
    float mSpectrumMin = -1, mSpectrumMax = -1;
};
}
