/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "iglobalspectrogramconfiguration.h"

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

    enum {
        LogMinWindowSize = 3,
        LogMaxWindowSize = 15,

        NumWindowSizes = LogMaxWindowSize - LogMinWindowSize + 1,
    };

    // Do not assume that this enumeration will remain the
    // same as NumberScaleType in future.  That enum may become
    // more general purpose.
    typedef int ScaleType;
    enum ScaleTypeValues : int {
        stLinear,
        stLogarithmic,
        stMel,
        stBark,
        stErb,
        stPeriod,

        stNumScaleTypes,
    };

    static const EnumValueSymbols& GetScaleNames();
    static const EnumValueSymbols& GetColorSchemeNames();
    static const TranslatableStrings& GetAlgorithmNames();

    static const Au3SpectrogramSettings& Get(const WaveTrack&);
    static Au3SpectrogramSettings& Get(WaveTrack&);

    Au3SpectrogramSettings() = default;
    Au3SpectrogramSettings(const Au3SpectrogramSettings& other);
    Au3SpectrogramSettings& operator=(const Au3SpectrogramSettings& other);
    ~Au3SpectrogramSettings();

    void CopyTo(::Track& track) const override;
    void WriteXMLAttributes(XMLWriter& writer) const override;
    bool HandleXMLAttribute(const std::string_view& attr, const XMLAttributeValueView& valueVuew) override;

    bool Validate(bool quiet);

    void InvalidateCaches();
    void DestroyWindows();
    void CacheWindows();
    void ConvertToEnumeratedWindowSizes();
    void ConvertToActualWindowSizes();

    // Need to be told what the bin unit is, as this structure does not know
    // the rate
    float findBin(float frequency, float binUnit) const;

    // If "bins" is false, units are Hz
    NumberScale GetScale(float minFreq, float maxFreq) const;

    // For now. When rulers are implemented for spectrogram view, this may need some refactoring.
    int minFreq = 1;
    int maxFreq = 20000;

    bool SpectralSelectionEnabled() const;

public:
    bool syncWithGlobalSettings = true;

    int range = 0;
    int gain = 0;
    int frequencyGain = 0;

    int windowType = 0;

private:
    int windowSize = 0;
public:
    int WindowSize() const { return windowSize; }
    void SetWindowSize(int size);

    int zeroPaddingFactor = 0;
public:
    size_t ZeroPaddingFactor() const
    {
        return algorithm == algPitchEAC ? 1 : zeroPaddingFactor;
    }

    size_t GetFFTLength() const; // window size (times zero padding, if STFT)
    size_t NBins() const;

    enum ColorScheme : int {
        // Keep in correspondence with AColor::colorSchemes, AColor::gradient_pre
        csColorNew = 0,
        csColorTheme,
        csGrayscale,
        csInvGrayscale,

        csNumColorScheme,
    };
    ColorScheme colorScheme = csColorNew;

    ScaleType scaleType = stLogarithmic;

    bool spectralSelection = false; // But should this vary per track? -- PRL - I've also asked the question, awaiting response. -- MH

    typedef int Algorithm;
    enum AlgorithmValues : int {
        algSTFT = 0,
        algReassignment,
        algPitchEAC,

        algNumAlgorithms,
    };
    Algorithm algorithm = algSTFT;

    // Variables used for computing the spectrum
    HFFT hFFT;
    Floats window;

    // Two other windows for computing reassigned spectrogram
    Floats tWindow;        // Window times time parameter
    Floats dWindow;        // Derivative of window
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
