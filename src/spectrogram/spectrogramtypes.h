/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/actions/actiontypes.h"

#include "shared/axis/axisscale.h"
#include "shared/axis/axistypes.h"
#include "shared/axis/numberscale.h"

namespace au::spectrogram {
static const muse::actions::ActionCode TRACK_SPECTROGRAM_SETTINGS_ACTION("track-spectrogram-settings");

/** SpectrogramScale and NumberScale are aliases for the generic axis types
 *  from the shared module. Kept here to avoid churn across spectrogram call
 *  sites. */
using SpectrogramScale = au::shared::AxisScale;
using NumberScale = au::shared::NumberScale;

enum class SpectrogramColorScheme {
    Roseus = 0,
    Classic,
    Grayscale,
    InverseGrayscale,
    _count
};

enum class SpectrogramAlgorithm {
    Frequencies = 0,
    Reassignment,
    Pitch,
    _count
};

enum class SpectrogramWindowType {
    Rectangular = 0,
    Bartlett,
    Hamming,
    Hann,
    Blackman,
    BlackmanHarris,
    Welch,
    Gaussian25,
    Gaussian35,
    Gaussian45,
    _count
};

struct SelectionInfo {
    static constexpr int UndefinedFrequency = -1;

    const double startTime = 0.0;
    const double endTime = 0.0;
    const double startFrequency = UndefinedFrequency;
    const double endFrequency = UndefinedFrequency;
    const bool clipSelected = false;
};

struct ClipChannelInfo {
    const int clipId = -1;
    const int trackId = -1;
    const int channel = -1;
    const int xPaintBegin = 0;
    const int xPaintEnd = 0;
};

struct ViewInfo {
    const double channelHeight = 0.;
    const double viewportStartTime = 0.;
    const double viewportEndTime = 0.;
    const double pixelsPerSecond = 0.; // aka zoom
};

class FrequencySelection
{
public:
    FrequencySelection() = default;
    FrequencySelection(int trackId);

    int trackId = -1;

    void setFrequencyRange(double f1, double f2, SpectrogramScale scale);

    double startFrequency() const { return m_startFrequency; }
    double endFrequency() const { return m_endFrequency; }
    double centerFrequency() const { return m_centerFrequency; }

    bool isValid() const;

    bool operator==(const FrequencySelection& other) const;
    bool operator!=(const FrequencySelection& other) const;

private:
    friend class FrequencySelectionController;

    double m_startFrequency = SelectionInfo::UndefinedFrequency;
    double m_endFrequency = SelectionInfo::UndefinedFrequency;
    double m_centerFrequency = SelectionInfo::UndefinedFrequency;
};

using SpectrogramRulerTick = au::shared::AxisTick;
using SpectrogramRulerTicks = au::shared::AxisTicks;

enum class SpectralEffectId {
    DeleteSelection,
    DeleteCenterFrequency,
    AmplifySelection,
    AmplifyCenterFrequency,
};

struct SpectralEffect {
    SpectralEffectId spectralEffectId;
    muse::actions::ActionCode action;
    muse::String title;
};

using SpectralEffectList = std::vector<SpectralEffect>;
}
