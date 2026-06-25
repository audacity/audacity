/*
 * Audacity: A Digital Audio Editor
 */
#include "spectrogramchannelrulermodel.h"

#include "framework/global/types/number.h"
#include "framework/global/log.h"
#include "spectrogramtypes.h"
#include "shared/axis/axislabel.h"
#include "shared/axis/axisticks.h"

namespace au::spectrogram {
SpectrogramChannelRulerModel::SpectrogramChannelRulerModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void SpectrogramChannelRulerModel::componentComplete()
{
    spectrogramService()->trackSpectrogramConfigurationChanged().onReceive(this, [this](int trackId){
        if (trackId == m_trackId) {
            updateTicks();
            emit ticksChanged();
            emit zoomStateChanged();
        }
    });

    spectrogramViewService()->rulerGuideFrequencyChanged().onReceive(this, [this](int trackId) {
        if (trackId == m_trackId) {
            emit rulerGuideYPosChanged();
        }
    });

    updateTicks();
    emit ticksChanged();

    uiConfig()->currentThemeChanged().onNotify(this, [this] {
        emit isHighContrastChanged();
    });
}

void SpectrogramChannelRulerModel::updateTicks()
{
    if (m_labelHeight <= 0 || m_channelHeight <= 0) {
        m_ticks = SpectrogramRulerTicks {};
        return;
    }
    const auto config = spectrogramService()->trackSpectrogramConfiguration(m_trackId);
    IF_ASSERT_FAILED(config) {
        return;
    }
    m_ticks = au::shared::axisTicks(config->minFreq(), config->maxFreq(), config->scale());
}

int SpectrogramChannelRulerModel::trackId() const
{
    return m_trackId;
}

void SpectrogramChannelRulerModel::setTrackId(int trackId)
{
    if (m_trackId == trackId) {
        return;
    }
    m_trackId = trackId;
    updateTicks();
    emit trackIdChanged();
    emit ticksChanged();
}

void SpectrogramChannelRulerModel::setLabelHeight(int height)
{
    if (m_labelHeight == height) {
        return;
    }
    m_labelHeight = height;
    updateTicks();
    emit labelHeightChanged();
    emit ticksChanged();
}

double SpectrogramChannelRulerModel::channelHeight() const
{
    return m_channelHeight;
}

void SpectrogramChannelRulerModel::setChannelHeight(double height)
{
    if (muse::is_equal(m_channelHeight, height)) {
        return;
    }
    m_channelHeight = height;
    updateTicks();
    emit channelHeightChanged();
    emit ticksChanged();
}

void SpectrogramChannelRulerModel::setRulerGuideYPos(double yPos)
{
    auto frequency = SelectionInfo::UndefinedFrequency;
    if (yPos >= 0 && yPos <= m_channelHeight) {
        frequency = positionToFrequency(yPos);
    }
    spectrogramViewService()->setRulerGuideFrequency(m_trackId, frequency);
}

double SpectrogramChannelRulerModel::rulerGuideYPos() const
{
    const auto freq = spectrogramViewService()->rulerGuideFrequency(m_trackId);
    return frequencyToPosition(freq);
}

namespace {
constexpr double ZOOM_FACTOR = 1.4142;     // sqrt(2)
}

void SpectrogramChannelRulerModel::zoomIn(double mouseY)
{
    zoomBy(1. / ZOOM_FACTOR, mouseY);
}

void SpectrogramChannelRulerModel::zoomOut(double mouseY)
{
    zoomBy(ZOOM_FACTOR, mouseY);
}

void SpectrogramChannelRulerModel::setPopupPosition(double yPos)
{
    m_popupPosition = yPos;
}

void SpectrogramChannelRulerModel::zoomInFromPopup()
{
    zoomBy(1. / ZOOM_FACTOR, m_popupPosition);
}

void SpectrogramChannelRulerModel::zoomOutFromPopup()
{
    zoomBy(ZOOM_FACTOR, m_popupPosition);
}

void SpectrogramChannelRulerModel::resetZoom()
{
    const auto config = spectrogramService()->trackSpectrogramConfiguration(m_trackId);
    IF_ASSERT_FAILED(config) {
        return;
    }

    config->setMinFreq(0.);
    config->setMaxFreq(spectrogramService()->frequencyHardMaximum(m_trackId));
    config->setUseGlobalSettings(false);

    updateTicks();
    emit ticksChanged();
    emit zoomStateChanged();
    spectrogramService()->notifyAboutTrackSpectrogramConfigurationChanged(m_trackId);
}

bool SpectrogramChannelRulerModel::isMinZoom() const
{
    const auto config = spectrogramService()->trackSpectrogramConfiguration(m_trackId);
    if (!config) {
        return true;
    }
    const auto hardMax = spectrogramService()->frequencyHardMaximum(m_trackId);
    return muse::is_equal(config->minFreq(), 0.) && muse::is_equal(config->maxFreq(), hardMax);
}

double SpectrogramChannelRulerModel::frequencyToPosition(double freq) const
{
    return spectrogramService()->frequencyToY(trackId(), freq, m_channelHeight);
}

double SpectrogramChannelRulerModel::positionToFrequency(double pos) const
{
    return spectrogramService()->yToFrequency(trackId(), pos, m_channelHeight);
}

void SpectrogramChannelRulerModel::zoomBy(double factor, double mousePos)
{
    const auto config = spectrogramService()->trackSpectrogramConfiguration(m_trackId);
    IF_ASSERT_FAILED(config) {
        return;
    }

    if (config->maxFreq() > config->minFreq()) {
        // We do the calculations in the "positional" domain, where equal y steps mean equal steps on the chosen
        // spectrogram scale (e.g. on the mel scale, 1 pixel may equal 0.1 mel).

        // Hard maximum frequency -> "hard minimum" position (frequency and positional domains have opposite polarities).
        const auto hardMinPos = frequencyToPosition(spectrogramService()->frequencyHardMaximum(m_trackId));

        // If the mouse is say at 300px on a 400px ruler, the space above the mouse will change from 300 to 300 * factor.
        const auto newSpaceAbove = mousePos * factor;

        // Do as though we displaced top and bottom from [0, height] to [newBottom, newTop].
        // Then calculate the corresponding frequencies. When setting them as min and max frequencies on the configuration,
        // the spectrogram and ruler painting will rescale their content so that this new range fits in [0, height] again.

        // Don't forget that the spectrogram display of a given track must be upper-bound to the track's frequency.
        const auto newTop = std::max(hardMinPos, mousePos - newSpaceAbove);
        const auto newBottom = newTop + m_channelHeight * factor;

        const auto newMaxFreq = positionToFrequency(newTop);
        const auto newMinFreq = std::max(0., positionToFrequency(newBottom));

        config->setMaxFreq(newMaxFreq);
        config->setMinFreq(newMinFreq);
    } else {
        // Special case: we are showing a spectrum slice and all positions map to the same frequency.
        // Workaround: augment the range to 1Hz.
        const auto newMinFreq = std::max(0., config->minFreq() - 1.);
        config->setMinFreq(newMinFreq);
        config->setMaxFreq(newMinFreq + 1);
    }

    // Changing track spectrogram setting decouples it from the global spectrogram settings.
    config->setUseGlobalSettings(false);

    updateTicks();

    emit ticksChanged();
    emit zoomStateChanged();
    spectrogramService()->notifyAboutTrackSpectrogramConfigurationChanged(m_trackId);
}

void SpectrogramChannelRulerModel::scrollBy(double delta)
{
    const auto config = spectrogramService()->trackSpectrogramConfiguration(m_trackId);
    IF_ASSERT_FAILED(config) {
        return;
    }

    std::optional<double> prevMinFreq;
    const auto LOGARITHMIC_DANGER = config->scale() == SpectrogramScale::Logarithmic;

    if (LOGARITHMIC_DANGER) {
        // Cheat
        prevMinFreq = config->minFreq();
        config->setMinFreq(std::max(1., config->minFreq()));
    }

    const auto hardMinFreq = LOGARITHMIC_DANGER ? 1. : 0.;
    const auto hardMaxFreq = spectrogramService()->frequencyHardMaximum(m_trackId);
    const auto hardMinPos = frequencyToPosition(hardMaxFreq);
    const auto hardMaxPos = frequencyToPosition(hardMinFreq);

    const auto minFreq = std::max(config->minFreq(), 0.);
    const auto maxFreq = std::min(hardMaxFreq, config->maxFreq());
    const auto minPos = frequencyToPosition(maxFreq);
    const auto maxPos = frequencyToPosition(minFreq);
    const auto posRange = maxPos - minPos;

    delta = std::clamp(delta, maxPos - hardMaxPos, minPos - hardMinPos);

    const auto newMinPos = std::max(hardMinPos, minPos - delta);
    const auto newMaxPos = std::min(hardMaxPos, newMinPos + posRange);
    if (newMinPos == minPos || newMaxPos == maxPos) {
        if (prevMinFreq.has_value()) {
            config->setMinFreq(*prevMinFreq);
        }
        return;
    }

    const auto newMinFreq = positionToFrequency(newMaxPos);
    const auto newMaxFreq = positionToFrequency(newMinPos);

    config->setMinFreq(newMinFreq);
    config->setMaxFreq(newMaxFreq);

    updateTicks();
    emit ticksChanged();
    spectrogramService()->notifyAboutTrackSpectrogramConfigurationChanged(m_trackId);
}

QVariantList SpectrogramChannelRulerModel::majorTicks() const
{
    return tickListToVariants(m_ticks.major);
}

QVariantList SpectrogramChannelRulerModel::minorTicks() const
{
    return tickListToVariants(m_ticks.minor);
}

QVariantList SpectrogramChannelRulerModel::tickListToVariants(const std::vector<au::shared::AxisTick>& ticks) const
{
    const std::vector<std::string> labels = au::shared::labelsForTicks(ticks, m_labelHeight, m_channelHeight);
    QVariantList list;
    list.reserve(ticks.size());
    for (auto i = 0; i < static_cast<int>(ticks.size()); ++i) {
        list.append(QVariant::fromValue(QVariantMap {
                { "y", (1.0 - ticks[i].position) * m_channelHeight },
                { "label", QString::fromStdString(labels[i]) },
            }));
    }
    return list;
}
}
