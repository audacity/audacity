/*
 * Audacity: A Digital Audio Editor
 */
#include "./channelspectralselectionmodel.h"
#include "shared/axis/numberscale.h"
#include "internal/spectrogramutils.h"

#include "framework/global/types/number.h"
#include "framework/global/log.h"

namespace au::spectrogram {
ChannelSpectralSelectionModel::ChannelSpectralSelectionModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void ChannelSpectralSelectionModel::componentComplete()
{
    spectrogramService()->trackSpectrogramConfigurationChanged().onReceive(this, [this](int trackId) {
        if (trackId == m_trackId) {
            emit selectionRangeChanged();
        }
    });

    spectrogramViewService()->rulerGuideFrequencyChanged().onReceive(this, [this](int trackId) {
        if (trackId == m_trackId) {
            emit rulerGuideFrequencyChanged();
        }
    });
}

double ChannelSpectralSelectionModel::positionToFrequency(double y) const
{
    return spectrogramService()->yToFrequency(m_trackId, y, m_channelHeight);
}

double ChannelSpectralSelectionModel::frequencyToPosition(double frequency) const
{
    return spectrogramService()->frequencyToY(m_trackId, frequency, m_channelHeight);
}

void ChannelSpectralSelectionModel::setTrackSampleRate(double rate)
{
    if (muse::is_equal(m_trackSampleRate, rate)) {
        return;
    }
    m_trackSampleRate = rate;
    emit trackSampleRateChanged();
    emit selectionRangeChanged();
}

void ChannelSpectralSelectionModel::setTrackId(int id)
{
    if (m_trackId == id) {
        return;
    }
    m_trackId = id;
    emit trackIdChanged();
    emit selectionRangeChanged();
}

void ChannelSpectralSelectionModel::setChannel(int channel)
{
    if (m_channel == channel) {
        return;
    }
    m_channel = channel;
    emit channelChanged();
    emit selectionRangeChanged();
}

void ChannelSpectralSelectionModel::setChannelHeight(double height)
{
    if (muse::is_equal(m_channelHeight, height)) {
        return;
    }
    m_channelHeight = height;
    emit channelHeightChanged();
    emit selectionRangeChanged();
}

void ChannelSpectralSelectionModel::setStartFrequency(double freq)
{
    if (muse::is_equal(m_startFrequency, freq)) {
        return;
    }
    m_startFrequency = freq;
    emit startFrequencyChanged();
    emit selectionRangeChanged();
}

void ChannelSpectralSelectionModel::setEndFrequency(double freq)
{
    if (muse::is_equal(m_endFrequency, freq)) {
        return;
    }
    m_endFrequency = freq;
    emit endFrequencyChanged();
    emit selectionRangeChanged();
}

void ChannelSpectralSelectionModel::setStartTime(double time)
{
    if (muse::is_equal(m_startTime, time)) {
        return;
    }
    m_startTime = time;
    emit startTimeChanged();
}

void ChannelSpectralSelectionModel::setEndTime(double time)
{
    if (muse::is_equal(m_endTime, time)) {
        return;
    }
    m_endTime = time;
    emit endTimeChanged();
}

double ChannelSpectralSelectionModel::selectionY() const
{
    return selectionYRange().first;
}

double ChannelSpectralSelectionModel::selectionHeight() const
{
    return selectionYRange().second;
}

std::pair<double, double> ChannelSpectralSelectionModel::selectionYRange() const
{
    if (m_startFrequency == SelectionInfo::UndefinedFrequency
        || m_endFrequency == SelectionInfo::UndefinedFrequency) {
        return { 0, 0 };
    }

    const auto config = spectrogramService()->trackSpectrogramConfiguration(m_trackId);
    if (!config) {
        return { 0, 0 };
    }

    const auto [minFreq, maxFreq] = spectrogramBounds(*config, m_trackSampleRate);

    const NumberScale numberScale(config->scale(), minFreq, maxFreq);
    auto y1 = m_channelHeight * (1 - numberScale.valueToPosition(m_startFrequency));
    auto y2 = m_channelHeight * (1 - numberScale.valueToPosition(m_endFrequency));
    if (y1 > y2) {
        std::swap(y1, y2);
    }

    return { y1, y2 - y1 };
}

void ChannelSpectralSelectionModel::startCenterFrequencyDrag()
{
    m_peakFinder = peakFinderFactory()->newInstance(m_trackId, m_channel, m_startTime, m_endTime);
    emit verticalDragActiveChanged();
}

void ChannelSpectralSelectionModel::dragCenterFrequency(double y)
{
    if (!m_peakFinder) {
        return;
    }

    const double frequency = positionToFrequency(y);
    if (frequency == SelectionInfo::UndefinedFrequency) {
        return;
    }

    const double peakFrequency = m_peakFinder->findPeak(frequency);
    frequencySelectionController()->setCenterFrequency(peakFrequency, false);
}

void ChannelSpectralSelectionModel::endCenterFrequencyDrag()
{
    const auto cf = frequencySelectionController()->frequencySelection().centerFrequency();
    frequencySelectionController()->setCenterFrequency(cf, true);
    m_peakFinder.reset();
    emit verticalDragActiveChanged();
}

void ChannelSpectralSelectionModel::onHoveringPositionChanged(double y)
{
    auto frequency = SelectionInfo::UndefinedFrequency;
    if (y >= 0 && y <= m_channelHeight) {
        frequency = positionToFrequency(y);
    }
    spectrogramViewService()->setRulerGuideFrequency(m_trackId, frequency);
}

double ChannelSpectralSelectionModel::rulerGuideFrequency() const
{
    return spectrogramViewService()->rulerGuideFrequency(m_trackId);
}

void ChannelSpectralSelectionModel::setRulerGuideFrequency(double frequency)
{
    spectrogramViewService()->setRulerGuideFrequency(m_trackId, frequency);
}
}
