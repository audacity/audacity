/*
 * Audacity: A Digital Audio Editor
 */
#include "./channelspectralselectionmodel.h"
#include "internal/numberscale.h"
#include "internal/spectrogramutils.h"

#include "framework/global/types/number.h"
#include "framework/global/log.h"

namespace au::spectrogram {
ChannelSpectralSelectionModel::ChannelSpectralSelectionModel(QObject* parent)
    : QObject(parent), muse::Injectable(muse::iocCtxForQmlObject(this))
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

void ChannelSpectralSelectionModel::setSelectionStartFrequency(double freq)
{
    if (muse::is_equal(m_selectionStartFrequency, freq)) {
        return;
    }
    m_selectionStartFrequency = freq;
    emit centerFrequencyChanged();
    emit selectionStartFrequencyChanged();
    emit selectionRangeChanged();
}

void ChannelSpectralSelectionModel::setSelectionEndFrequency(double freq)
{
    if (muse::is_equal(m_selectionEndFrequency, freq)) {
        return;
    }
    m_selectionEndFrequency = freq;
    emit centerFrequencyChanged();
    emit selectionEndFrequencyChanged();
    emit selectionRangeChanged();
}

void ChannelSpectralSelectionModel::setSelectionStartTime(double time)
{
    if (muse::is_equal(m_selectionStartTime, time)) {
        return;
    }
    m_selectionStartTime = time;
    emit selectionStartTimeChanged();
}

void ChannelSpectralSelectionModel::setSelectionEndTime(double time)
{
    if (muse::is_equal(m_selectionEndTime, time)) {
        return;
    }
    m_selectionEndTime = time;
    emit selectionEndTimeChanged();
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
    if (m_selectionStartFrequency == SelectionInfo::UndefinedFrequency
        || m_selectionEndFrequency == SelectionInfo::UndefinedFrequency) {
        return { 0, 0 };
    }

    const auto config = spectrogramService()->trackSpectrogramConfiguration(m_trackId);
    if (!config) {
        return { 0, 0 };
    }

    const auto [minFreq, maxFreq] = spectrogramBounds(*config, m_trackSampleRate);

    const NumberScale numberScale(config->scale(), minFreq, maxFreq);
    auto y1 = m_channelHeight * (1 - numberScale.valueToPosition(m_selectionStartFrequency));
    auto y2 = m_channelHeight * (1 - numberScale.valueToPosition(m_selectionEndFrequency));
    if (y1 > y2) {
        std::swap(y1, y2);
    }

    return { y1, y2 - y1 };
}

void ChannelSpectralSelectionModel::startCenterFrequencyDrag()
{
    m_peakFinder = peakFinderFactory()->newInstance(m_trackId, m_channel, m_selectionStartTime, m_selectionEndTime);
    m_dragStartFrequencySelection = frequencySelectionController()->frequencySelection();
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
    const double peakPosition = frequencyToPosition(peakFrequency);

    const auto startFreqPos = frequencyToPosition(m_dragStartFrequencySelection.startFrequency());
    const auto endFreqPos = frequencyToPosition(m_dragStartFrequencySelection.endFrequency());
    const auto range = endFreqPos - startFreqPos;
    auto newStartFreqPos = peakPosition - range / 2;
    auto newEndFreqPos = peakPosition + range / 2;

    const auto config = spectrogramService()->trackSpectrogramConfiguration(m_dragStartFrequencySelection.trackId);
    IF_ASSERT_FAILED(config) {
        return;
    }

    const auto minFreqPos = frequencyToPosition(config->minFreq());
    const auto maxFreqPos = frequencyToPosition(config->maxFreq());
    if (newStartFreqPos > minFreqPos) {
        const auto delta = newStartFreqPos - minFreqPos;
        newEndFreqPos = std::min<double>(newEndFreqPos + delta, minFreqPos);
        newStartFreqPos = minFreqPos;
    } else if (newEndFreqPos < maxFreqPos) {
        const auto delta = maxFreqPos - newEndFreqPos;
        newStartFreqPos = std::max<double>(newStartFreqPos - delta, maxFreqPos);
        newEndFreqPos = maxFreqPos;
    }

    if (newStartFreqPos == newEndFreqPos) {
        return;
    }

    const auto newStartFreq = positionToFrequency(newStartFreqPos);
    const auto newEndFreq = positionToFrequency(newEndFreqPos);

    FrequencySelection newSelection = m_dragStartFrequencySelection;
    newSelection.setFrequencyRange(newStartFreq, newEndFreq, config->scale());

    frequencySelectionController()->setFrequencySelection(std::move(newSelection), false);
}

void ChannelSpectralSelectionModel::endCenterFrequencyDrag()
{
    frequencySelectionController()->setFrequencySelection(frequencySelectionController()->frequencySelection(), true);
    m_peakFinder.reset();
    m_dragStartFrequencySelection = {};
    emit verticalDragActiveChanged();
}

double ChannelSpectralSelectionModel::centerFrequency() const
{
    return frequencySelectionController()->frequencySelection().centerFrequency();
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
