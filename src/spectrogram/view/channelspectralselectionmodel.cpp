/*
 * Audacity: A Digital Audio Editor
 */
#include "./channelspectralselectionmodel.h"
#include "internal/numberscale.h"
#include "internal/spectrogramutils.h"

#include "framework/global/types/number.h"

namespace au::spectrogram {
ChannelSpectralSelectionModel::ChannelSpectralSelectionModel(QObject* parent)
    : QObject(parent), muse::Injectable(muse::iocCtxForQmlObject(this))
{
}

double ChannelSpectralSelectionModel::positionToFrequency(double y) const
{
    const auto config = spectrogramService()->trackSpectrogramConfiguration(m_trackId);
    if (!config) {
        return SelectionInfo::UndefinedFrequency;
    }

    const auto [minFreq, maxFreq] = spectrogramBounds(*config, m_trackSampleRate);

    const NumberScale numberScale(config->scale(), minFreq, maxFreq);
    const double position = 1.0 - y / m_channelHeight;
    return numberScale.positionToValue(position);
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
    emit selectionStartFrequencyChanged();
    emit selectionRangeChanged();
}

void ChannelSpectralSelectionModel::setSelectionEndFrequency(double freq)
{
    if (muse::is_equal(m_selectionEndFrequency, freq)) {
        return;
    }
    m_selectionEndFrequency = freq;
    emit selectionEndFrequencyChanged();
    emit selectionRangeChanged();
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
}
