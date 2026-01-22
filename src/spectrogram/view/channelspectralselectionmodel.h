/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ispectrogramservice.h"

#include "framework/global/modularity/ioc.h"

#include <QObject>

namespace au::spectrogram {
class ChannelSpectralSelectionModel : public QObject, public muse::Injectable
{
    Q_OBJECT

    Q_PROPERTY(int trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged FINAL)

    // Input
    Q_PROPERTY(double trackSampleRate READ trackSampleRate WRITE setTrackSampleRate NOTIFY trackSampleRateChanged FINAL)
    Q_PROPERTY(double channelHeight READ channelHeight WRITE setChannelHeight NOTIFY channelHeightChanged FINAL)
    Q_PROPERTY(
        double selectionStartFrequency READ selectionStartFrequency WRITE setSelectionStartFrequency NOTIFY selectionStartFrequencyChanged FINAL)
    Q_PROPERTY(
        double selectionEndFrequency READ selectionEndFrequency WRITE setSelectionEndFrequency NOTIFY selectionEndFrequencyChanged FINAL)

    // Output
    Q_PROPERTY(double selectionY READ selectionY NOTIFY selectionRangeChanged FINAL)
    Q_PROPERTY(double selectionHeight READ selectionHeight NOTIFY selectionRangeChanged FINAL)

    muse::Inject<spectrogram::ISpectrogramService> spectrogramService { this };

public:
    ChannelSpectralSelectionModel(QQuickItem* parent = nullptr);
    ~ChannelSpectralSelectionModel() override = default;

    Q_INVOKABLE double positionToFrequency(double y) const;

    double trackSampleRate() const { return m_trackSampleRate; }
    void setTrackSampleRate(double rate);

    int trackId() const { return m_trackId; }
    void setTrackId(int id);

    double channelHeight() const { return m_channelHeight; }
    void setChannelHeight(double height);

    double selectionStartFrequency() const { return m_selectionStartFrequency; }
    void setSelectionStartFrequency(double freq);

    double selectionEndFrequency() const { return m_selectionEndFrequency; }
    void setSelectionEndFrequency(double freq);

    double selectionY() const;
    double selectionHeight() const;

signals:
    void trackSampleRateChanged();
    void zoomChanged();
    void channelHeightChanged();
    void trackIdChanged();
    void selectionStartFrequencyChanged();
    void selectionEndFrequencyChanged();
    void selectionRangeChanged();

private:
    std::pair<double, double> selectionYRange() const;

    double m_trackSampleRate = 0.0;
    int m_trackId = -1;
    double m_channelHeight = 0.0;
    double m_selectionStartFrequency = SelectionInfo::UndefinedFrequency;
    double m_selectionEndFrequency = SelectionInfo::UndefinedFrequency;
};
}
