/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ispectrogramservice.h"
#include "spectrogramtypes.h" // SelectionInfo
#include "internal/ipeakfinderfactory.h"
#include "internal/frequencyselectioncontroller.h"
#include "view/ispectrogramviewservice.h"

#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"

#include <QObject>
#include <QQmlParserStatus>

#include <utility>

namespace au::spectrogram {
class ChannelSpectralSelectionModel : public QObject, public QQmlParserStatus, public muse::Injectable, public muse::async::Asyncable
{
    Q_OBJECT
    Q_INTERFACES(QQmlParserStatus)

    Q_PROPERTY(int trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged FINAL)
    Q_PROPERTY(int channel READ channel WRITE setChannel NOTIFY channelChanged FINAL)

    // Input
    Q_PROPERTY(double trackSampleRate READ trackSampleRate WRITE setTrackSampleRate NOTIFY trackSampleRateChanged FINAL)
    Q_PROPERTY(double channelHeight READ channelHeight WRITE setChannelHeight NOTIFY channelHeightChanged FINAL)
    Q_PROPERTY(double startFrequency READ startFrequency WRITE setStartFrequency NOTIFY startFrequencyChanged FINAL)
    Q_PROPERTY(double endFrequency READ endFrequency WRITE setEndFrequency NOTIFY endFrequencyChanged FINAL)
    Q_PROPERTY(double startTime READ startTime WRITE setStartTime NOTIFY startTimeChanged FINAL)
    Q_PROPERTY(double endTime READ endTime WRITE setEndTime NOTIFY endTimeChanged FINAL)

    Q_PROPERTY(double rulerGuideFrequency READ rulerGuideFrequency WRITE setRulerGuideFrequency NOTIFY rulerGuideFrequencyChanged FINAL)

    // Output
    Q_PROPERTY(double selectionY READ selectionY NOTIFY selectionRangeChanged FINAL)
    Q_PROPERTY(double selectionHeight READ selectionHeight NOTIFY selectionRangeChanged FINAL)
    Q_PROPERTY(bool verticalDragActive READ verticalDragActive NOTIFY verticalDragActiveChanged FINAL)

    muse::Inject<ISpectrogramService> spectrogramService { this };
    muse::Inject<IPeakFinderFactory> peakFinderFactory { this };
    muse::Inject<IFrequencySelectionController> frequencySelectionController { this };
    muse::Inject<ISpectrogramViewService> spectrogramViewService { this };

public:
    ChannelSpectralSelectionModel(QObject* parent = nullptr);
    ~ChannelSpectralSelectionModel() override = default;

    Q_INVOKABLE void onHoveringPositionChanged(double y);

    double trackSampleRate() const { return m_trackSampleRate; }
    void setTrackSampleRate(double rate);

    int trackId() const { return m_trackId; }
    void setTrackId(int id);

    int channel() const { return m_channel; }
    void setChannel(int channel);

    double channelHeight() const { return m_channelHeight; }
    void setChannelHeight(double height);

    double startFrequency() const { return m_startFrequency; }
    void setStartFrequency(double freq);

    double endFrequency() const { return m_endFrequency; }
    void setEndFrequency(double freq);

    double selectionY() const;
    double selectionHeight() const;

    double startTime() const { return m_startTime; }
    void setStartTime(double time);

    double endTime() const { return m_endTime; }
    void setEndTime(double time);

    double rulerGuideFrequency() const;
    void setRulerGuideFrequency(double frequency);

    bool verticalDragActive() const { return m_peakFinder != nullptr; }

    Q_INVOKABLE void startCenterFrequencyDrag();
    Q_INVOKABLE void dragCenterFrequency(double y);
    Q_INVOKABLE void endCenterFrequencyDrag();

signals:
    void trackSampleRateChanged();
    void channelHeightChanged();
    void trackIdChanged();
    void channelChanged();
    void startFrequencyChanged();
    void endFrequencyChanged();
    void selectionRangeChanged();
    void verticalDragActiveChanged();
    void startTimeChanged();
    void endTimeChanged();
    void centerFrequencyChangeRequested(double frequency);
    void rulerGuideFrequencyChanged();

private:
    void classBegin() override {}
    void componentComplete() override;

    std::pair<double, double> selectionYRange() const;
    double positionToFrequency(double y) const;
    double frequencyToPosition(double frequency) const;

    double m_trackSampleRate = 0.0;
    int m_trackId = -1;
    int m_channel = -1;
    double m_channelHeight = 0.0;
    double m_startFrequency = SelectionInfo::UndefinedFrequency;
    double m_endFrequency = SelectionInfo::UndefinedFrequency;
    double m_startTime = 0.0;
    double m_endTime = 0.0;
    std::unique_ptr<IPeakFinder> m_peakFinder;
};
}
