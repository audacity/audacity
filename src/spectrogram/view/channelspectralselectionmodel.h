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
    Q_PROPERTY(
        double selectionStartFrequency READ selectionStartFrequency WRITE setSelectionStartFrequency NOTIFY startFrequencyChanged FINAL)
    Q_PROPERTY(
        double selectionEndFrequency READ selectionEndFrequency WRITE setSelectionEndFrequency NOTIFY endFrequencyChanged FINAL)
    Q_PROPERTY(double selectionStartTime READ selectionStartTime WRITE setSelectionStartTime NOTIFY selectionStartTimeChanged FINAL)
    Q_PROPERTY(double selectionEndTime READ selectionEndTime WRITE setSelectionEndTime NOTIFY selectionEndTimeChanged FINAL)

    Q_PROPERTY(double centerFrequency READ centerFrequency NOTIFY centerFrequencyChanged FINAL)
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

    double trackSampleRate() const { return m_trackSampleRate; }
    void setTrackSampleRate(double rate);

    int trackId() const { return m_trackId; }
    void setTrackId(int id);

    int channel() const { return m_channel; }
    void setChannel(int channel);

    double channelHeight() const { return m_channelHeight; }
    void setChannelHeight(double height);

    double selectionStartFrequency() const { return m_selectionStartFrequency; }
    void setSelectionStartFrequency(double freq);

    double selectionEndFrequency() const { return m_selectionEndFrequency; }
    void setSelectionEndFrequency(double freq);

    double selectionY() const;
    double selectionHeight() const;

    double selectionStartTime() const { return m_selectionStartTime; }
    void setSelectionStartTime(double time);

    double selectionEndTime() const { return m_selectionEndTime; }
    void setSelectionEndTime(double time);

    double centerFrequency() const;

    double rulerGuideFrequency() const;
    void setRulerGuideFrequency(double frequency);

    Q_INVOKABLE void setRulerGuideYPos(double y);

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
    void selectionStartTimeChanged();
    void selectionEndTimeChanged();
    void centerFrequencyChangeRequested(double frequency);
    void centerFrequencyChanged();
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
    double m_selectionStartFrequency = SelectionInfo::UndefinedFrequency;
    double m_selectionEndFrequency = SelectionInfo::UndefinedFrequency;
    double m_selectionStartTime = 0.0;
    double m_selectionEndTime = 0.0;
    std::unique_ptr<IPeakFinder> m_peakFinder;

    FrequencySelection m_dragStartFrequencySelection;
};
}
