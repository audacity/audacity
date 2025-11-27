/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>
#include <QVariantMap>

#include "framework/global/async/asyncable.h"

#include "framework/global/modularity/ioc.h"
#include "playback/iplaybackconfiguration.h"
#include "trackedit/itrackeditinteraction.h"
#include "trackedit/trackedittypes.h"

#include "projectscene/view/trackruler/itrackruler.h"

namespace au::projectscene {
class TrackRulerModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(std::vector<QVariantMap> fullSteps READ fullSteps NOTIFY fullStepsChanged)
    Q_PROPERTY(std::vector<QVariantMap> smallSteps READ smallSteps NOTIFY smallStepsChanged)

    Q_PROPERTY(bool isStereo READ isStereo WRITE setIsStereo NOTIFY isStereoChanged FINAL)
    Q_PROPERTY(bool isCollapsed READ isCollapsed WRITE setIsCollapsed NOTIFY isCollapsedChanged FINAL)
    Q_PROPERTY(int height READ height WRITE setHeight NOTIFY heightChanged FINAL)

    Q_PROPERTY(double channelHeightRatio READ channelHeightRatio WRITE setChannelHeightRatio NOTIFY channelHeightRatioChanged FINAL)

    Q_PROPERTY(int rulerType READ rulerType WRITE setRulerType NOTIFY rulerTypeChanged FINAL)

    Q_PROPERTY(trackedit::TrackId trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged FINAL)

    Q_PROPERTY(float verticalZoom READ verticalZoom WRITE setVerticalZoom FINAL)
    Q_PROPERTY(bool isDefaultZoom READ isDefaultZoom NOTIFY isDefaultZoomChanged FINAL)
    Q_PROPERTY(bool isMaxZoom READ isMaxZoom NOTIFY isMaxZoomChanged FINAL)
    Q_PROPERTY(bool isMinZoom READ isMinZoom NOTIFY isMinZoomChanged FINAL)

    muse::Inject<au::playback::IPlaybackConfiguration> configuration;
    muse::Inject<au::trackedit::ITrackeditInteraction> trackeditInteraction;

public:
    explicit TrackRulerModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();
    Q_INVOKABLE void zoomIn();
    Q_INVOKABLE void zoomOut();
    Q_INVOKABLE void resetZoom();

    std::vector<QVariantMap> fullSteps() const;
    std::vector<QVariantMap> smallSteps() const;
    Q_INVOKABLE QString sampleToText(double sample) const;

    bool isStereo() const;
    void setIsStereo(bool isStereo);

    bool isCollapsed() const;
    void setIsCollapsed(bool isCollapsed);

    int height() const;
    void setHeight(int height);

    double channelHeightRatio() const;
    void setChannelHeightRatio(double channelHeightRatio);

    int rulerType() const;
    void setRulerType(int rulerType);

    float verticalZoom() const;
    void setVerticalZoom(float verticalZoom);

    bool isDefaultZoom() const;
    bool isMaxZoom() const;
    bool isMinZoom() const;

    trackedit::TrackId trackId() const;
    void setTrackId(const trackedit::TrackId& newTrackId);

signals:
    void fullStepsChanged();
    void smallStepsChanged();

    void isStereoChanged();
    void isCollapsedChanged();
    void heightChanged();

    void channelHeightRatioChanged();

    void rulerTypeChanged();

    void isDefaultZoomChanged();
    void isMaxZoomChanged();
    void isMinZoomChanged();

    void trackIdChanged();
private:
    std::shared_ptr<ITrackRuler> buildRulerModel();
    double stepToPosition(double step, int channel, bool isNegativeSample) const;

    std::shared_ptr<ITrackRuler> m_model =  nullptr;

    trackedit::TrackId m_trackId = -1;
    bool m_isStereo = false;
    bool m_isCollapsed = false;
    int m_height = 0;
    double m_channelHeightRatio = 0.5;
    int m_rulerType = 2;
    float m_verticalZoom = 1.0f;
};
}
