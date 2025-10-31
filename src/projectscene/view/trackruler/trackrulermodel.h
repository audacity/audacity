/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>
#include <QVariantMap>

#include "global/async/asyncable.h"

#include "global/modularity/ioc.h"
#include "playback/iplaybackconfiguration.h"

#include "itrackrulermodel.h"

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

    Q_PROPERTY(int trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged FINAL)
    Q_PROPERTY(int rulerType READ rulerType WRITE setRulerType NOTIFY rulerTypeChanged FINAL)

    muse::Inject<au::playback::IPlaybackConfiguration> configuration;

public:
    explicit TrackRulerModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();

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

    int trackId() const;
    void setTrackId(int trackId);

    int rulerType() const;
    void setRulerType(int rulerType);

signals:
    void fullStepsChanged();
    void smallStepsChanged();

    void isStereoChanged();
    void isCollapsedChanged();
    void heightChanged();

    void channelHeightRatioChanged();

    void trackIdChanged();
    void rulerTypeChanged();
private:
    std::shared_ptr<ITrackRulerModel> buildRulerModel();
    double stepToPosition(double step, int channel, bool isNegativeSample) const;

    std::shared_ptr<ITrackRulerModel> m_model =  nullptr;

    bool m_isStereo = false;
    bool m_isCollapsed = false;
    int m_height = 0;
    double m_channelHeightRatio = 0.5;
    int m_trackId = -1;
    int m_rulerType = 2;
};
}
