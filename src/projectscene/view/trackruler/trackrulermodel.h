/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "global/async/asyncable.h"

#include "global/modularity/ioc.h"
#include "playback/iplaybackconfiguration.h"

#include "projectscene/view/trackruler/itrackrulermodel.h"

#include <QObject>
#include <QVariantMap>
#include <qtmetamacros.h>

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

signals:
    void fullStepsChanged();
    void smallStepsChanged();

    void isStereoChanged();
    void isCollapsedChanged();
    void heightChanged();

    void channelHeightRatioChanged();
private:
    std::shared_ptr<ITrackRulerModel> m_model =  nullptr;

    double stepToPosition(double step, int channel, bool isNegativeSample) const;

    bool m_isStereo = false;
    bool m_isCollapsed = false;
    int m_height = 0;
    double m_channelHeightRatio = 0.5;
};
}
