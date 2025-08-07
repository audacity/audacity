/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "projectscene/view/trackruler/itrackrulermodel.h"

#include <QObject>
#include <QVariantMap>

namespace au::projectscene {
class TrackRulerModel : public QObject
{
    Q_OBJECT

    Q_PROPERTY(std::vector<QVariantMap> fullSteps READ fullSteps NOTIFY fullStepsChanged)
    Q_PROPERTY(std::vector<QVariantMap> smallSteps READ smallSteps NOTIFY smallStepsChanged)

    Q_PROPERTY(bool isStereo READ isStereo WRITE setIsStereo NOTIFY isStereoChanged FINAL)
    Q_PROPERTY(bool isCollapsed READ isCollapsed WRITE setIsCollapsed NOTIFY isCollapsedChanged FINAL)
    Q_PROPERTY(int height READ height WRITE setHeight NOTIFY heightChanged FINAL)

    Q_PROPERTY(double channelHeightRatio READ channelHeightRatio WRITE setChannelHeightRatio NOTIFY channelHeightRatioChanged FINAL)

public:
    explicit TrackRulerModel(QObject* parent = nullptr);

    std::vector<QVariantMap> fullSteps() const;
    std::vector<QVariantMap> smallSteps() const;

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

    double stepToPosition(double step, int channel) const;

    bool m_isStereo = false;
    bool m_isCollapsed = false;
    int m_height = 0;
    double m_channelHeightRatio = 0.5;
};
}
