/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>
#include <QPointF>
#include <QVector>
#include <QAbstractItemModel>

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "automation/iclipgaininteraction.h"
#include "trackedit/iclipsinteraction.h"
#include "trackedit/iprojecthistory.h"

#include "trackedit/trackedittypes.h"
#include "projectscene/types/projectscenetypes.h"

namespace au::automation {
class ClipGainModel : public QAbstractListModel, public muse::async::Asyncable, public muse::Injectable
{
    Q_OBJECT

    muse::Inject<IClipGainInteraction> clipGainInteraction{ this };
    muse::Inject<trackedit::IClipsInteraction> clipsInteraction{ this };
    muse::Inject<trackedit::IProjectHistory> projectHistory{ this };

    Q_PROPERTY(projectscene::ClipKey clipKey READ clipKey WRITE setClipKey NOTIFY clipKeyChanged FINAL)

    Q_PROPERTY(QVector<QPointF> points READ points NOTIFY pointsChanged)

    Q_PROPERTY(double minValue READ minValue NOTIFY clipGainAutomationInfoChanged)
    Q_PROPERTY(double maxValue READ maxValue NOTIFY clipGainAutomationInfoChanged)
    Q_PROPERTY(double defaultValue READ defaultValue NOTIFY clipGainAutomationInfoChanged)
    Q_PROPERTY(bool exponential READ exponential NOTIFY clipGainAutomationInfoChanged)
    Q_PROPERTY(double ySplitNormalized READ ySplitNormalized NOTIFY clipGainAutomationInfoChanged)
    Q_PROPERTY(double ySplitValue READ ySplitValue NOTIFY clipGainAutomationInfoChanged)

    Q_PROPERTY(double clipStartTime READ clipStartTime NOTIFY clipTimeChanged)
    Q_PROPERTY(double clipEndTime READ clipEndTime NOTIFY clipTimeChanged)

public:
    explicit ClipGainModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    int rowCount(const QModelIndex& parent = {}) const override;
    QVariant data(const QModelIndex& index, int role) const override;
    QHash<int, QByteArray> roleNames() const override;

    QVector<QPointF> points() const;

    double minValue() const;
    double maxValue() const;
    double defaultValue() const;
    bool exponential() const;
    double ySplitNormalized() const;
    double ySplitValue() const;

    double clipStartTime() const;
    double clipEndTime() const;

    Q_INVOKABLE void setPoint(int index, double tAbs, double value, bool completed);
    Q_INVOKABLE void addPoint(double tAbs, double value, bool completed);
    Q_INVOKABLE void removePoint(int index, bool completed);
    Q_INVOKABLE void cancelDrag();

    projectscene::ClipKey clipKey() const;
    void setClipKey(const projectscene::ClipKey& key);

signals:
    void clipKeyChanged();
    void pointsChanged();
    void clipGainAutomationInfoChanged();
    void clipTimeChanged();

private:
    void reload();
    void clear();

    enum Role {
        TimeRole = Qt::UserRole + 1,
        ValueRole
    };

    projectscene::ClipKey m_clipKey;
    AutomationPoints m_points;
    AutomationInfo m_info;

    double m_clipStartTime = 0.0;
    double m_clipEndTime = 0.0;

    bool m_dragActive = false;
    int m_dragIndex = -1;
};
}
