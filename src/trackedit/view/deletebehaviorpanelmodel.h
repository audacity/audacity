#pragma once

#include <QObject>

#include "trackedit/trackedittypes.h"

namespace au::trackedit {
class DeleteBehaviorPanelModel : public QObject
{
    Q_OBJECT

    Q_PROPERTY(int deleteBehavior READ deleteBehavior WRITE setDeleteBehavior NOTIFY deleteBehaviorChanged FINAL)
    Q_PROPERTY(int closeGapBehavior READ closeGapBehavior WRITE setCloseGapBehavior NOTIFY closeGapBehaviorChanged FINAL)
    Q_PROPERTY(bool userMustChooseCloseGapBehavior READ userMustChooseCloseGapBehavior NOTIFY userMustChooseCloseGapBehaviorChanged FINAL)

    Q_PROPERTY(QVariantList deleteBehaviors READ deleteBehaviors CONSTANT FINAL)
    Q_PROPERTY(QVariantList closeGapBehaviors READ closeGapBehaviors CONSTANT FINAL)

public:
    explicit DeleteBehaviorPanelModel(QObject* parent = nullptr);

    int deleteBehavior() const;
    void setDeleteBehavior(int value);

    int closeGapBehavior() const;
    void setCloseGapBehavior(int value);

    bool userMustChooseCloseGapBehavior() const;

    QVariantList closeGapBehaviors() const;
    QVariantList deleteBehaviors() const;

signals:
    void deleteBehaviorChanged();
    void closeGapBehaviorChanged();
    void userMustChooseCloseGapBehaviorChanged();

private:
    DeleteBehavior m_deleteBehavior = DeleteBehavior::NotSet;
    CloseGapBehavior m_closeGapBehavior = static_cast<CloseGapBehavior>(0);
};
}
