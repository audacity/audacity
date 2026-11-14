#pragma once

#include <QObject>

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "ui/iuiconfiguration.h"

#include "trackedit/trackedittypes.h"

namespace au::trackedit {
class DeleteBehaviorPanelModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(int deleteBehavior READ deleteBehavior WRITE setDeleteBehavior NOTIFY deleteBehaviorChanged FINAL)
    Q_PROPERTY(int closeGapBehavior READ closeGapBehavior WRITE setCloseGapBehavior NOTIFY closeGapBehaviorChanged FINAL)
    Q_PROPERTY(bool addBorderToClipImageButtons READ addBorderToClipImageButtons NOTIFY uiThemeChanged FINAL)
    Q_PROPERTY(bool userMustChooseCloseGapBehavior READ userMustChooseCloseGapBehavior NOTIFY userMustChooseCloseGapBehaviorChanged FINAL)

    Q_PROPERTY(QVariantList deleteBehaviors READ deleteBehaviors NOTIFY uiThemeChanged FINAL)
    Q_PROPERTY(QVariantList closeGapBehaviors READ closeGapBehaviors CONSTANT FINAL)

    muse::GlobalInject<muse::ui::IUiConfiguration> uiConfiguration;

public:
    explicit DeleteBehaviorPanelModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    int deleteBehavior() const;
    void setDeleteBehavior(int value);

    int closeGapBehavior() const;
    void setCloseGapBehavior(int value);

    bool addBorderToClipImageButtons() const;

    bool userMustChooseCloseGapBehavior() const;

    QVariantList closeGapBehaviors() const;
    QVariantList deleteBehaviors() const;

signals:
    void deleteBehaviorChanged();
    void closeGapBehaviorChanged();
    void userMustChooseCloseGapBehaviorChanged();
    void uiThemeChanged();

private:
    DeleteBehavior m_deleteBehavior = DeleteBehavior::NotSet;
    CloseGapBehavior m_closeGapBehavior = static_cast<CloseGapBehavior>(0);
};
}
