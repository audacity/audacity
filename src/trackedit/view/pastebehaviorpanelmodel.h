#pragma once

#include <QObject>

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "ui/iuiconfiguration.h"

#include "trackedit/trackedittypes.h"

namespace au::trackedit {
class PasteBehaviorPanelModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(int pasteBehavior READ pasteBehavior WRITE setPasteBehavior NOTIFY pasteBehaviorChanged FINAL)
    Q_PROPERTY(int pasteInsertBehavior READ pasteInsertBehavior WRITE setPasteInsertBehavior NOTIFY pasteInsertBehaviorChanged FINAL)
    Q_PROPERTY(bool addBorderToClipImageButtons READ addBorderToClipImageButtons NOTIFY uiThemeChanged FINAL)
    Q_PROPERTY(
        bool userMustChoosePasteInsertBehavior READ userMustChoosePasteInsertBehavior NOTIFY userMustChoosePasteInsertBehaviorChanged FINAL)

    Q_PROPERTY(QVariantList pasteBehaviors READ pasteBehaviors NOTIFY uiThemeChanged FINAL)
    Q_PROPERTY(QVariantList pasteInsertBehaviors READ pasteInsertBehaviors CONSTANT FINAL)

    muse::Inject<muse::ui::IUiConfiguration> uiConfiguration;

public:
    explicit PasteBehaviorPanelModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    int pasteBehavior() const;
    void setPasteBehavior(int value);

    int pasteInsertBehavior() const;
    void setPasteInsertBehavior(int value);

    bool addBorderToClipImageButtons() const;

    bool userMustChoosePasteInsertBehavior() const;

    QVariantList pasteInsertBehaviors() const;
    QVariantList pasteBehaviors() const;

signals:
    void pasteBehaviorChanged();
    void pasteInsertBehaviorChanged();
    void userMustChoosePasteInsertBehaviorChanged();
    void uiThemeChanged();

private:
    PasteBehavior m_pasteBehavior = PasteBehavior::PasteOverlap;
    PasteInsertBehavior m_pasteInsertBehavior = static_cast<PasteInsertBehavior>(0);
};
}
