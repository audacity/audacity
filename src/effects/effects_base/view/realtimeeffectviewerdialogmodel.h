/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "modularity/ioc.h"
#include "ieffectinstancesregister.h"
#include "ieffectsprovider.h"
#include "effectstypes.h"
#include "effects/effects_base/irealtimeeffectservice.h"
#include "context/iglobalcontext.h"
#include "actions/actionable.h"
#include "async/asyncable.h"
#include "ui/view/navigationpanel.h"
#include "uicomponents/view/dialogview.h"

#include <QObject>

namespace au::effects {
class RealtimeEffectViewerDialogModel : public QObject, public muse::Injectable, public muse::async::Asyncable,
    public muse::actions::Actionable
{
    Q_OBJECT
    Q_PROPERTY(QString effectState READ prop_effectState WRITE prop_setEffectState FINAL)
    Q_PROPERTY(QString title READ prop_title NOTIFY titleChanged);
    Q_PROPERTY(QString trackName READ prop_trackName NOTIFY trackNameChanged);
    Q_PROPERTY(bool isActive READ prop_isActive WRITE prop_setIsActive NOTIFY isActiveChanged);
    Q_PROPERTY(bool isMasterEffect READ prop_isMasterEffect NOTIFY isMasterEffectChanged);
    Q_PROPERTY(muse::uicomponents::DialogView * dialogView READ prop_dialogView WRITE prop_setDialogView NOTIFY dialogViewChanged);
    Q_PROPERTY(
        muse::ui::NavigationPanel
        * navigationPanel READ prop_navigationPanel WRITE prop_setNavigationPanel NOTIFY navigationPanelChanged);
    Q_PROPERTY(EffectFamily effectFamily READ prop_effectFamily NOTIFY effectFamilyChanged);

    muse::Inject<IEffectInstancesRegister> instancesRegister;
    muse::Inject<IEffectsProvider> effectsProvider;
    muse::Inject<effects::IRealtimeEffectService> realtimeEffectService;
    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<muse::ui::INavigationController> navigationController;

public:
    Q_INVOKABLE void load();

    RealtimeEffectViewerDialogModel(QObject* parent = nullptr);
    ~RealtimeEffectViewerDialogModel() override;

    QString prop_effectState() const;
    void prop_setEffectState(const QString& effectState);
    QString prop_trackName() const;
    QString prop_title() const;

    bool prop_isActive() const;
    void prop_setIsActive(bool isActive);

    bool prop_isMasterEffect() const;

    muse::uicomponents::DialogView* prop_dialogView() const;
    void prop_setDialogView(muse::uicomponents::DialogView* dialogView);

    muse::ui::NavigationPanel* prop_navigationPanel() const;
    void prop_setNavigationPanel(muse::ui::NavigationPanel* navigationPanel);

    EffectFamily prop_effectFamily() const;

signals:
    void trackNameChanged();
    void titleChanged();
    void isActiveChanged();
    void isMasterEffectChanged();
    void dialogViewChanged();
    void navigationPanelChanged();
    void effectFamilyChanged();

private:
    void subscribe();
    void unregisterState();
    void restoreFocusToMainToolBar();
    bool eventFilter(QObject* obj, QEvent* event) override;

    RealtimeEffectStatePtr m_effectState;
    muse::uicomponents::DialogView* m_dialogView = nullptr;
    muse::ui::NavigationPanel* m_navigationPanel = nullptr;
};
}
