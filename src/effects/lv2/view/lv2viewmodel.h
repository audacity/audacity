/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "lv2uihandler.h"

#include "effects/effects_base/view/abstracteffectviewmodel.h"
#include "trackedit/iprojecthistory.h"

#include "libraries/lib-lv2/LV2UIFeaturesList.h"
#include "libraries/lib-lv2/LV2Instance.h"
#include "libraries/lib-lv2/LV2Wrapper.h"

#include "modularity/ioc.h"

#include <QObject>
#include <QQuickItem>

#include <variant>

class LV2PortUIStates;
struct LV2EffectOutputs;
typedef unsigned long XID;

namespace au::effects {
class ILv2IdleUi;
class Lv2ViewModel : public AbstractEffectViewModel
{
    Q_OBJECT
    Q_PROPERTY(QString title READ title WRITE setTitle NOTIFY titleChanged FINAL)
    Q_PROPERTY(QString effectState READ effectState WRITE setEffectState NOTIFY effectStateChanged FINAL)
    Q_PROPERTY(QString unsupportedUiReason READ unsupportedUiReason NOTIFY unsupportedUiReasonChanged FINAL)

    muse::Inject<trackedit::IProjectHistory> projectHistory;

public:
    Lv2ViewModel(QObject* parent = nullptr);
    ~Lv2ViewModel() override;

    Q_INVOKABLE void deinit();

    QString effectState() const;
    void setEffectState(const QString& state);

    QString unsupportedUiReason() const;

    QString title() const;
    void setTitle(const QString& title);

signals:
    void titleChanged();
    void externalUiClosed();
    void effectStateChanged();
    void unsupportedUiReasonChanged();

private:
    friend class Lv2UiHandler;
    void doInit() override;
    void doStartPreview() override;
    int onResizeUi(int width, int height);
    void onUiClosed();
    void onKeyPressed(Qt::Key);
    void onSuilPortWrite(uint32_t port_index, uint32_t buffer_size, uint32_t protocol, const void* buffer);
    uint32_t suilPortIndex(const char* port_symbol);

    bool isRealtimeInstance() const;

private:
    using SuilInstancePtr = Lilv_ptr<SuilInstance, suil_instance_free>;

    bool buildFancy();
    bool buildPlain();
    void startUiTimer(bool fancy);
    void startSettingsTimer();
    void onIdle();
    void makeDirty();
    void makeDirtyIfSettingsChanged();
    std::optional<XID> x11Window() const;

    Lv2UiHandler m_handler;

    int m_minimumWidth = 0;
    QString m_title;
    RealtimeEffectStatePtr m_effectState;

    std::shared_ptr<LV2Instance> m_instance;
    std::unique_ptr<LV2Wrapper> m_wrapper;
    const LilvPlugin* m_lilvPlugin = nullptr;
    const LV2Ports* m_ports = nullptr;
    std::unique_ptr<LV2PortUIStates> m_portUIStates;
    const LV2EffectOutputs* m_realtimeOutputs = nullptr;
    EffectSettingsAccessPtr m_settingsAccess;

    SuilInstancePtr m_suilInstance;
    std::shared_ptr<SuilHost> mSuilHost;
    std::optional<const LV2UIFeaturesList> mUiFeatures;

    QTimer m_uiTimer;

    using ILv2IdleUiUPtr = std::unique_ptr<ILv2IdleUi>;
    ILv2IdleUiUPtr m_pluginUi;
    bool m_isX11Window = false;
    std::shared_ptr<char> m_lifetime = std::make_shared<char>();

    QTimer m_settingsTimer;
    bool m_settingsChanged = false;

    std::string m_unsupportedUiReason;
};
}
