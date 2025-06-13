/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/effects_base/ieffectinstancesregister.h"

#include "libraries/lib-lv2/LV2UIFeaturesList.h"
#include "libraries/lib-lv2/LV2Wrapper.h"
#include "libraries/lib-lv2/lv2_external_ui.h"

#include "modularity/ioc.h"

#include <QObject>
#include <QQuickItem>

class LV2PortUIStates;
struct LV2EffectOutputs;

namespace au::effects {
class Lv2ViewModel;

class MyUiHandler : public LV2UIFeaturesList::UIHandler
{
public:
    MyUiHandler(Lv2ViewModel& viewModel);
    ~MyUiHandler() override = default;

private:
    int ui_resize(int width, int height) override;
    void ui_closed() override;
    void suil_port_write(uint32_t port_index, uint32_t buffer_size, uint32_t protocol, const void* buffer) override;
    uint32_t suil_port_index(const char* port_symbol) override;

    Lv2ViewModel& m_viewModel;
};

//! TODO Move to builtin module
class Lv2ViewModel : public QObject
{
    Q_OBJECT
    Q_PROPERTY(int instanceId READ instanceId WRITE setInstanceId NOTIFY instanceIdChanged FINAL)
    Q_PROPERTY(QString title READ title NOTIFY titleChanged FINAL)
    Q_PROPERTY(QString effectState READ effectState WRITE setEffectState NOTIFY effectStateChanged FINAL)

    muse::Inject<IEffectInstancesRegister> instancesRegister;

public:
    Lv2ViewModel(QObject* parent = nullptr);
    ~Lv2ViewModel() override;

    Q_INVOKABLE void init();

    int instanceId() const;
    void setInstanceId(int newInstanceId);

    QString effectState() const;
    void setEffectState(const QString& state);

    QString title() const;

signals:
    void instanceIdChanged();
    void titleChanged();
    void externalUiClosed();
    void effectStateChanged();

private:
    friend class MyUiHandler;
    int ui_resize(int width, int height);
    void ui_closed();
    void suil_port_write(uint32_t port_index, uint32_t buffer_size, uint32_t protocol, const void* buffer);
    uint32_t suil_port_index(const char* port_symbol);

private:
    using SuilInstancePtr = Lilv_ptr<SuilInstance, suil_instance_free>;
    void onIdle();

    MyUiHandler m_handler;

    int m_instanceId = -1;
    QString m_title;
    RealtimeEffectStatePtr m_effectState;
    std::unique_ptr<LV2Wrapper> m_wrapper;
    const LilvPlugin* m_lilvPlugin = nullptr;
    const LV2Ports* m_ports = nullptr;
    std::unique_ptr<LV2PortUIStates> m_portUIStates;
    const LV2EffectOutputs* m_realtimeOutputs = nullptr;
    EffectSettingsAccessPtr m_settingsAccess;

    SuilInstancePtr m_suilInstance;
    std::shared_ptr<SuilHost> mSuilHost;
    std::optional<const LV2UIFeaturesList> mUiFeatures;

    std::unique_ptr<QTimer> m_externalUiTimer;
    LV2_External_UI_Widget* m_externalUiWidget = nullptr;

    const LV2UI_Idle_Interface* mUiIdleInterface = nullptr;
    const LV2UI_Show_Interface* mUiShowInterface = nullptr;
};
}
