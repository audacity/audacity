/*
 * Audacity: A Digital Audio Editor
 */
#include "lv2viewmodel.h"

#include "effects/effects_base/effectstypes.h"
#include "au3wrap/internal/wxtypes_convert.h"

#include "libraries/lib-effects/EffectManager.h"
#include "libraries/lib-effects/Effect.h"

#include "libraries/lib-lv2/LV2Instance.h"
#include "libraries/lib-lv2/LoadLV2.h"
#include "libraries/lib-lv2/LV2Utils.h"
#include "libraries/lib-lv2/LV2UIFeaturesList.h"

#include "lv2/ui/ui.h"
#include "suil/suil.h"

#include "global/defer.h"
#include "log.h"

#include <dlfcn.h>

namespace au::effects {
namespace {
class LV2Effect : public LV2EffectBase
{
public:
    LV2Effect(const LilvPlugin& plug)
        : LV2EffectBase{plug} {}
};

std::shared_ptr<SuilHost> getSuilHost()
{
    // This is a unique_ptr specialization
    using SuilHostPtr = Lilv_ptr<SuilHost, suil_host_free>;
    // The host has no dependency on the plug-in and can be shared among
    // validators
    static std::weak_ptr<SuilHost> sSuilHost;
    std::shared_ptr<SuilHost> result = sSuilHost.lock();
    if (!result) {
        // shared_ptr erases type of custom deleter of SuilHostPtr
        sSuilHost = result = SuilHostPtr{ suil_host_new(
                                              LV2UIFeaturesList::suil_port_write,
                                              LV2UIFeaturesList::suil_port_index, nullptr, nullptr) };
    }
    return result;
}
}

// Inject factory hook to make LV2Effect capable of UI
static LV2EffectsModule::Factory::SubstituteInUnique<LV2Effect> scope;

Lv2ViewModel::Lv2ViewModel(QObject* parent)
    : QObject(parent), m_handler(*this)
{}

Lv2ViewModel::~Lv2ViewModel()
{
    if (m_externalUiTimer) {
        m_externalUiTimer->stop();
    }
    if (m_externalUiWidget) {
        LV2_EXTERNAL_UI_HIDE(m_externalUiWidget);
    }
    if (mUiShowInterface && mUiShowInterface->hide) {
        mUiShowInterface->hide(suil_instance_get_handle(m_suilInstance.get()));
    }

    instancesRegister()->settingsAccessById(m_instanceId)->Flush();
}

namespace {
bool uiIsX11(const char* host_type_uri)
{
    return !strcmp(host_type_uri, LV2_UI__X11UI);
}

unsigned uiIsSupported(const char* host_type_uri,
                       const char* plugin_type_uri)
{
    if (!strcmp(host_type_uri, plugin_type_uri)
        ||
        // We give X11 UIs a chance to be run externally.
        uiIsX11(plugin_type_uri)) {
        return 1;
    }
    return 0;
}
}

void Lv2ViewModel::init()
{
    IF_ASSERT_FAILED(m_instanceId >= 0) {
        return;
    }

    const auto instance = std::dynamic_pointer_cast<LV2Instance>(instancesRegister()->instanceById(m_instanceId));
    IF_ASSERT_FAILED(instance) {
        return;
    }

    const EffectSettings* settings = instancesRegister()->settingsById(m_instanceId);
    IF_ASSERT_FAILED(settings) {
        return;
    }

    const EffectId id = instancesRegister()->effectIdByInstanceId(m_instanceId);
    const LV2Effect* const effect = dynamic_cast<LV2Effect*>(EffectManager::Get().GetEffect(id.toStdString()));
    IF_ASSERT_FAILED(effect) {
        return;
    }

    m_lilvPlugin = &effect->mPlug;
    m_ports = &effect->mPorts;
    m_portUIStates = std::make_unique<LV2PortUIStates>(instance->GetPortStates(), *m_ports);

    m_title = au3::wxToString(effect->GetSymbol().Internal());
    emit titleChanged();

    constexpr auto sampleRate = 44100.0; // TODO get actual value
    m_wrapper = instance->MakeWrapper(*settings, sampleRate, nullptr);
    IF_ASSERT_FAILED(m_wrapper) {
        return;
    }

    using namespace LV2Symbols;
    // Set the native UI type
    const char* const hostNativeType = LV2_UI__Qt6UI;

    // Determine if the plugin has a supported UI
    const LilvUI* ui = nullptr;
    const LilvNode* uiType = nullptr;
    using LilvUIsPtr = Lilv_ptr<LilvUIs, lilv_uis_free>;

    LilvUIsPtr uis{ lilv_plugin_get_uis(&effect->mPlug) };
    if (uis) {
        if (LilvNodePtr ui_host_uri{ lilv_new_uri(gWorld, hostNativeType) }) {
            LILV_FOREACH(uis, iter, uis.get()) {
                ui = lilv_uis_get(uis.get(), iter);
                if (lilv_ui_is_supported(ui,
                                         uiIsSupported, ui_host_uri.get(), &uiType)) {
                    break;
                }
                if (lilv_ui_is_a(ui, node_Gtk) || lilv_ui_is_a(ui, node_Gtk3)) {
                    uiType = node_Gtk;
                    break;
                }
                ui = nullptr;
            }
        }
    }

    // Check for other supported UIs
    if (!ui && uis) {
        LILV_FOREACH(uis, iter, uis.get()) {
            ui = lilv_uis_get(uis.get(), iter);
            if (lilv_ui_is_a(ui, node_ExternalUI) || lilv_ui_is_a(ui, node_ExternalUIOld)) {
                uiType = node_ExternalUI;
                break;
            }
            ui = nullptr;
        }
    }

    // No usable UI found
    if (ui == nullptr) {
        return;
    }

    const auto uinode = lilv_ui_get_uri(ui);
    lilv_world_load_resource(gWorld, uinode);
    auto& features = mUiFeatures.emplace(m_wrapper->GetFeatures(), &m_handler, uinode,
                                         &m_wrapper->GetInstance(), nullptr);
    if (!features.mOk) {
        return;
    }

    const char* const uiTypeUri = lilv_node_as_uri(uiType);

    const char* const containerTypeUri
        = uiType == node_ExternalUI ? LV2_EXTERNAL_UI__Widget
          : // Setting containerTypeUri to nullptr will prevent SUIL from trying to wrap the UI
          uiIsX11(uiTypeUri) ? nullptr
          : LV2_UI__Qt6UI;

    const LilvCharsPtr uiBundlePath
    { lilv_file_uri_parse(lilv_node_as_uri(lilv_ui_get_bundle_uri(ui)), nullptr) };
    const LilvCharsPtr uiBinaryPath
    { lilv_file_uri_parse(lilv_node_as_uri(lilv_ui_get_binary_uri(ui)), nullptr) };
    const char* const pluginUri = lilv_node_as_uri(lilv_plugin_get_uri(&effect->mPlug));
    const char* const uiUri = lilv_node_as_uri(lilv_ui_get_uri(ui));

    // Create the suil host
    IF_ASSERT_FAILED(mSuilHost = getSuilHost()) {
        return;
    }

    const std::vector<const LV2_Feature*> featurePointers = features.GetFeaturePointers();
    m_suilInstance.reset(suil_instance_new(mSuilHost.get(), &m_handler, containerTypeUri, pluginUri, uiUri, uiTypeUri, uiBundlePath.get(),
                                           uiBinaryPath.get(), featurePointers.data()));

    if (!m_suilInstance) {
        // TODO: fallback to plain UI.
        return;
    }

    const auto runUiLoop = containerTypeUri == LV2_EXTERNAL_UI__Widget || containerTypeUri == nullptr;
    if (!runUiLoop) {
        return;
    }

    m_externalUiTimer = std::make_unique<QTimer>(this);
    std::function<void()> timerCallback;

    if (uiType == node_ExternalUI) {
        m_externalUiWidget = static_cast<LV2_External_UI_Widget*>(suil_instance_get_widget(m_suilInstance.get()));
        timerCallback = [this] {
            m_externalUiWidget->run(m_externalUiWidget);
        };
        m_externalUiWidget->show(m_externalUiWidget);
    } else {
        mUiIdleInterface = static_cast<const LV2UI_Idle_Interface*>(
            suil_instance_extension_data(m_suilInstance.get(), LV2_UI__idleInterface));

        mUiShowInterface = static_cast<const LV2UI_Show_Interface*>(
            suil_instance_extension_data(m_suilInstance.get(), LV2_UI__showInterface));

        if (!mUiIdleInterface || !mUiShowInterface) {
            // TODO: is this an error?
            return;
        }

        SuilHandle const handle = suil_instance_get_handle(m_suilInstance.get());

        timerCallback = [this, handle] {
            if (mUiIdleInterface && mUiIdleInterface->idle && mUiIdleInterface->idle(handle)) {
                mUiIdleInterface = nullptr;
                // The UI has been closed, stop the timer and hide the UI
                m_externalUiTimer->stop();
                if (mUiShowInterface->hide) {
                    mUiShowInterface->hide(handle);
                }
                emit externalUiClosed();
            }
        };

        if (mUiShowInterface->show) {
            mUiShowInterface->show(handle);
        }
    }

    connect(m_externalUiTimer.get(), &QTimer::timeout, this, timerCallback);
    m_externalUiTimer->start(30);
}

int Lv2ViewModel::instanceId() const
{
    return m_instanceId;
}

void Lv2ViewModel::setInstanceId(int newInstanceId)
{
    if (m_instanceId == newInstanceId) {
        return;
    }
    m_instanceId = newInstanceId;
    emit instanceIdChanged();
}

QString Lv2ViewModel::title() const
{
    return m_title;
}

int Lv2ViewModel::ui_resize(int /* width */, int /* height */)
{
    // TODO inform view
    return 0;
}

void Lv2ViewModel::ui_closed()
{
    emit externalUiClosed();
}

void Lv2ViewModel::suil_port_write(uint32_t port_index, uint32_t buffer_size, uint32_t protocol, const void* buffer)
{
    // Handle implicit floats
    if (protocol == 0 && buffer_size == sizeof(float)) {
        if (auto it = m_ports->mControlPortMap.find(port_index);
            it != m_ports->mControlPortMap.end()) {
            const auto value = *static_cast<const float*>(buffer);
            const auto access = instancesRegister()->settingsAccessById(m_instanceId);
            access->ModifySettings(
                [&](EffectSettings& settings)
            {
                GetSettings(settings).values[it->second] = value;
                return nullptr;
            });

            // Publish({ size_t(port_index), value });
        }
    }
    // Handle event transfers
    else if (protocol == LV2Symbols::urid_EventTransfer) {
        const LV2AtomPortStatePtr& atomPortState = m_portUIStates->mControlIn;
        if (atomPortState && port_index == atomPortState->mpPort->mIndex) {
            atomPortState->ReceiveFromDialog(buffer, buffer_size);
        }
    }
}

uint32_t Lv2ViewModel::suil_port_index(const char* port_symbol)
{
    for (size_t i = 0, cnt = lilv_plugin_get_num_ports(m_lilvPlugin); i < cnt; ++i) {
        const auto port = lilv_plugin_get_port_by_index(m_lilvPlugin, i);
        if (strcmp(port_symbol,
                   lilv_node_as_string(lilv_port_get_symbol(m_lilvPlugin, port))) == 0) {
            return lilv_port_get_index(m_lilvPlugin, port);
        }
    }
    return LV2UI_INVALID_PORT_INDEX;
}

MyUiHandler::MyUiHandler(Lv2ViewModel& viewModel)
    : m_viewModel(viewModel)
{}

int MyUiHandler::ui_resize(int width, int height)
{
    return m_viewModel.ui_resize(width, height);
}

void MyUiHandler::ui_closed()
{
    m_viewModel.ui_closed();
}

void MyUiHandler::suil_port_write(uint32_t port_index, uint32_t buffer_size, uint32_t protocol, const void* buffer)
{
    m_viewModel.suil_port_write(port_index, buffer_size, protocol, buffer);
}

uint32_t MyUiHandler::suil_port_index(const char* port_symbol)
{
    return m_viewModel.suil_port_index(port_symbol);
}
}
