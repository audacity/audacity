/*
 * Audacity: A Digital Audio Editor
 */
#include "lv2viewmodel.h"
#include "lv2idleuifactory.h"
#include "ilv2idleui.h"

#include "effects/effects_base/effectstypes.h"
#include "au3wrap/internal/wxtypes_convert.h"
#include "playback/iaudiooutput.h"
#include "playback/iplayer.h"

#include "libraries/lib-effects/EffectManager.h"
#include "libraries/lib-effects/Effect.h"
#include "libraries/lib-realtime-effects/RealtimeEffectState.h"
#include "libraries/lib-lv2/LV2Instance.h"
#include "libraries/lib-lv2/LoadLV2.h"
#include "libraries/lib-lv2/LV2Utils.h"
#include "libraries/lib-lv2/LV2UIFeaturesList.h"
#include "libraries/lib-lv2/LV2Ports.h"

#include "lv2/ui/ui.h"
#include "suil/suil.h"

#include "global/defer.h"
#include "log.h"

#include <dlfcn.h>

#include "gtkheaders.h"
#include <X11/Xlib.h>

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
    // The disconnects below should not be necessary, but to be on the safe side ...
    m_uiTimer.stop();
    QObject::disconnect(&m_uiTimer, &QTimer::timeout, this, &Lv2ViewModel::onIdle);
    m_settingsTimer.stop();
    QObject::disconnect(&m_settingsTimer, &QTimer::timeout, this, &Lv2ViewModel::makeDirtyIfSettingsChanged);
    if (isRealtimeInstance() && m_settingsChanged) {
        makeDirty();
    }
}

void Lv2ViewModel::makeDirty()
{
    projectHistory()->modifyState();
    projectHistory()->markUnsaved();
}

void Lv2ViewModel::makeDirtyIfSettingsChanged()
{
    if (m_settingsChanged) {
        m_settingsChanged = false;
        makeDirty();
    }
}

namespace {
bool usesX11(const char* pluginTypeUri)
{
    return !strcmp(pluginTypeUri, LV2_UI__X11UI) || !strcmp(pluginTypeUri, LV2_UI__GtkUI);
}

unsigned uiIsSupported(const char* hostTypeUri,
                       const char* pluginTypeUri)
{
    if (!strcmp(hostTypeUri, pluginTypeUri)
        ||
        // We give X11 UIs a chance to be run externally.
        usesX11(pluginTypeUri)) {
        return 1;
    }
    return 0;
}
}

QString Lv2ViewModel::effectState() const
{
    if (!m_effectState) {
        return {};
    }
    return QString::number(reinterpret_cast<uintptr_t>(m_effectState.get()));
}

void Lv2ViewModel::setEffectState(const QString& state)
{
    if (state == effectState()) {
        return;
    }
    m_effectState = reinterpret_cast<RealtimeEffectState*>(state.toULongLong())->shared_from_this();
    m_realtimeOutputs = dynamic_cast<const LV2EffectOutputs*>(m_effectState->GetOutputs());
    emit effectStateChanged();
}

void Lv2ViewModel::init()
{
    IF_ASSERT_FAILED(m_instanceId >= 0) {
        return;
    }

    m_instance = std::dynamic_pointer_cast<LV2Instance>(instancesRegister()->instanceById(m_instanceId));
    IF_ASSERT_FAILED(m_instance) {
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
    m_portUIStates = std::make_unique<LV2PortUIStates>(m_instance->GetPortStates(), *m_ports);
    m_settingsAccess = instancesRegister()->settingsAccessById(m_instanceId);

    const auto sampleRate = playback()->audioOutput()->sampleRate();
    m_wrapper = m_instance->MakeWrapper(*settings, sampleRate, nullptr);
    IF_ASSERT_FAILED(m_wrapper) {
        return;
    }

    const auto fancy
        = buildFancy() ? std::make_optional(true)
          : buildPlain() ? std::make_optional(false)
          : std::nullopt;
    if (!fancy.has_value()) {
        // Do not assert before we've implemented buildPlain()
        return;
    }

    startUiTimer(*fancy);
    if (isRealtimeInstance()) {
        startSettingsTimer();
    }
}

void Lv2ViewModel::deinit()
{
    if (std::holds_alternative<ILv2IdleUiUPtr>(m_pluginUi)) {
        std::get<ILv2IdleUiUPtr>(m_pluginUi)->hideExternalUi();
    }
}

void Lv2ViewModel::preview()
{
    IF_ASSERT_FAILED(m_settingsAccess) {
        return;
    }

    m_settingsAccess->ModifySettings([this](EffectSettings& settings) {
        executionScenario()->previewEffect(instanceId(), settings);
        return nullptr;
    });
}

GtkWindow* Lv2ViewModel::gtkWindow() const
{
    if (!m_hasGtkWidget) {
        return nullptr;
    }
    GtkWidget* const gtkWidget = static_cast<GtkWidget*>(suil_instance_get_widget(m_suilInstance.get()));
    if (!GTK_IS_WIDGET(gtkWidget)) {
        return nullptr;
    }
    GtkWidget* const topLevel = gtk_widget_get_toplevel(gtkWidget);
    if (GTK_IS_WINDOW(topLevel)) {
        return GTK_WINDOW(topLevel);
    }
    return nullptr;
}

std::optional<XID> Lv2ViewModel::x11Window() const
{
    if (!m_isX11Window) {
        return std::nullopt;
    }
    return reinterpret_cast<XID>(suil_instance_get_widget(m_suilInstance.get()));
}

void Lv2ViewModel::startSettingsTimer()
{
    connect(&m_settingsTimer, &QTimer::timeout, this, &Lv2ViewModel::makeDirtyIfSettingsChanged);
    m_settingsTimer.start(1000);
}

bool Lv2ViewModel::buildFancy()
{
    using namespace LV2Symbols;
    // Set the native UI type
    const char* const hostNativeType = LV2_UI__Qt6UI;

    // Determine if the plugin has a supported UI
    const LilvUI* ui = nullptr;
    const LilvNode* uiType = nullptr;
    using LilvUIsPtr = Lilv_ptr<LilvUIs, lilv_uis_free>;

    LilvUIsPtr uis{ lilv_plugin_get_uis(m_lilvPlugin) };
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
        m_unsupportedUiReason = "No UI provided by the plugin (Please report if AU3 provides a UI for this plugin)";
        emit unsupportedUiReasonChanged();
        return false;
    }

    const auto uinode = lilv_ui_get_uri(ui);
    lilv_world_load_resource(gWorld, uinode);
    auto& features = mUiFeatures.emplace(m_wrapper->GetFeatures(), &m_handler, uinode,
                                         &m_wrapper->GetInstance(), nullptr);
    if (!features.mOk) {
        return false;
    }

    const char* const uiTypeUri = lilv_node_as_uri(uiType);
    const auto isExternalUi = strcmp(uiTypeUri, lilv_node_as_string(node_ExternalUI)) == 0;
    m_hasGtkWidget = strcmp(uiTypeUri, LV2_UI__GtkUI) == 0;
    m_isX11Window = strcmp(uiTypeUri, LV2_UI__X11UI) == 0;

    const char* const containerTypeUri
        = isExternalUi ? LV2_EXTERNAL_UI__Widget
          : // Setting containerTypeUri to nullptr will prevent SUIL from trying to wrap the UI
          usesX11(uiTypeUri) ? nullptr
          : LV2_UI__Qt6UI;

    const LilvCharsPtr uiBundlePath
    { lilv_file_uri_parse(lilv_node_as_uri(lilv_ui_get_bundle_uri(ui)), nullptr) };
    const LilvCharsPtr uiBinaryPath
    { lilv_file_uri_parse(lilv_node_as_uri(lilv_ui_get_binary_uri(ui)), nullptr) };
    const char* const pluginUri = lilv_node_as_uri(lilv_plugin_get_uri(m_lilvPlugin));
    const char* const uiUri = lilv_node_as_uri(lilv_ui_get_uri(ui));

    // Create the suil host
    IF_ASSERT_FAILED(mSuilHost = getSuilHost()) {
        return false;
    }

    const std::vector<const LV2_Feature*> featurePointers = features.GetFeaturePointers();
    m_suilInstance.reset(suil_instance_new(mSuilHost.get(), &m_handler, containerTypeUri, pluginUri, uiUri, uiTypeUri, uiBundlePath.get(),
                                           uiBinaryPath.get(), featurePointers.data()));

    if (!m_suilInstance || suil_instance_get_widget(m_suilInstance.get()) == nullptr) {
        m_unsupportedUiReason = usesX11(uiTypeUri) ? "X11 UI refusing to be externalized" : "Unknown reason (please report)";
        emit unsupportedUiReasonChanged();
        return false;
    }

    // Increment the reference count of the UI binary (and hence of the libraries it uses).
    // Hence we ensure that the UI binary doesn't get unloaded when we close the UI,
    // or some plugin crash the app when re-opening the UI.
    // It seems that not all libraries handle well being unloaded and reloaded...
    dlopen(uiBinaryPath.get(), RTLD_NOW | RTLD_GLOBAL);

    if (auto idleUi = tryCreateLv2IdleUi(*m_suilInstance, isExternalUi)) {
        m_pluginUi = std::move(idleUi);
    } else {
        const auto uri = lilv_node_as_uri(uiType);
        if (!strcmp(uri, LV2_UI__GtkUI)) {
            auto keyPressedCb = [this](Qt::Key key) { onKeyPressed(key); };
            m_pluginUi = std::make_unique<Gtk2Ui>(*m_suilInstance, [this] { onUiClosed(); }, std::move(keyPressedCb), m_title);
        } else {
            m_unsupportedUiReason = "Idle UI creation failed";
            emit unsupportedUiReasonChanged();
            return false;
        }
    }

    if (auto window = gtkWindow()) {
        gtk_window_set_title(window, m_title.toUtf8().constData());
        gtk_window_present(window);
    }

    if (const std::optional<XID> window = x11Window()) {
        Display* const display = XOpenDisplay(nullptr);
        if (display) {
            XMapRaised(display, *window);
            XFlush(display);
            XCloseDisplay(display);
        }
    }

    return true;
}

bool Lv2ViewModel::buildPlain()
{
    return false;
}

void Lv2ViewModel::startUiTimer(bool fancy)
{
    const auto& mySettings = GetSettings(m_settingsAccess->Get());
    const LV2Wrapper* const pMaster = m_instance->GetMaster();

    // Copied from AU3, but I couldn't find a situation where `pMaster` is not null while a UI update is needed.
    if (pMaster && mySettings.mpState) {
        // Maybe there are other important side effects on the instance besides
        // changes of port values
        lilv_state_restore(mySettings.mpState.get(), &pMaster->GetInstance(),
                           nullptr, nullptr, 0, nullptr);
        // Destroy the short lived carrier of preset state
        mySettings.mpState.reset();
    }

    auto& values = mySettings.values;
    {
        const auto sampleRate = playback()->audioOutput()->sampleRate();
        size_t index = 0;
        for (auto& state : m_portUIStates->mControlPortStates) {
            auto& port = state.mpPort;
            if (port->mIsInput) {
                state.mTmp = values[index] * (port->mSampleRate ? sampleRate : 1.0);
            }
            ++index;
        }
    }

    if (fancy) {
        assert(m_suilInstance);
        size_t index = 0;
        for (auto& port : m_ports->mControlPorts) {
            constexpr uint32_t floatFormat = 0;
            if (port->mIsInput) {
                suil_instance_port_event(m_suilInstance.get(), port->mIndex, sizeof(float), floatFormat, &values[index]);
            }
            ++index;
        }
    } else {
        // TODO
    }

    connect(&m_uiTimer, &QTimer::timeout, this, &Lv2ViewModel::onIdle);
    m_uiTimer.start(30);
}

void Lv2ViewModel::onIdle()
{
    auto& portUIStates = *m_portUIStates;

    if (auto& atomState = portUIStates.mControlOut) {
        atomState->SendToDialog([&](const LV2_Atom* atom, uint32_t size) {
            suil_instance_port_event(m_suilInstance.get(),
                                     atomState->mpPort->mIndex, size,
                                     // Means this event sends some structured data:
                                     LV2Symbols::urid_EventTransfer, atom);
        });
    }

    m_settingsAccess->Flush();
    auto& values = GetSettings(m_settingsAccess->Get()).values;

    size_t index = 0;
    for (auto& state : portUIStates.mControlPortStates) {
        auto& port = state.mpPort;

        const auto pValue = port->mIsInput
                            ? &values[index]
                            : m_realtimeOutputs ? &m_realtimeOutputs->values[index]
                            : nullptr;
        if (pValue) {
            auto& value = *pValue;
            // Let UI know that a port's value has changed
            constexpr uint32_t floatFormat = 0;
            if (value != state.mLst) {
                suil_instance_port_event(m_suilInstance.get(), port->mIndex, sizeof(value), floatFormat, &value);
                state.mLst = value;
            }
        }
        ++index;
    }

    // There is a suspicion that `updateExternalUi()` could result in synchronous destruction of this class.
    // Hence, we keep this for the end of the method and check that the lifetime hasn't expired before emitting the signal.
    auto lifetime = std::weak_ptr<char>(m_lifetime);
    if (std::holds_alternative<ILv2IdleUiUPtr>(m_pluginUi) && !std::get<ILv2IdleUiUPtr>(m_pluginUi)->updateExternalUi()) {
        if (!lifetime.expired()) {
            emit externalUiClosed();
        }
    }
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

void Lv2ViewModel::setTitle(const QString& title)
{
    if (m_title == title) {
        return;
    }
    m_title = title;
    emit titleChanged();
}

int Lv2ViewModel::onResizeUi(int /* width */, int /* height */)
{
    // TODO inform view
    return 0;
}

void Lv2ViewModel::onUiClosed()
{
    emit externalUiClosed();
}

void Lv2ViewModel::onKeyPressed(Qt::Key key)
{
    switch (key) {
    case Qt::Key_Escape:
        // Close the UI on Escape key press
        onUiClosed();
        break;
    case Qt::Key_Space: {
        const auto player = playback()->player();
        if (player->playbackStatus() == playback::PlaybackStatus::Stopped) {
            player->play();
        } else {
            player->stop();
        }
    }
    break;
    default: break;
    }
}

void Lv2ViewModel::onSuilPortWrite(uint32_t port_index, uint32_t buffer_size, uint32_t protocol, const void* buffer)
{
    // Handle implicit floats
    if (protocol == 0 && buffer_size == sizeof(float)) {
        if (auto it = m_ports->mControlPortMap.find(port_index);
            it != m_ports->mControlPortMap.end()) {
            const auto value = *static_cast<const float*>(buffer);
            m_settingsAccess->ModifySettings(
                [&](EffectSettings& settings)
            {
                float& currentValue = GetSettings(settings).values[it->second];
                if (currentValue != value) {
                    currentValue = value;
                    m_settingsChanged = true;
                }
                return nullptr;
            });

            // TODO: see what this publication was for in Au3
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

uint32_t Lv2ViewModel::suilPortIndex(const char* port_symbol)
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

bool Lv2ViewModel::isRealtimeInstance() const
{
    return m_effectState != nullptr;
}

QString Lv2ViewModel::unsupportedUiReason() const
{
    return QString::fromStdString(m_unsupportedUiReason);
}
}
