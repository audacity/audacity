/*
* Audacity: A Digital Audio Editor
*/
#include "effectsactionscontroller.h"

#include "effects/effects_base/effectscommands.h"
#include "effects/effects_base/effectstypes.h"
#include "wx/string.h"

#include "au3-components/EffectAutomationParameters.h"
#include "au3wrap/internal/wxtypes_convert.h"

#include "log.h"

using namespace muse;
using namespace muse::rcommand;
using namespace au::effects;

static const muse::Uri PLUGIN_MANAGER_URI("audacity://effects/plugin_manager");

void EffectsActionsController::init()
{
    effectsProvider()->effectMetaListChanged().onNotify(this, [this](){
        registerActions();
    });

    registerActions();
}

void EffectsActionsController::registerActions()
{
    commandDispatcher()->unreg(this);

    auto cd = commandDispatcher();

    const EffectMetaList effects = effectsProvider()->effectMetaList();
    for (const EffectMeta& e : effects) {
        const EffectId effectId = e.id;
        cd->onRequest(this, makeEffectOpenCommand(effectId), [this, effectId]() { return openEffect(effectId); });
        cd->onRequest(this, makeEffectApplyCommand(effectId), [this, effectId](const Params& params) {
            return applyEffect(effectId, params);
        });
        cd->onRequest(this, makeEffectConfigureCommand(effectId),
                      ICommandDispatcher::AsyncCallBack([this, effectId](const Request& request,
                                                                         const ICommandDispatcher::OnResponse& onResponse) {
            QMetaObject::invokeMethod(&m_queuedCallContext, [this, request, effectId, onResponse]() {
                onResponse(configureEffect(request, effectId));
            }, Qt::QueuedConnection);
        }));
    }

    cd->onRequest(this, EFFECTS_REPEAT_LAST_EFFECT_COMMAND, [this]() { return repeatLastEffect(); });
    cd->onRequest(this, EFFECTS_PLUGIN_MANAGER_COMMAND, [this]() { return openPluginManager(); });
    cd->onRequest(this, EFFECTS_TOGGLE_VENDOR_UI_COMMAND, [this](const Params& params) { return toggleVendorUI(params); });
    cd->onRequest(this, EFFECTS_PRESET_APPLY_COMMAND, [this](const Params& params) { return applyPreset(params); });
    cd->onRequest(this, EFFECTS_PRESET_SAVE_COMMAND, [this](const Params& params) { return savePreset(params); });
    cd->onRequest(this, EFFECTS_PRESET_SAVE_AS_COMMAND, [this](const Params& params) { return savePresetAs(params); });
    cd->onRequest(this, EFFECTS_PRESET_DELETE_COMMAND, [this](const Params& params) { return deletePreset(params); });
    cd->onRequest(this, EFFECTS_PRESET_IMPORT_COMMAND, [this](const Params& params) { return importPreset(params); });
    cd->onRequest(this, EFFECTS_PRESET_EXPORT_COMMAND, [this](const Params& params) { return exportPreset(params); });

    commandShortcutsRegister()->reload();
}

muse::Ret EffectsActionsController::openEffect(const EffectId& effectId)
{
    IF_ASSERT_FAILED(!effectId.empty()) {
        return make_ret(Ret::Code::BadArgs);
    }
    playbackController()->stop();

    return effectExecutionScenario()->performEffect(effectId);
}

static std::string serializeEffectParams(const Params& params)
{
    CommandParameters eap;
    for (const auto& [key, val] : params) {
        eap.Write(wxString::FromUTF8(key), wxString::FromUTF8(val.toString()));
    }

    wxString effectParams;
    eap.GetParameters(effectParams);

    return effectParams.ToStdString(wxConvUTF8);
}

static Params deserializeEffectParams(const std::string& serialized)
{
    Params params;

    CommandParameters eap(wxString::FromUTF8(serialized));
    wxString key;
    long index = 0;
    bool more = eap.GetFirstEntry(key, index);
    while (more) {
        wxString value;
        eap.Read(key, &value);
        params[key.ToStdString(wxConvUTF8)] = Val(value.ToStdString(wxConvUTF8));
        more = eap.GetNextEntry(key, index);
    }

    return params;
}

muse::Ret EffectsActionsController::applyEffect(const EffectId& effectId, const Params& params)
{
    IF_ASSERT_FAILED(!effectId.empty()) {
        return make_ret(Ret::Code::BadArgs);
    }

    const std::string effectParams = serializeEffectParams(params);

    LOGI() << "applyEffect: effectId=" << effectId << ", params=" << effectParams;

    playbackController()->stop();
    const muse::Ret ret = effectExecutionScenario()->performEffect(effectId, effectParams);
    if (!ret) {
        LOGE() << "applyEffect failed: effectId=" << effectId << ", code=" << ret.code() << ", text=" << ret.text();
    }
    return ret;
}

Response EffectsActionsController::configureEffect(const Request& request, const EffectId& effectId)
{
    IF_ASSERT_FAILED(!effectId.empty()) {
        return make_response(request, make_ret(Ret::Code::BadArgs));
    }

    const RetVal<std::string> result = effectExecutionScenario()->configureEffect(effectId, serializeEffectParams(request.params));
    if (!result.ret) {
        return make_response(request, result.ret);
    }

    return make_response(request, make_ok(), deserializeEffectParams(result.val));
}

muse::Ret EffectsActionsController::repeatLastEffect()
{
    playbackController()->stop();

    return effectExecutionScenario()->repeatLastProcessor();
}

muse::Ret EffectsActionsController::applyPreset(const Params& params)
{
    IF_ASSERT_FAILED(params.contains(EFFECTS_INSTANCE_ID_PARAM) && params.contains(EFFECTS_PRESET_ID_PARAM)) {
        return make_ret(Ret::Code::BadArgs);
    }

    EffectInstanceId effectInstanceId = params.at(EFFECTS_INSTANCE_ID_PARAM).toInt();
    PresetId presetId = au::au3::wxFromStdString(params.at(EFFECTS_PRESET_ID_PARAM).toString());
    presetsScenario()->loadPreset(effectInstanceId, presetId);
    return make_ok();
}

muse::Ret EffectsActionsController::savePresetAs(const Params& params)
{
    IF_ASSERT_FAILED(params.contains(EFFECTS_INSTANCE_ID_PARAM)) {
        return make_ret(Ret::Code::BadArgs);
    }

    EffectInstanceId effectInstanceId = params.at(EFFECTS_INSTANCE_ID_PARAM).toInt();
    presetsScenario()->savePresetAs(effectInstanceId);
    return make_ok();
}

muse::Ret EffectsActionsController::savePreset(const Params& params)
{
    IF_ASSERT_FAILED(params.contains(EFFECTS_INSTANCE_ID_PARAM) && params.contains(EFFECTS_PRESET_ID_PARAM)) {
        return make_ret(Ret::Code::BadArgs);
    }

    const EffectInstanceId effectInstanceId = params.at(EFFECTS_INSTANCE_ID_PARAM).toInt();
    const PresetId presetId = au::au3::wxFromStdString(params.at(EFFECTS_PRESET_ID_PARAM).toString());
    presetsScenario()->savePreset(effectInstanceId, presetId);
    return make_ok();
}

muse::Ret EffectsActionsController::deletePreset(const Params& params)
{
    IF_ASSERT_FAILED(params.contains(EFFECTS_EFFECT_ID_PARAM) && params.contains(EFFECTS_PRESET_ID_PARAM)) {
        return make_ret(Ret::Code::BadArgs);
    }

    EffectId effectId = EffectId::fromStdString(params.at(EFFECTS_EFFECT_ID_PARAM).toString());
    PresetId presetId = au::au3::wxFromStdString(params.at(EFFECTS_PRESET_ID_PARAM).toString());
    presetsScenario()->deletePreset(effectId, presetId);
    return make_ok();
}

muse::Ret EffectsActionsController::importPreset(const Params& params)
{
    IF_ASSERT_FAILED(params.contains(EFFECTS_INSTANCE_ID_PARAM)) {
        return make_ret(Ret::Code::BadArgs);
    }

    EffectInstanceId effectInstanceId = params.at(EFFECTS_INSTANCE_ID_PARAM).toInt();
    presetsScenario()->importPreset(effectInstanceId);
    return make_ok();
}

muse::Ret EffectsActionsController::exportPreset(const Params& params)
{
    IF_ASSERT_FAILED(params.contains(EFFECTS_INSTANCE_ID_PARAM)) {
        return make_ret(Ret::Code::BadArgs);
    }

    EffectInstanceId effectInstanceId = params.at(EFFECTS_INSTANCE_ID_PARAM).toInt();
    presetsScenario()->exportPreset(effectInstanceId);
    return make_ok();
}

muse::Ret EffectsActionsController::toggleVendorUI(const Params& params)
{
    IF_ASSERT_FAILED(params.contains(EFFECTS_EFFECT_ID_PARAM)) {
        return make_ret(Ret::Code::BadArgs);
    }

    const EffectId effectId = EffectId::fromStdString(params.at(EFFECTS_EFFECT_ID_PARAM).toString());
    const EffectUIMode currentMode = configuration()->effectUIMode(effectId);
    const EffectUIMode newMode = (currentMode == EffectUIMode::VendorUI) ? EffectUIMode::FallbackUI : EffectUIMode::VendorUI;
    configuration()->setEffectUIMode(effectId, newMode);
    return make_ok();
}

muse::Ret EffectsActionsController::openPluginManager()
{
    interactive()->open(PLUGIN_MANAGER_URI);
    return make_ok();
}
