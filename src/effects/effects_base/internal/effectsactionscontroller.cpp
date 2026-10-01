/*
* Audacity: A Digital Audio Editor
*/
#include "effectsactionscontroller.h"

#include <algorithm>

#include "effects/effects_base/effectscommands.h"
#include "effects/effects_base/effectstypes.h"
#include "effects/effects_base/internal/effectsutils.h"
#include "effectsuiactions.h"

#include "framework/rcommand/actiontocommand.h"

#include "spectrogram/spectrogramtypes.h"
#include "wx/string.h"

#include "au3-components/EffectAutomationParameters.h"
#include "au3wrap/internal/wxtypes_convert.h"

#include "log.h"

using namespace muse;
using namespace muse::actions;
using namespace muse::rcommand;
using namespace au::effects;

static const ActionCode REPEAT_LAST_EFFECT_CODE("repeat-last-effect");
static const ActionCode PLUGIN_MANAGER_CODE("plugin-manager");
static const ActionQuery EFFECT_OPEN_QUERY("action://effects/open");
static const ActionQuery EFFECT_APPLY_QUERY("action://effects/apply");
static const ActionQuery TOGGLE_VENDOR_UI_QUERY("action://effects/toggle_vendor_ui");
static const ActionQuery PRESET_APPLY_QUERY("action://effects/presets/apply");
static const ActionQuery PRESET_SAVE_QUERY("action://effects/presets/save");
static const ActionQuery PRESET_SAVE_AS_QUERY("action://effects/presets/save_as");
static const ActionQuery PRESET_DELETE_QUERY("action://effects/presets/delete");
static const ActionQuery PRESET_IMPORT_QUERY("action://effects/presets/import");
static const ActionQuery PRESET_EXPORT_QUERY("action://effects/presets/export");

static const muse::Uri PLUGIN_MANAGER_URI("audacity://effects/plugin_manager");

namespace {
CommandQuery queryParamsConv(const Command& command, const ActionData& args)
{
    CommandQuery query(command);
    if (args.empty()) {
        return query;
    }

    const ActionQuery legacy(args.arg<std::string>(0));
    query.setParams(legacy.params());
    return query;
}

CommandQuery effectOpenConv(const Command& command, const ActionData& args)
{
    IF_ASSERT_FAILED(!args.empty()) {
        return CommandQuery(command);
    }

    const ActionQuery legacy(args.arg<std::string>(0));
    return CommandQuery(makeEffectOpenCommand(effectIdFromAction(legacy)));
}

CommandQuery effectApplyConv(const Command& command, const ActionData& args, const EffectMetaList& effects)
{
    IF_ASSERT_FAILED(!args.empty()) {
        return CommandQuery(command);
    }

    const ActionQuery legacy(args.arg<std::string>(0));
    const EffectId effectIdOrTitle = effectIdFromAction(legacy);
    // Search effect by id with a convenience fallback to title for scripting
    const auto it = std::find_if(effects.begin(), effects.end(), [&](const EffectMeta& meta) {
        return meta.id == effectIdOrTitle || meta.title == effectIdOrTitle;
    });
    if (it == effects.end()) {
        LOGE() << "no effect found for symbol: " << effectIdOrTitle;
        return CommandQuery(command);
    }

    CommandQuery query(makeEffectApplyCommand(it->id));
    for (const auto& [key, val] : legacy.params()) {
        if (key != "effectId") {
            query.addParam(key, val);
        }
    }
    return query;
}
}

void EffectsActionsController::init()
{
    m_uiActions = std::make_shared<EffectsUiActions>(iocContext(), this);

    effectsProvider()->effectMetaListChanged().onNotify(this, [this](){
        registerActions();
    });

    registerActions();

    effectExecutionScenario()->lastProcessorIsNowAvailable().onNotify(this, [this] {
        m_canReceiveActionsChanged.send({ REPEAT_LAST_EFFECT_CODE });
    });

    frequencySelectionController()->frequencySelectionChanged().onReceive(this, [this](bool complete) {
        if (complete) {
            notifyAboutSpectralEffectsAvailability();
        }
    });
}

void EffectsActionsController::notifyAboutSpectralEffectsAvailability()
{
    ActionCodeList codes;
    const auto spectralEffects = spectralEffectsRegister()->spectralEffects();
    for (const auto& spectralEffect : spectralEffects) {
        codes.push_back(spectralEffect.action);
    }
    m_canReceiveActionsChanged.send(codes);
}

void EffectsActionsController::registerActions()
{
    dispatcher()->unReg(this);
    commandDispatcher()->unreg(this);

    auto cd = commandDispatcher();

    const EffectMetaList effects = effectsProvider()->effectMetaList();
    for (const EffectMeta& e : effects) {
        const EffectId effectId = e.id;
        cd->onRequest(this, makeEffectOpenCommand(effectId), [this, effectId]() { return openEffect(effectId); });
        cd->onRequest(this, makeEffectApplyCommand(effectId), [this, effectId](const Params& params) {
            return applyEffect(effectId, params);
        });
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

    const Convertor applyConv = [this](const Command& command, const ActionData& args) {
        return effectApplyConv(command, args, effectsProvider()->effectMetaList());
    };

    const std::vector<ActionToCommand> actionToCommand = {
        { EFFECT_OPEN_QUERY.toString(), Command(), effectOpenConv },
        { REPEAT_LAST_EFFECT_CODE, EFFECTS_REPEAT_LAST_EFFECT_COMMAND, {} },
        { PLUGIN_MANAGER_CODE, EFFECTS_PLUGIN_MANAGER_COMMAND, {} },
        { EFFECT_APPLY_QUERY.toString(), Command(), applyConv },
        { TOGGLE_VENDOR_UI_QUERY.toString(), EFFECTS_TOGGLE_VENDOR_UI_COMMAND, queryParamsConv },
        { PRESET_APPLY_QUERY.toString(), EFFECTS_PRESET_APPLY_COMMAND, queryParamsConv },
        { PRESET_SAVE_QUERY.toString(), EFFECTS_PRESET_SAVE_COMMAND, queryParamsConv },
        { PRESET_SAVE_AS_QUERY.toString(), EFFECTS_PRESET_SAVE_AS_COMMAND, queryParamsConv },
        { PRESET_DELETE_QUERY.toString(), EFFECTS_PRESET_DELETE_COMMAND, queryParamsConv },
        { PRESET_IMPORT_QUERY.toString(), EFFECTS_PRESET_IMPORT_COMMAND, queryParamsConv },
        { PRESET_EXPORT_QUERY.toString(), EFFECTS_PRESET_EXPORT_COMMAND, queryParamsConv },
    };
    registerActionToCommand(this, actionToCommand, commandDispatcher(), dispatcher());

    m_uiActions->reload();
    uiActionsRegister()->unreg(m_uiActions);
    uiActionsRegister()->reg(m_uiActions);
    shortcutsRegister()->reload();
}

muse::Ret EffectsActionsController::openEffect(const EffectId& effectId)
{
    IF_ASSERT_FAILED(!effectId.empty()) {
        return make_ret(Ret::Code::BadArgs);
    }
    playbackController()->stop();

    return effectExecutionScenario()->performEffect(effectId);
}

muse::Ret EffectsActionsController::applyEffect(const EffectId& effectId, const Params& params)
{
    IF_ASSERT_FAILED(!effectId.empty()) {
        return make_ret(Ret::Code::BadArgs);
    }

    CommandParameters eap;
    for (const auto& [key, val] : params) {
        eap.Write(wxString::FromUTF8(key), wxString::FromUTF8(val.toString()));
    }
    wxString effectParams;
    eap.GetParameters(effectParams);

    LOGI() << "applyEffect: effectId=" << effectId << ", params=" << effectParams.ToStdString(wxConvUTF8);

    playbackController()->stop();
    const muse::Ret ret = effectExecutionScenario()->performEffect(effectId, effectParams.ToStdString(wxConvUTF8));
    if (!ret) {
        LOGE() << "applyEffect failed: effectId=" << effectId << ", code=" << ret.code() << ", text=" << ret.text();
    }
    return ret;
}

muse::Ret EffectsActionsController::repeatLastEffect()
{
    playbackController()->stop();

    return effectExecutionScenario()->repeatLastProcessor();
}

muse::Ret EffectsActionsController::applyPreset(const Params& params)
{
    IF_ASSERT_FAILED(params.contains("instanceId") && params.contains("presetId")) {
        return make_ret(Ret::Code::BadArgs);
    }

    EffectInstanceId effectInstanceId = params.at("instanceId").toInt();
    PresetId presetId = au::au3::wxFromStdString(params.at("presetId").toString());
    presetsScenario()->loadPreset(effectInstanceId, presetId);
    return make_ok();
}

muse::Ret EffectsActionsController::savePresetAs(const Params& params)
{
    IF_ASSERT_FAILED(params.contains("instanceId")) {
        return make_ret(Ret::Code::BadArgs);
    }

    EffectInstanceId effectInstanceId = params.at("instanceId").toInt();
    presetsScenario()->savePresetAs(effectInstanceId);
    return make_ok();
}

muse::Ret EffectsActionsController::savePreset(const Params& params)
{
    IF_ASSERT_FAILED(params.contains("instanceId") && params.contains("presetId")) {
        return make_ret(Ret::Code::BadArgs);
    }

    const EffectInstanceId effectInstanceId = params.at("instanceId").toInt();
    const PresetId presetId = au::au3::wxFromStdString(params.at("presetId").toString());
    presetsScenario()->savePreset(effectInstanceId, presetId);
    return make_ok();
}

muse::Ret EffectsActionsController::deletePreset(const Params& params)
{
    IF_ASSERT_FAILED(params.contains("effectId") && params.contains("presetId")) {
        return make_ret(Ret::Code::BadArgs);
    }

    EffectId effectId = EffectId::fromStdString(params.at("effectId").toString());
    PresetId presetId = au::au3::wxFromStdString(params.at("presetId").toString());
    presetsScenario()->deletePreset(effectId, presetId);
    return make_ok();
}

muse::Ret EffectsActionsController::importPreset(const Params& params)
{
    IF_ASSERT_FAILED(params.contains("instanceId")) {
        return make_ret(Ret::Code::BadArgs);
    }

    EffectInstanceId effectInstanceId = params.at("instanceId").toInt();
    presetsScenario()->importPreset(effectInstanceId);
    return make_ok();
}

muse::Ret EffectsActionsController::exportPreset(const Params& params)
{
    IF_ASSERT_FAILED(params.contains("instanceId")) {
        return make_ret(Ret::Code::BadArgs);
    }

    EffectInstanceId effectInstanceId = params.at("instanceId").toInt();
    presetsScenario()->exportPreset(effectInstanceId);
    return make_ok();
}

muse::Ret EffectsActionsController::toggleVendorUI(const Params& params)
{
    IF_ASSERT_FAILED(params.contains("effectId")) {
        return make_ret(Ret::Code::BadArgs);
    }

    const EffectId effectId = EffectId::fromStdString(params.at("effectId").toString());
    const EffectUIMode currentMode = configuration()->effectUIMode(effectId);
    const EffectUIMode newMode = (currentMode == EffectUIMode::VendorUI) ? EffectUIMode::FallbackUI : EffectUIMode::VendorUI;
    configuration()->setEffectUIMode(effectId, newMode);
    return make_ok();
}

bool EffectsActionsController::canReceiveAction(const muse::actions::ActionCode& code) const
{
    if (code == REPEAT_LAST_EFFECT_CODE) {
        return effectExecutionScenario()->lastProcessorIsAvailable();
    } else {
        const auto spectralEffects = spectralEffectsRegister()->spectralEffects();
        const auto it = std::find_if(spectralEffects.begin(), spectralEffects.end(), [&code](const auto& spectralEffect) {
            return spectralEffect.action == code;
        });
        if (it != spectralEffects.end()) {
            const spectrogram::FrequencySelection selection = frequencySelectionController()->frequencySelection();
            return frequencySelectionController()->showsSpectrogram(selection.trackId) && selection.isValid();
        }

        return true;
    }
}

muse::async::Channel<muse::actions::ActionCodeList> EffectsActionsController::canReceiveActionsChanged() const
{
    return m_canReceiveActionsChanged;
}

muse::Ret EffectsActionsController::openPluginManager()
{
    interactive()->open(PLUGIN_MANAGER_URI);
    return make_ok();
}
