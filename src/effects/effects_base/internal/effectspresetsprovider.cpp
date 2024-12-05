/*
* Audacity: A Digital Audio Editor
*/
#include "effectspresetsprovider.h"

#include "global/containers.h"
#include "global/translation.h"
#include "global/io/file.h"
#include "global/io/fileinfo.h"

#include "libraries/lib-effects/Effect.h"
#include "libraries/lib-effects/EffectManager.h"
#include "libraries/lib-module-manager/PluginManager.h"

#include "au3wrap/internal/wxtypes_convert.h"

#include "log.h"

using namespace muse;
using namespace au::effects;

const EffectSettingsManager& EffectsPresetsProvider::settingsManager(const EffectId& effectId) const
{
    Effect* effect = effectsProvider()->effect(effectId);
    DO_ASSERT(effect);
    return effect->GetDefinition();
}

PresetIdList EffectsPresetsProvider::factoryPresets(const EffectId& effectId) const
{
    const EffectSettingsManager& sm = settingsManager(effectId);
    return sm.GetFactoryPresets();
}

PresetIdList EffectsPresetsProvider::userPresets(const EffectId& effectId) const
{
    Effect* effect = effectsProvider()->effect(effectId);
    IF_ASSERT_FAILED(effect) {
        return {};
    }
    PresetIdList presets = GetUserPresets(*effect);
    return presets;
}

muse::async::Channel<EffectId> EffectsPresetsProvider::userPresetsChanged() const
{
    return m_userPresetsChanged;
}

Ret EffectsPresetsProvider::applyPreset(const EffectInstanceId& effectInstanceId, const PresetId& presetId)
{
    const EffectId effectId = instancesRegister()->effectIdByInstanceId(effectInstanceId);
    const EffectSettingsManager& sm = settingsManager(effectId);
    EffectSettings* settings = instancesRegister()->settingsById(effectInstanceId);

    Ret ret;

    // try apply factory
    bool isFactory = false;
    {
        PresetIdList presets = factoryPresets(effectId);
        int idx = muse::indexOf(presets, presetId);
        isFactory = idx >= 0;
        if (isFactory) {
            OptionalMessage msg = sm.LoadFactoryPreset(idx, *settings);
            ret = msg ? muse::make_ok() : muse::make_ret(Ret::Code::InternalError);
            if (!ret) {
                LOGE() << "failed load factory preset";
            }
        }
    }

    // try user
    if (!isFactory) {
        OptionalMessage msg = sm.LoadUserPreset(presetId, *settings);
        ret = msg ? muse::make_ok() : muse::make_ret(Ret::Code::InternalError);
        if (!ret) {
            LOGE() << "failed load user preset";
        }
    }

    if (ret) {
        instancesRegister()->notifyAboutSettingsChanged(effectInstanceId);
    }

    return ret;
}

Ret EffectsPresetsProvider::saveCurrentAsPreset(const EffectInstanceId& effectInstanceId)
{
    const EffectId effectId = instancesRegister()->effectIdByInstanceId(effectInstanceId);
    const EffectSettingsManager& sm = settingsManager(effectId);
    EffectSettings* settings = instancesRegister()->settingsById(effectInstanceId);

    RetVal<Val> rv = interactive()->open("audacity://effects/presets/input_name");
    if (!rv.ret) {
        return rv.ret;
    }

    std::string name = rv.val.toString();
    if (name.empty()) {
        return muse::make_ret(Ret::Code::Cancel);
    }

    bool ok = sm.SaveUserPreset(UserPresetsGroup(wxString(name)), *settings);

    if (ok) {
        m_userPresetsChanged.send(effectId);
    }

    return ok ? muse::make_ok() : muse::make_ret(Ret::Code::InternalError);
}

muse::Ret EffectsPresetsProvider::deletePreset(const EffectId& effectId, const PresetId& presetId)
{
    IInteractive::Result res = interactive()->question(
        muse::trc("effects", "Delete Preset"),
        muse::mtrc("effects", "Are you sure you want to delete \"%1\"?")
        .arg(au3::wxToString(presetId)).toStdString(),
        { IInteractive::Button::No, IInteractive::Button::Yes });

    if (res.button() == static_cast<int>(muse::IInteractive::Button::No)) {
        return muse::make_ret(Ret::Code::Cancel);
    }

    auto& pluginManager = PluginManager::Get();
    bool ok = pluginManager.RemoveConfigSubgroup(
        PluginSettings::Private,
        au3::wxFromString(effectId),
        UserPresetsGroup(presetId)
        );

    if (ok) {
        m_userPresetsChanged.send(effectId);
    }

    return ok ? muse::make_ok() : muse::make_ret(Ret::Code::InternalError);
}

static std::vector<std::string> presetFilesFilter()
{
    return { muse::trc("effects", "Presets") + " (*.txt)",
             muse::trc("global", "All files") + " (*)" };
}

muse::Ret EffectsPresetsProvider::importPreset(const EffectInstanceId& effectInstanceId)
{
    const EffectId effectId = instancesRegister()->effectIdByInstanceId(effectInstanceId);
    Effect* effect = effectsProvider()->effect(effectId);
    IF_ASSERT_FAILED(effect) {
        return muse::make_ret(Ret::Code::InternalError);
    }

    EffectSettings* settings = instancesRegister()->settingsById(effectInstanceId);
    IF_ASSERT_FAILED(settings) {
        return muse::make_ret(Ret::Code::InternalError);
    }

    if (m_lastImportPath.empty()) {
        m_lastImportPath = globalConfiguration()->homePath();
    }

    const std::string interactiveTitle = muse::trc("effects", "Import Effect Parameters");
    io::path_t path = interactive()->selectOpeningFile(QString::fromStdString(interactiveTitle),
                                                       m_lastImportPath,
                                                       presetFilesFilter());

    if (path.empty()) {
        return muse::make_ret(Ret::Code::Cancel);
    }

    m_lastImportPath = io::FileInfo(path).dirPath();

    ByteArray data;
    Ret ret = io::File::readFile(path, data);
    if (!ret) {
        return ret;
    }

    wxString params(data.constChar());

    wxString ident = params.BeforeFirst(':');
    params = params.AfterFirst(':');

    auto commandId = effect->GetSquashedName(effect->GetSymbol().Internal());

    if (ident != commandId) {
        // effect identifiers are a sensible length!
        // must also have some params.
        std::string msg;
        if ((params.Length() < 2) || (ident.Length() < 2) || (ident.Length() > 30)) {
            msg = muse::mtrc("effects", "%1: is not a valid presets file.").arg(path.toString()).toStdString();
        } else {
            msg = muse::mtrc("effects", "%1: is for a different Effect, Generator or Analyzer.").arg(path.toString()).toStdString();
        }
        interactive()->error(interactiveTitle, msg);
        ret = muse::make_ret(Ret::Code::NotSupported);
    }

    if (ret) {
        OptionalMessage res = effect->LoadSettingsFromString(params, *settings);
        ret = res ? muse::make_ok() : muse::make_ret(Ret::Code::InternalError);
        if (ret) {
            instancesRegister()->notifyAboutSettingsChanged(effectInstanceId);
        } else {
            LOGE() << "failed load settings from: " << data.constData();
        }
    }

    return ret;
}

muse::Ret EffectsPresetsProvider::exportPreset(const EffectInstanceId& effectInstanceId)
{
    const EffectId effectId = instancesRegister()->effectIdByInstanceId(effectInstanceId);
    Effect* effect = effectsProvider()->effect(effectId);
    IF_ASSERT_FAILED(effect) {
        return muse::make_ret(Ret::Code::InternalError);
    }

    EffectSettings* settings = instancesRegister()->settingsById(effectInstanceId);
    IF_ASSERT_FAILED(settings) {
        return muse::make_ret(Ret::Code::InternalError);
    }

    io::path_t path = interactive()->selectSavingFile(muse::qtrc("effects", "Export Effect Parameters"),
                                                      globalConfiguration()->homePath(),
                                                      presetFilesFilter());

    if (path.empty()) {
        return muse::make_ret(Ret::Code::Cancel);
    }

    wxString params;
    effect->SaveSettingsAsString(*settings, params);
    auto commandId = effect->GetSquashedName(effect->GetSymbol().Internal());
    params = commandId.GET() + ":" + params;

    std::string str = au3::wxToStdSting(params);
    ByteArray data = ByteArray::fromRawData(str.c_str(), str.size());

    Ret ret = io::File::writeFile(path, data);

    return ret;
}
