/*
* Audacity: A Digital Audio Editor
*/
#include "effectpresetsprovider.h"

#include "global/containers.h"
#include "global/io/file.h"

#include "libraries/lib-effects/Effect.h"
#include "libraries/lib-effects/EffectManager.h"
#include "libraries/lib-module-manager/PluginManager.h"

#include "au3wrap/internal/wxtypes_convert.h"

#include "../effecterrors.h"

#include "log.h"

using namespace muse;
using namespace au::effects;

static const PresetId DEFAULT_PRESET("default");

const EffectSettingsManager& EffectPresetsProvider::settingsManager(const EffectId& effectId) const
{
    Effect* effect = effectsProvider()->effect(effectId);
    DO_ASSERT(effect);
    return effect->GetDefinition();
}

PresetIdList EffectPresetsProvider::factoryPresets(const EffectId& effectId) const
{
    const EffectSettingsManager& sm = settingsManager(effectId);
    return sm.GetFactoryPresets();
}

PresetIdList EffectPresetsProvider::userPresets(const EffectId& effectId) const
{
    Effect* effect = effectsProvider()->effect(effectId);
    IF_ASSERT_FAILED(effect) {
        return {};
    }
    PresetIdList presets = GetUserPresets(*effect);
    return presets;
}

muse::async::Channel<EffectId> EffectPresetsProvider::userPresetsChanged() const
{
    return m_userPresetsChanged;
}

Ret EffectPresetsProvider::applyPreset(const EffectInstanceId& effectInstanceId, const PresetId& presetId)
{
    const EffectId effectId = instancesRegister()->effectIdByInstanceId(effectInstanceId);
    const EffectSettingsManager& sm = settingsManager(effectId);
    EffectSettingsAccess* access = instancesRegister()->settingsAccessById(effectInstanceId);

    Ret ret;

    // try apply default
    bool isApplied = false;
    if (DEFAULT_PRESET == presetId) {
        isApplied = true;
        OptionalMessage msg;
        access->ModifySettings([&](EffectSettings& settings)
        {
            msg = sm.LoadFactoryDefaults(settings);
            return nullptr;
        });
        ret = msg ? muse::make_ok() : make_ret(Err::InternalError);
        if (!ret) {
            LOGE() << "failed load factory defaults";
        }
    }

    // try apply factory
    if (!isApplied) {
        PresetIdList presets = factoryPresets(effectId);
        int idx = muse::indexOf(presets, presetId);
        if (idx >= 0) {
            isApplied = true;

            OptionalMessage msg;
            access->ModifySettings([&](EffectSettings& settings) {
                msg = sm.LoadFactoryPreset(idx, settings);
                return nullptr;
            });
            ret = msg ? muse::make_ok() : make_ret(Err::InternalError);
            if (!ret) {
                LOGE() << "failed load factory preset";
            }
        }
    }

    // try apply user
    if (!isApplied) {
        isApplied = true;
        OptionalMessage msg;
        access->ModifySettings([&](EffectSettings& settings) {
            msg = sm.LoadUserPreset(UserPresetsGroup(wxString(presetId)), settings);
            return nullptr;
        });
        ret = msg ? muse::make_ok() : make_ret(Err::InternalError);
        if (!ret) {
            LOGE() << "failed load user preset";
        }
    }

    if (ret) {
        instancesRegister()->notifyAboutSettingsChanged(effectInstanceId);
    }

    return ret;
}

Ret EffectPresetsProvider::saveCurrentAsPreset(const EffectInstanceId& effectInstanceId, const std::string& presetName)
{
    const EffectId effectId = instancesRegister()->effectIdByInstanceId(effectInstanceId);
    const EffectSettingsManager& sm = settingsManager(effectId);

    instancesRegister()->requestUpdateSettings(effectInstanceId);
    const EffectSettings* settings = instancesRegister()->settingsById(effectInstanceId);
    IF_ASSERT_FAILED(settings) {
        return make_ret(Err::InternalError);
    }

    bool ok = sm.SaveUserPreset(UserPresetsGroup(wxString(presetName)), *settings);

    if (ok) {
        m_userPresetsChanged.send(effectId);
    }

    return ok ? muse::make_ok() : make_ret(Err::InternalError);
}

muse::Ret EffectPresetsProvider::deletePreset(const EffectId& effectId, const PresetId& presetId)
{
    auto& pluginManager = PluginManager::Get();
    bool ok = pluginManager.RemoveConfigSubgroup(
        PluginSettings::Private,
        au3::wxFromString(effectId),
        UserPresetsGroup(presetId)
        );

    if (ok) {
        m_userPresetsChanged.send(effectId);
    }

    return ok ? muse::make_ok() : make_ret(Err::InternalError);
}

muse::Ret EffectPresetsProvider::importPreset(const EffectInstanceId& effectInstanceId, const muse::io::path_t& filePath)
{
    const EffectId effectId = instancesRegister()->effectIdByInstanceId(effectInstanceId);
    Effect* effect = effectsProvider()->effect(effectId);
    IF_ASSERT_FAILED(effect) {
        return make_ret(Err::InternalError);
    }

    EffectSettingsAccess* access = instancesRegister()->settingsAccessById(effectInstanceId);
    IF_ASSERT_FAILED(access) {
        return make_ret(Err::InternalError);
    }

    ByteArray data;
    Ret ret = io::File::readFile(filePath, data);
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
            ret = make_ret(Err::PresetNotValid);
        } else {
            ret = make_ret(Err::PresetMismatch);
        }
    }

    if (ret) {
        OptionalMessage res;
        access->ModifySettings([&](EffectSettings& settings) {
            res = effect->LoadSettingsFromString(params, settings);
            return nullptr;
        });
        ret = res ? muse::make_ok() : make_ret(Err::InternalError);
        if (ret) {
            instancesRegister()->notifyAboutSettingsChanged(effectInstanceId);
        } else {
            LOGE() << "failed load settings from: " << data.constData();
        }
    }

    return ret;
}

muse::Ret EffectPresetsProvider::exportPreset(const EffectInstanceId& effectInstanceId, const io::path_t& filePath)
{
    const EffectId effectId = instancesRegister()->effectIdByInstanceId(effectInstanceId);
    Effect* effect = effectsProvider()->effect(effectId);
    IF_ASSERT_FAILED(effect) {
        return make_ret(Err::InternalError);
    }

    const EffectSettings* settings = instancesRegister()->settingsById(effectInstanceId);
    IF_ASSERT_FAILED(settings) {
        return make_ret(Err::InternalError);
    }

    wxString params;
    effect->SaveSettingsAsString(*settings, params);
    auto commandId = effect->GetSquashedName(effect->GetSymbol().Internal());
    params = commandId.GET() + ":" + params;

    std::string str = au3::wxToStdSting(params);
    ByteArray data = ByteArray::fromRawData(str.c_str(), str.size());

    Ret ret = io::File::writeFile(filePath, data);

    return ret;
}
