/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../ieffectpresetsscenario.h"

#include "framework/global/io/path.h"

#include "framework/global/modularity/ioc.h"
#include "framework/global/iglobalconfiguration.h"
#include "framework/interactive/iinteractive.h"

#include "../ieffectpresetsprovider.h"
#include "../ieffectinstancesregister.h"

namespace au::effects {
class EffectPresetsScenario : public IEffectPresetsScenario, public muse::Contextable
{
    muse::GlobalInject<muse::IGlobalConfiguration> globalConfiguration;

    muse::Inject<muse::IInteractive> interactive{ this };
    muse::Inject<IEffectPresetsProvider> presetsProvider{ this };
    muse::Inject<IEffectInstancesRegister> instancesRegister{ this };

public:
    EffectPresetsScenario(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void loadPreset(const EffectInstanceId& effectInstanceId, const PresetId& presetId) override;
    void savePresetAs(const EffectInstanceId& effectInstanceId) override;
    void savePreset(const EffectInstanceId& effectInstanceId, const PresetId& presetId) override;
    void deletePreset(const EffectId& effectId, const PresetId& presetId) override;
    void importPreset(const EffectInstanceId& effectInstanceId) override;
    void exportPreset(const EffectInstanceId& effectInstanceId) override;

private:

    void showError(const muse::Ret& ret, const std::string& text = std::string());

    muse::io::path_t m_lastImportPath;
    muse::io::path_t m_lastExportPath;
};
}
