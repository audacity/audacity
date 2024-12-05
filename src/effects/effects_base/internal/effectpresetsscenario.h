/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../ieffectpresetsscenario.h"

#include "global/io/path.h"

#include "modularity/ioc.h"
#include "global/iinteractive.h"
#include "global/iglobalconfiguration.h"
#include "../ieffectpresetsprovider.h"

namespace au::effects {
class EffectPresetsScenario : public IEffectPresetsScenario
{
    muse::Inject<IEffectPresetsProvider> presetsProvider;
    muse::Inject<muse::IInteractive> interactive;
    muse::Inject<muse::IGlobalConfiguration> globalConfiguration;

public:
    EffectPresetsScenario() = default;

    void applyPreset(const EffectInstanceId& effectInstanceId, const PresetId& presetId) override;
    void saveCurrentAsPreset(const EffectInstanceId& effectInstanceId) override;
    void deletePreset(const EffectId& effectId, const PresetId& presetId) override;
    void importPreset(const EffectInstanceId& effectInstanceId) override;
    void exportPreset(const EffectInstanceId& effectInstanceId) override;

private:

    void showError(const muse::Ret& ret, const std::string& text = std::string());

    muse::io::path_t m_lastImportPath;
    muse::io::path_t m_lastExportPath;
};
}
