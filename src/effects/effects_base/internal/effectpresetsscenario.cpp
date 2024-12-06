/*
* Audacity: A Digital Audio Editor
*/
#include "effectpresetsscenario.h"

#include "global/translation.h"
#include "global/io/fileinfo.h"

#include "au3wrap/internal/wxtypes_convert.h"

#include "../effecterrors.h"

#include "log.h"

using namespace muse;
using namespace au::effects;

void EffectPresetsScenario::showError(const muse::Ret& ret, const std::string& text)
{
    interactive()->error(ret.text(), text);
}

void EffectPresetsScenario::applyPreset(const EffectInstanceId& effectInstanceId, const PresetId& presetId)
{
    Ret ret = presetsProvider()->applyPreset(effectInstanceId, presetId);
    if (!ret) {
        showError(ret);
    }
}

void EffectPresetsScenario::saveCurrentAsPreset(const EffectInstanceId& effectInstanceId)
{
    RetVal<Val> rv = interactive()->open("audacity://effects/presets/input_name");
    std::string name = rv.val.toString();
    if (rv.ret.code() == (int)Ret::Code::Cancel || name.empty()) {
        LOGD() << "input preset name is canceled";
        return;
    }

    if (!rv.ret) {
        showError(rv.ret);
    }

    Ret ret = presetsProvider()->saveCurrentAsPreset(effectInstanceId, name);
    if (!ret) {
        showError(ret);
    }
}

void EffectPresetsScenario::deletePreset(const EffectId& effectId, const PresetId& presetId)
{
    IInteractive::Result res = interactive()->question(
        muse::trc("effects", "Delete Preset"),
        muse::mtrc("effects", "Are you sure you want to delete \"%1\"?")
        .arg(au3::wxToString(presetId)).toStdString(),
        { IInteractive::Button::No, IInteractive::Button::Yes });

    if (res.button() == (int)muse::IInteractive::Button::No) {
        LOGD() << "delete preset is canceled";
        return;
    }

    Ret ret = presetsProvider()->deletePreset(effectId, presetId);
    if (!ret) {
        showError(ret);
    }
}

static std::vector<std::string> presetFilesFilter()
{
    return { muse::trc("effects", "Presets") + " (*.txt)",
             muse::trc("global", "All files") + " (*)" };
}

void EffectPresetsScenario::importPreset(const EffectInstanceId& effectInstanceId)
{
    if (m_lastImportPath.empty()) {
        m_lastImportPath = globalConfiguration()->homePath();
    }

    const std::string interactiveTitle = muse::trc("effects", "Import Effect Parameters");
    io::path_t path = interactive()->selectOpeningFile(QString::fromStdString(interactiveTitle),
                                                       m_lastImportPath,
                                                       presetFilesFilter());

    if (path.empty()) {
        LOGD() << "select file to import is canceled";
        return;
    }

    m_lastImportPath = io::FileInfo(path).dirPath();

    Ret ret = presetsProvider()->importPreset(effectInstanceId, path);
    if (!ret) {
        std::string msg;
        if (ret.code() == (int)Err::PresetNotValid) {
            msg = muse::mtrc("effects", "%1: is not a valid presets file.").arg(path.toString()).toStdString();
        } else if (ret.code() == (int)Err::PresetMismatch) {
            msg = muse::mtrc("effects", "%1: is for a different Effect, Generator or Analyzer.").arg(path.toString()).toStdString();
        }

        showError(ret, msg);
    }
}

void EffectPresetsScenario::exportPreset(const EffectInstanceId& effectInstanceId)
{
    if (m_lastExportPath.empty()) {
        m_lastExportPath = globalConfiguration()->homePath();
    }

    io::path_t path = interactive()->selectSavingFile(muse::qtrc("effects", "Export Effect Parameters"),
                                                      m_lastExportPath,
                                                      presetFilesFilter());

    if (path.empty()) {
        LOGD() << "select file to export is canceled";
        return;
    }

    m_lastExportPath = io::FileInfo(path).dirPath();

    Ret ret = presetsProvider()->exportPreset(effectInstanceId, path);
    if (!ret) {
        showError(ret);
    }
}
