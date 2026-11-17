/*
* Audacity: A Digital Audio Editor
*/
#include "exportsimplestepmodel.h"

#include "framework/global/translation.h"

#include "../internal/exportcommandoptions.h"
#include "../internal/exportstephandlers.h"
#include "exportstepoutput.h"

using namespace au::importexport;
using namespace muse;
using namespace muse::rcommand;

ExportSimpleStepModel::ExportSimpleStepModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

QString ExportSimpleStepModel::title() const
{
    return EXPORT_SIMPLE_STEP_TITLE.qTranslated();
}

QVariantList ExportSimpleStepModel::presetOptions() const
{
    QVariantList options;
    for (const auto& entry : ExportPresetContract::table) {
        options << QVariantMap {
            { "text", QString::fromUtf8(entry.description.data(), static_cast<qsizetype>(entry.description.size())) },
            { "value", QString::fromUtf8(entry.sval.data(), static_cast<qsizetype>(entry.sval.size())) },
            };
    }
    return options;
}

QString ExportSimpleStepModel::preset() const
{
    return QString::fromStdString(EnumContract<ExportPreset>::str_val(m_preset));
}

QString ExportSimpleStepModel::presetResult() const
{
    const ExportPresetSettings preset = presetSettings(m_preset);
    const ExportFormatCommand command = formatCommand(preset.settings, std::string(), preset.channels, m_folder, m_fileName);
    const QString summary = QString::fromStdString(formatSummary(command));

    return preset.channels == ExportChannelLayout::Mono ? qtrc("export", "%1, mono").arg(summary) : summary;
}

QString ExportSimpleStepModel::fileName() const
{
    return QString::fromStdString(m_fileName);
}

void ExportSimpleStepModel::setFileName(const QString& fileName)
{
    const std::string value = fileName.toStdString();
    if (m_fileName == value) {
        return;
    }

    m_fileName = value;
    emit fileNameChanged();
    emit outputChanged();
}

QString ExportSimpleStepModel::folder() const
{
    return QString::fromStdString(m_folder);
}

void ExportSimpleStepModel::setFolder(const QString& folder)
{
    const std::string value = folder.toStdString();
    if (m_folder == value) {
        return;
    }

    m_folder = value;
    emit folderChanged();
    emit outputChanged();
}

QString ExportSimpleStepModel::defaultFolder() const
{
    return stepDefaultFolder(exportConfiguration()->directoryPath());
}

QString ExportSimpleStepModel::preview() const
{
    return stepOutputPreview(exportConfiguration()->directoryPath(), globalContext()->currentProject(), m_folder, m_fileName,
                             extensionOf(presetSettings(m_preset).settings));
}

QString ExportSimpleStepModel::fileNameWarning() const
{
    return stepFileNameWarning(m_fileName);
}

void ExportSimpleStepModel::load(const QVariantMap& data)
{
    const RetVal<ExportSimpleStep> step = simpleStepFromVal(stepDataFromMap(data));
    const ExportSimpleStep loaded = step.ret ? step.val : ExportSimpleStep {};
    m_preset = loaded.preset;
    m_folder = loaded.folder;
    m_fileName = loaded.fileName;

    emit presetChanged();
    emit fileNameChanged();
    emit folderChanged();
    emit outputChanged();
}

void ExportSimpleStepModel::setPreset(const QString& preset)
{
    const std::optional<ExportPreset> value = EnumContract<ExportPreset>::enum_val(preset.toStdString());
    if (!value || *value == m_preset) {
        return;
    }

    m_preset = *value;
    emit presetChanged();
    emit outputChanged();
}

QVariantMap ExportSimpleStepModel::stepData() const
{
    return stepDataToMap(toVal(ExportSimpleStep { m_preset, m_folder, m_fileName }));
}
