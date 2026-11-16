/*
* Audacity: A Digital Audio Editor
*/
#include "exportstepoutput.h"

#include "framework/global/translation.h"

#include "../exportcommands.h"
#include "../internal/exportcommandoptions.h"

using namespace muse;

QString au::importexport::stepDefaultFolder(const io::path_t& exportDir)
{
    return (exportDir + "/" + MACRO_OUTPUT_DIR).toQString();
}

QString au::importexport::stepOutputPreview(const io::path_t& exportDir, const project::IAudacityProjectPtr& project,
                                            const std::string& folder, const std::string& fileName, std::string_view extension)
{
    const io::path_t projectPath = project ? project->path() : io::path_t();
    const std::string projectName = project ? project->displayName().toStdString() : EXPORT_PROJECT_NAME_VARIABLE;

    const OutputPathContext context = macroOutputPathContext(exportDir, projectPath, projectName);
    const RetVal<io::path_t> path = exportOutputPath(folder, fileName, context, extension);
    if (!path.ret) {
        return qtrc("export",
                    "The output path is not valid. Use a full folder path. It can start with {ProjectDir} when the project is saved.");
    }

    return path.val.toQString();
}

QString au::importexport::stepFileNameWarning(const std::string& fileName)
{
    if (fileName.empty() || fileName.find(EXPORT_PROJECT_NAME_VARIABLE) != std::string::npos) {
        return QString();
    }

    return qtrc("export", "Add %1 to the file name to run the macro on files")
           .arg(QString::fromStdString(EXPORT_PROJECT_NAME_VARIABLE));
}

Val au::importexport::stepDataFromMap(const QVariantMap& map)
{
    ValMap values;
    for (auto it = map.cbegin(); it != map.cend(); ++it) {
        values[it.key().toStdString()] = Val::fromQVariant(it.value());
    }
    return Val(values);
}

QVariantMap au::importexport::stepDataToMap(const Val& val)
{
    QVariantMap map;
    for (const auto& [key, value] : val.toMap()) {
        map[QString::fromStdString(key)] = value.toQVariant();
    }
    return map;
}
