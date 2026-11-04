/*
* Audacity: A Digital Audio Editor
*/
#include "macroactions.h"

#include "framework/global/io/path.h"
#include "framework/global/stringutils.h"
#include "framework/global/translation.h"

using namespace au::macromanager;
using namespace muse;

namespace {
const Uri MACRO_NAME_URI("audacity://macromanager/macroname");
const std::string MACRO_FILE_SUFFIX(".aumacro");
}

std::optional<std::string> MacroActions::requestNewName() const
{
    return requestName(UriQuery(MACRO_NAME_URI));
}

void MacroActions::rename(const std::string& name) const
{
    UriQuery uri(MACRO_NAME_URI);
    uri.addParam("title", Val(qtrc("macromanager", "Rename macro")));
    uri.addParam("macroName", Val(name));

    const std::optional<std::string> newName = requestName(uri);
    if (!newName) {
        return;
    }

    const Ret ret = repository()->rename(name, *newName);
    if (!ret) {
        interactive()->error(trc("macromanager", "The macro could not be renamed"), ret.text());
    }
}

void MacroActions::remove(const std::string& name) const
{
    const std::vector<std::string> dependents = repository()->referencingMacros(name);
    const QString macroName = QString::fromStdString(name);
    const std::string text = dependents.empty()
                             ? qtrc("macromanager", "\"%1\" will be deleted").arg(macroName).toStdString()
                             : qtrc("macromanager", "\"%1\" is used by other macros. Deleting it will also remove it from: %2")
                             .arg(macroName, QString::fromStdString(strings::join(dependents, ", "))).toStdString();

    const IInteractive::Result result = interactive()->warningSync(
        trc("macromanager", "Delete macro"), text,
    {
        IInteractive::ButtonData(IInteractive::Button::Cancel, trc("global", "Cancel")),
        IInteractive::ButtonData(IInteractive::Button::Ok, trc("macromanager", "Confirm"), true)
    },
        int(IInteractive::Button::Ok), { IInteractive::Option::WithIcon },
        trc("macromanager", "Delete macro"));

    if (!result.isButton(IInteractive::Button::Ok)) {
        return;
    }

    const Ret ret = repository()->remove(name);
    if (!ret) {
        interactive()->error(trc("macromanager", "The macro could not be deleted"), ret.text());
    }
}

void MacroActions::exportMacro(const Macro& macro) const
{
    const io::path_t defaultPath = globalConfiguration()->userDataPath() + "/" + io::escapeFileName(macro.name) + MACRO_FILE_SUFFIX;
    const io::path_t path = interactive()->selectSavingFileSync(trc("macromanager", "Export macro"), defaultPath, macroFileFilter());
    if (path.empty()) {
        return;
    }

    const Ret ret = repository()->writeMacroFile(macro, path);
    if (!ret) {
        interactive()->error(trc("macromanager", "The macro could not be exported"), ret.text());
    }
}

io::path_t MacroActions::selectImportFile() const
{
    return interactive()->selectOpeningFileSync(trc("macromanager", "Import macro"), globalConfiguration()->userDataPath(),
                                                macroFileFilter());
}

std::optional<std::string> MacroActions::requestName(UriQuery uri) const
{
    ValList existingNames;
    for (const Macro& macro : repository()->macros()) {
        existingNames.push_back(Val(macro.name));
    }

    uri.addParam("existingNames", Val(existingNames));

    const RetVal<Val> result = interactive()->openSync(uri);
    if (!result.ret) {
        return std::nullopt;
    }

    const QString name = result.val.toQString().trimmed();
    if (name.isEmpty()) {
        return std::nullopt;
    }

    return name.toStdString();
}

std::vector<std::string> MacroActions::macroFileFilter() const
{
    return { trc("macromanager", "Macro files") + " (*" + MACRO_FILE_SUFFIX + ")" };
}
