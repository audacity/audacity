/*
* Audacity: A Digital Audio Editor
*/
#include "macromanagermodel.h"

#include "framework/global/io/path.h"
#include "framework/global/translation.h"

using namespace au::macromanager;
using namespace muse;

namespace {
const Uri MACRO_NAME_URI("audacity://macromanager/macroname");
const Uri DELETE_MACRO_URI("audacity://macromanager/deletemacro");
const Uri EDIT_MACRO_URI("audacity://macromanager/editmacro");
const std::string MACRO_FILE_SUFFIX(".json");

std::vector<std::string> macroFileFilter()
{
    return { trc("macromanager", "Macro files") + " (*" + MACRO_FILE_SUFFIX + ")" };
}
}

MacroManagerModel::MacroManagerModel(QObject* parent)
    : QAbstractListModel(parent), muse::Contextable(muse::iocCtxForQmlObject(this)), m_runOnFilesLauncher(this)
{
}

QVariant MacroManagerModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid() || !isRowValid(index.row())) {
        return QVariant();
    }

    const Macro& macro = m_macros.at(index.row());

    switch (role) {
    case TitleRole: return QString::fromStdString(macro.name);
    case IsBuiltinRole: return macro.builtin;
    default: return QVariant();
    }
}

int MacroManagerModel::rowCount(const QModelIndex&) const
{
    return static_cast<int>(m_macros.size());
}

QHash<int, QByteArray> MacroManagerModel::roleNames() const
{
    static const QHash<int, QByteArray> roles {
        { TitleRole, "title" },
        { IsBuiltinRole, "isBuiltin" }
    };

    return roles;
}

bool MacroManagerModel::isEmpty() const
{
    return m_macros.empty();
}

void MacroManagerModel::init()
{
    reload();

    repository()->macrosChanged().onNotify(this, [this]() {
        reload();
    });
}

void MacroManagerModel::newMacro()
{
    const std::optional<QString> name = requestMacroName(UriQuery(MACRO_NAME_URI));
    if (!name) {
        return;
    }

    const Macro macro { name->toStdString(), {} };
    if (!repository()->save(macro)) {
        return;
    }

    openEditor(macro.name);
}

void MacroManagerModel::edit(int row)
{
    if (!isRowValid(row)) {
        return;
    }

    openEditor(m_macros.at(row).name);
}

void MacroManagerModel::rename(int row)
{
    if (!isRowValid(row)) {
        return;
    }

    const std::string oldName = m_macros.at(row).name;

    UriQuery uri(MACRO_NAME_URI);
    uri.addParam("title", Val(qtrc("macromanager", "Rename macro")));
    uri.addParam("macroName", Val(oldName));

    const std::optional<QString> newName = requestMacroName(uri);
    if (!newName) {
        return;
    }

    repository()->rename(oldName, newName->toStdString());
}

void MacroManagerModel::remove(int row)
{
    if (!isRowValid(row)) {
        return;
    }

    const std::string name = m_macros.at(row).name;

    UriQuery uri(DELETE_MACRO_URI);
    uri.addParam("macroName", Val(name));

    const RetVal<Val> result = interactive()->openSync(uri);
    if (!result.ret) {
        return;
    }

    repository()->remove(name);
}

void MacroManagerModel::run(int row)
{
    if (!isRowValid(row)) {
        return;
    }

    runner()->run(m_macros.at(row));
}

void MacroManagerModel::runOnFiles(int row)
{
    if (!isRowValid(row)) {
        return;
    }

    m_runOnFilesLauncher.launch(m_macros.at(row));
}

void MacroManagerModel::importMacro()
{
    const io::path_t path = interactive()->selectOpeningFileSync(trc("macromanager", "Import macro"),
                                                                 globalConfiguration()->userDataPath(), macroFileFilter());
    if (path.empty()) {
        return;
    }

    const RetVal<Macro> macro = repository()->readMacroFile(path);
    if (!macro.ret) {
        interactive()->error(trc("macromanager", "The macro could not be imported"), macro.ret.text());
        return;
    }

    if (!confirmUnknownCommands(unknownCommands(macro.val))) {
        return;
    }

    if (repository()->macro(macro.val.name) && !confirmReplace(macro.val.name)) {
        return;
    }

    repository()->save(macro.val);
}

void MacroManagerModel::exportMacro(int row)
{
    if (!isRowValid(row)) {
        return;
    }

    const Macro& macro = m_macros.at(row);

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

bool MacroManagerModel::isRowValid(int row) const
{
    return row >= 0 && row < rowCount();
}

std::vector<std::string> MacroManagerModel::unknownCommands(const Macro& macro) const
{
    std::vector<std::string> unknown;
    for (const MacroStep& step : macro.steps) {
        if (!commandsRegister()->commandInfo(step.command).isValid()) {
            unknown.push_back(step.command.toString());
        }
    }

    return unknown;
}

bool MacroManagerModel::confirmUnknownCommands(const std::vector<std::string>& commands) const
{
    if (commands.empty()) {
        return true;
    }

    std::string list;
    for (const std::string& command : commands) {
        list += "\n" + command;
    }

    const IInteractive::Result result = interactive()->warningSync(
        trc("macromanager", "Some commands of this macro are not available"),
        trc("macromanager", "The macro can be imported, but it will fail at these steps:") + list,
        { IInteractive::Button::Cancel, IInteractive::Button::Continue }, IInteractive::Button::Continue);

    return result.standardButton() == IInteractive::Button::Continue;
}

bool MacroManagerModel::confirmReplace(const std::string& name) const
{
    const IInteractive::Result result = interactive()->questionSync(
        trc("macromanager", "Replace macro?"),
        qtrc("macromanager", "A macro named \"%1\" already exists. Do you want to replace it?").arg(QString::fromStdString(
                                                                                                        name)).toStdString(),
        { IInteractive::Button::No, IInteractive::Button::Yes }, IInteractive::Button::Yes);

    return result.standardButton() == IInteractive::Button::Yes;
}

void MacroManagerModel::reload()
{
    beginResetModel();
    m_macros = repository()->macros();
    endResetModel();

    emit isEmptyChanged();
}

void MacroManagerModel::openEditor(const std::string& name)
{
    UriQuery uri(EDIT_MACRO_URI);
    uri.addParam("macroName", Val(name));

    interactive()->open(uri);
}

std::optional<QString> MacroManagerModel::requestMacroName(const UriQuery& uri) const
{
    const RetVal<Val> result = interactive()->openSync(uri);
    if (!result.ret) {
        return std::nullopt;
    }

    const QString title = result.val.toQString().trimmed();
    if (title.isEmpty()) {
        return std::nullopt;
    }

    return title;
}
