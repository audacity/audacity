/*
* Audacity: A Digital Audio Editor
*/
#include "macromanagermodel.h"

#include "framework/global/translation.h"

using namespace au::macromanager;
using namespace muse;

namespace {
const Uri EDIT_MACRO_URI("audacity://macromanager/editmacro");
}

MacroManagerModel::MacroManagerModel(QObject* parent)
    : QAbstractListModel(parent), muse::Contextable(muse::iocCtxForQmlObject(this)), m_actions(this), m_runOnFilesLauncher(this)
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
    const std::optional<std::string> name = m_actions.requestNewName();
    if (!name) {
        return;
    }

    const Macro macro { *name, {} };
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

    m_actions.rename(m_macros.at(row).name);
}

void MacroManagerModel::remove(int row)
{
    if (!isRowValid(row)) {
        return;
    }

    m_actions.remove(m_macros.at(row).name);
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
    const io::path_t path = m_actions.selectImportFile();
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

    const Ret ret = repository()->save(macro.val);
    if (!ret) {
        interactive()->error(trc("macromanager", "The macro could not be imported"), ret.text());
    }
}

void MacroManagerModel::exportMacro(int row)
{
    if (!isRowValid(row)) {
        return;
    }

    m_actions.exportMacro(m_macros.at(row));
}

bool MacroManagerModel::isRowValid(int row) const
{
    return row >= 0 && row < rowCount();
}

std::vector<std::string> MacroManagerModel::unknownCommands(const Macro& macro) const
{
    std::vector<std::string> unknown;
    for (const MacroItem& item : macro.steps) {
        if (const MacroStep* step = std::get_if<MacroStep>(&item)) {
            if (!commandsRegister()->commandInfo(step->command).isValid()) {
                unknown.push_back(step->command.toString());
            }
        } else if (const std::string& name = std::get<MacroReference>(item).name; !repository()->macro(name)) {
            unknown.push_back(name);
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
