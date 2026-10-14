/*
* Audacity: A Digital Audio Editor
*/
#include "macromanagermodel.h"

#include "translation.h"

using namespace au::macromanager;
using namespace muse;

namespace {
const Uri MACRO_NAME_URI("audacity://macromanager/macroname");
const Uri DELETE_MACRO_URI("audacity://macromanager/deletemacro");
const Uri EDIT_MACRO_URI("audacity://macromanager/editmacro");
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

bool MacroManagerModel::isRowValid(int row) const
{
    return row >= 0 && row < rowCount();
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
