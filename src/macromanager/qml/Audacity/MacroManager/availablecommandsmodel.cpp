/*
* Audacity: A Digital Audio Editor
*/
#include "availablecommandsmodel.h"

#include <algorithm>

#include "commanddisplay.h"

using namespace au::macromanager;
using namespace muse;

namespace {
const std::vector<std::string> EXCLUDED_PATH_PREFIXES {
    "dock/",
    "diagnostics/",
    "navigation/",
    "update/",
};

bool isMacroEligible(const rcommand::CommandInfo& info)
{
    if (!info.availabilities.testFlag(rcommand::Availability::Extension)) {
        return false;
    }

    const std::string& path = info.command.path();
    return std::ranges::none_of(EXCLUDED_PATH_PREFIXES, [&path](const std::string& prefix) {
        return path.starts_with(prefix);
    });
}
}

AvailableCommandsModel::AvailableCommandsModel(QObject* parent)
    : QAbstractListModel(parent)
{
}

QVariant AvailableCommandsModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid() || index.row() >= rowCount()) {
        return QVariant();
    }

    const Item& item = m_items.at(index.row());

    switch (role) {
    case CodeRole: return item.code;
    case TitleRole: return item.title;
    case SubtitleRole: return item.subtitle;
    case IsMacroRole: return item.isMacro;
    default: return QVariant();
    }
}

int AvailableCommandsModel::rowCount(const QModelIndex&) const
{
    return m_items.size();
}

QHash<int, QByteArray> AvailableCommandsModel::roleNames() const
{
    static const QHash<int, QByteArray> roles {
        { CodeRole, "code" },
        { TitleRole, "title" },
        { SubtitleRole, "subtitle" },
        { IsMacroRole, "isMacro" }
    };

    return roles;
}

QString AvailableCommandsModel::searchText() const
{
    return m_searchText;
}

void AvailableCommandsModel::load(const QString& editedMacroId)
{
    m_editedMacroId = MacroId(editedMacroId.toStdString());

    commandsRegister()->commandInfoListChanged().onNotify(this, [this]() {
        reload();
    });

    repository()->macrosChanged().onNotify(this, [this]() {
        reload();
    });

    reload();
}

void AvailableCommandsModel::setSearchText(const QString& text)
{
    if (m_searchText == text) {
        return;
    }

    m_searchText = text;
    emit searchTextChanged();

    reload();
}

QVariantMap AvailableCommandsModel::itemAt(int row) const
{
    if (row < 0 || row >= m_items.size()) {
        return {};
    }

    const Item& item = m_items.at(row);

    return {
        { "code", item.code },
        { "title", item.title },
        { "isMacro", item.isMacro }
    };
}

void AvailableCommandsModel::reload()
{
    m_allItems.clear();
    appendCommands();
    appendMacros();

    std::ranges::sort(m_allItems, [](const Item& a, const Item& b) {
        if (a.isMacro != b.isMacro) {
            return b.isMacro;
        }
        return QString::localeAwareCompare(a.title, b.title) < 0;
    });

    QList<Item> items;
    std::ranges::copy_if(m_allItems, std::back_inserter(items), [this](const Item& item) {
        return matchesSearch(item);
    });

    if (items == m_items) {
        return;
    }

    beginResetModel();
    m_items = std::move(items);
    endResetModel();
}

void AvailableCommandsModel::appendCommands()
{
    for (const rcommand::CommandInfo& info : commandsRegister()->commandInfoList()) {
        if (!isMacroEligible(info)) {
            continue;
        }

        const CommandDisplay display = commandDisplay(info, *effectsProvider());
        m_allItems.append({ QString::fromStdString(info.command.toString()), display.title, display.subtitle, false });
    }
}

void AvailableCommandsModel::appendMacros()
{
    const std::optional<Macro> editedMacro = m_editedMacroId ? repository()->macro(*m_editedMacroId) : std::nullopt;
    const std::string edited = editedMacro ? editedMacro->name : std::string();
    const std::vector<std::string> dependents = repository()->dependentMacros(edited);

    for (const Macro& macro : repository()->macros()) {
        if (macro.name == edited || std::ranges::find(dependents, macro.name) != dependents.end()) {
            continue;
        }

        const QString name = QString::fromStdString(macro.name);
        m_allItems.append({ name, name, QString(), true });
    }
}

bool AvailableCommandsModel::matchesSearch(const Item& item) const
{
    const QStringList words = m_searchText.simplified().split(u' ', Qt::SkipEmptyParts);
    return std::ranges::all_of(words, [&item](const QString& word) {
        return item.title.contains(word, Qt::CaseInsensitive) || item.subtitle.contains(word, Qt::CaseInsensitive);
    });
}
