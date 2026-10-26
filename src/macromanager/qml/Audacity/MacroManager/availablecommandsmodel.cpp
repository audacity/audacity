/*
* Audacity: A Digital Audio Editor
*/
#include "availablecommandsmodel.h"

#include <algorithm>

using namespace au::macromanager;
using namespace muse;

namespace {
// Stopgap: CommandInfo::availabilities defaults to All, so the Availability::API check below
// does not filter anything yet. Remove this list when commands set their availabilities properly.
const std::vector<std::string> EXCLUDED_PATH_PREFIXES {
    "effects/open/",
    "effects/configure/",
    "effects/presets/",
    "effects/plugin-manager",
    "effects/repeat-last-effect",
    "effects/toggle-vendor-ui",
    "cloud/",
    "dock/",
    "diagnostics/",
    "navigation/",
    "track-navigation/",
    "testflow/",
    "update/",
};

bool isMacroEligible(const rcommand::CommandInfo& info)
{
    if (!info.availabilities.testFlag(rcommand::Availability::API)) {
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
        { IsMacroRole, "isMacro" }
    };

    return roles;
}

QString AvailableCommandsModel::searchText() const
{
    return m_searchText;
}

void AvailableCommandsModel::load(const QString& editedMacroName)
{
    m_editedMacroName = editedMacroName;

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
    beginResetModel();

    m_allItems.clear();
    appendCommands();
    appendMacros();

    std::ranges::sort(m_allItems, [](const Item& a, const Item& b) {
        return QString::localeAwareCompare(a.title, b.title) < 0;
    });

    m_items.clear();
    std::ranges::copy_if(m_allItems, std::back_inserter(m_items), [this](const Item& item) {
        return matchesSearch(item);
    });

    endResetModel();
}

void AvailableCommandsModel::appendCommands()
{
    for (const rcommand::CommandInfo& info : commandsRegister()->commandInfoList()) {
        if (!isMacroEligible(info)) {
            continue;
        }

        m_allItems.append({ QString::fromStdString(info.command.toString()), info.title.qTranslatedWithoutMnemonic(), false });
    }
}

void AvailableCommandsModel::appendMacros()
{
    const std::string edited = m_editedMacroName.toStdString();
    const std::vector<std::string> dependents = repository()->dependentMacros(edited);

    for (const Macro& macro : repository()->macros()) {
        if (macro.name == edited || std::ranges::find(dependents, macro.name) != dependents.end()) {
            continue;
        }

        const QString name = QString::fromStdString(macro.name);
        m_allItems.append({ name, name, true });
    }
}

bool AvailableCommandsModel::matchesSearch(const Item& item) const
{
    return m_searchText.isEmpty() || item.title.contains(m_searchText, Qt::CaseInsensitive);
}
