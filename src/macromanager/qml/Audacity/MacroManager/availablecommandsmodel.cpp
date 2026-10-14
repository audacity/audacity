/*
* Audacity: A Digital Audio Editor
*/
#include "availablecommandsmodel.h"

#include <algorithm>

using namespace au::macromanager;
using namespace muse;

namespace {
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
        { TitleRole, "title" }
    };

    return roles;
}

QString AvailableCommandsModel::searchText() const
{
    return m_searchText;
}

void AvailableCommandsModel::load()
{
    commandsRegister()->commandInfoListChanged().onNotify(this, [this]() {
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
        { "title", item.title }
    };
}

void AvailableCommandsModel::reload()
{
    beginResetModel();

    m_allItems.clear();
    for (const rcommand::CommandInfo& info : commandsRegister()->commandInfoList()) {
        if (!isMacroEligible(info)) {
            continue;
        }

        m_allItems.append({ QString::fromStdString(info.command.toString()), info.title.qTranslatedWithoutMnemonic() });
    }

    std::ranges::sort(m_allItems, [](const Item& a, const Item& b) {
        return QString::localeAwareCompare(a.title, b.title) < 0;
    });

    m_items.clear();
    std::ranges::copy_if(m_allItems, std::back_inserter(m_items), [this](const Item& item) {
        return matchesSearch(item);
    });

    endResetModel();
}

bool AvailableCommandsModel::matchesSearch(const Item& item) const
{
    return m_searchText.isEmpty() || item.title.contains(m_searchText, Qt::CaseInsensitive);
}
