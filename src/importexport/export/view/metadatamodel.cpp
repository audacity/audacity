/*
 * Audacity: A Digital Audio Editor
 */

#include "project/internal/au3/au3tagsaccessor.h"
#include "translation.h"

#include "metadatamodel.h"

using namespace au::importexport;

MetadataModel::MetadataModel(QObject* parent)
    : QAbstractListModel(parent)
{
}

MetadataModel::~MetadataModel()
{
}

QVariant MetadataModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid()) {
        return {};
    }

    const int row = index.row();
    if (row < 0 || row >= rowCount()) {
        return {};
    }

    if (row < int(kStdTags.size())) {
        // Standard tags
        if (role == RoleTag) {
            return QString(project::LABEL_MAP[kStdTags[row]]);
        }
        if (role == RoleValue) {
            return QString(m_meta.*(project::kStdMembers[row]));
        }
        return {};
    }

    // Additional tags
    const int extra = row - int(kStdTags.size());
    const QString& key = m_additionalKeys[extra];
    if (role == RoleTag) {
        return key;
    }
    if (role == RoleValue) {
        return m_meta.additionalTags.value(key);
    }
    return {};
}

int MetadataModel::rowCount(const QModelIndex& parent) const
{
    if (parent.isValid()) {
        return 0;
    }

    return int(kStdTags.size()) + m_additionalKeys.size();
}

QHash<int, QByteArray> MetadataModel::roleNames() const
{
    static const QHash<int, QByteArray> roles {
        { RoleTag, "tag" },
        { RoleValue, "value" }
    };

    return roles;
}

void MetadataModel::load()
{
    beginResetModel();

    m_meta = tagsAccessor()->tags();
    m_additionalKeys = m_meta.additionalTags.keys();

    endResetModel();
}

void MetadataModel::apply()
{
    tagsAccessor()->setTags(m_meta);
}

bool MetadataModel::isStandardTag(const int index)
{
    if (index < 0 || index >= rowCount()) {
        return false;
    }

    // first 9 tags are always standard tags
    if (index < int(kStdTags.size())) {
        return true;
    }

    return false;
}

void MetadataModel::loadTemplate()
{
    NOT_IMPLEMENTED;
}

void MetadataModel::saveTemplate()
{
    NOT_IMPLEMENTED;
}

void MetadataModel::setAsDefault()
{
    NOT_IMPLEMENTED;
}

void MetadataModel::addTag()
{
    const int insertRow = rowCount();

    beginInsertRows(QModelIndex(), insertRow, insertRow);

    QString key = muse::qtrc("metadata", "New tag");

    m_meta.additionalTags.insert(key, QString());
    m_additionalKeys.append(key);

    endInsertRows();
}

void MetadataModel::deleteTag(const int index)
{
    const int standardCount = static_cast<int>(project::kStdMembers.size());
    if (index < standardCount || index >= rowCount()) {
        return;
    }

    beginRemoveRows(QModelIndex(), index, index);

    const QString key = m_additionalKeys.takeAt(index - standardCount);
    m_meta.additionalTags.remove(key);

    endRemoveRows();
}

void MetadataModel::renameTag(int row, const QString& newTag)
{
    // disallow renaming standard tag
    const int offset = static_cast<int>(project::kStdMembers.size());
    if (row < offset || row >= rowCount()) {
        return;
    }

    const QString trimmed = newTag.trimmed();
    if (trimmed.isEmpty()) {
        return;
    }

    // disallow renaming to any standard tag name
    if (au::project::isStandardTag(trimmed)) {
        return;
    }

    const int keyIndex = row - offset;
    if (keyIndex < 0 || keyIndex >= m_additionalKeys.size()) {
        return;
    }

    const QString oldKey = m_additionalKeys.at(keyIndex);
    if (oldKey == trimmed) {
        return;
    }

    const QVariant value = m_meta.additionalTags.value(oldKey);
    m_meta.additionalTags.remove(oldKey);
    m_meta.additionalTags.insert(trimmed, value);

    m_additionalKeys[keyIndex] = trimmed;

    const QModelIndex idx = index(row, 0);
    emit dataChanged(idx, idx, { RoleTag, RoleValue });
}

void MetadataModel::setTagValue(int row, const QString& value)
{
    const int offset = static_cast<int>(project::kStdMembers.size());
    if (row < 0 || row >= rowCount()) {
        return;
    }

    if (row < offset) {
        const auto member = project::kStdMembers[row];
        if (m_meta.*member == value) {
            return;
        }
        m_meta.*member = value;

        const QModelIndex idx = index(row, 0);
        emit dataChanged(idx, idx, { RoleValue });
        return;
    }

    const int keyIndex = row - offset;
    if (keyIndex < 0 || keyIndex >= m_additionalKeys.size()) {
        return;
    }

    const QString& key = m_additionalKeys.at(keyIndex);
    const QString old = m_meta.additionalTags.value(key).toString();
    if (old == value) {
        return;
    }

    m_meta.additionalTags.insert(key, value);
    const QModelIndex idx = index(row, 0);
    emit dataChanged(idx, idx, { RoleValue });
}
