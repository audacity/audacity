/*
 * Audacity: A Digital Audio Editor
 */

#include <QtCore/qxmlstream.h>

#include "global/translation.h"

#include "project/internal/au3/au3tagsaccessor.h"

#include "metadatamodel.h"

using namespace au::importexport;

MetadataModel::MetadataModel(QObject* parent)
    : QAbstractListModel(parent)
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
    project::ProjectMeta projectMeta = tagsAccessor()->tags();
    std::string defaultTemplate = exportConfiguration()->defaultMetadata();

    beginResetModel();

    project::ProjectMeta toLoad;
    if (isMetadataEmpty(projectMeta) && !defaultTemplate.empty()) {
        toLoad = parseXml(QString::fromStdString(defaultTemplate));
    } else {
        toLoad = tagsAccessor()->tags();
    }

    m_meta = std::move(toLoad);
    m_additionalKeys = m_meta.additionalTags.keys();

    endResetModel();
}

void MetadataModel::apply()
{
    tagsAccessor()->setTags(m_meta);
    configuration()->applySettings();
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
    std::vector<std::string> filter { muse::trc("metadata", "XML files") + " (*.xml)" };

    muse::io::path_t defaultDir = projectConfiguration()->lastOpenedProjectsPath();

    if (defaultDir.empty()) {
        defaultDir = projectConfiguration()->userProjectsPath();
    }

    if (defaultDir.empty()) {
        defaultDir = projectConfiguration()->defaultUserProjectsPath();
    }

    muse::io::path_t filePath = interactive()->selectOpeningFileSync(muse::trc("metadata", "Save"), defaultDir, filter);

    QFile f(filePath.toQString());
    if (!f.open(QIODevice::ReadOnly | QIODevice::Text)) {
        return;
    }

    project::ProjectMeta loaded = parseXml(QString::fromUtf8(f.readAll()));

    beginResetModel();
    m_meta = std::move(loaded);
    m_additionalKeys = m_meta.additionalTags.keys();
    endResetModel();
}

void MetadataModel::saveTemplate()
{
    std::vector<std::string> filter { muse::trc("metadata", "XML files") + " (*.xml)" };

    muse::io::path_t defaultDir = projectConfiguration()->lastOpenedProjectsPath();

    if (defaultDir.empty()) {
        defaultDir = projectConfiguration()->userProjectsPath();
    }

    if (defaultDir.empty()) {
        defaultDir = projectConfiguration()->defaultUserProjectsPath();
    }

    muse::io::path_t filePath = interactive()->selectSavingFileSync(muse::trc("metadata", "Save"), defaultDir, filter);

    QFile f(filePath.toQString());
    if (!f.open(QIODevice::WriteOnly | QIODevice::Truncate | QIODevice::Text)) {
        interactive()->errorSync(muse::trc("metadata", "Error saving template"),
                                 muse::trc("metadata", "Unable to save metadata template into given file."));
        return;
    }

    const QString xml = buildXml(true);
    const QByteArray utf8 = xml.toUtf8();
    f.write(utf8);
    f.close();
}

void MetadataModel::setAsDefault()
{
    const QString xml = buildXml(true);
    exportConfiguration()->setDefaultMetadata(xml.toStdString());
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

QString MetadataModel::buildXml(bool autoFormat) const
{
    QString xml;
    QXmlStreamWriter w(&xml);
    w.setAutoFormatting(autoFormat);
    w.writeStartElement(QStringLiteral("tags"));

    for (size_t i = 0; i < kStdTags.size(); ++i) {
        const QString& name = kStdTags[i];
        const QString& val  = m_meta.*(project::kStdMembers[i]);
        w.writeEmptyElement(QStringLiteral("tag"));
        w.writeAttribute(QStringLiteral("name"),  name);
        w.writeAttribute(QStringLiteral("value"), val);
    }

    for (auto it = m_meta.additionalTags.cbegin(); it != m_meta.additionalTags.cend(); ++it) {
        const QString& name = it.key();
        const QString val  = it.value().toString();
        w.writeEmptyElement(QStringLiteral("tag"));
        w.writeAttribute(QStringLiteral("name"),  name);
        w.writeAttribute(QStringLiteral("value"), val);
    }

    w.writeEndElement();
    w.writeEndDocument();
    return xml;
}

au::project::ProjectMeta MetadataModel::parseXml(const QString& xml) const
{
    QXmlStreamReader r(xml);

    au::project::ProjectMeta loaded = m_meta; // keep filePath, thumbnail, etc.
    for (size_t i = 0; i < kStdTags.size(); ++i) {
        loaded.*(project::kStdMembers[i]) = QString();
    }
    loaded.additionalTags.clear();

    while (!r.atEnd() && !r.hasError()) {
        auto token = r.readNext();
        if (token == QXmlStreamReader::StartElement && r.name() == QLatin1String("tag")) {
            const auto attrs = r.attributes();
            const QString name = attrs.value(QStringLiteral("name")).toString();
            const QString val  = attrs.value(QStringLiteral("value")).toString();

            bool assigned = false;
            for (size_t i = 0; i < kStdTags.size(); ++i) {
                if (name == kStdTags[i]) {
                    loaded.*(project::kStdMembers[i]) = val;
                    assigned = true;
                    break;
                }
            }
            if (!assigned && !name.isEmpty()) {
                loaded.additionalTags.insert(name, val);
            }
        }
    }

    if (r.hasError()) {
        interactive()->errorSync(muse::trc("metadata", "Error loading template"),
                                 muse::trc("metadata", "Unable to load metadata template from given file."));
        return {};
    }

    return loaded;
}

bool MetadataModel::isMetadataEmpty(const au::project::ProjectMeta& meta) const
{
    for (size_t i = 0; i < project::kStdMembers.size(); ++i) {
        const QString& value = meta.*(project::kStdMembers[i]);
        if (!value.trimmed().isEmpty()) {
            return false;
        }
    }
    return true;
}
