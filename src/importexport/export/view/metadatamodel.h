/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "project/itagsaccessor.h"

#include "project/types/projectmeta.h"

namespace au::importexport {
class MetadataModel : public QAbstractListModel, public muse::async::Asyncable
{
    Q_OBJECT

    muse::Inject<project::ITagsAccessor> tagsAccessor;

public:
    explicit MetadataModel(QObject* parent = nullptr);
    ~MetadataModel();

    QVariant data(const QModelIndex& index, int role) const override;
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    QHash<int, QByteArray> roleNames() const override;

    enum Roles {
        RoleTag = Qt::UserRole + 1,
        RoleValue
    };

    Q_INVOKABLE void load();
    Q_INVOKABLE void apply();

    Q_INVOKABLE bool isStandardTag(const int index);

    Q_INVOKABLE void loadTemplate();
    Q_INVOKABLE void saveTemplate();
    Q_INVOKABLE void setAsDefault();
    Q_INVOKABLE void addTag();
    Q_INVOKABLE void deleteTag(const int index);

    Q_INVOKABLE void renameTag(int index, const QString& newTag);
    Q_INVOKABLE void setTagValue(int index, const QString& value);

private:
    inline static const std::array<QString, 9> kStdTags = {
        QString("TITLE"),
        QString("ARTIST"),
        QString("ALBUM"),
        QString("TRACKNUMBER"),
        QString("YEAR"),
        QString("GENRE"),
        QString("COMMENTS"),
        QString("Software"),
        QString("Copyright")
    };

    project::ProjectMeta m_meta;
    // needed for model purposes, so every new added tag
    // gets appended to the bottom of the table. ProjectMeta's
    // QVariantMap is not sorted.
    QStringList m_additionalKeys;
};
}
