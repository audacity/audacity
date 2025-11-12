/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "iinteractive.h"
#include "project/imetadata.h"
#include "project/iprojectconfiguration.h"
#include "iexportconfiguration.h"
#include "appshell/iappshellconfiguration.h"

#include "project/types/projectmeta.h"

namespace au::importexport {
class MetadataModel : public QAbstractListModel, public muse::async::Asyncable
{
    Q_OBJECT

    muse::Inject<muse::IInteractive> interactive;
    muse::Inject<project::IMetadata> metadata;
    muse::Inject<project::IProjectConfiguration> projectConfiguration;
    muse::Inject<importexport::IExportConfiguration> exportConfiguration;
    muse::Inject<appshell::IAppShellConfiguration> configuration;

public:
    explicit MetadataModel(QObject* parent = nullptr);

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

    QString buildXml() const;
    project::ProjectMeta parseXml(const QString& xml) const;

    bool isMetadataEmpty(const au::project::ProjectMeta& meta) const;

private:
    inline static const std::array<std::string, 6> kStdTags =
    {
        muse::trc("metadata", "Artist name"),
        muse::trc("metadata", "Track title"),
        muse::trc("metadata", "Album title"),
        muse::trc("metadata", "Track number"),
        muse::trc("metadata", "Year"),
        muse::trc("metadata", "Comments"),
    };

    project::ProjectMeta m_meta;
    // needed for model purposes, so every new added tag
    // gets appended to the bottom of the table. ProjectMeta's
    // QVariantMap is not sorted.
    QStringList m_additionalKeys;
};
}
