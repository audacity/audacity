/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QAbstractListModel>

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "iexporter.h"
#include "iexportconfiguration.h"

using namespace au::importexport;

class DynamicExportOptionsModel : public QAbstractListModel, public muse::async::Asyncable
{
    Q_OBJECT

    muse::Inject<IExporter> exporter;
    muse::Inject<IExportConfiguration> exportConfiguration;

public:
    explicit DynamicExportOptionsModel(QObject* parent = nullptr);

    enum Flag {
        None     = 0x0,
        ReadOnly = 0x1,
        Hidden   = 0x2
    };
    Q_DECLARE_FLAGS(Flags, Flag)

    enum Roles {
        IdRole = Qt::UserRole + 1,
        TitleRole,
        TypeRole,
        ReadOnlyRole,
        HiddenRole,
        ValueRole,
        MinRole,
        MaxRole,
        NamesRole,
        ValuesRole
    };

    struct Row {
        int id = 0;
        QString title;
        ExportOptionType::Type type = ExportOptionType::Type::TypeString;
        Flags flags = None;
        QVariant value;
        int min = 0;
        int max = 0;
        QStringList names;
        QVariantList values;
    };

    Q_INVOKABLE void init();

    int rowCount(const QModelIndex& parent = {}) const override;
    QVariant data(const QModelIndex& index, int role) const override;
    bool setData(const QModelIndex& index, const QVariant& value, int role) override;
    Qt::ItemFlags flags(const QModelIndex& index) const override;
    QHash<int, QByteArray> roleNames() const override;

    Q_INVOKABLE void reloadFromEditor();

signals:
    void optionChanged(int id, QVariant value);

public:
    std::vector<Row> m_rows;
};
