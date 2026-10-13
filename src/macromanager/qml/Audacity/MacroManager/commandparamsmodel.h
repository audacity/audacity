/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QAbstractListModel>
#include <QVariantMap>
#include <QtQml/qqmlregistration.h>

#include "modularity/ioc.h"
#include "rcommand/icommandsregister.h"

namespace au::macromanager {
class CommandParamsModel : public QAbstractListModel
{
    Q_OBJECT
    QML_ELEMENT;

    Q_PROPERTY(QString title READ title NOTIFY titleChanged FINAL)

    muse::GlobalInject<muse::rcommand::ICommandsRegister> commandsRegister;

public:
    enum class ParamType {
        String,
        Integer,
        Float,
        Boolean,
        Array
    };
    Q_ENUM(ParamType)

    explicit CommandParamsModel(QObject* parent = nullptr);

    QVariant data(const QModelIndex& index, int role) const override;
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    QHash<int, QByteArray> roleNames() const override;

    QString title() const;

    Q_INVOKABLE void load(const QString& command, const QVariantMap& params);
    Q_INVOKABLE void setValue(int row, const QVariant& value);
    Q_INVOKABLE QVariantMap params() const;

signals:
    void titleChanged();

private:
    enum Roles {
        KeyRole = Qt::UserRole + 1,
        DescriptionRole,
        TypeRole,
        MinRole,
        MaxRole,
        ValueRole
    };

    struct Item {
        QString key;
        QString description;
        ParamType type = ParamType::String;
        QVariant min;
        QVariant max;
        QVariant value;
    };

    QString m_title;
    QList<Item> m_items;
};
}
