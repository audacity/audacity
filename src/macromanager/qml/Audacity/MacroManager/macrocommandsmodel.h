/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QAbstractListModel>
#include <QtQml/qqmlregistration.h>

namespace au::macromanager {
class MacroCommandsModel : public QAbstractListModel
{
    Q_OBJECT
    QML_ELEMENT;

public:
    explicit MacroCommandsModel(QObject* parent = nullptr);

    QVariant data(const QModelIndex& index, int role) const override;
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    QHash<int, QByteArray> roleNames() const override;

    Q_INVOKABLE void add(const QString& code, const QString& title);
    Q_INVOKABLE void insert(int row, const QString& code, const QString& title);
    Q_INVOKABLE void move(int from, int to);
    Q_INVOKABLE void remove(int row);

private:
    enum Roles {
        StepRole = Qt::UserRole + 1,
        CodeRole,
        TitleRole
    };

    struct Item {
        QString code;
        QString title;
    };

    bool isRowValid(int row) const;
    void notifyStepsChanged(int fromRow, int toRow);

    QList<Item> m_items;
};
}
