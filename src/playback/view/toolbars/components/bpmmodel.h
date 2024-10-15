/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QAbstractListModel>
#include <QQuickItem>

#include "timecodeformatter.h"
#include "fieldsinteractioncontroller.h"

namespace au::playback {
class BPMModel : public QAbstractListModel
{
    Q_OBJECT

    Q_PROPERTY(double value READ value WRITE setValue NOTIFY valueChanged FINAL)

    Q_PROPERTY(int currentEditedFieldIndex READ currentEditedFieldIndex
               WRITE setCurrentEditedFieldIndex NOTIFY currentEditedFieldIndexChanged FINAL)

    Q_PROPERTY(QQuickItem * visualItem READ visualItem WRITE setVisualItem CONSTANT)

public:
    explicit BPMModel(QObject* parent = nullptr);

    Q_INVOKABLE void upValue();
    Q_INVOKABLE void downValue();

    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role) const override;
    QHash<int, QByteArray> roleNames() const override;

    double value() const;
    void setValue(double value);

    int currentEditedFieldIndex() const;
    void setCurrentEditedFieldIndex(int index);

    QQuickItem* visualItem() const;
    void setVisualItem(QQuickItem* item);

signals:
    void valueChanged();
    void currentEditedFieldIndexChanged();
    void visualItemChanged();

private:
    enum Roles {
        rSymbol = Qt::UserRole + 1,
        rIsEditable
    };

    void reloadFormatter();
    void initFormatter();

    void initFieldInteractionController();

    void updateValueString();

    double m_value = -1.0;
    QString m_valueString;

    std::shared_ptr<TimecodeFormatter> m_formatter;
    std::shared_ptr<FieldsInteractionController> m_fieldsInteractionController;
};
}
