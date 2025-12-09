/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QAbstractListModel>
#include <QQuickItem>

#include "framework/uicomponents/qml/Muse/UiComponents/menuitem.h"

#include "uicomponents/types/numerictypes.h"

#include "timecodeformatter.h"
#include "fieldsinteractioncontroller.h"

namespace au::uicomponents {
struct NumericViewFormat {
    NumericViewFormatType type = NumericViewFormatType::Undefined;
    QString title;
    QString formatStr;

    bool isValid() const
    {
        return type != NumericViewFormatType::Undefined;
    }
};

class NumericViewModel : public QAbstractListModel
{
    Q_OBJECT

    Q_PROPERTY(double value READ value WRITE setValue NOTIFY valueChanged FINAL)

    Q_PROPERTY(double sampleRate READ sampleRate WRITE setSampleRate FINAL)
    Q_PROPERTY(double tempo READ tempo WRITE setTempo FINAL)
    Q_PROPERTY(int upperTimeSignature READ upperTimeSignature WRITE setUpperTimeSignature FINAL)
    Q_PROPERTY(int lowerTimeSignature READ lowerTimeSignature WRITE setLowerTimeSignature FINAL)

    Q_PROPERTY(QVariantList availableFormats READ availableFormats_property NOTIFY availableFormatsChanged)
    Q_PROPERTY(int currentFormat READ currentFormat WRITE setCurrentFormat NOTIFY currentFormatChanged FINAL)
    Q_PROPERTY(QString currentFormatStr READ currentFormatStr WRITE setCurrentFormatStr NOTIFY currentFormatChanged FINAL)

    Q_PROPERTY(int currentEditedFieldIndex READ currentEditedFieldIndex
               WRITE setCurrentEditedFieldIndex NOTIFY currentEditedFieldIndexChanged FINAL)

    Q_PROPERTY(QQuickItem * visualItem READ visualItem WRITE setVisualItem)

public:
    explicit NumericViewModel(QObject* parent = nullptr);

    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role) const override;
    QHash<int, QByteArray> roleNames() const override;
    QString valueString() const;

    double value() const;
    virtual void setValue(double value);

    double sampleRate() const;
    void setSampleRate(double sampleRate);

    double tempo() const;
    void setTempo(double tempo);

    int upperTimeSignature() const;
    void setUpperTimeSignature(int timeSignature);

    int lowerTimeSignature() const;
    void setLowerTimeSignature(int timeSignature);

    int currentFormat() const;
    void setCurrentFormat(int format);

    QString currentFormatStr() const;
    void setCurrentFormatStr(const QString& formatStr);

    muse::uicomponents::MenuItemList availableFormats();
    QVariantList availableFormats_property();

    int currentEditedFieldIndex() const;
    void setCurrentEditedFieldIndex(int index);

    QQuickItem* visualItem() const;
    void setVisualItem(QQuickItem* item);

signals:
    void valueChanged();
    void availableFormatsChanged();
    void currentFormatChanged();
    void currentEditedFieldIndexChanged();
    void visualItemChanged();

protected:
    enum Roles {
        rSymbol = Qt::UserRole + 1,
        rIsEditable
    };

    void initFieldInteractionController();
    void initFormatter();
    void updateValueString(bool toNearest = true);
    virtual void reloadFormatter() = 0;

    const NumericViewFormat& currentViewFormat() const;

    double m_value = -1.0;
    QString m_valueString;

    double m_sampleRate = 1.0;
    double m_tempo = 0;
    int m_upperTimeSignature = 0;
    int m_lowerTimeSignature = 0;

    QList<NumericViewFormat> m_availableViewFormats;
    NumericViewFormatType m_currentFormat = NumericViewFormatType::Undefined;

    std::shared_ptr<TimecodeFormatter> m_formatter;
    std::shared_ptr<FieldsInteractionController> m_fieldsInteractionController;
};
}
