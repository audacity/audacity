/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QAbstractListModel>
#include <QQuickItem>

#include "numericformatter.h"

namespace au::playback {
class TimecodeModel : public QAbstractListModel
{
    Q_OBJECT

    Q_PROPERTY(double value READ value WRITE setValue NOTIFY valueChanged FINAL)
    Q_PROPERTY(double sampleRate READ sampleRate WRITE setSampleRate NOTIFY sampleRateChanged FINAL)

    Q_PROPERTY(QVariantList availableFormats READ availableFormats NOTIFY availableFormatsChanged)
    Q_PROPERTY(int currentFormat READ currentFormat WRITE setCurrentFormat NOTIFY currentFormatChanged FINAL)

    Q_PROPERTY(int currentEditedFieldIndex READ currentEditedFieldIndex
               WRITE setCurrentEditedFieldIndex NOTIFY currentEditedFieldIndexChanged FINAL)

    Q_PROPERTY(QQuickItem * visualItem READ visualItem WRITE setVisualItem NOTIFY visualItemChanged)

public:
    explicit TimecodeModel(QObject* parent = nullptr);

    enum class ViewFormatType {
        Undefined = -1,
        Seconds,
        SecondsMilliseconds,
        HHMMSS,
        DDHHMMSS,
        HHMMSSHundredths,
        HHMMSSMilliseconds,
        HHMMSSSamples,
        Samples,
        HHMMSSFilmFrames,
        FilmFrames,
        HHMMSSNTSCDropFrames,
        HHMMSSNTSCNonDropFrames,
        NTSCFrames,
        HHMMSSPALFrames,
        PALFrames,
        HHMMSSCDDAFrames,
        CDDAFrames,
        BarBeat,
        BarBeatTick
    };

    struct ViewFormat {
        ViewFormatType type = ViewFormatType::Undefined;
        QString title;
        QString formatStr;
    };

    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role) const override;
    QHash<int, QByteArray> roleNames() const override;

    double value() const;
    void setValue(double value);

    int currentFormat() const;
    void setCurrentFormat(int format);

    int currentEditedFieldIndex() const;
    void setCurrentEditedFieldIndex(int index);

    QQuickItem* visualItem() const;
    void setVisualItem(QQuickItem* newVisualItem);

    QVariantList availableFormats();

    double sampleRate() const;
    void setSampleRate(double sampleRate);

signals:
    void valueChanged();
    void sampleRateChanged();

    void availableFormatsChanged();
    void currentFormatChanged();

    void currentEditedFieldIndexChanged();

    void visualItemChanged();

private:
    enum Roles {
        rSymbol = Qt::UserRole + 1,
        rIsEditable
    };

    bool eventFilter(QObject* watched, QEvent* event) override;
    bool isMouseWithinBoundaries(const QPoint& mousePos) const;

    void moveCurrentEditedField(int moveKey);
    void adjustCurrentEditedField(int adjustKey);

    void updateValueString();

    double m_value = -1.0;
    QString m_valueString;

    double m_sampleRate = 1.0;

    int m_currentEditedFieldIndex = -1;

    std::shared_ptr<NumericConverterFormatter> m_formater;

    QList<ViewFormat> m_availableViewFormats;
    int m_currentFormat = int(ViewFormatType::HHMMSS);

    QQuickItem* m_visualItem = nullptr;
};
}
