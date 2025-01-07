/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QAbstractListModel>
#include <QQuickItem>

#include "timecodemodeselector.h"
#include "timecodeformatter.h"
#include "fieldsinteractioncontroller.h"

namespace au::playback {
class TimecodeModel : public QAbstractListModel
{
    Q_OBJECT

    Q_PROPERTY(TimecodeMode mode READ mode WRITE setMode FINAL)
    Q_PROPERTY(double value READ value WRITE setValue NOTIFY valueChanged FINAL)

    Q_PROPERTY(double sampleRate READ sampleRate WRITE setSampleRate FINAL)
    Q_PROPERTY(double tempo READ tempo WRITE setTempo FINAL)
    Q_PROPERTY(int upperTimeSignature READ upperTimeSignature WRITE setUpperTimeSignature FINAL)
    Q_PROPERTY(int lowerTimeSignature READ lowerTimeSignature WRITE setLowerTimeSignature FINAL)

    Q_PROPERTY(QVariantList availableFormats READ availableFormats NOTIFY availableFormatsChanged)
    Q_PROPERTY(int currentFormat READ currentFormat WRITE setCurrentFormat NOTIFY currentFormatChanged FINAL)
    Q_PROPERTY(QString currentFormatStr READ currentFormatStr WRITE setCurrentFormatStr NOTIFY currentFormatChanged FINAL)

    Q_PROPERTY(int currentEditedFieldIndex READ currentEditedFieldIndex
               WRITE setCurrentEditedFieldIndex NOTIFY currentEditedFieldIndexChanged FINAL)

    Q_PROPERTY(QQuickItem * visualItem READ visualItem WRITE setVisualItem CONSTANT)

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
    QString valueString() const;

    TimecodeMode mode() const;
    void setMode(TimecodeMode mode);

    double value() const;
    void setValue(double value);

    int currentFormat() const;
    void setCurrentFormat(int format);

    QString currentFormatStr() const;
    void setCurrentFormatStr(const QString& formatStr);

    int currentEditedFieldIndex() const;
    void setCurrentEditedFieldIndex(int index);

    QQuickItem* visualItem() const;
    void setVisualItem(QQuickItem* item);

    QVariantList availableFormats();

    double sampleRate() const;
    void setSampleRate(double sampleRate);

    double tempo() const;
    void setTempo(double tempo);

    int upperTimeSignature() const;
    void setUpperTimeSignature(int timeSignature);

    int lowerTimeSignature() const;
    void setLowerTimeSignature(int timeSignature);

signals:
    void valueChanged();

    void availableFormatsChanged();
    void currentFormatChanged();

    void currentEditedFieldIndexChanged();

private:
    enum Roles {
        rSymbol = Qt::UserRole + 1,
        rIsEditable
    };

    void reloadFormatter();
    void initFormatter();

    void initFieldInteractionController();

    void updateValueString();

    TimecodeMode m_mode = TimecodeMode::TimePoint;

    double m_value = -1.0;
    QString m_valueString;

    double m_sampleRate = 1.0;
    double m_tempo = 0;
    int m_upperTimeSignature = 0;
    int m_lowerTimeSignature = 0;

    std::shared_ptr<TimecodeFormatter> m_formatter;
    std::shared_ptr<FieldsInteractionController> m_fieldsInteractionController;

    QList<ViewFormat> m_availableViewFormats;
    int m_currentFormat = int(ViewFormatType::HHMMSS);
};
}
