/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>
#include <QVariantList>
#include <QVariantMap>

#include "../internal/selectstep.h"

namespace au::trackedit {
class SelectStepParamsModel : public QObject
{
    Q_OBJECT

    Q_PROPERTY(QString title READ title CONSTANT FINAL)

    Q_PROPERTY(QVariantList timeKindOptions READ timeKindOptions CONSTANT FINAL)
    Q_PROPERTY(QString timeKind READ timeKind WRITE setTimeKind NOTIFY stepChanged FINAL)
    Q_PROPERTY(QVariantList startAnchorOptions READ startAnchorOptions CONSTANT FINAL)
    Q_PROPERTY(QVariantList endAnchorOptions READ endAnchorOptions NOTIFY stepChanged FINAL)
    Q_PROPERTY(QString startAnchor READ startAnchor WRITE setStartAnchor NOTIFY stepChanged FINAL)
    Q_PROPERTY(QString endAnchor READ endAnchor WRITE setEndAnchor NOTIFY stepChanged FINAL)
    Q_PROPERTY(double startOffset READ startOffset WRITE setStartOffset NOTIFY stepChanged FINAL)
    Q_PROPERTY(double endOffset READ endOffset WRITE setEndOffset NOTIFY stepChanged FINAL)

    Q_PROPERTY(QVariantList tracksKindOptions READ tracksKindOptions CONSTANT FINAL)
    Q_PROPERTY(QString tracksKind READ tracksKind WRITE setTracksKind NOTIFY stepChanged FINAL)
    Q_PROPERTY(int firstTrack READ firstTrack WRITE setFirstTrack NOTIFY stepChanged FINAL)
    Q_PROPERTY(int trackCount READ trackCount WRITE setTrackCount NOTIFY stepChanged FINAL)
    Q_PROPERTY(QVariantList modeOptions READ modeOptions CONSTANT FINAL)
    Q_PROPERTY(QString mode READ mode WRITE setMode NOTIFY stepChanged FINAL)

    Q_PROPERTY(QString summary READ summary NOTIFY stepChanged FINAL)

public:
    explicit SelectStepParamsModel(QObject* parent = nullptr);

    QString title() const;

    QVariantList timeKindOptions() const;
    QString timeKind() const;
    void setTimeKind(const QString& kind);
    QVariantList startAnchorOptions() const;
    QVariantList endAnchorOptions() const;
    QString startAnchor() const;
    void setStartAnchor(const QString& anchor);
    QString endAnchor() const;
    void setEndAnchor(const QString& anchor);
    double startOffset() const;
    void setStartOffset(double offset);
    double endOffset() const;
    void setEndOffset(double offset);

    QVariantList tracksKindOptions() const;
    QString tracksKind() const;
    void setTracksKind(const QString& kind);
    int firstTrack() const;
    void setFirstTrack(int track);
    int trackCount() const;
    void setTrackCount(int count);
    QVariantList modeOptions() const;
    QString mode() const;
    void setMode(const QString& mode);

    QString summary() const;

    Q_INVOKABLE void load(const QVariantMap& data);
    Q_INVOKABLE QVariantMap stepData() const;

signals:
    void stepChanged();

private:
    SelectEnhancedStep step() const;

    SelectTimeKind m_timeKind = SelectTimeKindContract::def;
    SelectTimeRange m_timeRange;
    SelectTracksKind m_tracksKind = SelectTracksKindContract::def;
    SelectTrackRange m_trackRange;
};
}
