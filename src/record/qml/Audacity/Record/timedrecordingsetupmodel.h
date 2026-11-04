/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <optional>

#include <QDateTime>
#include <QObject>
#include <QTimer>
#include <QVariantMap>
#include <QtQml/qqmlregistration.h>

#include "framework/global/async/asyncable.h"
#include "framework/global/io/ifilesystem.h"
#include "framework/global/modularity/ioc.h"
#include "framework/interactive/iinteractive.h"

#include "context/iglobalcontext.h"
#include "importexport/export/iexportconfiguration.h"
#include "importexport/export/iexporter.h"
#include "project/iprojectconfiguration.h"
#include "record/irecordconfiguration.h"
#include "record/timedrecordingtypes.h"

namespace au::record {
class TimedRecordingSetupModel : public QObject, public muse::async::Asyncable, public muse::Contextable
{
    Q_OBJECT
    QML_ELEMENT;

    Q_PROPERTY(bool startNow READ startNow WRITE setStartNow NOTIFY scheduleChanged FINAL)
    Q_PROPERTY(QString startDate READ startDate WRITE setStartDate NOTIFY scheduleChanged FINAL)
    Q_PROPERTY(QString startTime READ startTime WRITE setStartTime NOTIFY scheduleChanged FINAL)

    Q_PROPERTY(bool endAfterDuration READ endAfterDuration WRITE setEndAfterDuration NOTIFY scheduleChanged FINAL)
    Q_PROPERTY(double durationSeconds READ durationSeconds WRITE setDurationSeconds NOTIFY scheduleChanged FINAL)
    Q_PROPERTY(QString endDate READ endDate WRITE setEndDate NOTIFY scheduleChanged FINAL)
    Q_PROPERTY(QString endTime READ endTime WRITE setEndTime NOTIFY scheduleChanged FINAL)

    Q_PROPERTY(QString dateFormat READ dateFormat CONSTANT FINAL)
    Q_PROPERTY(int durationFormat READ durationFormat CONSTANT FINAL)

    Q_PROPERTY(bool saveProjectEnabled READ saveProjectEnabled WRITE setSaveProjectEnabled NOTIFY scheduleChanged FINAL)
    Q_PROPERTY(QString saveProjectPath READ saveProjectPath WRITE setSaveProjectPath NOTIFY scheduleChanged FINAL)

    Q_PROPERTY(bool exportAudioEnabled READ exportAudioEnabled WRITE setExportAudioEnabled NOTIFY scheduleChanged FINAL)
    Q_PROPERTY(QString exportAudioPath READ exportAudioPath WRITE setExportAudioPath NOTIFY scheduleChanged FINAL)

    Q_PROPERTY(bool quitAudacityEnabled READ quitAudacityEnabled WRITE setQuitAudacityEnabled NOTIFY scheduleChanged FINAL)

    Q_PROPERTY(QString summary READ summary NOTIFY summaryChanged FINAL)
    Q_PROPERTY(QString scheduleError READ scheduleError NOTIFY summaryChanged FINAL)
    Q_PROPERTY(bool startTimeInvalid READ startTimeInvalid NOTIFY summaryChanged FINAL)
    Q_PROPERTY(bool endTimeInvalid READ endTimeInvalid NOTIFY summaryChanged FINAL)
    Q_PROPERTY(bool canStart READ canStart NOTIFY summaryChanged FINAL)

    muse::GlobalInject<muse::io::IFileSystem> fileSystem;
    muse::GlobalInject<project::IProjectConfiguration> projectConfiguration;
    muse::GlobalInject<importexport::IExportConfiguration> exportConfiguration;
    muse::GlobalInject<IRecordConfiguration> recordConfiguration;
    muse::ContextInject<context::IGlobalContext> globalContext{ this };
    muse::ContextInject<muse::IInteractive> interactive{ this };
    muse::ContextInject<importexport::IExporter> exporter{ this };

public:
    explicit TimedRecordingSetupModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();
    Q_INVOKABLE QVariantMap scheduleData() const;
    Q_INVOKABLE bool confirmOverwrite();
    Q_INVOKABLE void browseSaveProjectPath();
    Q_INVOKABLE void openExportSettings();

    bool startNow() const;
    void setStartNow(bool startNow);
    QString startDate() const;
    void setStartDate(const QString& date);
    QString startTime() const;
    void setStartTime(const QString& time);

    bool endAfterDuration() const;
    void setEndAfterDuration(bool endAfterDuration);
    double durationSeconds() const;
    void setDurationSeconds(double seconds);
    QString endDate() const;
    void setEndDate(const QString& date);
    QString endTime() const;
    void setEndTime(const QString& time);

    bool saveProjectEnabled() const;
    void setSaveProjectEnabled(bool enabled);
    QString saveProjectPath() const;
    void setSaveProjectPath(const QString& path);

    bool exportAudioEnabled() const;
    void setExportAudioEnabled(bool enabled);
    QString exportAudioPath() const;
    void setExportAudioPath(const QString& path);

    bool quitAudacityEnabled() const;
    void setQuitAudacityEnabled(bool enabled);

    QString dateFormat() const;
    int durationFormat() const;

    QString summary() const;
    QString scheduleError() const;
    bool startTimeInvalid() const;
    bool endTimeInvalid() const;
    bool canStart() const;

signals:
    void scheduleChanged();
    void summaryChanged();

private:
    template<typename T>
    void set(T& member, const T& value);

    std::optional<TimedRecordingSchedule> schedule() const;
    std::optional<QDateTime> parseDateTime(const QString& date, const QString& time) const;
    std::string exportFormat() const;
    std::vector<std::string> exportExtensions() const;
    QString withExportExtension(const QString& path) const;
    void update();

    bool m_startNow = true;
    QString m_startDate;
    QString m_startTime;

    bool m_endAfterDuration = true;
    double m_durationSeconds = 0;
    QString m_endDate;
    QString m_endTime;

    bool m_saveProjectEnabled = false;
    QString m_saveProjectPath;

    bool m_exportAudioEnabled = false;
    QString m_exportAudioPath;

    bool m_quitAudacityEnabled = false;

    QString m_dateFormat;
    QString m_summary;
    QString m_scheduleError;
    bool m_startTimeInvalid = false;
    bool m_endTimeInvalid = false;
    bool m_canStart = false;

    QTimer m_clock;
};
}
