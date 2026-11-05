/*
* Audacity: A Digital Audio Editor
*/
#include "timedrecordingsetupmodel.h"

#include <algorithm>

#include <QLocale>
#include <QRegularExpression>

#include "framework/global/stringutils.h"
#include "framework/global/translation.h"

#include "project/types/projecttypes.h"
#include "uicomponents/types/numerictypes.h"

#include "timedrecordingviewformatter.h"

using namespace au::record;

namespace {
constexpr int CLOCK_INTERVAL_MS = 1000;

const muse::UriQuery EXPORT_SETTINGS_URI("audacity://project/export");

const QString TIME_FORMAT("HH:mm:ss");
const QStringList ALTERNATIVE_TIME_FORMATS { "H:mm:ss", "H:mm" };

QString numericDateFormat()
{
    QString format = QLocale().dateFormat(QLocale::ShortFormat);
    format.replace(QRegularExpression("y+"), "yyyy");
    format.replace(QRegularExpression("M+"), "MM");
    format.replace(QRegularExpression("d+"), "dd");
    return format;
}

QString withProjectSuffix(const QString& path)
{
    const QString trimmed = path.trimmed();
    if (trimmed.isEmpty() || au::project::isAudacity4File(trimmed)) {
        return trimmed;
    }
    return trimmed + "." + QString::fromStdString(au::project::AUP4);
}

QString exportBaseName(const QString& projectName)
{
    if (au::project::isAudacity4File(projectName) || au::project::isAudacityUnsavedFile(projectName)) {
        return muse::io::completeBasename(projectName).toQString();
    }
    return projectName;
}

QDateTime currentDateTime()
{
    QDateTime now = QDateTime::currentDateTime();
    now.setTime(QTime(now.time().hour(), now.time().minute(), now.time().second()));
    return now;
}
}

TimedRecordingSetupModel::TimedRecordingSetupModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this)), m_dateFormat(numericDateFormat())
{
    m_clock.setInterval(CLOCK_INTERVAL_MS);
    connect(&m_clock, &QTimer::timeout, this, &TimedRecordingSetupModel::update);
    connect(this, &TimedRecordingSetupModel::scheduleChanged, this, &TimedRecordingSetupModel::update);
}

void TimedRecordingSetupModel::init()
{
    const TimedRecordingOptions options = recordConfiguration()->timedRecordingOptions();
    const QDateTime now = currentDateTime();
    const QDateTime end = now.addSecs(options.durationSeconds);

    m_startDate = now.date().toString(m_dateFormat);
    m_startTime = now.time().toString(TIME_FORMAT);
    m_durationSeconds = static_cast<double>(options.durationSeconds);
    m_endDate = end.date().toString(m_dateFormat);
    m_endTime = end.time().toString(TIME_FORMAT);
    m_saveProjectEnabled = options.saveProject;
    m_exportAudioEnabled = options.exportAudio;
    m_quitAudacityEnabled = options.quitAudacity;

    const project::IAudacityProjectPtr project = globalContext()->currentProject();
    if (project && !project->isNewlyCreated() && !project->isCloudProject()) {
        m_saveProjectPath = project->path().toQString();
    }

    const muse::io::path_t exportDirectory = exportConfiguration()->directoryPath();
    if (project && !exportDirectory.empty()) {
        m_exportAudioPath = withExportExtension(exportDirectory.appendingComponent(exportBaseName(project->displayName())).toQString());
    }

    emit scheduleChanged();
    m_clock.start();
}

QVariantMap TimedRecordingSetupModel::scheduleData() const
{
    const std::optional<TimedRecordingSchedule> s = schedule();
    if (!s) {
        return {};
    }

    const auto msSinceEpoch = [](TimedRecordingSchedule::TimePoint timePoint) {
        return static_cast<qint64>(std::chrono::duration_cast<std::chrono::milliseconds>(timePoint.time_since_epoch()).count());
    };

    QVariantMap data;
    if (s->start) {
        data["startMs"] = msSinceEpoch(*s->start);
    }
    if (const TimedRecordingSchedule::TimePoint* end = std::get_if<TimedRecordingSchedule::TimePoint>(&s->end)) {
        data["endMs"] = msSinceEpoch(*end);
    }
    data["durationSeconds"] = static_cast<qint64>(m_durationSeconds);

    if (m_saveProjectEnabled) {
        data["saveProjectPath"] = withProjectSuffix(m_saveProjectPath);
    }

    if (m_exportAudioEnabled) {
        data["exportAudioPath"] = withExportExtension(m_exportAudioPath);
        data["exportFormat"] = QString::fromStdString(exportFormat());
    }

    data["quitAudacity"] = quitAudacityEnabled();

    return data;
}

bool TimedRecordingSetupModel::confirmOverwrite()
{
    QStringList existing;

    if (m_saveProjectEnabled) {
        const QString path = withProjectSuffix(m_saveProjectPath);
        const project::IAudacityProjectPtr project = globalContext()->currentProject();
        const bool isCurrentProject = project && muse::io::path_t(path) == project->path();
        if (!isCurrentProject && fileSystem()->exists(path)) {
            existing << path;
        }
    }

    if (m_exportAudioEnabled) {
        const QString path = withExportExtension(m_exportAudioPath);
        if (fileSystem()->exists(path)) {
            existing << path;
        }
    }

    if (existing.isEmpty()) {
        return true;
    }

    const std::string question = existing.size() == 1
                                 ? muse::qtrc("record", "This file already exists:\n%1\n\nDo you want to overwrite it?")
                                 .arg(existing.front()).toStdString()
                                 : muse::qtrc("record", "These files already exist:\n%1\n\nDo you want to overwrite them?")
                                 .arg(existing.join('\n')).toStdString();

    const int overwriteBtn = int(muse::IInteractive::Button::CustomButton) + 1;
    const muse::IInteractive::ButtonDatas buttons {
        muse::IInteractive::ButtonData(overwriteBtn, muse::trc("record", "Overwrite")),
        interactive()->buttonData(muse::IInteractive::Button::Cancel)
    };

    const muse::IInteractive::Result result = interactive()->questionSync("", question, buttons);
    return result.button() == overwriteBtn;
}

void TimedRecordingSetupModel::browseSaveProjectPath()
{
    muse::io::path_t defaultPath = m_saveProjectPath;
    if (defaultPath.empty()) {
        defaultPath = projectConfiguration()->defaultSavingFilePath(globalContext()->currentProject());
    }

    const std::vector<std::string> filter { muse::trc("project", "Audacity 4 files") + " (*." + project::AUP4 + ")" };
    const muse::io::path_t path = interactive()->selectSavingFileSync(muse::trc("record", "Save project as"), defaultPath, filter);
    if (path.empty()) {
        return;
    }

    projectConfiguration()->setLastSavedProjectsPath(muse::io::dirpath(path));
    setSaveProjectPath(withProjectSuffix(path.toQString()));
}

bool TimedRecordingSetupModel::startNow() const
{
    return m_startNow;
}

void TimedRecordingSetupModel::setStartNow(bool startNow)
{
    set(m_startNow, startNow);
}

QString TimedRecordingSetupModel::startDate() const
{
    return m_startDate;
}

void TimedRecordingSetupModel::setStartDate(const QString& date)
{
    set(m_startDate, date);
}

QString TimedRecordingSetupModel::startTime() const
{
    return m_startTime;
}

void TimedRecordingSetupModel::setStartTime(const QString& time)
{
    set(m_startTime, time);
}

bool TimedRecordingSetupModel::endAfterDuration() const
{
    return m_endAfterDuration;
}

void TimedRecordingSetupModel::setEndAfterDuration(bool endAfterDuration)
{
    set(m_endAfterDuration, endAfterDuration);
}

double TimedRecordingSetupModel::durationSeconds() const
{
    return m_durationSeconds;
}

void TimedRecordingSetupModel::setDurationSeconds(double seconds)
{
    set(m_durationSeconds, seconds);
}

QString TimedRecordingSetupModel::endDate() const
{
    return m_endDate;
}

void TimedRecordingSetupModel::setEndDate(const QString& date)
{
    set(m_endDate, date);
}

QString TimedRecordingSetupModel::endTime() const
{
    return m_endTime;
}

void TimedRecordingSetupModel::setEndTime(const QString& time)
{
    set(m_endTime, time);
}

void TimedRecordingSetupModel::openExportSettings()
{
    muse::UriQuery uri(EXPORT_SETTINGS_URI);
    uri.addParam("settingsOnly", muse::Val(true));
    uri.addParam("filePath", muse::Val(withExportExtension(m_exportAudioPath).toStdString()));

    interactive()->open(uri).onResolve(this, [this](const muse::Val& value) {
        const QString path = value.toQString();
        if (!path.isEmpty()) {
            setExportAudioPath(path);
        }
    });
}

bool TimedRecordingSetupModel::saveProjectEnabled() const
{
    return m_saveProjectEnabled;
}

void TimedRecordingSetupModel::setSaveProjectEnabled(bool enabled)
{
    set(m_saveProjectEnabled, enabled);
}

QString TimedRecordingSetupModel::saveProjectPath() const
{
    return m_saveProjectPath;
}

void TimedRecordingSetupModel::setSaveProjectPath(const QString& path)
{
    set(m_saveProjectPath, path);
}

bool TimedRecordingSetupModel::exportAudioEnabled() const
{
    return m_exportAudioEnabled;
}

void TimedRecordingSetupModel::setExportAudioEnabled(bool enabled)
{
    set(m_exportAudioEnabled, enabled);
}

QString TimedRecordingSetupModel::exportAudioPath() const
{
    return m_exportAudioPath;
}

void TimedRecordingSetupModel::setExportAudioPath(const QString& path)
{
    set(m_exportAudioPath, path);
}

bool TimedRecordingSetupModel::quitAudacityEnabled() const
{
    return m_quitAudacityEnabled && (m_saveProjectEnabled || m_exportAudioEnabled);
}

void TimedRecordingSetupModel::setQuitAudacityEnabled(bool enabled)
{
    set(m_quitAudacityEnabled, enabled);
}

QString TimedRecordingSetupModel::dateFormat() const
{
    return m_dateFormat;
}

int TimedRecordingSetupModel::durationFormat() const
{
    return static_cast<int>(uicomponents::TimecodeFormatType::DDHHMMSS);
}

QString TimedRecordingSetupModel::summary() const
{
    return m_summary;
}

QString TimedRecordingSetupModel::scheduleError() const
{
    return m_scheduleError;
}

bool TimedRecordingSetupModel::startTimeInvalid() const
{
    return m_startTimeInvalid;
}

bool TimedRecordingSetupModel::endTimeInvalid() const
{
    return m_endTimeInvalid;
}

bool TimedRecordingSetupModel::canStart() const
{
    return m_canStart;
}

template<typename T>
void TimedRecordingSetupModel::set(T& member, const T& value)
{
    if (member == value) {
        return;
    }
    member = value;
    emit scheduleChanged();
}

std::optional<TimedRecordingSchedule> TimedRecordingSetupModel::schedule() const
{
    TimedRecordingSchedule s;
    s.end = TimedRecordingSchedule::Duration(static_cast<int64_t>(m_durationSeconds));

    if (!m_startNow) {
        const std::optional<QDateTime> start = parseDateTime(m_startDate, m_startTime);
        if (!start) {
            return std::nullopt;
        }
        s.start = TimedRecordingViewFormatter::toTimePoint(*start);
    }

    if (!m_endAfterDuration) {
        const std::optional<QDateTime> end = parseDateTime(m_endDate, m_endTime);
        if (!end) {
            return std::nullopt;
        }
        s.end = TimedRecordingViewFormatter::toTimePoint(*end);
    }

    return s;
}

std::optional<QDateTime> TimedRecordingSetupModel::parseDateTime(const QString& date, const QString& time) const
{
    const QDate d = QDate::fromString(date.trimmed(), m_dateFormat);
    if (!d.isValid()) {
        return std::nullopt;
    }

    QTime t = QTime::fromString(time.trimmed(), TIME_FORMAT);
    for (const QString& format : ALTERNATIVE_TIME_FORMATS) {
        if (t.isValid()) {
            break;
        }
        t = QTime::fromString(time.trimmed(), format);
    }
    if (!t.isValid()) {
        return std::nullopt;
    }

    return QDateTime(d, t);
}

std::string TimedRecordingSetupModel::exportFormat() const
{
    const std::string format = exportConfiguration()->currentFormat();
    if (!format.empty()) {
        return format;
    }

    const std::vector<std::string> formats = exporter()->formatsList();
    return formats.empty() ? std::string() : formats.front();
}

std::vector<std::string> TimedRecordingSetupModel::exportExtensions() const
{
    return exporter()->formatExtensions(exportFormat());
}

QString TimedRecordingSetupModel::withExportExtension(const QString& path) const
{
    const QString trimmed = path.trimmed();
    const std::vector<std::string> extensions = exportExtensions();
    if (trimmed.isEmpty() || extensions.empty()) {
        return trimmed;
    }

    const std::string suffix = muse::strings::toLower(muse::io::suffix(trimmed));
    if (std::find(extensions.begin(), extensions.end(), suffix) != extensions.end()) {
        return trimmed;
    }

    return trimmed + "." + QString::fromStdString(extensions.front());
}

void TimedRecordingSetupModel::update()
{
    const QDateTime now = currentDateTime();
    const std::optional<TimedRecordingSchedule> s = schedule();
    const TimedRecordingSchedule::TimePoint nowTimePoint = TimedRecordingViewFormatter::toTimePoint(now);

    bool startTimeInvalid = false;
    bool endTimeInvalid = false;
    QString scheduleError;
    if (s && s->start && *s->start <= nowTimePoint) {
        startTimeInvalid = true;
        scheduleError = TimedRecordingViewFormatter::startTimePassedError(now);
    }

    if (const TimedRecordingSchedule::TimePoint* end = s ? std::get_if<TimedRecordingSchedule::TimePoint>(&s->end) : nullptr) {
        if (*end <= nowTimePoint) {
            endTimeInvalid = true;
            if (scheduleError.isEmpty()) {
                scheduleError = TimedRecordingViewFormatter::endTimePassedError(now);
            }
        } else if (s->start && *end <= *s->start) {
            endTimeInvalid = true;
            if (scheduleError.isEmpty()) {
                scheduleError = TimedRecordingViewFormatter::endBeforeStartError();
            }
        }
    }

    const bool scheduleValid = s && scheduleError.isEmpty() && s->isValid(nowTimePoint);
    const bool completionActionsValid = (!m_saveProjectEnabled || !m_saveProjectPath.trimmed().isEmpty())
                                        && (!m_exportAudioEnabled || !m_exportAudioPath.trimmed().isEmpty());
    const bool canStart = scheduleValid && completionActionsValid;
    QString summary;
    if (scheduleValid) {
        const std::optional<QDateTime> start = s->start ? std::optional(TimedRecordingViewFormatter::toDateTime(*s->start)) : std::nullopt;
        summary = TimedRecordingViewFormatter::summary(now, start, s->recordingDuration(nowTimePoint).count(), m_endAfterDuration);
    }

    if (canStart == m_canStart && summary == m_summary && scheduleError == m_scheduleError
        && startTimeInvalid == m_startTimeInvalid && endTimeInvalid == m_endTimeInvalid) {
        return;
    }

    m_canStart = canStart;
    m_summary = summary;
    m_scheduleError = scheduleError;
    m_startTimeInvalid = startTimeInvalid;
    m_endTimeInvalid = endTimeInvalid;
    emit summaryChanged();
}
