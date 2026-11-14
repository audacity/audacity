#include "CrashpadConfigurer.h"

#include <client/crashpad_client.h>
#include <client/crash_report_database.h>
#include <client/settings.h>

#include "internal/Util.h"

CrashpadConfigurer& CrashpadConfigurer::SetHandlerPathUTF8(const std::string& handlerPath)
{
    mHandlerPath = handlerPath;
    return *this;
}

CrashpadConfigurer& CrashpadConfigurer::SetDatabasePathUTF8(const std::string& database)
{
    mDatabasePath = database;
    return *this;
}

CrashpadConfigurer& CrashpadConfigurer::SetMetricsDirUTF8(const std::string& metricsDir)
{
    mMetricsDir = metricsDir;
    return *this;
}

CrashpadConfigurer& CrashpadConfigurer::SetURL(const std::string& url)
{
    mURL = url;
    return *this;
}

CrashpadConfigurer& CrashpadConfigurer::SetArguments(const std::vector<std::string>& arguments)
{
    mArguments = arguments;
    return *this;
}

CrashpadConfigurer& CrashpadConfigurer::SetAnnotations(const std::map<std::string, std::string>& annotations)
{
    mAnnotations = annotations;
    return *this;
}

CrashpadConfigurer& CrashpadConfigurer::SetAttachmentsUTF8(const std::vector<std::string>& attachments)
{
    mAttachments = attachments;
    return *this;
}

CrashpadConfigurer& CrashpadConfigurer::SetDatabaseUploadEnabled(bool enabled)
{
    mDatabaseUploadsEnabled = enabled;
    return *this;
}

void CrashpadConfigurer::Start()
{
    base::FilePath databasePath(makeFilePath<base::FilePath::StringType>(mDatabasePath));

    auto database = crashpad::CrashReportDatabase::Initialize(databasePath);
    if (database == nullptr) {
        throw std::runtime_error("Crash report database error");
    }
    auto dbSettings = database->GetSettings();
    if (dbSettings == nullptr) {
        throw std::runtime_error("Cannot access crash report database settings");
    }
    dbSettings->SetUploadsEnabled(mDatabaseUploadsEnabled);

    base::FilePath metricsDir(makeFilePath<base::FilePath::StringType>(mMetricsDir));
    base::FilePath handlerPath(makeFilePath<base::FilePath::StringType>(mHandlerPath));
    std::vector<base::FilePath> attachments;
    std::transform(mAttachments.begin(), mAttachments.end(), std::back_inserter(attachments), [](const std::string& path) {
        return base::FilePath(makeFilePath<base::FilePath::StringType>(path));
    });

    crashpad::CrashpadClient client;

    auto success = client.StartHandler(
        handlerPath,
        databasePath,
        metricsDir,
        mURL,
        mAnnotations,
        mArguments,
        true,
        false,
        attachments);

    if (!success) {
        throw std::runtime_error("Cannot start crashpad handler");
    }
}
