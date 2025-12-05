#pragma once

#include <string>
#include <map>
#include <vector>

class CRASHPAD_CONFIGURER_API CrashpadConfigurer final
{
    std::string mHandlerPath;
    std::string mDatabasePath;
    std::string mMetricsDir;
    std::string mURL;
    std::vector<std::string> mArguments;
    std::map<std::string, std::string> mAnnotations;
    std::vector<std::string> mAttachments;
    bool mDatabaseUploadsEnabled { false };
public:
    CrashpadConfigurer& SetHandlerPathUTF8(const std::string& handlerPath);
    CrashpadConfigurer& SetDatabasePathUTF8(const std::string& database);
    CrashpadConfigurer& SetMetricsDirUTF8(const std::string& metricsDir);
    CrashpadConfigurer& SetURL(const std::string&);
    CrashpadConfigurer& SetArguments(const std::vector<std::string>& arguments);
    CrashpadConfigurer& SetAnnotations(const std::map<std::string, std::string>& annotations);
    CrashpadConfigurer& SetAttachmentsUTF8(const std::vector<std::string>& attachments);
    CrashpadConfigurer& SetDatabaseUploadEnabled(bool enabled);

    void Start();
};
