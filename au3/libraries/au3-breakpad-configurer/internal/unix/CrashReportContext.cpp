/*!********************************************************************
*
 Audacity: A Digital Audio Editor

 CrashReportContext.cpp

 Vitaly Sverchinsky

 Some parts of the code are designed to operate while app is crashing,
 so there may be some restrictions on heap usage. For more information
 please read Breakpad documentation.

 **********************************************************************/
#include "CrashReportContext.h"

#include <errno.h>
#include <map>
#include <sstream>

#if defined(__APPLE__)
#include "client/mac/handler/exception_handler.h"
#else
#include "client/linux/handler/exception_handler.h"
#endif

bool SendReport(CrashReportContext* c, const char* minidumpPath)
{
    auto pid = fork();
    if (pid == 0) {
        if (c->mParameters[0] != 0) {
            execl(c->mSenderPath, CRASHREPORTER_PROGRAM_NAME, "-a", c->mParameters, "-u", c->mReportURL, minidumpPath, NULL);
        } else {
            execl(c->mSenderPath, CRASHREPORTER_PROGRAM_NAME, "-u", c->mReportURL, minidumpPath, NULL);
        }
        fprintf(stderr, "Failed to start handler: %s\n", strerror(errno));
        abort();
    }
    return pid != -1;
}

namespace {
//converts parameters map to a string, so that the Crash Reporting program
//would understand them
std::string StringifyParameters(const std::map<std::string, std::string>& parameters)
{
    std::stringstream stream;

    std::size_t parameterIndex = 0;
    std::size_t parametersCount = parameters.size();
    for (auto& pair : parameters) {
        stream << pair.first.c_str() << "=\"" << pair.second.c_str() << "\"";
        ++parameterIndex;
        if (parameterIndex < parametersCount) {
            stream << ",";
        }
    }
    return stream.str();
}

//copy contents of src to a raw char dest buffer,
//returns false if dest is not large enough
bool StrcpyChecked(char* dest, size_t destsz, const std::string& src)
{
    if (src.length() < destsz) {
        memcpy(dest, src.c_str(), src.length());
        dest[src.length()] = '\0';
        return true;
    }
    return false;
}

#if defined(__APPLE__)

static constexpr size_t MaxDumpPathLength{ 4096 };
static char DumpPath[MaxDumpPathLength];

bool DumpCallback(const char* dump_dir, const char* minidump_id, void* context, bool succeeded)
{
    if (succeeded) {
        const int PathDumpLength = strlen(dump_dir) + strlen("/") + strlen(minidump_id) + strlen(".dmp");
        if (PathDumpLength < MaxDumpPathLength) {
            strcpy(DumpPath, dump_dir);
            strcat(DumpPath, "/");
            strcat(DumpPath, minidump_id);
            strcat(DumpPath, ".dmp");
            auto crashReportContext = static_cast<CrashReportContext*>(context);
            return SendReport(crashReportContext, DumpPath);
        }
        return false;
    }
    return succeeded;
}

#else
bool DumpCallback(const google_breakpad::MinidumpDescriptor& descriptor, void* context, bool succeeded)
{
    if (succeeded) {
        auto crashReportContext = static_cast<CrashReportContext*>(context);
        return SendReport(crashReportContext, descriptor.path());
    }
    return succeeded;
}

#endif
}

bool CrashReportContext::SetSenderPathUTF8(const std::string& path)
{
    return StrcpyChecked(mSenderPath, MaxBufferLength, path + "/" + CRASHREPORTER_PROGRAM_NAME);
}

bool CrashReportContext::SetReportURL(const std::string& url)
{
    return StrcpyChecked(mReportURL, MaxBufferLength, url);
}

bool CrashReportContext::SetParameters(const std::map<std::string, std::string>& p)
{
    auto str = StringifyParameters(p);
    return StrcpyChecked(mParameters, MaxBufferLength, str);
}

void CrashReportContext::StartHandler(const std::string& databasePath)
{
    //intentinal leak: error hooks may be useful while application is terminating
    //CrashReportContext data should be alive too...
#if (__APPLE__)
    static auto handler = new google_breakpad::ExceptionHandler(
        databasePath,
        nullptr,
        DumpCallback,
        this,
        true,
        nullptr
        );
#else
    google_breakpad::MinidumpDescriptor descriptor(databasePath);
    static auto handler = new google_breakpad::ExceptionHandler(
        descriptor,
        nullptr,
        DumpCallback,
        this,
        true,
        -1
        );
#endif
}
