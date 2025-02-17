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

#include <locale>
#include <codecvt>
#include <sstream>
#include "client/windows/handler/exception_handler.h"

namespace {
//copy src(null-terminated) to dst,
//returns false if dest is not large enough
bool StrcpyChecked(wchar_t* dest, size_t destsz, const wchar_t* src)
{
    auto len = wcslen(src);
    if (len < destsz) {
        memcpy(dest, src, sizeof(wchar_t) * len);
        dest[len] = 0;
        return true;
    }
    return false;
}

//appends src(null-terminated) to dest, destsz is the total dest buffer size, not remaining
//returns false if dest is not large enough
bool StrcatChecked(wchar_t* dest, size_t destsz, const wchar_t* src)
{
    auto srclen = wcslen(src);
    auto dstlen = wcslen(dest);
    if (srclen + dstlen < destsz) {
        memcpy(dest + dstlen, src, sizeof(wchar_t) * srclen);
        dest[srclen + dstlen] = 0;
        return true;
    }
    return false;
}

//converts parameters map to a string, so that the Crash Reporting program
//would understand them
std::string StringifyParameters(const std::map<std::string, std::string>& parameters)
{
    std::stringstream stream;

    std::size_t parameterIndex = 0;
    std::size_t parametersCount = parameters.size();
    for (auto& pair : parameters) {
        stream << pair.first.c_str() << "=\\\"" << pair.second.c_str() << "\\\"";
        ++parameterIndex;
        if (parameterIndex < parametersCount) {
            stream << ",";
        }
    }
    return stream.str();
}
}

bool MakeCommand(CrashReportContext* c, const wchar_t* path, const wchar_t* id)
{
    //utility path
    auto ok = StrcpyChecked(c->mCommand, CrashReportContext::MaxCommandLength, L"\"");
    ok = ok && StrcatChecked(c->mCommand, CrashReportContext::MaxCommandLength, c->mSenderPath);
    ok = ok && StrcatChecked(c->mCommand, CrashReportContext::MaxCommandLength, L"\"");

    //parameters: /p "..."
    if (ok && c->mParameters[0] != 0) {
        ok = ok && StrcatChecked(c->mCommand, CrashReportContext::MaxCommandLength, L" /a \"");
        ok = ok && StrcatChecked(c->mCommand, CrashReportContext::MaxCommandLength, c->mParameters);
        ok = ok && StrcatChecked(c->mCommand, CrashReportContext::MaxCommandLength, L"\"");
    }
    //crash report URL: /u https://...
    ok = ok && StrcatChecked(c->mCommand, CrashReportContext::MaxBufferLength, L" /u \"");
    ok = ok && StrcatChecked(c->mCommand, CrashReportContext::MaxBufferLength, c->mReportURL);
    ok = ok && StrcatChecked(c->mCommand, CrashReportContext::MaxBufferLength, L"\" ");
    //minidump path: path/to/minidump.dmp
    ok = ok && StrcatChecked(c->mCommand, CrashReportContext::MaxCommandLength, L" \"");
    ok = ok && StrcatChecked(c->mCommand, CrashReportContext::MaxCommandLength, path);
    ok = ok && StrcatChecked(c->mCommand, CrashReportContext::MaxCommandLength, L"\\");
    ok = ok && StrcatChecked(c->mCommand, CrashReportContext::MaxCommandLength, id);
    ok = ok && StrcatChecked(c->mCommand, CrashReportContext::MaxCommandLength, L".dmp\"");
    return ok;
}

bool SendReport(CrashReportContext* c, const wchar_t* path, const wchar_t* id)
{
    if (!MakeCommand(c, path, id)) {
        return false;
    }

    STARTUPINFOW si;
    ZeroMemory(&si, sizeof(si));
    si.cb = sizeof(si);
    si.dwFlags = STARTF_USESHOWWINDOW;
    si.wShowWindow = SW_SHOW;

    PROCESS_INFORMATION pi;
    ZeroMemory(&pi, sizeof(pi));

    if (CreateProcessW(NULL, c->mCommand, NULL, NULL, FALSE, 0, NULL, NULL, &si, &pi)) {
        CloseHandle(pi.hProcess);
        CloseHandle(pi.hThread);
        return true;
    } else {
        return false;
    }
}

bool UploadReport(
    const wchar_t* dump_path,
    const wchar_t* minidump_id,
    void* context,
    EXCEPTION_POINTERS* /*exinfo*/,
    MDRawAssertionInfo* /*assertion*/,
    bool succeeded)
{
    CrashReportContext* crashReportContext = static_cast<CrashReportContext*>(context);
    if (!SendReport(crashReportContext, dump_path, minidump_id)) {
        return false;
    }
    return succeeded;
}

bool CrashReportContext::SetSenderPathUTF8(const std::string& path)
{
    auto fullpath = path + "\\" + CRASHREPORTER_PROGRAM_NAME;
    return StrcpyChecked(mSenderPath, MaxBufferLength, std::wstring_convert<std::codecvt_utf8<wchar_t>, wchar_t>().from_bytes(
                             fullpath).c_str());
}

bool CrashReportContext::SetReportURL(const std::string& url)
{
    return StrcpyChecked(mReportURL, MaxBufferLength, std::wstring(url.begin(), url.end()).c_str());
}

bool CrashReportContext::SetParameters(const std::map<std::string, std::string>& p)
{
    auto str = StringifyParameters(p);
    return StrcpyChecked(mParameters, MaxBufferLength, std::wstring(str.begin(), str.end()).c_str());
}

void CrashReportContext::StartHandler(const std::string& databasePath)
{
    //intentinal leak: error hooks may be useful while application is terminating
    //CrashReportContext data should be alive too...
    static auto handler = new google_breakpad::ExceptionHandler(
        std::wstring_convert<std::codecvt_utf8<wchar_t>, wchar_t>().from_bytes(databasePath),
        NULL,
        UploadReport,
        this,
        google_breakpad::ExceptionHandler::HANDLER_ALL);
}
