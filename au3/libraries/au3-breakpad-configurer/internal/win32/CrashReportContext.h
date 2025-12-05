/*!********************************************************************
*
 Audacity: A Digital Audio Editor

 CrashReportContext.h

 Vitaly Sverchinsky

 **********************************************************************/

#pragma once

#include <string>
#include <map>

//!This object is for internal usage.
/*! Simple POD type, holds user data required to start handler.
 * Fields are initialized with Set* methods,
 * which may return false if internal buffer isn't large enough
 * to store value passed as an argument.
 * After initialization call StartHandler providing path to the
 * database, where minidumps will be stored.
 */
class CrashReportContext final
{
    static constexpr size_t MaxBufferLength{ 2048 };
    static constexpr size_t MaxCommandLength{ 8192 };

    wchar_t mSenderPath[MaxBufferLength]{};
    wchar_t mReportURL[MaxBufferLength]{};
    wchar_t mParameters[MaxBufferLength]{};

    //this is a buffer where the command will be built at runtime
    wchar_t mCommand[MaxCommandLength]{};

public:
    bool SetSenderPathUTF8(const std::string& path);
    bool SetReportURL(const std::string& path);
    bool SetParameters(const std::map<std::string, std::string>& p);

    void StartHandler(const std::string& databasePath);

private:
    //helper functions which need access to a private data, but should not be exposed to a public class interface
    friend bool MakeCommand(CrashReportContext* ctx, const wchar_t* path, const wchar_t* id);
    friend bool SendReport(CrashReportContext* ctx, const wchar_t* path, const wchar_t* id);
};
