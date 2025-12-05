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
class CrashReportContext
{
    static constexpr size_t MaxBufferLength{ 2048 };

    char mSenderPath[MaxBufferLength]{};
    char mReportURL[MaxBufferLength]{};
    char mParameters[MaxBufferLength]{};

public:

    bool SetSenderPathUTF8(const std::string& path);
    bool SetReportURL(const std::string& url);
    bool SetParameters(const std::map<std::string, std::string>& p);

    void StartHandler(const std::string& databasePath);

private:
    //helper function which need access to a private data, but should not be exposed to a public class interface
    friend bool SendReport(CrashReportContext* ctx, const char* minidumpPath);
};
