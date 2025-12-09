/*!********************************************************************
*
 Audacity: A Digital Audio Editor

 BreakpadConfigurer.cpp

 Vitaly Sverchinsky

 **********************************************************************/

#include "BreakpadConfigurer.h"

#if defined(WIN32)
#include "internal/win32/CrashReportContext.h"
#else
#include "internal/unix/CrashReportContext.h"
#endif

BreakpadConfigurer& BreakpadConfigurer::SetDatabasePathUTF8(const std::string& pathUTF8)
{
    mDatabasePathUTF8 = pathUTF8;
    return *this;
}

BreakpadConfigurer& BreakpadConfigurer::SetReportURL(const std::string& reportURL)
{
    mReportURL = reportURL;
    return *this;
}

BreakpadConfigurer& BreakpadConfigurer::SetParameters(const std::map<std::string, std::string>& parameters)
{
    mParameters = parameters;
    return *this;
}

BreakpadConfigurer& BreakpadConfigurer::SetSenderPathUTF8(const std::string& pathUTF8)
{
    mSenderPathUTF8 = pathUTF8;
    return *this;
}

void BreakpadConfigurer::Start()
{
    static CrashReportContext context{};
    bool ok = context.SetSenderPathUTF8(mSenderPathUTF8);
    ok = ok && context.SetReportURL(mReportURL);
    ok = ok && context.SetParameters(mParameters);
    if (ok) {
        context.StartHandler(mDatabasePathUTF8);
    }
}
