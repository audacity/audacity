#include "CommonHeaders.h"

#include <cstdlib>
#include <sstream>

#include <wx/platinfo.h>
#include <wx/intl.h>

#include "lib-string-utils/CodeConversions.h"
#include "lib-network-manager/Request.h"
#include "lib-string-utils/UrlEncode.h"

namespace audacity
{
namespace telemetry
{
namespace details
{
namespace
{
void GetOSString (std::ostringstream& output, const wxPlatformInfo& platformInfo)
{
    const wxOperatingSystemId osID = platformInfo.GetOperatingSystemId();

    if (osID & wxOS_WINDOWS)
    {
        output <<
               "Windows NT " <<
               platformInfo.GetOSMajorVersion() <<
               "." <<
               platformInfo.GetOSMinorVersion() <<
               "; ";

        if (platformInfo.GetArchitecture() == wxARCH_64)
            output << "Win64; x64";
        else
            output << "Win32; x86";
    }
    else if (osID & wxOS_MAC)
    {
        output << "Macintosh; ";
#ifdef __arm64__
        output << "AppleSilicon ";
#else
        output << "Intel ";
#endif
        if (platformInfo.GetOSMajorVersion() == 10)
        {
            output <<
                   "Mac OS X 10_" <<
                   platformInfo.GetOSMinorVersion() <<
                   "_" <<
                   platformInfo.GetOSMicroVersion();
        }
        else
        {
            output <<
                   "macOS 11_" <<
                   platformInfo.GetOSMinorVersion();
        }
    }
    else
    {
        if (osID & wxOS_UNIX_LINUX)
            output << "Linux; ";
        else if (osID & wxOS_UNIX_FREEBSD)
            output << "FreeBSD; ";
        else if (osID & wxOS_UNIX_OPENBSD)
            output << "OpenBSD; ";
        else
            output << "Unix; ";

        if (platformInfo.GetArchitecture() == wxARCH_64)
            output << "x86_64";
        else
            output << "x86";
    }
}

std::string getSafeVersion (const std::string& version)
{
    const size_t dashPos = version.find("-");

    if (dashPos == std::string::npos)
        return version;

    const size_t secondDashPos = version.find("-", dashPos + 1);

    return version.substr(0, secondDashPos);
}
}

CommonHeaders::CommonHeaders (const std::string& appName, const std::string& appVersion)
{
    const wxPlatformInfo platformInfo = wxPlatformInfo::Get();

    std::ostringstream stream;

    stream <<
        appName << "/" <<
        getSafeVersion(appVersion) << " (";

    GetOSString(stream, platformInfo);

    stream << ") LibTelemetry/1.0 (wxWidgets/" <<
           wxMAJOR_VERSION << "." << wxMINOR_VERSION << "." << wxRELEASE_NUMBER <<
           ")";

    mUserAgent = stream.str();

    mAcceptLanguage = ToUTF8(wxLocale(wxLocale::GetSystemLanguage()).GetCanonicalName());

    mReferer =
            "https://www.audacityteam.org/?platform=" +
            UrlEncode(ToUTF8(platformInfo.GetOperatingSystemIdName())) +
            "&version=" + UrlEncode(appVersion);
}

void CommonHeaders::setupHeaders (network_manager::Request* request) const
{
    request->setHeader("User-Agent", mUserAgent);
    request->setHeader("Accept-Language", mAcceptLanguage);
    request->setHeader("Referer", mReferer);
    request->setHeader("Origin", "https://audacityteam.org");
}

}
}
}
