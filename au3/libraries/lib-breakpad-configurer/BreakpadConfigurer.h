/*!********************************************************************
*
 Audacity: A Digital Audio Editor

 BreakpadConfigurer.h

 Vitaly Sverchinsky

 **********************************************************************/

#pragma once

#include <string>
#include <map>

//! This class is used to configure Breakpad's handler before start.
/*! Typically handler should be started as early as possible.
* BreakpadConfigurer may be a short living object, it is used to configure
* Breakpad handler, and run it with BreakpadConfigurer::Start() method,
* It's expected that Start() will be called once during application
* lifetime, any calls to Set* methods after handler is started will be ignored.
* The handler itself simply starts crash sender program, passing all the details
* (path crash dump, report url, parameters...) as a command-line arguments to it.
* Please read official documentation for details:
* https://chromium.googlesource.com/breakpad/breakpad
*/
class CRASHREPORTS_API BreakpadConfigurer final
{
    std::string mDatabasePathUTF8;
    std::string mSenderPathUTF8;
    std::string mReportURL;
    std::map<std::string, std::string> mParameters;
public:
    //! Sets the directory where crashreports will be stored (should have rw permission)
    BreakpadConfigurer& SetDatabasePathUTF8(const std::string& pathUTF8);
    //! Sets report URL to the crash reporting server (URL-Encoded, optional)
    BreakpadConfigurer& SetReportURL(const std::string& reportURL);
    //! Sets an additional parameters that should be sent to a crash reporting server (ASCII encoded)
    BreakpadConfigurer& SetParameters(const std::map<std::string, std::string>& parameters);
    //! Sets a path to a directory where crash reporter sending program is located
    BreakpadConfigurer& SetSenderPathUTF8(const std::string& pathUTF8);

    //! Starts the handler
    void Start();
};
