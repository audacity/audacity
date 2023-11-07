/*!********************************************************************
*
 Audacity: A Digital Audio Editor

 CrashReportApp.h

 Vitaly Sverchinsky

 **********************************************************************/

#include <wx/wx.h>
#include <map>
#include <string>

//! Crash reporter GUI application
/*! Used to send crash reports to a remote server, or view them.
 * Shows brief report content, and allows user to send report to developers.
 * Reporting URL and other parameters are specified as a command line arguments.
 */
class CrashReportApp final : public wxApp
{
    std::string mURL;
    wxString mMinidumpPath;
    std::map<std::string, std::string> mArguments;

    bool mSilent{ false };
    bool mShowError { false };
public:
    bool OnInit() override;
    void OnInitCmdLine(wxCmdLineParser& parser) override;
    bool OnCmdLineParsed(wxCmdLineParser& parser) override;

private:
    void ShowCrashReport(const wxString& header, const wxString& text);
};

DECLARE_APP(CrashReportApp);
