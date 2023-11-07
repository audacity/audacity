#include "CrashReportApp.h"

#include <sstream>
#include <memory>

#include <wx/cmdline.h>
#include <wx/chartype.h>
#include <wx/artprov.h>
#include <wx/filename.h>
#include <wx/stdpaths.h>
#include <wx/wrapsizer.h>
#include <wx/hyperlink.h>

#include "google_breakpad/processor/basic_source_line_resolver.h"
#include "google_breakpad/processor/minidump_processor.h"
#include "google_breakpad/processor/process_state.h"
#include "google_breakpad/processor/minidump.h"
#include "processor/stackwalk_common.h"

#include "warning.xpm"

#include <Internat.h>

#if defined(_WIN32)
#include <locale>
#include <codecvt>
#include "client/windows/sender/crash_report_sender.h"

namespace
{
    std::wstring ToPlatformString(const std::string& utf8)
    {
        return std::wstring_convert<std::codecvt_utf8<std::wstring::traits_type::char_type>, std::wstring::traits_type::char_type>().from_bytes(utf8);
    }

    wxString StringifyReportResult(google_breakpad::ReportResult result)
    {
       switch(result)
       {
       case google_breakpad::RESULT_FAILED:
           return "RESULT_FAILED";
       case google_breakpad::RESULT_REJECTED:
           return "RESULT_REJECTED";
       case google_breakpad::RESULT_SUCCEEDED:
           return "RESULT_SUCCEEDED";
       case google_breakpad::RESULT_THROTTLED:
           return "RESULT_THROTTLED";
       }
       return "unknown result";
    }

    bool SendMinidump(const std::string& url, const wxString& minidumpPath, const std::map<std::string, std::string>& arguments, const wxString& commentsFilePath, wxString& errorString)
    {
        std::map<std::wstring, std::wstring> files;
        files[L"upload_file_minidump"] = minidumpPath.wc_str();
        if (!commentsFilePath.empty())
        {
            files[wxFileName(commentsFilePath).GetFullName().wc_str()] = commentsFilePath.wc_str();
        }

        std::map<std::wstring, std::wstring> parameters;
        for (auto& p : arguments)
        {
            parameters[ToPlatformString(p.first)] = ToPlatformString(p.second);
        }

        google_breakpad::CrashReportSender sender(L"");

        auto result = sender.SendCrashReport(
            ToPlatformString(url),
            parameters,
            files,
            nullptr
        );

        if(result != google_breakpad::RESULT_SUCCEEDED)
            errorString = wxString::Format("SendMinidump filed with result: %s", StringifyReportResult(result));
        
        return result == google_breakpad::RESULT_SUCCEEDED;
    }
}
#else

#include "common/linux/http_upload.h"

namespace
{
    bool SendMinidump(const std::string& url, const wxString& minidumpPath, const std::map<std::string, std::string>& arguments, const wxString& commentsFilePath, wxString& error)
    {
        std::map<std::string, std::string> files;
        files["upload_file_minidump"] = minidumpPath.ToStdString();
        if (!commentsFilePath.empty())
        {
            files["comments.txt"] = commentsFilePath.ToStdString();
        }

        std::string response, error;
        bool success = google_breakpad::HTTPUpload::SendRequest(
            url,
            arguments,
            files,
            std::string(),
            std::string(),
            std::string(),
            &response,
            NULL,
            &error);

        if(!success)
            errorString = wxString::Format("SendMinidump failed with error: %s", error.c_str());
            
        return success;
    }
}

#endif

constexpr bool CrashReportAppHasUserComment = false;

IMPLEMENT_APP(CrashReportApp);
namespace
{
    std::map<std::string, std::string> parseArguments(const std::string& str)
    {
        int TOKEN_IDENTIFIER{ 0 };
        constexpr int TOKEN_EQ{ 1 };
        constexpr int TOKEN_COMMA{ 2 };
        constexpr int TOKEN_VALUE{ 3 };

        int i = 0;

        std::string key;
        int state = TOKEN_COMMA;
        std::map<std::string, std::string> result;
        while (true)
        {
            if (str[i] == 0)
                break;
            else if (isspace(str[i]))
                ++i;
            else if (isalpha(str[i]))
            {
                if (state != TOKEN_COMMA)
                    throw std::logic_error("malformed parameters string: unexpected identifier");

                int begin = i;
                while (isalnum(str[i]) || str[i] == '[' || str[i] == ']')
                    ++i;

                key = str.substr(begin, i - begin);
                state = TOKEN_IDENTIFIER;
            }
            else if (str[i] == '=')
            {
                if (state != TOKEN_IDENTIFIER)
                    throw std::logic_error("malformed parameters string: unexpected '=' symbol");
                ++i;
                state = TOKEN_EQ;
            }
            else if (str[i] == '\"')
            {
                if (state != TOKEN_EQ)
                    throw std::logic_error("malformed parameters string: unexpected '\"' symbol");

                int begin = ++i;
                while (true)
                {
                    if (str[i] == 0)
                        throw std::logic_error("unterminated string literal");
                    else if (str[i] == '\"')
                    {
                        if (i > begin)
                            result[key] = str.substr(begin, i - begin);
                        else
                            result[key] = std::string();
                        ++i;
                        state = TOKEN_VALUE;
                        break;
                    }
                    ++i;
                }
            }
            else if (str[i] == ',')
            {
                if (state != TOKEN_VALUE)
                    throw std::logic_error("malformed parameters string: unexpected ',' symbol");
                state = TOKEN_COMMA;
                ++i;
            }
            else
                throw std::logic_error("malformed parameters string");
        }
        if (state != TOKEN_VALUE)
            throw std::logic_error("malformed parameters string");

        return result;
    }

    void PrintMinidump(google_breakpad::Minidump& minidump)
    {
        google_breakpad::BasicSourceLineResolver resolver;
        google_breakpad::MinidumpProcessor minidumpProcessor(nullptr, &resolver);
        google_breakpad::MinidumpThreadList::set_max_threads(std::numeric_limits<uint32_t>::max());
        google_breakpad::MinidumpMemoryList::set_max_regions(std::numeric_limits<uint32_t>::max());
        
        google_breakpad::ProcessState processState;
        
        if (minidumpProcessor.Process(&minidump, &processState) != google_breakpad::PROCESS_OK)
        {
            printf("Failed to process minidump");
        }
        else
        {
            google_breakpad::PrintProcessState(processState, true, false, &resolver);
        }
    }

    wxString MakeDumpString(google_breakpad::Minidump& minidump, const wxString& temp)
    {
#if _WIN32
        auto stream = _wfreopen(temp.wc_str(), L"w+", stdout);
#else
        auto stream = freopen(temp.utf8_str().data(), "w+", stdout);
#endif
        if (stream == NULL)
            throw std::runtime_error("Failed to print minidump: cannot open temp file");
        PrintMinidump(minidump);
        fflush(stdout);

        auto length = ftell(stream);
        std::vector<char> bytes(length);
        fseek(stream, 0, SEEK_SET);
        fread(&bytes[0], 1, length, stream);
        fclose(stream);
        
#if _WIN32
        _wremove(temp.wc_str());
#else
        remove(temp.utf8_str().data());
#endif

        return wxString::From8BitData(&bytes[0], bytes.size());
    }

    wxString MakeHeaderString(google_breakpad::Minidump& minidump)
    {
        if (auto exception = minidump.GetException())
        {
            if (auto rawException = exception->exception())
            {
                // i18n-hint C++ programming assertion
                return wxString::Format(_("Exception code 0x%x"), rawException->exception_record.exception_code);
            }
            else
            {
                // i18n-hint C++ programming assertion
                return _("Unknown exception");
            }
        }
        else if (auto assertion = minidump.GetAssertion())
        {
            auto expression = assertion->expression();
            if (!expression.empty())
            {
                return expression;
            }
        }
        return _("Unknown error");
    }

    void DoShowCrashReportFrame(const wxString& header, const wxString& dump, const std::function<bool(const wxString& comment)>& onSend)
    {
        static constexpr int MaxUserCommentLength = 2000;

        auto dialog = new wxDialog(
            nullptr, 
            wxID_ANY, 
            _("Problem Report for Audacity"),
            wxDefaultPosition, 
            wxDefaultSize, 
            wxDEFAULT_FRAME_STYLE & ~(wxRESIZE_BORDER | wxMAXIMIZE_BOX)//disable frame resize
        );
        
        //fixes focus issue with Windows build-in screen reader, but breaks VoiceOver
#if defined(__WXMSW__)
        dialog->SetFocus();
#endif

        auto mainLayout = new wxBoxSizer(wxVERTICAL);

        auto headerLayout = new wxBoxSizer(wxHORIZONTAL);
        headerLayout->Add(new wxStaticBitmap(dialog, wxID_ANY, wxIcon(warning)));
        headerLayout->AddSpacer(5);

        auto headerText = new wxStaticText(dialog, wxID_ANY, header);
        headerText->SetFont(wxFont(wxFontInfo().Bold()));
        headerLayout->Add(headerText, wxSizerFlags().Align(wxALIGN_CENTER_VERTICAL));

        mainLayout->Add(headerLayout, wxSizerFlags().Border(wxALL));
        if (onSend != nullptr)
        {
            mainLayout->AddSpacer(5);
            mainLayout->Add(new wxStaticText(dialog, wxID_ANY, _("Click \"Send\" to submit the report to Audacity. This information is collected anonymously.")), wxSizerFlags().Border(wxALL));
        }
        mainLayout->AddSpacer(10);
        mainLayout->Add(new wxStaticText(dialog, wxID_ANY, _("Problem details")), wxSizerFlags().Border(wxALL));

        auto dumpTextCtrl = new wxTextCtrl(dialog, wxID_ANY, dump, wxDefaultPosition, wxSize(500, 300), wxTE_RICH | wxTE_READONLY | wxTE_MULTILINE | wxTE_DONTWRAP);
        dumpTextCtrl->SetFont(wxFont(wxFontInfo().Family(wxFONTFAMILY_TELETYPE)));
        dumpTextCtrl->ShowPosition(0);//scroll to top
        mainLayout->Add(dumpTextCtrl, wxSizerFlags().Border(wxALL).Expand());

        auto buttonsLayout = new wxBoxSizer(wxHORIZONTAL);
        
        wxTextCtrl* commentCtrl = nullptr;

        if (onSend != nullptr && CrashReportAppHasUserComment)
        {
            mainLayout->AddSpacer(10);
            mainLayout->Add(new wxStaticText(dialog, wxID_ANY, _("Comments")), wxSizerFlags().Border(wxALL));

            commentCtrl = new wxTextCtrl(dialog, wxID_ANY, wxEmptyString, wxDefaultPosition, wxSize(500, 100), wxTE_MULTILINE);
            commentCtrl->SetMaxLength(MaxUserCommentLength);

            mainLayout->Add(commentCtrl, wxSizerFlags().Border(wxALL).Expand());
        }

        if (onSend != nullptr)
        {
            /* i18n-hint: %s will be replaced with "our Privacy Policy" */
            const wxString translatedText = _("See %s for more info.");

            /* i18n-hint: Title of hyperlink to the privacy policy. This is an
               object of "See". */
            const wxString translatedLink = _("our Privacy Policy");

            const size_t placeholderPosition = translatedText.Find(wxT("%s"));

            if (placeholderPosition != wxString::npos)
            {
                auto privacyPolicyLayout = new wxWrapSizer();

                privacyPolicyLayout->Add(
                   new wxStaticText(dialog, wxID_ANY, translatedText.substr(0, placeholderPosition)),
                   wxSizerFlags().Proportion(0).Border(wxUP | wxDOWN));

                privacyPolicyLayout->Add(
                   new wxHyperlinkCtrl(
                      dialog, wxID_ANY, translatedLink,
                      "https://www.audacityteam.org/about/desktop-privacy-notice/"),
                   wxSizerFlags().Proportion(0).Border(wxUP | wxDOWN));

                if (placeholderPosition + 2 < translatedText.Length())
                {
                   privacyPolicyLayout->Add(
                      new wxStaticText(
                         dialog, wxID_ANY,
                         translatedText.substr(placeholderPosition + 2)),
                      wxSizerFlags().Proportion(1).Border(wxUP | wxDOWN));
                }

                mainLayout->Add(
                   privacyPolicyLayout, wxSizerFlags().Border(wxALL));
            }


            auto dontSendButton = new wxButton(dialog, wxID_ANY, XC("&Don't send", "crash reporter button").Translation());
            auto sendButton = new wxButton(dialog, wxID_ANY, XC("&Send", "crash reporter button").Translation());

            dontSendButton->Bind(wxEVT_BUTTON, [dialog](wxCommandEvent&)
                {
                    dialog->Close(true);
                });
            sendButton->Bind(wxEVT_BUTTON, [dialog, commentCtrl, onSend](wxCommandEvent&)
                {
                    const wxString comment =
                        commentCtrl != nullptr ? 
                            commentCtrl->GetValue() : 
                            wxString {};

                    if (onSend(comment))
                    {
                        dialog->Close(true);
                    }
                });

            buttonsLayout->Add(dontSendButton);
            buttonsLayout->AddSpacer(5);
            buttonsLayout->Add(sendButton);
        }
        else
        {
            auto okButton = new wxButton(dialog, wxID_OK, wxT("OK"));
            okButton->Bind(wxEVT_BUTTON, [dialog](wxCommandEvent&)
                {
                    dialog->Close(true);
                });
            buttonsLayout->Add(okButton);
        }

        mainLayout->Add(buttonsLayout, wxSizerFlags().Border(wxALL).Align(wxALIGN_RIGHT));
        dialog->SetSizerAndFit(mainLayout);

        dialog->Bind(wxEVT_CLOSE_WINDOW, [dialog](wxCloseEvent&) {
            dialog->Destroy();
        });
            
        dialog->Show(true);
    }
}


bool CrashReportApp::OnInit()
{
    if (!wxApp::OnInit())
        return false;
    
    if (mSilent)
    {
        if (!mURL.empty())
        {
            wxString error;
            auto result = SendMinidump(mURL, mMinidumpPath, mArguments, wxEmptyString, error);
            if(!result && mShowError)
                wxMessageBox(error);
        }
    }
    else
    {
        static std::unique_ptr<wxLocale> sLocale(new wxLocale(wxLANGUAGE_DEFAULT));
#if defined(__WXOSX__)
        sLocale->AddCatalogLookupPathPrefix(wxT("../Resources"));
#elif defined(__WXMSW__)
        sLocale->AddCatalogLookupPathPrefix(wxT("Languages"));
#elif defined(__WXGTK__)
        sLocale->AddCatalogLookupPathPrefix(wxT("./locale"));
        sLocale->AddCatalogLookupPathPrefix(wxString::Format(wxT("%s/share/locale"), wxT(INSTALL_PREFIX)));
#endif
        sLocale->AddCatalog("audacity");
        sLocale->AddCatalog("wxstd");
        
        google_breakpad::Minidump minidump(mMinidumpPath.ToStdString(), false);
        if (minidump.Read())
        {
            SetExitOnFrameDelete(true);

            wxFileName temp(mMinidumpPath);
            temp.SetExt("tmp");

            try
            {
                ShowCrashReport(MakeHeaderString(minidump), MakeDumpString(minidump, temp.GetFullPath()));
            }
            catch (std::exception& e)
            {
                wxMessageBox(e.what());
                return false;
            }
            return true;
        }
    }
    return false;
}

void CrashReportApp::OnInitCmdLine(wxCmdLineParser& parser)
{
    static const wxCmdLineEntryDesc cmdLineEntryDesc[] =
    {
         { wxCMD_LINE_SWITCH, "s", "silent", "Send without displaying the confirmation dialog" },
         {wxCMD_LINE_SWITCH, "e", "error_code", "Show error code/message on sending failure" },
         { wxCMD_LINE_OPTION, "u", "url", "Crash report server URL", wxCMD_LINE_VAL_STRING, wxCMD_LINE_PARAM_OPTIONAL },
         { wxCMD_LINE_OPTION, "a", "args", "A set of arguments to send", wxCMD_LINE_VAL_STRING, wxCMD_LINE_PARAM_OPTIONAL },
         { wxCMD_LINE_PARAM,  NULL, NULL, "path to minidump file", wxCMD_LINE_VAL_STRING, wxCMD_LINE_OPTION_MANDATORY },
         { wxCMD_LINE_NONE }
    };

    parser.SetDesc(cmdLineEntryDesc);
    
    wxApp::OnInitCmdLine(parser);
}

bool CrashReportApp::OnCmdLineParsed(wxCmdLineParser& parser)
{
    wxString url;
    wxString arguments;
    if (parser.Found("u", &url))
    {
        mURL = url.ToStdString();
    }
    if (parser.Found("a", &arguments))
    {
        try
        {
            mArguments = parseArguments(arguments.ToStdString());
        }
        catch (std::exception& e)
        {
            wxMessageBox(e.what());
            return false;
        }
    }
    mMinidumpPath = parser.GetParam(0);
    mSilent = parser.Found("s");
    mShowError = parser.Found("e");
    
    return wxApp::OnCmdLineParsed(parser);
}

void CrashReportApp::ShowCrashReport(const wxString& header, const wxString& text)
{
    if (mURL.empty())
    {
        DoShowCrashReportFrame(header, text, nullptr);
    }
    else
    {
        DoShowCrashReportFrame(header, text, [this](const wxString& comments)
            {
                wxString commentsFilePath;
                if (!comments.empty())
                {
                    wxFileName temp(mMinidumpPath);
                    temp.SetName(temp.GetName() + "-comments");
                    temp.SetExt("txt");
                    commentsFilePath = temp.GetFullPath();
                    wxFile file;
                    if (file.Open(commentsFilePath, wxFile::write))
                    {
                        file.Write(comments);
                        file.Close();
                    }
                }

                wxString error;
                auto result = SendMinidump(mURL, mMinidumpPath, mArguments, commentsFilePath, error);
                if (!commentsFilePath.empty())
                    wxRemoveFile(commentsFilePath);

                if (!result)
                {
                    if(mShowError)
                        wxMessageBox(error);
                    else
                        wxMessageBox(_("Failed to send crash report"));
                }
                return result;
            });
    }
}
