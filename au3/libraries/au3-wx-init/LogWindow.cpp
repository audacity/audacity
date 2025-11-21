/**********************************************************************

Audacity: A Digital Audio Editor

LogWindow.cpp

Paul Licameli split from AudacityLogger.cpp

**********************************************************************/
#include "LogWindow.h"

#include <optional>
#include <wx/filedlg.h>
#include <wx/frame.h>
#include <wx/icon.h>
#include <wx/settings.h>
#include <wx/textctrl.h>
#include <wx/weakref.h>

#include "AudacityLogger.h"
#include "AudacityMessageBox.h"
#include "FileNames.h"
#include "Internat.h"
#include "MemoryX.h"
#include "Prefs.h"
#include "SelectFile.h"
#include "ShuttleGui.h"

#include "AudacityLogoAlpha.xpm"

// If wxLogWindow is used and initialized before the Mac's "root" window, then
//     Audacity may crash when terminating.  It's not fully understood why this occurs
//     but it probably has to do with the order of deletion.  However, deferring the
//     creation of the log window until it is actually shown circumvents the problem.
enum
{
    LoggerID_Save = wxID_HIGHEST + 1,
    LoggerID_Clear,
    LoggerID_Close
};

namespace {
Destroy_ptr<wxFrame> sFrame;
wxWeakRef<wxTextCtrl> sText;

struct LogWindowUpdater : public PrefsListener
{
    // PrefsListener implementation
    void UpdatePrefs() override;
};
// Unique PrefsListener can't be statically constructed before the application
// object initializes, so use optional
std::optional<LogWindowUpdater> pUpdater;

void OnCloseWindow(wxCloseEvent& e);
void OnClose(wxCommandEvent& e);
void OnClear(wxCommandEvent& e);
void OnSave(wxCommandEvent& e);
}

void LogWindow::Show(bool show)
{
    // Hide the frame if created, otherwise do nothing
    if (!show) {
        if (sFrame) {
            sFrame->Show(false);
        }
        return;
    }

    // If the frame already exists, refresh its contents and show it
    auto pLogger = AudacityLogger::Get();
    if (sFrame) {
        if (!sFrame->IsShown() && sText) {
            if (pLogger) {
                sText->ChangeValue(pLogger->GetBuffer());
            }
            sText->SetInsertionPointEnd();
            sText->ShowPosition(sText->GetLastPosition());
        }
        sFrame->Show();
        sFrame->Raise();
        return;
    }

    // This is the first use, so create the frame
    Destroy_ptr<wxFrame> frame
    { safenew wxFrame(NULL, wxID_ANY, _("Audacity Log")) };
    frame->SetName(frame->GetTitle());
    frame->SetBackgroundColour(wxSystemSettings::GetColour(wxSYS_COLOUR_3DFACE));

    // loads either the XPM or the windows resource, depending on the platform
    {
#if !defined(__WXMAC__) && !defined(__WXX11__)
#if defined(__WXMSW__)
        wxIcon ic{ wxICON(AudacityLogo) };
#elif defined(__WXGTK__)
        wxIcon ic{ wxICON(AudacityLogoAlpha) };
#else
        wxIcon ic{};
        ic.CopyFromBitmap(theTheme.Bitmap(bmpAudacityLogo48x48));
#endif
        frame->SetIcon(ic);
#endif
    }

    // Log text
    ShuttleGui S(frame.get(), eIsCreating);

    S.Style(wxNO_BORDER | wxTAB_TRAVERSAL).Prop(true).StartPanel();
    {
        S.StartVerticalLay(true);
        {
            sText = S.Style(wxTE_MULTILINE | wxHSCROLL | wxTE_READONLY | wxTE_RICH)
                    .AddTextWindow({});

            // Populated TextWindow created above
            if (pLogger) {
                *sText << pLogger->GetBuffer();
            }

            S.AddSpace(0, 5);
            S.StartHorizontalLay(wxALIGN_CENTER, 0);
            {
                S.AddSpace(10, 0);
                S.Id(LoggerID_Save).AddButton(XXO("&Save..."));
                S.Id(LoggerID_Clear).AddButton(XXO("Cl&ear"));
                S.Id(LoggerID_Close).AddButton(XXO("&Close"));
                S.AddSpace(10, 0);
            }
            S.EndHorizontalLay();
            S.AddSpace(0, 3);
        }
        S.EndVerticalLay();
    }
    S.EndPanel();

    // Give a place for the menu help text to go
    // frame->CreateStatusBar();

    frame->Layout();

    // Hook into the frame events
    frame->Bind(wxEVT_CLOSE_WINDOW, OnCloseWindow);

    frame->Bind(wxEVT_COMMAND_MENU_SELECTED, OnSave, LoggerID_Save);
    frame->Bind(wxEVT_COMMAND_MENU_SELECTED, OnClear, LoggerID_Clear);
    frame->Bind(wxEVT_COMMAND_MENU_SELECTED, OnClose, LoggerID_Close);
    frame->Bind(wxEVT_COMMAND_BUTTON_CLICKED, OnSave, LoggerID_Save);
    frame->Bind(wxEVT_COMMAND_BUTTON_CLICKED, OnClear, LoggerID_Clear);
    frame->Bind(wxEVT_COMMAND_BUTTON_CLICKED, OnClose, LoggerID_Close);

    sFrame = std::move(frame);

    sFrame->Show();

    if (pLogger) {
        pLogger->Flush();
    }

    // Also create the listeners
    if (!pUpdater) {
        pUpdater.emplace();
    }

    if (pLogger) {
        pLogger->SetListener([]{
            if (auto pLogger = AudacityLogger::Get()) {
                if (sFrame && sFrame->IsShown()) {
                    if (sText) {
                        sText->ChangeValue(pLogger->GetBuffer());
                    }
                    return true;
                }
            }
            return false;
        });

        // Initial flush populates sText
        pLogger->Flush();
    }
}

void LogWindow::Destroy()
{
    sFrame.reset();
}

namespace {
void OnCloseWindow(wxCloseEvent& WXUNUSED(e))
{
#if defined(__WXMAC__)
    // On the Mac, destroy the window rather than hiding it since the
    // log menu will override the root windows menu if there is no
    // project window open.
    sFrame.reset();
#else
    sFrame->Show(false);
#endif
}

void OnClose(wxCommandEvent& WXUNUSED(e))
{
    wxCloseEvent dummy;
    OnCloseWindow(dummy);
}

void OnClear(wxCommandEvent& WXUNUSED(e))
{
    auto pLogger = AudacityLogger::Get();
    if (pLogger) {
        pLogger->ClearLog();
    }
}

void OnSave(wxCommandEvent& WXUNUSED(e))
{
    wxString fName = _("log.txt");

    fName = SelectFile(FileNames::Operation::Export,
                       XO("Save log to:"),
                       wxEmptyString,
                       fName,
                       wxT("txt"),
                       { FileNames::TextFiles },
                       wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER,
                       sFrame.get());

    if (fName.empty()) {
        return;
    }

    if (!(sText && sText->SaveFile(fName))) {
        AudacityMessageBox(
            XO("Couldn't save log to file: %s").Format(fName),
            XO("Warning"),
            wxICON_EXCLAMATION,
            sFrame.get());
        return;
    }
}

void LogWindowUpdater::UpdatePrefs()
{
    //! Re-create the non-modal window in case of change of preferred language
    if (sFrame) {
        bool shown = sFrame->IsShown();
        if (shown) {
            LogWindow::Show(false);
        }
        sFrame.reset();
        if (shown) {
            LogWindow::Show(true);
        }
    }
}
}
