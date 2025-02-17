/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan

   Paul Licameli split from CommandTargets.cpp

******************************************************************//**

\file wxCommandTargets.cpp
\brief Contains definitions for CommandType class that use wxWidgets

\class InteractiveOutputTargets
\brief InteractiveOutputTargets is an output target that pops up a
dialog, if necessary.

\class LongMessageDialog
\brief LongMessageDialog is a dialog with a Text Window in it to
capture the more lengthy output from some commands.

*//*******************************************************************/
#include "wxCommandTargets.h"

#include <wx/app.h>
#include <wx/statusbr.h>
#include <wx/textctrl.h>
#include "ShuttleGui.h"
#include "CommandContext.h"
#include "BasicUI.h"
#include "wxPanelWrapper.h"

#include <locale>
#include <sstream>

#if 0
GUIProgressTarget::~GUIProgressTarget() = default;
#endif

class AUDACITY_DLL_API LongMessageDialog /* not final */ : public wxDialogWrapper
{
public:
    // constructors and destructors
    LongMessageDialog(wxWindow* parent, const TranslatableString& title, int type = 0, int flags = wxDEFAULT_DIALOG_STYLE,
                      int additionalButtons = 0);
    ~LongMessageDialog();

    bool Init();
    virtual void OnOk(wxCommandEvent& evt);
    virtual void OnCancel(wxCommandEvent& evt);

    static void AcceptText(const wxString& Text);
    static void Flush();

    wxTextCtrl* mTextCtrl;
    wxString mText;
    static LongMessageDialog* pDlg;
private:
    int mType;
    int mAdditionalButtons;

    DECLARE_EVENT_TABLE()
    wxDECLARE_NO_COPY_CLASS(LongMessageDialog);
};

LongMessageDialog* LongMessageDialog::pDlg = NULL;

BEGIN_EVENT_TABLE(LongMessageDialog, wxDialogWrapper)
EVT_BUTTON(wxID_OK, LongMessageDialog::OnOk)
END_EVENT_TABLE()

LongMessageDialog::LongMessageDialog(wxWindow* parent,
                                     const TranslatableString& title,
                                     int type,
                                     int flags,
                                     int additionalButtons)
    : wxDialogWrapper(parent, wxID_ANY, title, wxDefaultPosition, wxDefaultSize, flags | wxRESIZE_BORDER)
{
    mType = type;
    mAdditionalButtons = additionalButtons;
    SetName(XO("Long Message"));
    // The long message adds lots of short strings onto this one.
    // So preallocate to make it faster.
    // Needs 37Kb for all commands.
    mText.Alloc(40000);
}

LongMessageDialog::~LongMessageDialog()
{
    pDlg = NULL;
}

bool LongMessageDialog::Init()
{
    ShuttleGui S(this, eIsCreating);

    S.SetBorder(5);
    S.StartVerticalLay(true);
    {
        mTextCtrl = S.AddTextWindow("");
        long buttons = eOkButton;
        S.AddStandardButtons(buttons | mAdditionalButtons);
    }
    S.EndVerticalLay();

    Layout();
    Fit();
    SetMinSize(wxSize(600, 350));
    Center();
    return true;
}

void LongMessageDialog::OnOk(wxCommandEvent& WXUNUSED(evt))
{
    //Close(true);
    Destroy();
}

void LongMessageDialog::OnCancel(wxCommandEvent& WXUNUSED(evt))
{
    //Close(true);
    Destroy();
}

void LongMessageDialog::AcceptText(const wxString& Text)
{
    if (pDlg == NULL) {
        pDlg = new LongMessageDialog(
            wxTheApp->GetTopWindow(), XO("Long Message"));
        pDlg->Init();
        pDlg->Show();
    }
    pDlg->mText = pDlg->mText + Text;
}

void LongMessageDialog::Flush()
{
    if (pDlg) {
        if (!pDlg->mText.EndsWith("\n\n")) {
            pDlg->mText += "\n\n";
            pDlg->mTextCtrl->SetValue(pDlg->mText);
            pDlg->mTextCtrl->ShowPosition(pDlg->mTextCtrl->GetLastPosition());
        }
    }
}

/**
CommandMessageTarget that displays messages from a command in the
LongMessageDialog
*/
class MessageDialogTarget final : public CommandMessageTarget
{
public:
    virtual ~MessageDialogTarget() { Flush(); }
    void Update(const wxString& message) override
    {
        LongMessageDialog::AcceptText(message);
    }

    void Flush() override
    {
        LongMessageDialog::Flush();
    }
};

/// Extended Target Factory with more options.
class ExtTargetFactory final : public TargetFactory
{
public:
    static std::shared_ptr<CommandMessageTarget> LongMessages()
    {
        return std::make_shared<MessageDialogTarget>();
    }
};

namespace {
struct InteractiveOutputTargets : CommandOutputTargets {
    InteractiveOutputTargets()
        : CommandOutputTargets{
                               ExtTargetFactory::ProgressDefault(),
                               ExtTargetFactory::LongMessages(),
                               ExtTargetFactory::MessageDefault()
                               } {}
};

// Inject a substitute factory
static CommandContext::TargetFactory::SubstituteInUnique<
    InteractiveOutputTargets
    > scope;
}

StatusBarTarget::~StatusBarTarget() = default;

void StatusBarTarget::Update(const wxString& message)
{
    mStatus.SetStatusText(message, 0);
}
