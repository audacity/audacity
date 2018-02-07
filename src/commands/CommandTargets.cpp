/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan

******************************************************************//**

\file CommandTargets.cpp
\brief Contains definitions for CommandType class

\class InteractiveOutputTarget
\brief InteractiveOutputTarget is an output target that pops up a 
dialog, if necessary.

*//*******************************************************************/

#include "../Audacity.h"
#include "CommandTargets.h"
#include <wx/string.h>
#include "../ShuttleGui.h"
#include "../Project.h"



/// Dialog for long messages.
class AUDACITY_DLL_API LongMessageDialog /* not final */ : public wxDialogWrapper
{
public:
   // constructors and destructors
   LongMessageDialog(wxWindow * parent,
                const wxString & title,
                int type = 0,
                int flags = wxDEFAULT_DIALOG_STYLE,
                int additionalButtons = 0);
   ~LongMessageDialog();

   bool Init();
   virtual void OnOk(wxCommandEvent & evt);
   virtual void OnCancel(wxCommandEvent & evt);

   static void AcceptText( const wxString & Text );

   wxTextCtrl * mText;
   static LongMessageDialog * pDlg;
private:
   int mType;
   int mAdditionalButtons;

   DECLARE_EVENT_TABLE()
   wxDECLARE_NO_COPY_CLASS(LongMessageDialog);
};


LongMessageDialog * LongMessageDialog::pDlg = NULL;


BEGIN_EVENT_TABLE(LongMessageDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, LongMessageDialog::OnOk)
END_EVENT_TABLE()

LongMessageDialog::LongMessageDialog(wxWindow * parent,
                           const wxString & title,
                           int type,
                           int flags,
                           int additionalButtons)
: wxDialogWrapper(parent, wxID_ANY, title, wxDefaultPosition, wxDefaultSize, flags | wxRESIZE_BORDER)
{
   mType = type;
   mAdditionalButtons = additionalButtons;
}

LongMessageDialog::~LongMessageDialog(){
   pDlg = NULL;
}


bool LongMessageDialog::Init()
{
   ShuttleGui S(this, eIsCreating);

   S.SetBorder(5);
   S.StartVerticalLay(true);
   {
      mText = S.AddTextWindow( "" );
      long buttons = eOkButton;
      S.AddStandardButtons(buttons|mAdditionalButtons);
   }
   S.EndVerticalLay();

   Layout();
   Fit();
   SetMinSize(wxSize(600,700));
   Center();
   return true;
}

void LongMessageDialog::OnOk(wxCommandEvent & WXUNUSED(evt)){
   //Close(true);
   Destroy();
}

void LongMessageDialog::OnCancel(wxCommandEvent & WXUNUSED(evt)){
   //Close(true);
   Destroy();
}

void LongMessageDialog::AcceptText( const wxString & Text )
{
   if( pDlg == NULL ){
      pDlg = new LongMessageDialog( GetActiveProject(), "Long Message" );
      pDlg->Init();
      pDlg->Show();
   }
   pDlg->mText->SetValue( pDlg->mText->GetValue( ) + "\n" + Text );
}






/// Displays messages from a command in an AudacityMessageBox
class MessageDialogTarget final : public CommandMessageTarget
{
public:
   virtual ~MessageDialogTarget() {}
   void Update(const wxString &message) override
   {
      LongMessageDialog::AcceptText(message);
   }
};



/// Extended Target Factory with more options.
class ExtTargetFactory : public TargetFactory
{
public:
   static std::shared_ptr<CommandMessageTarget> LongMessages()
   {
      return std::make_shared<MessageDialogTarget>();
   }

};



InteractiveOutputTarget::InteractiveOutputTarget() : 
   CommandOutputTarget( 
      ExtTargetFactory::ProgressDefault(), 
      ExtTargetFactory::LongMessages(), 
      ExtTargetFactory::MessageDefault()
   )
{
}
