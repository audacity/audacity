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

void CommandMessageTarget::StartArray()
{
   wxString Padding;
   Padding.Pad( mCounts.GetCount() *2 -2);
   Update( wxString::Format( "%s%s[ ", ( mCounts.Last() > 0 ) ? ",\n" : "\n", Padding ));
   mCounts.Last() += 1;
   mCounts.push_back( 0 );
}

void CommandMessageTarget::EndArray(){
   if( mCounts.GetCount() > 1 ){
      mCounts.pop_back();
   }
   Update( " ]" );
}
void CommandMessageTarget::StartStruct(){
   wxString Padding;
   Padding.Pad( mCounts.GetCount() *2 -2);
   Update( wxString::Format( "%s%s{ ", ( mCounts.Last() > 0 ) ? ",\n" : "\n", Padding ));
   mCounts.Last() += 1;
   mCounts.push_back( 0 );
}
void CommandMessageTarget::EndStruct(){
   if( mCounts.GetCount() > 1 ){
      mCounts.pop_back();
   }
   Update( " }" );
}
void CommandMessageTarget::AddItem(const wxString &value, const wxString &name){
   Update( wxString::Format( "%s%s%s\"%s\"", (mCounts.Last()>0)?", ":"", name, !name.IsEmpty()?":":"",value));
   mCounts.Last() += 1;
}
void CommandMessageTarget::AddItem(const bool value,      const wxString &name){
   Update( wxString::Format( "%s%s%s%s", (mCounts.Last()>0)?", ":"", name, !name.IsEmpty()?":":"",value?"True":"False"));
   mCounts.Last() += 1;
}
void CommandMessageTarget::AddItem(const double value,    const wxString &name){
   Update( wxString::Format( "%s%s%s%g", (mCounts.Last()>0)?", ":"", name, !name.IsEmpty()?":":"",value));
   mCounts.Last() += 1;
}

void CommandMessageTarget::AddField(const wxString &name){
   Update( wxString::Format( "%s%s%s", (mCounts.Last()>0)?", ":"", name, !name.IsEmpty()?":":""));
   mCounts.Last() = 0; // Lie so that we don't get a comma.
}

void CommandMessageTarget::Flush(){
}



void LispyCommandMessageTarget::StartArray()
{
   wxString Padding;
   Padding.Pad( mCounts.GetCount() *2 -2);
   Update( wxString::Format( "%s%s( ", ( mCounts.Last() > 0 ) ? ",\n" : "\n", Padding ));
   mCounts.Last() += 1;
   mCounts.push_back( 0 );
}

void LispyCommandMessageTarget::EndArray(){
   if( mCounts.GetCount() > 1 ){
      mCounts.pop_back();
   }
   Update( " )" );
}
void LispyCommandMessageTarget::StartStruct(){
   wxString Padding;
   Padding.Pad( mCounts.GetCount() *2 -2);
   Update( wxString::Format( "%s%s( ", ( mCounts.Last() > 0 ) ? ",\n" : "\n", Padding ));
   mCounts.Last() += 1;
   mCounts.push_back( 0 );
}
void LispyCommandMessageTarget::EndStruct(){
   if( mCounts.GetCount() > 1 ){
      mCounts.pop_back();
   }
   Update( " )" );
}
void LispyCommandMessageTarget::AddItem(const wxString &value, const wxString &name){
   Update( wxString::Format( "%s%s%s\"%s\"", (mCounts.Last()>0)?", ":"", name, !name.IsEmpty()?",":"",value));
   mCounts.Last() += 1;
}
void LispyCommandMessageTarget::AddItem(const bool value,      const wxString &name){
   Update( wxString::Format( "%s%s%s%s", (mCounts.Last()>0)?", ":"", name, !name.IsEmpty()?",":"",value?"True":"False"));
   mCounts.Last() += 1;
}
void LispyCommandMessageTarget::AddItem(const double value,    const wxString &name){
   Update( wxString::Format( "%s%s%s%g", (mCounts.Last()>0)?", ":"", name, !name.IsEmpty()?",":"",value));
   mCounts.Last() += 1;
}

void LispyCommandMessageTarget::AddField(const wxString &name){
   Update( wxString::Format( "%s%s%s", (mCounts.Last()>0)?", ":"", name, !name.IsEmpty()?",":""));
   mCounts.Last() = 0; // Lie so that we don't get a comma.
}







void DeformattedCommandMessageTarget::StartArray()
{
   wxString Padding;
   Padding.Pad( mCounts.GetCount() *2 -2);
   if( mCounts.GetCount() <= 3 )
      Update( wxString::Format( "%s%s ", ( mCounts.Last() > 0 ) ? " \n" : "", Padding ));
   mCounts.Last() += 1;
   mCounts.push_back( 0 );
}

void DeformattedCommandMessageTarget::EndArray(){
   if( mCounts.GetCount() > 1 ){
      mCounts.pop_back();
   }
   if( mCounts.GetCount() <= 3 )
     Update( " " );
}
void DeformattedCommandMessageTarget::StartStruct(){
   wxString Padding;
   Padding.Pad( mCounts.GetCount() *2 -2);
   if( mCounts.GetCount() <= 3 )
      Update( wxString::Format( "%s%s ", ( mCounts.Last() > 0 ) ? " \n" : "", Padding ));
   mCounts.Last() += 1;
   mCounts.push_back( 0 );
}
void DeformattedCommandMessageTarget::EndStruct(){
   if( mCounts.GetCount() > 1 ){
      mCounts.pop_back();
   }
   if( mCounts.GetCount() <= 3 )
      Update( " " );
}
void DeformattedCommandMessageTarget::AddItem(const wxString &value, const wxString &name){
   if( mCounts.GetCount() <= 3 )
      Update( wxString::Format( "%s\"%s\"", (mCounts.Last()>0)?" ":"",value));
   mCounts.Last() += 1;
}
void DeformattedCommandMessageTarget::AddItem(const bool value,      const wxString &name){
   if( mCounts.GetCount() <= 3 )
      Update( wxString::Format( "%s%s", (mCounts.Last()>0)?" ":"",value?"True":"False"));
   mCounts.Last() += 1;
}
void DeformattedCommandMessageTarget::AddItem(const double value,    const wxString &name){
   if( mCounts.GetCount() <= 3 )
      Update( wxString::Format( "%s%g", (mCounts.Last()>0)?" ":"", value));
   mCounts.Last() += 1;
}

void DeformattedCommandMessageTarget::AddField(const wxString &name){
   mCounts.Last() = 0; // Lie so that we don't get a comma.
}





LispifiedCommandOutputTarget::LispifiedCommandOutputTarget( CommandOutputTarget & target )
 : CommandOutputTarget() ,
   pToRestore( &target )
{
   mProgressTarget = std::move(target.mProgressTarget), 
   mStatusTarget = std::make_shared<LispyCommandMessageTarget>( *target.mStatusTarget.get() ), 
   mErrorTarget = std::move( target.mErrorTarget );
}

LispifiedCommandOutputTarget::~LispifiedCommandOutputTarget()
{
   pToRestore->mProgressTarget = std::move( mProgressTarget );
   //The status was never captured so does not need restoring.
   //pToRestore->mStatusTarget = std::move( mStatusTarget );
   pToRestore->mErrorTarget = std::move( mErrorTarget );
}

DeformattedCommandOutputTarget::DeformattedCommandOutputTarget( CommandOutputTarget & target )
 : CommandOutputTarget() ,
   pToRestore( &target )
{
   mProgressTarget = std::move(target.mProgressTarget), 
   mStatusTarget = std::make_shared<DeformattedCommandMessageTarget>( *target.mStatusTarget.get() ), 
   mErrorTarget = std::move( target.mErrorTarget );
}

DeformattedCommandOutputTarget::~DeformattedCommandOutputTarget()
{
   pToRestore->mProgressTarget = std::move( mProgressTarget );
   //The status was never captured so does not need restoring.
   //pToRestore->mStatusTarget = std::move( mStatusTarget );
   pToRestore->mErrorTarget = std::move( mErrorTarget );
}













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
   static void Flush();

   wxTextCtrl * mTextCtrl;
   wxString mText;
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
      mTextCtrl = S.AddTextWindow( "" );
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
   pDlg->mText = pDlg->mText + Text;
}

void LongMessageDialog::Flush()
{
   if( pDlg ){
      pDlg->mText += "\n\n";
      pDlg->mTextCtrl->SetValue( pDlg->mText );
      pDlg->mTextCtrl->ShowPosition( pDlg->mTextCtrl->GetLastPosition() );
   }
}






/// Displays messages from a command in an AudacityMessageBox
class MessageDialogTarget final : public CommandMessageTarget
{
public:
   virtual ~MessageDialogTarget() {Flush();}
   void Update(const wxString &message) override
   {
      LongMessageDialog::AcceptText(message);
   }
   void Flush() override
   {
      LongMessageDialog::Flush();
   }
};



/// Extended Target Factory with more options.
class ExtTargetFactory : public TargetFactory
{
public:
   static std::unique_ptr<CommandMessageTarget> LongMessages()
   {
      return std::make_unique<MessageDialogTarget>();
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
