/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan

******************************************************************//**

\file CommandTargets.cpp
\brief Contains definitions for CommandType class

\class InteractiveOutputTargets
\brief InteractiveOutputTargets is an output target that pops up a 
dialog, if necessary.

\class LongMessageDialog
\brief LongMessageDialog is a dialog with a Text Window in it to 
capture the more lengthy output from some commands.

*//*******************************************************************/


#include "CommandTargets.h"

#include <wx/app.h>
#include <wx/statusbr.h>
#include <wx/textctrl.h>
#include "ShuttleGui.h"
#include "../widgets/AudacityMessageBox.h"
#include "wxPanelWrapper.h"

#include <locale>
#include <sstream>

void CommandMessageTarget::StartArray()
{
   wxString Padding;
   Padding.Pad( mCounts.size() *2 -2);
   Update( wxString::Format( "%s%s[ ", ( mCounts.back() > 0 ) ? ",\n" : "\n", Padding ));
   mCounts.back() += 1;
   mCounts.push_back( 0 );
}

void CommandMessageTarget::EndArray(){
   if( mCounts.size() > 1 ){
      mCounts.pop_back();
   }
   Update( " ]" );
}
void CommandMessageTarget::StartStruct(){
   wxString Padding;
   Padding.Pad( mCounts.size() *2 -2);
   Update( wxString::Format( "%s%s{ ", ( mCounts.back() > 0 ) ? ",\n" : "\n", Padding ));
   mCounts.back() += 1;
   mCounts.push_back( 0 );
}
void CommandMessageTarget::EndStruct(){
   if( mCounts.size() > 1 ){
      mCounts.pop_back();
   }
   Update( " }" );
}
void CommandMessageTarget::AddItem(const wxString &value, const wxString &name){
   wxString Padding;
   Padding.Pad( mCounts.size() *2 -2);
   Padding = (( value.length() < 15 ) || (mCounts.back()<=0))  ? wxString{} : wxString("\n") + Padding;
   if( name.empty() )
      Update( wxString::Format( "%s%s\"%s\"", (mCounts.back()>0)?", ":"", Padding, Escaped(value)));
   else
      Update( wxString::Format( "%s%s\"%s\":\"%s\"", (mCounts.back()>0)?", ":"", Padding, name, Escaped(value)));
   mCounts.back() += 1;
}

void CommandMessageTarget::AddBool(const bool value,      const wxString &name){
   if( name.empty() )
      Update( wxString::Format( "%s\"%s\"", (mCounts.back()>0)?", ":"", value?"true":"false"));
   else
      Update( wxString::Format( "%s\"%s\":\"%s\"", (mCounts.back()>0)?", ":"", name,value?"true":"false"));
   mCounts.back() += 1;
}

void CommandMessageTarget::AddItem(const double value,    const wxString &name){
   std::stringstream str;
   std::locale nolocale("C");
   str.imbue(nolocale);

   if( name.empty() )
      str << ((mCounts.back()>0)? ", " : "") << value;
   else
      str << ((mCounts.back()>0)? ", " : "") << "\"" << name << "\"" << ":" << value;

   Update( str.str() );
   mCounts.back() += 1;
}

void CommandMessageTarget::StartField(const wxString &name){
   if( name.empty() )
      Update( wxString::Format( "%s", (mCounts.back()>0)? ", " : ""));
   else
      Update( wxString::Format( "%s\"%s\":", (mCounts.back()>0) ?", ":"", name));
   mCounts.back() += 1;
   mCounts.push_back( 0 );
}

void CommandMessageTarget::EndField(){
   if( mCounts.size() > 1 ){
      mCounts.pop_back();
   }
}

void CommandMessageTarget::Flush(){
}

wxString CommandMessageTarget::Escaped( const wxString & str){
   wxString Temp = str;
   Temp.Replace( "\"", "\\\"");
   return Temp;
}



void LispyCommandMessageTarget::StartArray()
{
   wxString Padding;
   Padding.Pad( mCounts.size() *2 -2);
   Update( wxString::Format( (mCounts.back()>0)?"\n%s(":"(", Padding ));
   mCounts.back() += 1;
   mCounts.push_back( 0 );
}

void LispyCommandMessageTarget::EndArray(){
   if( mCounts.size() > 1 ){
      mCounts.pop_back();
   }
   Update( ")" );
}
void LispyCommandMessageTarget::StartStruct(){
   wxString Padding;
   Padding.Pad( mCounts.size() *2 -2);
   Update( wxString::Format( (mCounts.back()>0)?"\n%s(":"(", Padding ));
   mCounts.back() += 1;
   mCounts.push_back( 0 );
}
void LispyCommandMessageTarget::EndStruct(){
   if( mCounts.size() > 1 ){
      mCounts.pop_back();
   }
   Update( ")" );
}
void LispyCommandMessageTarget::AddItem(const wxString &value, const wxString &name){
   wxString Padding;
   if( name.empty() )
      Update( wxString::Format( "%s%s\"%s\"", (mCounts.back()>0)?" ":"", Padding, Escaped(value)));
   else 
      Update( wxString::Format( "%s%s(%s \"%s\")", (mCounts.back()>0)?" ":"", Padding, name, Escaped(value)));
   mCounts.back() += 1;
}
void LispyCommandMessageTarget::AddBool(const bool value,      const wxString &name){
   if( name.empty() )
      Update( wxString::Format( "%s%s", (mCounts.back()>0)?" ":"",value?"True":"False"));
   else 
      Update( wxString::Format( "%s(%s %s)", (mCounts.back()>0)?" ":"", name,value?"True":"False"));
   mCounts.back() += 1;
}
void LispyCommandMessageTarget::AddItem(const double value,    const wxString &name){
   if( name.empty() )
      Update( wxString::Format( "%s%g", (mCounts.back()>0)?" ":"", value));
   else 
      Update( wxString::Format( "%s(%s %g)", (mCounts.back()>0)?" ":"", name,value));
   mCounts.back() += 1;
}

void LispyCommandMessageTarget::StartField(const wxString &name){
   Update( wxString::Format( "%s(%s", (mCounts.back()>0)?" ":"", name ));
   mCounts.back() += 1;
   mCounts.push_back( 0 );
}

void LispyCommandMessageTarget::EndField(){
   if( mCounts.size() > 1 ){
      mCounts.pop_back();
   }
   Update( ")" );
}






void BriefCommandMessageTarget::StartArray()
{
   wxString Padding;
   Padding.Pad( mCounts.size() *2 -2);
   if( mCounts.size() <= 3 )
      Update( wxString::Format( "%s%s ", ( mCounts.back() > 0 ) ? " \n" : "", Padding ));
   mCounts.back() += 1;
   mCounts.push_back( 0 );
}

void BriefCommandMessageTarget::EndArray(){
   if( mCounts.size() > 1 ){
      mCounts.pop_back();
   }
   if( mCounts.size() <= 3 )
     Update( " " );
}
void BriefCommandMessageTarget::StartStruct(){
   wxString Padding;
   Padding.Pad( mCounts.size() *2 -2);
   if( mCounts.size() <= 3 )
      Update( wxString::Format( "%s%s ", ( mCounts.back() > 0 ) ? " \n" : "", Padding ));
   mCounts.back() += 1;
   mCounts.push_back( 0 );
}
void BriefCommandMessageTarget::EndStruct(){
   if( mCounts.size() > 1 ){
      mCounts.pop_back();
   }
   if( mCounts.size() <= 3 )
      Update( " " );
}
void BriefCommandMessageTarget::AddItem(const wxString &value, const wxString &WXUNUSED(name)){
   if( mCounts.size() <= 3 )
      Update( wxString::Format( "%s\"%s\"", (mCounts.back()>0)?" ":"",Escaped(value)));
   mCounts.back() += 1;
}
void BriefCommandMessageTarget::AddBool(const bool value,      const wxString &WXUNUSED(name)){
   if( mCounts.size() <= 3 )
      Update( wxString::Format( "%s%s", (mCounts.back()>0)?" ":"",value?"True":"False"));
   mCounts.back() += 1;
}
void BriefCommandMessageTarget::AddItem(const double value,    const wxString &WXUNUSED(name)){
   if( mCounts.size() <= 3 )
      Update( wxString::Format( "%s%g", (mCounts.back()>0)?" ":"", value));
   mCounts.back() += 1;
}

void BriefCommandMessageTarget::StartField(const wxString &WXUNUSED(name)){
   mCounts.back() += 1;
   mCounts.push_back( 0 );
}

void BriefCommandMessageTarget::EndField(){
   if( mCounts.size() > 1 ){
      mCounts.pop_back();
   }
}


void MessageBoxTarget::Update(const wxString &message)
{
   // Should these messages be localized?
   AudacityMessageBox( Verbatim( message ) );
}


LispifiedCommandOutputTargets::LispifiedCommandOutputTargets( CommandOutputTargets & target )
 : CommandOutputTargets() ,
   pToRestore( &target )
{
   mProgressTarget = std::move(target.mProgressTarget), 
   mStatusTarget = std::make_shared<LispyCommandMessageTarget>( *target.mStatusTarget.get() ), 
   mErrorTarget = std::move( target.mErrorTarget );
}

LispifiedCommandOutputTargets::~LispifiedCommandOutputTargets()
{
   pToRestore->mProgressTarget = std::move( mProgressTarget );
   //The status was never captured so does not need restoring.
   //pToRestore->mStatusTarget = std::move( mStatusTarget );
   pToRestore->mErrorTarget = std::move( mErrorTarget );
}

BriefCommandOutputTargets::BriefCommandOutputTargets( CommandOutputTargets & target )
 : CommandOutputTargets() ,
   pToRestore( &target )
{
   mProgressTarget = std::move(target.mProgressTarget), 
   mStatusTarget = std::make_shared<BriefCommandMessageTarget>( *target.mStatusTarget.get() ), 
   mErrorTarget = std::move( target.mErrorTarget );
}

BriefCommandOutputTargets::~BriefCommandOutputTargets()
{
   pToRestore->mProgressTarget = std::move( mProgressTarget );
   //The status was never captured so does not need restoring.
   //pToRestore->mStatusTarget = std::move( mStatusTarget );
   pToRestore->mErrorTarget = std::move( mErrorTarget );
}











class AUDACITY_DLL_API LongMessageDialog /* not final */ : public wxDialogWrapper
{
public:
   // constructors and destructors
   LongMessageDialog(wxWindow * parent,
                const TranslatableString & title,
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
                           const TranslatableString & title,
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
   SetMinSize(wxSize(600,350));
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
      pDlg = new LongMessageDialog(
         wxTheApp->GetTopWindow(), XO( "Long Message" ) );
      pDlg->Init();
      pDlg->Show();
   }
   pDlg->mText = pDlg->mText + Text;
}

void LongMessageDialog::Flush()
{
   if( pDlg ){
      if( !pDlg->mText.EndsWith( "\n\n" ))
      {
         pDlg->mText += "\n\n";
         pDlg->mTextCtrl->SetValue( pDlg->mText );
         pDlg->mTextCtrl->ShowPosition( pDlg->mTextCtrl->GetLastPosition() );
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
   static std::shared_ptr<CommandMessageTarget> LongMessages()
   {
      return std::make_shared<MessageDialogTarget>();
   }
};



InteractiveOutputTargets::InteractiveOutputTargets() : 
   CommandOutputTargets( 
      ExtTargetFactory::ProgressDefault(), 
      ExtTargetFactory::LongMessages(), 
      ExtTargetFactory::MessageDefault()
   )
{
}

void StatusBarTarget::Update(const wxString &message)
{
   mStatus.SetStatusText(message, 0);
}
