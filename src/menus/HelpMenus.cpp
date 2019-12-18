#include "../Audacity.h"
#include "../Experimental.h"

#include <wx/bmpbuttn.h>
#include <wx/textctrl.h>
#include <wx/frame.h>

#include "../AboutDialog.h"
#include "../AllThemeResources.h"
#include "../AudacityLogger.h"
#include "../AudioIOBase.h"
#include "../CommonCommandFlags.h"
#include "../CrashReport.h"
#include "../Dependencies.h"
#include "../FileNames.h"
#include "../HelpText.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../ProjectSelectionManager.h"
#include "../ShuttleGui.h"
#include "../SplashDialog.h"
#include "../Theme.h"
#include "../commands/CommandContext.h"
#include "../commands/CommandManager.h"
#include "../prefs/PrefsDialog.h"
#include "../widgets/AudacityMessageBox.h"
#include "../widgets/HelpSystem.h"

#if defined(EXPERIMENTAL_CRASH_REPORT)
#include <wx/debugrpt.h>
#endif

// private helper classes and functions
namespace {

void ShowDiagnostics(
   AudacityProject &project, const wxString &info,
   const TranslatableString &description, const wxString &defaultPath)
{
   auto &window = GetProjectFrame( project );
   wxDialogWrapper dlg( &window, wxID_ANY, description);
   dlg.SetName();
   ShuttleGui S(&dlg, eIsCreating);

   wxTextCtrl *text;
   S.StartVerticalLay();
   {
      text = S.Id(wxID_STATIC)
         .Style(wxTE_MULTILINE | wxTE_READONLY)
         .AddTextWindow(info);
      S.AddStandardButtons(eOkButton | eCancelButton);
   }
   S.EndVerticalLay();

   dlg.FindWindowById(wxID_OK)->SetLabel(_("&Save"));
   dlg.SetSize(350, 450);

   if (dlg.ShowModal() == wxID_OK)
   {
      const auto fileDialogTitle = XO("Save %s").Format( description );
      wxString fName = FileNames::SelectFile(FileNames::Operation::Export,
         fileDialogTitle,
         wxEmptyString,
         defaultPath,
         wxT("txt"),
         wxT("*.txt"),
         wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER,
         &window);
      if (!fName.empty())
      {
         if (!text->SaveFile(fName))
         {
            AudacityMessageBox(
               XO("Unable to save %s").Format( description ).Translation(),
               fileDialogTitle.Translation());
         }
      }
   }
}

/** @brief Class which makes a dialog for displaying quick fixes to common issues.
 *
 * This class originated with the 'Stuck in a mode' problem, where far too many
 * users get into a mode without realising, and don't know how to get out.
 * It is a band-aid, and we should do more towards a full and proper solution
 * where there are fewer special modes, and they don't persisit.
 */
class QuickFixDialog : public wxDialogWrapper
{
public: 
   QuickFixDialog(wxWindow * pParent);
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
   void AddStuck( ShuttleGui & S, bool & bBool, wxString Pref,  wxString Prompt, wxString Help );

   void OnOk(wxCommandEvent &event);
   void OnCancel(wxCommandEvent &event);
   void OnHelp(wxCommandEvent &event);
   void OnFix(wxCommandEvent &event);

   wxString StringFromEvent( wxCommandEvent &event );

   int mItem;
   bool mbSyncLocked;
   bool mbInSnapTo;
   bool mbSoundActivated;
   DECLARE_EVENT_TABLE()
};

#define FixButtonID           7001
#define HelpButtonID          7011
#define FakeButtonID          7021

BEGIN_EVENT_TABLE(QuickFixDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK,                                            QuickFixDialog::OnOk)
   EVT_BUTTON(wxID_CANCEL,                                        QuickFixDialog::OnCancel)
   EVT_BUTTON(wxID_HELP,                                          QuickFixDialog::OnHelp)
   EVT_COMMAND_RANGE(FixButtonID,  HelpButtonID-1, wxEVT_BUTTON,  QuickFixDialog::OnFix)
   EVT_COMMAND_RANGE(HelpButtonID, FakeButtonID-1, wxEVT_BUTTON,  QuickFixDialog::OnHelp)
END_EVENT_TABLE();

QuickFixDialog::QuickFixDialog(wxWindow * pParent) :
      wxDialogWrapper(pParent, wxID_ANY, XO("Do you have these problems?"),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE )
{
   const long SNAP_OFF = 0;

   gPrefs->Read(wxT("/GUI/SyncLockTracks"), &mbSyncLocked, false);
   mbInSnapTo = gPrefs->Read(wxT("/SnapTo"), SNAP_OFF) !=0;
   gPrefs->Read(wxT("/AudioIO/SoundActivatedRecord"), &mbSoundActivated, false);

   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);

   Fit();
   auto sz = GetSize();
   SetMinSize( sz );
   SetMaxSize( sz );

   // The close button has the cancel id and acts exactly the same as cancel.
   wxButton * pWin = (wxButton*)FindWindowById( wxID_CANCEL );
   if( pWin )
      pWin->SetFocus( );
   Center();
}

void QuickFixDialog::AddStuck( ShuttleGui & S, bool & bBool, wxString Pref, wxString Prompt, wxString Help )
{
   mItem++;
   if( !bBool)
      return;
   S.AddFixedText( Prompt );
   S.Id(FixButtonID + mItem).AddButton( _("Fix") )->SetClientObject(
      safenew wxStringClientData(Pref));

   {
     // Replace standard Help button with smaller icon button.
      // bs->AddButton(safenew wxButton(parent, wxID_HELP));
      auto b = safenew wxBitmapButton(S.GetParent(), HelpButtonID+mItem, theTheme.Bitmap( bmpHelpIcon ));
      b->SetToolTip( _("Help") );
      b->SetLabel(_("Help"));       // for screen readers
      b->SetClientObject( safenew wxStringClientData( Help ));
      S.AddWindow( b );
   }
}

void QuickFixDialog::PopulateOrExchange(ShuttleGui & S)
{

   S.StartVerticalLay(1);
   S.StartStatic( _("Quick Fixes"));

   // These aren't all possible modes one can be stuck in, but they are some of them.
   bool bStuckInMode = mbSyncLocked || mbInSnapTo || mbSoundActivated;

   if( !bStuckInMode ){
      SetLabel(XO("Nothing to do"));
      S.AddFixedText(_("No quick, easily fixed problems were found"));
   }
   else {
      S.StartMultiColumn(3, wxALIGN_CENTER);
      {
         mItem = -1;

         // Use # in the URLs to ensure we go to the online version of help.
         // Local help may well not be installed.
         AddStuck( S, mbSyncLocked, "/GUI/SyncLockTracks", _("Clocks on the Tracks"), "Quick_Fix#sync_lock" );
         AddStuck( S, mbInSnapTo, "/SnapTo", _("Can't select precisely"), "Quick_Fix#snap_to" );
         AddStuck( S, mbSoundActivated, "/AudioIO/SoundActivatedRecord", _("Recording stops and starts"), "Quick_Fix#sound_activated_recording" );
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   S.StartHorizontalLay(wxALIGN_CENTER_HORIZONTAL, 0);
      S.AddStandardButtons(eCloseButton + (bStuckInMode ? 0 : eHelpButton));
   S.EndHorizontalLay();

   S.EndVerticalLay();

   wxButton * pBtn = (wxButton*)FindWindowById( wxID_HELP );
   if( pBtn )
      pBtn->SetClientObject( safenew wxStringClientData( "Quick_Fix#" ));

}

void QuickFixDialog::OnOk(wxCommandEvent &event)
{
   (void)event;// Compiler food
   EndModal(wxID_OK);
}

void QuickFixDialog::OnCancel(wxCommandEvent &event)
{
   (void)event;// Compiler food
   EndModal(wxID_CANCEL);
}

wxString QuickFixDialog::StringFromEvent( wxCommandEvent &event )
{
   wxButton * pBtn = (wxButton*)event.GetEventObject();
   if( !pBtn ){
      wxFAIL_MSG( "Event Object not found");
      return "";
   }
   wxStringClientData * pStrCd = (wxStringClientData*)(pBtn->GetClientObject());
   if( !pStrCd ){
      wxFAIL_MSG( "Client Data not found");
      return "";
   }
   wxString Str = pStrCd->GetData();
   if( Str.empty()){
      wxFAIL_MSG( "String data empty");
      return "";
   }
   return Str;
}

void QuickFixDialog::OnHelp(wxCommandEvent &event)
{
   HelpSystem::ShowHelp(this, StringFromEvent( event ), true);
}

void QuickFixDialog::OnFix(wxCommandEvent &event)
{
   wxString Str = StringFromEvent( event );
   gPrefs->Write( Str, 0);
   gPrefs->Flush();

   if ( auto pProject = GetActiveProject() ) {
      // Sadly SnapTo has to be handled specially, as it is not part of the standard
      // preference dialogs.
      if( Str == "/SnapTo" )
      {
         ProjectSelectionManager::Get( *pProject ).AS_SetSnapTo( 0 );
      }
      else
      {
         // This is overkill (aka slow), as all preferences are reloaded and all 
         // toolbars recreated.
         // Overkill probably doesn't matter, as this command is infrequently used.
         DoReloadPreferences( *pProject );
      }
   }
   
   // Change the label after doing the fix, as the fix may take a second or two.
   wxButton * pBtn = (wxButton*)event.GetEventObject();
   if( pBtn )
      pBtn->SetLabel( _("Fixed") );

   // The close button has the cancel id and acts exactly the same as cancel.
   wxButton * pWin = (wxButton*)FindWindowById( wxID_CANCEL );
   if( pWin )
      pWin->SetFocus( );
}

}

namespace HelpActions {

// exported helper functions

// Menu handler functions

struct Handler : CommandHandlerObject {

void OnQuickFix(const CommandContext &context)
{
   auto &project = context.project;
   QuickFixDialog dlg( &GetProjectFrame( project ) );
   dlg.ShowModal();
}

void OnQuickHelp(const CommandContext &context)
{
   auto &project = context.project;
   HelpSystem::ShowHelp(
      &GetProjectFrame( project ),
      wxT("Quick_Help"));
}

void OnManual(const CommandContext &context)
{
   auto &project = context.project;
   HelpSystem::ShowHelp(
      &GetProjectFrame( project ),
      wxT("Main_Page"));
}

void OnAudioDeviceInfo(const CommandContext &context)
{
   auto &project = context.project;
   auto gAudioIO = AudioIOBase::Get();
   wxString info = gAudioIO->GetDeviceInfo();
   ShowDiagnostics( project, info,
      XO("Audio Device Info"), wxT("deviceinfo.txt") );
}

#ifdef EXPERIMENTAL_MIDI_OUT
void OnMidiDeviceInfo(const CommandContext &context)
{
   auto &project = context.project;
   auto gAudioIO = AudioIOBase::Get();
   wxString info = gAudioIO->GetMidiDeviceInfo();
   ShowDiagnostics( project, info,
      XO("MIDI Device Info"), wxT("midideviceinfo.txt") );
}
#endif

void OnShowLog( const CommandContext &context )
{
   auto logger = AudacityLogger::Get();
   if (logger) {
      logger->Show();
   }
}

#if defined(EXPERIMENTAL_CRASH_REPORT)
void OnCrashReport(const CommandContext &WXUNUSED(context) )
{
// Change to "1" to test a real crash
#if 0
   char *p = 0;
   *p = 1234;
#endif
   CrashReport::Generate(wxDebugReport::Context_Current);
}
#endif

void OnCheckDependencies(const CommandContext &context)
{
   auto &project = context.project;
   ::ShowDependencyDialogIfNeeded(&project, false);
}

void OnCheckForUpdates(const CommandContext &WXUNUSED(context))
{
   ::OpenInDefaultBrowser( VerCheckUrl());
}

void OnAbout(const CommandContext &context)
{
#ifdef __WXMAC__
   // Modeless dialog, consistent with other Mac applications
   // Simulate the application Exit menu item
   wxCommandEvent evt{ wxEVT_MENU, wxID_ABOUT };
   wxTheApp->AddPendingEvent( evt );
#else
   auto &project = context.project;
   auto &window = GetProjectFrame( project );

   // Windows and Linux still modal.
   AboutDialog dlog( &window );
   dlog.ShowModal();
#endif
}

#if 0
// Legacy handlers, not used as of version 2.3.0

// Only does the update checks if it's an ALPHA build and not disabled by
// preferences.
void MayCheckForUpdates(AudacityProject &project)
{
#ifdef IS_ALPHA
   OnCheckForUpdates(project);
#endif
}

void OnHelpWelcome(const CommandContext &context)
{
   SplashDialog::DoHelpWelcome( context.project );
}

#endif

}; // struct Handler

} // namespace

static CommandHandlerObject &findCommandHandler(AudacityProject &) {
   // Handler is not stateful.  Doesn't need a factory registered with
   // AudacityProject.
   static HelpActions::Handler instance;
   return instance;
};

// Menu definitions

#define FN(X) findCommandHandler, \
   static_cast<CommandFunctorPointer>(& HelpActions::Handler :: X)

MenuTable::BaseItemPtr HelpMenu( AudacityProject & )
{
#ifdef __WXMAC__
      wxApp::s_macHelpMenuTitleName = _("&Help");
#endif

   using namespace MenuTable;

   return Menu( XO("&Help"),
      // QuickFix menu item not in Audacity 2.3.1 whilst we discuss further.
#ifdef EXPERIMENTAL_DA
      // DA: Has QuickFix menu item.
      Command( wxT("QuickFix"), XXO("&Quick Fix..."), FN(OnQuickFix),
         AlwaysEnabledFlag ),
      // DA: 'Getting Started' rather than 'Quick Help'.
      Command( wxT("QuickHelp"), XXO("&Getting Started"), FN(OnQuickHelp) ),
      // DA: Emphasise it is the Audacity Manual (No separate DA manual).
      Command( wxT("Manual"), XXO("Audacity &Manual"), FN(OnManual) ),
#else
      Command( wxT("QuickHelp"), XXO("&Quick Help..."), FN(OnQuickHelp),
         AlwaysEnabledFlag ),
      Command( wxT("Manual"), XXO("&Manual..."), FN(OnManual),
         AlwaysEnabledFlag ),
#endif

      Separator(),

      Menu( XO("&Diagnostics"),
         Command( wxT("DeviceInfo"), XXO("Au&dio Device Info..."),
            FN(OnAudioDeviceInfo),
            AudioIONotBusyFlag ),
   #ifdef EXPERIMENTAL_MIDI_OUT
         Command( wxT("MidiDeviceInfo"), XXO("&MIDI Device Info..."),
            FN(OnMidiDeviceInfo),
            AudioIONotBusyFlag ),
   #endif
         Command( wxT("Log"), XXO("Show &Log..."), FN(OnShowLog),
            AlwaysEnabledFlag ),
   #if defined(EXPERIMENTAL_CRASH_REPORT)
         Command( wxT("CrashReport"), XXO("&Generate Support Data..."),
            FN(OnCrashReport), AlwaysEnabledFlag ),
   #endif
         Command( wxT("CheckDeps"), XXO("Chec&k Dependencies..."),
            FN(OnCheckDependencies),
            AudioIONotBusyFlag )
      ),

#ifndef __WXMAC__
      Separator(),
#endif

      // DA: Does not fully support update checking.
#ifndef EXPERIMENTAL_DA
      Command( wxT("Updates"), XXO("&Check for Updates..."),
         FN(OnCheckForUpdates),
         AlwaysEnabledFlag ),
#endif
      Command( wxT("About"), XXO("&About Audacity..."), FN(OnAbout),
         AlwaysEnabledFlag )
   );
}

#undef FN
