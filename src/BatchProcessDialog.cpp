/**********************************************************************

  Audacity: A Digital Audio Editor

  ApplyMacroDialog.cpp

  Dominic Mazzoni
  James Crook

*******************************************************************//*!

\class ApplyMacroDialog
\brief Shows progress in executing commands in MacroCommands.

*//*******************************************************************/


#include "BatchProcessDialog.h"

#include <wx/setup.h> // for wxUSE_* macros

#ifdef __WXMSW__
    #include  <wx/ownerdrw.h>
#endif

#include <wx/defs.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/log.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/listctrl.h>
#include <wx/button.h>
#include <wx/imaglist.h>
#include <wx/settings.h>

#include "Clipboard.h"
#include "ShuttleGui.h"
#include "Menus.h"
#include "Prefs.h"
#include "Project.h"
#include "ProjectFileManager.h"
#include "ProjectHistory.h"
#include "ProjectManager.h"
#include "ProjectWindow.h"
#include "SelectUtilities.h"
#include "Track.h"
#include "commands/CommandManager.h"
#include "effects/Effect.h"
#include "effects/EffectUI.h"
#include "../images/Arrow.xpm"
#include "../images/Empty9x16.xpm"
#include "UndoManager.h"

#include "AllThemeResources.h"

#include "FileDialog/FileDialog.h"
#include "FileNames.h"
#include "import/Import.h"
#include "widgets/AudacityMessageBox.h"
#include "AudacityTextEntryDialog.h"
#include "widgets/HelpSystem.h"

#if wxUSE_ACCESSIBILITY
#include "WindowAccessible.h"
#endif

#define MacrosPaletteTitle XO("Macros Palette")
#define ManageMacrosTitle XO("Manage Macros")


// Separate numerical range from the additional buttons
// in the expanded view (which start at 10,000).
#define MacrosListID       7001
#define CommandsListID     7002
#define ApplyToProjectID   7003
#define ApplyToFilesID     7004
#define ExpandID           7005
#define ShrinkID           7006

BEGIN_EVENT_TABLE(ApplyMacroDialog, wxDialogWrapper)
   EVT_BUTTON(ApplyToProjectID, ApplyMacroDialog::OnApplyToProject)
   EVT_BUTTON(ApplyToFilesID, ApplyMacroDialog::OnApplyToFiles)
   EVT_BUTTON(wxID_CANCEL, ApplyMacroDialog::OnCancel)
   EVT_BUTTON(wxID_CLOSE, ApplyMacroDialog::OnCancel)
   EVT_BUTTON(wxID_HELP, ApplyMacroDialog::OnHelp)
END_EVENT_TABLE()

ApplyMacroDialog::ApplyMacroDialog(
   wxWindow * parent, AudacityProject &project, bool bInherited):
   wxDialogWrapper(parent, wxID_ANY, MacrosPaletteTitle,
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
   , mMacroCommands{ project }
   , mProject{ project }
   , mCatalog( &project )
{
   mAbort = false;
   mbExpanded = false;
   if( bInherited )
      return;
   SetLabel(MacrosPaletteTitle);          // Provide visual label
   SetName(MacrosPaletteTitle);           // Provide audible label
   Populate();

}

ApplyMacroDialog::~ApplyMacroDialog()
{
}

void ApplyMacroDialog::Populate()
{
   //------------------------- Main section --------------------
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
   // Get and validate the currently active macro
   mActiveMacro = gPrefs->Read(wxT("/Batch/ActiveMacro"), wxT(""));
   // Go populate the macros list.
   PopulateMacros();

   Layout();
   Fit();
   wxSize sz = GetSize();
   SetSizeHints( sz );

   // Size and place window
   SetSize(std::min(wxSystemSettings::GetMetric(wxSYS_SCREEN_X) * 3 / 4, sz.GetWidth()),
           std::min(wxSystemSettings::GetMetric(wxSYS_SCREEN_Y) * 4 / 5, 400));

   Center();

   // Set the column size for the macros list.
   sz = mMacros->GetClientSize();
   mMacros->SetColumnWidth(0, sz.x);
}

/// Defines the dialog and does data exchange with it.
void ApplyMacroDialog::PopulateOrExchange(ShuttleGui &S)
{
   /*i18n-hint: A macro is a sequence of commands that can be applied
      * to one or more audio files.*/
   S.StartStatic(XO("Select Macro"), 1);
   {
      mMacros = S.Id(MacrosListID).Prop(1)
         .Style(wxSUNKEN_BORDER | wxLC_REPORT | wxLC_HRULES | wxLC_VRULES |
             wxLC_SINGLE_SEL)
              // i18n-hint: This is the heading for a column in the edit macros dialog
              .AddListControlReportMode( { XO("Macro") } );
   }
   S.EndStatic();

   S.StartHorizontalLay(wxEXPAND, 0);
   {
      S.AddPrompt( XXO("Apply Macro to:") );
      wxButton* btn = S.Id(ApplyToProjectID)
         .Name(XO("Apply macro to project"))
         .AddButton(XXO("&Project"));
#if wxUSE_ACCESSIBILITY
      // so that name can be set on a standard control
      btn->SetAccessible(safenew WindowAccessible(btn));
#endif

      btn = S.Id(ApplyToFilesID)
         .Name(XO("Apply macro to files..."))
         .AddButton(XXO("&Files..."));
#if wxUSE_ACCESSIBILITY
      // so that name can be set on a standard control
      btn->SetAccessible(safenew WindowAccessible(btn));
#endif
   }
   S.EndHorizontalLay();

   S.StartHorizontalLay(wxEXPAND, 0);
   {
      /* i18n-hint: The Expand button makes the dialog bigger, with more in it */
      mResize = S.Id(ExpandID).AddButton(XXO("&Expand"));
      S.AddSpace( 10,10,1 );
      S.AddStandardButtons( eCloseButton | eHelpButton);
   }
   S.EndHorizontalLay();
}

/// This clears and updates the contents of mMacros, the list of macros.
/// It has cut-and-paste code from PopulateList, and both should call 
/// a shared function.
void ApplyMacroDialog::PopulateMacros()
{
   auto names = mMacroCommands.GetNames();
   int i;

   int topItem = mMacros->GetTopItem();
   mMacros->DeleteAllItems();
   for (i = 0; i < (int)names.size(); i++) {
      mMacros->InsertItem(i, names[i]);
   }

   int item = mMacros->FindItem(-1, mActiveMacro);
   bool bFound = item >=0;
   if (item == -1) {
      item = 0;
      mActiveMacro = mMacros->GetItemText(0);
   }

   // Select the name in the list...this will fire an event.
   mMacros->SetItemState(item, wxLIST_STATE_SELECTED | wxLIST_STATE_FOCUSED,
      wxLIST_STATE_SELECTED | wxLIST_STATE_FOCUSED);

   if( 0 <= topItem && topItem < (int)mMacros->GetItemCount())
   {
      // Workaround for scrolling being windows only.
      // Try to scroll back to where we once were...
      mMacros->EnsureVisible( (int)mMacros->GetItemCount() -1 );
      mMacros->EnsureVisible( topItem );
      // And then make sure whatever is selected is still visible...
      if( bFound )
         mMacros->EnsureVisible( item );
   }
}

void ApplyMacroDialog::OnHelp(wxCommandEvent & WXUNUSED(event))
{
   const auto &page = GetHelpPageName();
   HelpSystem::ShowHelp(this, page, true);
}

void ApplyMacroDialog::OnApplyToProject(wxCommandEvent & WXUNUSED(event))
{
   long item = mMacros->GetNextItem(-1,
                                    wxLIST_NEXT_ALL,
                                    wxLIST_STATE_SELECTED);

   if (item == -1) {
      AudacityMessageBox(XO("No macro selected"));
      return;
   }
   ApplyMacroToProject( item );
}

CommandID ApplyMacroDialog::MacroIdOfName( const wxString & MacroName )
{
   wxString Temp = MacroName;
   Temp.Replace(" ","");
   Temp = wxString( "Macro_" ) + Temp;
   return Temp;
}

// Apply macro, given its ID.
// Does nothing if not found, rather than returning an error.
void ApplyMacroDialog::ApplyMacroToProject( const CommandID & MacroID, bool bHasGui )
{
   for( int i=0;i<mMacros->GetItemCount();i++){
      wxString name = mMacros->GetItemText(i);
      if( MacroIdOfName( name ) == MacroID ){
         ApplyMacroToProject( i, bHasGui );
         return;
      }
   }
}

// Apply macro, given its number in the list.
void ApplyMacroDialog::ApplyMacroToProject( int iMacro, bool bHasGui )
{
   wxString name = mMacros->GetItemText(iMacro);
   if( name.empty() )
      return;

#ifdef OPTIONAL_ACTIVITY_WINDOW
   wxDialogWrapper activityWin( this, wxID_ANY, GetTitle());
   activityWin.SetName();
   ShuttleGui S(&activityWin, eIsCreating);

   S.StartHorizontalLay(wxCENTER, false);
   {
      S.StartStatic( {}, false);   // deliberately not translated (!)
      {
         S.SetBorder(20);
         S.AddFixedText(XO("Applying '%s' to current project")
            .Format( name ) );
      }
      S.EndStatic();
   }
   S.EndHorizontalLay();

   activityWin.Layout();
   activityWin.Fit();
   activityWin.CenterOnScreen();
   // Avoid overlap with progress.
   int x,y;
   activityWin.GetPosition( &x, &y );
   activityWin.Move(wxMax(0,x-300), 0);
   activityWin.Show();

   // Without this the newly created dialog may not show completely.
   wxYield();
#endif

   //Since we intend to keep this dialog open, there is no reason to hide it 
   //and then show it again.
   //if( bHasGui )
   //   Hide();

   gPrefs->Write(wxT("/Batch/ActiveMacro"), name);
   gPrefs->Flush();

   mMacroCommands.ReadMacro(name);

   // The disabler must get deleted before the EndModal() call.  Otherwise,
   // the menus on OSX will remain disabled.
   bool success;
   {
#ifdef OPTIONAL_ACTIVITY_WINDOW
      wxWindowDisabler wd(&activityWin);
#endif
      success = GuardedCall< bool >(
         [this]{ return mMacroCommands.ApplyMacro(mCatalog); } );
   }

   if( !bHasGui )
      return;

   Show();
   Raise();
}

void ApplyMacroDialog::OnApplyToFiles(wxCommandEvent & WXUNUSED(event))
{
   long item = mMacros->GetNextItem(-1,
                                    wxLIST_NEXT_ALL,
                                    wxLIST_STATE_SELECTED);
   if (item == -1) {
      AudacityMessageBox( XO("No macro selected") );
      return;
   }

   wxString name = mMacros->GetItemText(item);
   gPrefs->Write(wxT("/Batch/ActiveMacro"), name);
   gPrefs->Flush();

   AudacityProject *project = &mProject;
   if (!TrackList::Get( *project ).empty()) {
      AudacityMessageBox(
         XO("Please save and close the current project first.") );
      return;
   }

   // This insures that we start with an empty and temporary project
   ProjectFileManager::Get(*project).CloseProject();
   ProjectFileManager::Get(*project).OpenProject();

   auto prompt =  XO("Select file(s) for batch processing...");

   const auto fileTypes = Importer::Get().GetFileTypes();

   auto path = FileNames::FindDefaultPath(FileNames::Operation::Open);
   FileDialogWrapper dlog(this,
      prompt,
      path,
      wxT(""),
      fileTypes,
      wxFD_OPEN | wxFD_MULTIPLE | wxRESIZE_BORDER);

   dlog.SetFilterIndex( Importer::SelectDefaultOpenType( fileTypes ) );
   if (dlog.ShowModal() != wxID_OK) {
      Raise();
      return;
   }
   Raise();
   
   wxArrayString files;
   dlog.GetPaths(files);

   files.Sort();

   wxDialogWrapper activityWin(this, wxID_ANY, Verbatim( GetTitle() ),
      wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER
      );
   activityWin.SetName();
   ShuttleGui S(&activityWin, eIsCreating);

   wxListCtrl * fileList = NULL;

   S.StartVerticalLay(1);
   {
      S.StartStatic(XO("Applying..."), 1);
      {
         auto imageList = std::make_unique<wxImageList>(9, 16);
         imageList->Add(wxIcon(empty9x16_xpm));
         imageList->Add(wxIcon(arrow_xpm));

         fileList = S.Id(CommandsListID)
            .Style(wxSUNKEN_BORDER | wxLC_REPORT | wxLC_HRULES | wxLC_VRULES |
                wxLC_SINGLE_SEL)
            .AddListControlReportMode( { XO("File") } );
         // AssignImageList takes ownership
         fileList->AssignImageList(imageList.release(), wxIMAGE_LIST_SMALL);
      }
      S.EndStatic();

      S.StartHorizontalLay(wxCENTER, 0);
      {
         S.Id(wxID_CANCEL).AddButton(XXO("&Cancel"));
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();

   int i;
   for (i = 0; i < (int)files.size(); i++ ) {
      fileList->InsertItem(i, files[i], i == 0);
   }

   // Set the column size for the files list.
   fileList->SetColumnWidth(0, wxLIST_AUTOSIZE);

   int width = wxMin( fileList->GetColumnWidth(0), 1000);
   wxSize sz = fileList->GetClientSize();
   if (sz.GetWidth() < width ) {
      sz.SetWidth(width);
      if (sz.GetHeight() < width *0.7)
         sz.SetHeight(width * 0.7);
      fileList->SetInitialSize(sz);
   }

   activityWin.Layout();
   activityWin.Fit();
   activityWin.CenterOnScreen();
   // Avoid overlap with progress.
   int x,y;
   activityWin.GetPosition( &x, &y );
   activityWin.Move(wxMax(0,x-300), 0);
   activityWin.Show();

   // Without this the newly created dialog may not show completely.
   wxYield();
   // We could avoid hiding, but there are many dialogs on screen,
   // and hiding this one temporarily has some advantages.
   Hide();

   mMacroCommands.ReadMacro(name); 
   {
      auto &globalClipboard = Clipboard::Get();

      // DV: Macro invocation on file will reset the project to the
      // initial state. There is a possibility, that clipboard will contain
      // references to the data removed
      if (globalClipboard.Project().lock().get() == project)
         globalClipboard.Clear();

      // Move global clipboard contents aside temporarily
      Clipboard::Scope scope;

      wxWindowDisabler wd(&activityWin);
      for (i = 0; i < (int)files.size(); i++) {
         if (i > 0) {
            //Clear the arrow in previous item.
            fileList->SetItemImage(i - 1, 0, 0);
         }
         fileList->SetItemImage(i, 1, 1);
         fileList->EnsureVisible(i);

         auto success = GuardedCall< bool >([&] {
            ProjectFileManager::Get(*project).Import(files[i]);
            ProjectWindow::Get(*project).ZoomAfterImport(nullptr);
            SelectUtilities::DoSelectAll(*project);
            if (!mMacroCommands.ApplyMacro(mCatalog))
               return false;

            if (!activityWin.IsShown() || mAbort)
               return false;

            return true;
         });

         // Ensure project is completely reset
         ProjectManager::Get(*project).ResetProjectToEmpty();
         // Bug2567:
         // Must also destroy the clipboard, to be sure sample blocks are
         // all freed and their ids can be reused safely in the next pass
         globalClipboard.Clear();

         if (!success)
            break;
      }
   }

   Show();
   Raise();
}

void ApplyMacroDialog::OnCancel(wxCommandEvent & WXUNUSED(event))
{
   Hide();
}

/////////////////////////////////////////////////////////////////////
#include <wx/textdlg.h>
#include "BatchCommandDialog.h"

enum {
   AddButtonID = 10000,
   RemoveButtonID,
   RenameButtonID,
   RestoreButtonID,
   ImportButtonID,
   ExportButtonID,
   SaveButtonID,

   DefaultsButtonID,

   InsertButtonID,
   EditButtonID,
   DeleteButtonID,
   UpButtonID,
   DownButtonID,

// MacrosListID             7005
// CommandsListID,       7002
// Re-Use IDs from ApplyMacroDialog.
   ApplyToProjectButtonID = ApplyToProjectID,
   ApplyToFilesButtonID = ApplyToFilesID,
};

BEGIN_EVENT_TABLE(MacrosWindow, ApplyMacroDialog)
   EVT_LIST_ITEM_SELECTED(MacrosListID, MacrosWindow::OnMacroSelected)
   EVT_LIST_ITEM_SELECTED(CommandsListID, MacrosWindow::OnListSelected)
   EVT_LIST_BEGIN_LABEL_EDIT(MacrosListID, MacrosWindow::OnMacrosBeginEdit)
   EVT_LIST_END_LABEL_EDIT(MacrosListID, MacrosWindow::OnMacrosEndEdit)
   EVT_BUTTON(AddButtonID, MacrosWindow::OnAdd)
   EVT_BUTTON(RemoveButtonID, MacrosWindow::OnRemove)
   EVT_BUTTON(RenameButtonID, MacrosWindow::OnRename)
   EVT_BUTTON(RestoreButtonID, MacrosWindow::OnRestore)
   EVT_BUTTON(ImportButtonID, MacrosWindow::OnImport)
   EVT_BUTTON(ExportButtonID, MacrosWindow::OnExport)
   EVT_BUTTON(SaveButtonID, MacrosWindow::OnSave)

   EVT_BUTTON(ExpandID, MacrosWindow::OnExpand)
   EVT_BUTTON(ShrinkID, MacrosWindow::OnShrink)
   EVT_SIZE(MacrosWindow::OnSize)

   EVT_LIST_ITEM_ACTIVATED(CommandsListID, MacrosWindow::OnCommandActivated)
   EVT_BUTTON(InsertButtonID, MacrosWindow::OnInsert)
   EVT_BUTTON(EditButtonID, MacrosWindow::OnEditCommandParams)
   EVT_BUTTON(DeleteButtonID, MacrosWindow::OnDelete)
   EVT_BUTTON(UpButtonID, MacrosWindow::OnUp)
   EVT_BUTTON(DownButtonID, MacrosWindow::OnDown)

   EVT_BUTTON(wxID_OK, MacrosWindow::OnOK)
   EVT_BUTTON(wxID_CANCEL, MacrosWindow::OnCancel)
   EVT_BUTTON(wxID_CLOSE, MacrosWindow::OnCancel)

   EVT_KEY_DOWN(MacrosWindow::OnKeyDown)
END_EVENT_TABLE()

enum {
   ItemNumberColumn,
   ActionColumn,
   ParamsColumn,
};

/// Constructor
MacrosWindow::MacrosWindow(
   wxWindow * parent, AudacityProject &project, bool bExpanded):
   ApplyMacroDialog(parent, project, true)
   , mProject{ project }
{
   mbExpanded = bExpanded;
   auto Title = WindowTitle();
   SetLabel( Title );   // Provide visual label
   SetName(  Title );   // Provide audible label
   SetTitle( Title );

   mChanged = false;
   mSelectedCommand = 0;

   if( mbExpanded )
      Populate();
   else
      ApplyMacroDialog::Populate();
}

MacrosWindow::~MacrosWindow()
{
}

/// Creates the dialog and its contents.
void MacrosWindow::Populate()
{
   //------------------------- Main section --------------------
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------

   // Get and validate the currently active macro
   mActiveMacro = gPrefs->Read(wxT("/Batch/ActiveMacro"), wxT(""));
   // Go populate the macros list.
   PopulateMacros();

   // We have a bare list.  We need to add columns and content.
   PopulateList();

   // Layout and set minimum size of window
   Layout();
   Fit();
   SetSizeHints(GetSize());

   // Size and place window
   SetSize(std::min(wxSystemSettings::GetMetric(wxSYS_SCREEN_X) * 3 / 4, 800),
           std::min(wxSystemSettings::GetMetric(wxSYS_SCREEN_Y) * 4 / 5, 400));
   Center();

   // Set the column size for the macros list.
   wxSize sz = mMacros->GetClientSize();
   mMacros->SetColumnWidth(0, sz.x);

   // Size columns properly
   FitColumns();
}

/// Defines the dialog and does data exchange with it.
void MacrosWindow::PopulateOrExchange(ShuttleGui & S)
{
   S.StartHorizontalLay(wxEXPAND, 1);
   {
      S.StartStatic(XO("Select Macro"),0);
      {
         S.StartHorizontalLay(wxEXPAND,1);
         {
            mMacros = S.Id(MacrosListID).Prop(1)
               .Style(wxSUNKEN_BORDER | wxLC_REPORT | wxLC_HRULES
                      | wxLC_SINGLE_SEL | wxLC_EDIT_LABELS)
              // i18n-hint: This is the heading for a column in the edit macros dialog
              .AddListControlReportMode( { XO("Macro") } );
            S.StartVerticalLay(wxALIGN_TOP, 0);
            {
               S.Id(AddButtonID).AddButton(XXO("&New"), wxALIGN_LEFT);
               mRemove = S.Id(RemoveButtonID).AddButton(XXO("Remo&ve"), wxALIGN_LEFT);
               mRename = S.Id(RenameButtonID).AddButton(XXO("&Rename..."), wxALIGN_LEFT);
               mRestore = S.Id(RestoreButtonID).AddButton(XXO("Re&store"), wxALIGN_LEFT);
               mImport = S.Id(ImportButtonID).AddButton(XO("I&mport..."), wxALIGN_LEFT);
               mExport = S.Id(ExportButtonID).AddButton(XO("E&xport..."), wxALIGN_LEFT);
            }
            S.EndVerticalLay();
         }
         S.EndHorizontalLay();
      }
      S.EndStatic();

      S.StartStatic(XO("Edit Steps"), true);
      {
         S.StartHorizontalLay(wxEXPAND,1);
         {
            mList = S.Id(CommandsListID)
               .Style(wxSUNKEN_BORDER | wxLC_REPORT | wxLC_HRULES | wxLC_VRULES |
                   wxLC_SINGLE_SEL)
               .AddListControlReportMode({
                  /* i18n-hint: This is the number of the command in the list */
                  { XO("Num"), wxLIST_FORMAT_RIGHT },
                  { XO("Command  "), wxLIST_FORMAT_RIGHT },
                  { XO("Parameters"), wxLIST_FORMAT_LEFT }
                });

            S.StartVerticalLay(wxALIGN_TOP, 0);
            {
               S.Id(InsertButtonID).AddButton(XXO("&Insert"), wxALIGN_LEFT);
               S.Id(EditButtonID).AddButton(XXO("&Edit..."), wxALIGN_LEFT);
               S.Id(DeleteButtonID).AddButton(XXO("De&lete"), wxALIGN_LEFT);
               S.Id(UpButtonID).AddButton(XXO("Move &Up"), wxALIGN_LEFT);
               S.Id(DownButtonID).AddButton(XXO("Move &Down"), wxALIGN_LEFT);
               mSave = S.Id(SaveButtonID).AddButton(XO("&Save"), wxALIGN_LEFT);
               mSave->Enable( mChanged );
            }
            S.EndVerticalLay();
         }
         S.EndHorizontalLay();
      }
      S.EndStatic();
   }
   S.EndHorizontalLay();

   S.StartHorizontalLay(wxEXPAND, 0);
   {  
      /* i18n-hint: The Shrink button makes the dialog smaller, with less in it */
      mResize = S.Id(ShrinkID).AddButton(XXO("Shrin&k"));
      // Using variable text just to get the positioning options.
      S.Prop(0).AddVariableText(
         XO("Apply Macro to:"), false, wxALL | wxALIGN_CENTRE_VERTICAL );
      wxButton* btn = S.Id(ApplyToProjectID)
         .Name(XO("Apply macro to project"))
         .AddButton(XXO("&Project"));
#if wxUSE_ACCESSIBILITY
      // so that name can be set on a standard control
      btn->SetAccessible(safenew WindowAccessible(btn));
#endif

      btn = S.Id(ApplyToFilesID)
         .Name(XO("Apply macro to files..."))
         .AddButton(XXO("&Files..."));
#if wxUSE_ACCESSIBILITY
      // so that name can be set on a standard control
      btn->SetAccessible(safenew WindowAccessible(btn));
#endif
      S.AddSpace( 10,10,1 );
      // Bug 2524 OK button does much the same as cancel, so remove it.
      // OnCancel prompts you if there has been a change.
      // OnOK saves without prompting.
      // That difference is too slight to merit a button, and with the OK
      // button, people might expect the dialog to apply the macro too.
      S.AddStandardButtons( /*eOkButton |*/ eCloseButton | eHelpButton);
   }

   S.EndHorizontalLay();

   
   return;
}

/// This clears and updates the contents of mList, the commands for the current macro.
void MacrosWindow::PopulateList()
{
   int topItem = mList->GetTopItem();
   mList->DeleteAllItems();

   for (int i = 0; i < mMacroCommands.GetCount(); i++) {
      AddItem(mMacroCommands.GetCommand(i),
              mMacroCommands.GetParams(i));
   }
   /*i18n-hint: This is the last item in a list.*/
   AddItem(_("- END -"), wxT(""));

   // Select the name in the list...this will fire an event.
   if (mSelectedCommand >= (int)mList->GetItemCount()) {
      mSelectedCommand = 0;
   }
   mList->SetItemState(mSelectedCommand,
      wxLIST_STATE_SELECTED | wxLIST_STATE_FOCUSED,
      wxLIST_STATE_SELECTED | wxLIST_STATE_FOCUSED);
   if( 0 <= topItem && topItem < (int)mList->GetItemCount())
   {
      // Workaround for scrolling being windows only.
      // Try to scroll back to where we once were...
      mList->EnsureVisible( (int)mList->GetItemCount() -1 );
      mList->EnsureVisible( topItem );
      // And then make sure whatever is selected is still visible...
      if (mSelectedCommand >= 0) {
         mList->EnsureVisible( mSelectedCommand );
      }
   }
}

/// Add one item into mList
void MacrosWindow::AddItem(const CommandID &Action, const wxString &Params)
{
   auto entry = mCatalog.ByCommandId(Action);
   auto friendlyName = entry != mCatalog.end()
      ? entry->name.StrippedTranslation()
      :
         // uh oh, using GET to expose an internal name to the user!
         // in default of any better friendly name
        Action.GET();

   int i = mList->GetItemCount();

   mList->InsertItem(i, wxString::Format(wxT(" %02i"), i + 1));
   mList->SetItem(i, ActionColumn, friendlyName );
   mList->SetItem(i, ParamsColumn, Params );
}

void MacrosWindow::UpdateMenus()
{
   // OK even on mac, as dialog is modal.
   auto p = &mProject;
   MenuManager::Get(*p).RebuildMenuBar(*p);
}

void MacrosWindow::UpdateDisplay( bool bExpanded )
{
   // If we failed to save changes, we abandon the attempt to 
   // change the expand/shrink state of the GUI.
   if( !SaveChanges() )
      return;

   mbExpanded = bExpanded;

   mChanged = false;
   // if we try to access the about to be destroyed mSave button 
   // inappropriately, we need to crash rather than (sometimes) silently 
   // succeed.
   mSave = nullptr;

   DestroyChildren();
   SetSizer( nullptr );

   mSelectedCommand = 0;
   SetMinSize( wxSize( 200,200 ));

   // Get and set position for optical stability.
   // Expanded and shrunk dialogs 'stay where they were'.
   // That's OK , and what we want, even if we exapnd off-screen.
   // We won't shrink to being off-screen, since the shrink button 
   // was clicked, so must have been on screen.
   wxPoint p = GetPosition( );
   if( mbExpanded )
      Populate();
   else
      ApplyMacroDialog::Populate();
   SetPosition( p );
   mResize->SetFocus();

   auto Title = WindowTitle();
   SetLabel( Title );         // Provide visual label
   SetName( Title );          // Provide audible label
   SetTitle( Title );
}

void MacrosWindow::OnExpand(wxCommandEvent &WXUNUSED(event))
{  UpdateDisplay( true );}

void MacrosWindow::OnShrink(wxCommandEvent &WXUNUSED(event))
{  
   if( ChangeOK() )
      UpdateDisplay( false );
}


bool MacrosWindow::ChangeOK()
{
   if (mChanged) {
      int id;

      auto title = XO("%s changed").Format( mActiveMacro );
      auto msg = XO("Do you want to save the changes?");

      id = AudacityMessageBox(
         msg,
         title,
         wxYES_NO | wxCANCEL);
      if (id == wxCANCEL) {
         return false;
      }

      if (id == wxYES) {
         if (!mMacroCommands.WriteMacro(mActiveMacro)) {
            return false;
         }
      }

      mChanged = false;
      mSave->Enable( mChanged );
   }

   return true;
}
/// An item in the macros list has been selected.
void MacrosWindow::OnMacroSelected(wxListEvent & event)
{
   if (!ChangeOK()) {
      event.Veto();
      return;
   }

   int item = event.GetIndex();

   mActiveMacro = mMacros->GetItemText(item);
   ShowActiveMacro();
}

void MacrosWindow::ShowActiveMacro()
{
   mMacroCommands.ReadMacro(mActiveMacro);
   if( !mbExpanded )
      return;
   
   if (mMacroCommands.IsFixed(mActiveMacro)) {
      mRemove->Disable();
      mRename->Disable();
      mRestore->Enable();
   }
   else {
      mRemove->Enable();
      mRename->Enable();
      mRestore->Disable();
   }

   PopulateList();
}

/// An item in the macros list has been selected.
void MacrosWindow::OnListSelected(wxListEvent & WXUNUSED(event))
{
   FitColumns();
}

/// The window has been resized.
void MacrosWindow::OnSize(wxSizeEvent & WXUNUSED(event))
{
   // Refresh the layout and re-fit the columns.
   Layout();
   if( !mbExpanded )
      return;
   FitColumns();
}

void MacrosWindow::FitColumns()
{

#if defined(__WXMAC__)
   // wxMac uses a hard coded width of 150 when wxLIST_AUTOSIZE_USEHEADER
   // is specified, so we calculate the width ourselves. This method may
   // work equally well on other platforms.
   for (size_t c = 0; c < mList->GetColumnCount(); c++) {
      wxListItem info;
      int width;

      mList->SetColumnWidth(c, wxLIST_AUTOSIZE);
      info.Clear();
      info.SetId(c);
      info.SetMask(wxLIST_MASK_TEXT | wxLIST_MASK_WIDTH);
      mList->GetColumn(c, info);

      mList->GetTextExtent(info.GetText(), &width, NULL);
      width += 2 * 4;    // 2 * kItemPadding - see listctrl_mac.cpp
      width += 16;       // kIconWidth - see listctrl_mac.cpp

      mList->SetColumnWidth(c, wxMax(width, mList->GetColumnWidth(c)));
   }

   // Looks strange, but it forces the horizontal scrollbar to get
   // drawn.  If not done, strange column sizing can occur if the
   // user attempts to resize the columns.
   mList->SetClientSize(mList->GetClientSize());
#else
   mList->SetColumnWidth(0, wxLIST_AUTOSIZE_USEHEADER);
   mList->SetColumnWidth(1, wxLIST_AUTOSIZE_USEHEADER);
   mList->SetColumnWidth(2, wxLIST_AUTOSIZE);
#endif

   int bestfit = mList->GetColumnWidth(2);
   int clientsize = mList->GetClientSize().GetWidth();
   int col0 = mList->GetColumnWidth(0);
   int col1 = mList->GetColumnWidth(1);
   bestfit = (bestfit > clientsize-col0-col1)? bestfit : clientsize-col0-col1;
   mList->SetColumnWidth(2, bestfit);

}

///
void MacrosWindow::OnMacrosBeginEdit(wxListEvent &event)
{
   int itemNo = event.GetIndex();

   wxString macro = mMacros->GetItemText(itemNo);

   if (mMacroCommands.IsFixed(macro)) {
      wxBell();
      event.Veto();
   }
   if( mMacroBeingRenamed.IsEmpty())
      mMacroBeingRenamed = macro;
}

///
void MacrosWindow::OnMacrosEndEdit(wxListEvent &event)
{
   if (event.IsEditCancelled()) {
      mMacroBeingRenamed = "";
      return;
   }

   if( mMacroBeingRenamed.IsEmpty())
      return;

   wxString newname = event.GetLabel();

   mMacroCommands.RenameMacro(mMacroBeingRenamed, newname);
   if( mMacroBeingRenamed == mActiveMacro )
      mActiveMacro = newname;
   mMacroBeingRenamed="";
   PopulateMacros();
   UpdateMenus();   
   event.Veto();
}

///
void MacrosWindow::OnAdd(wxCommandEvent & WXUNUSED(event))
{
   // Similar to Bug 2284 we may need to save a changed macro.
   if (!ChangeOK()) {
      return;
   }

   while (true) {
      AudacityTextEntryDialog d(this,
         XO("Enter name of new macro"),
         XO("Name of new macro"));
      d.SetName(d.GetTitle());
      wxString name;

      if (d.ShowModal() == wxID_CANCEL) {
         Raise();
         return;
      }
      Raise();

      name = d.GetValue().Strip(wxString::both);

      if (name.length() == 0) {
         AudacityMessageBox(
            XO("Name must not be blank"),
            WindowTitle(),
            wxOK | wxICON_ERROR,
            this);
         continue;
      }

      if (name.Contains(wxFILE_SEP_PATH) ||
          name.Contains(wxFILE_SEP_PATH_UNIX)) {
         AudacityMessageBox(
            /*i18n-hint: The %c will be replaced with 'forbidden characters', like '/' and '\'.*/
            XO("Names may not contain '%c' and '%c'")
               .Format(wxFILE_SEP_PATH, wxFILE_SEP_PATH_UNIX),
            WindowTitle(),
            wxOK | wxICON_ERROR,
            this);
         continue;
      }

      mMacroCommands.AddMacro(name);

      mActiveMacro = name;

      PopulateMacros();
      UpdateMenus();

      break;
   }
}

///
void MacrosWindow::OnRemove(wxCommandEvent & WXUNUSED(event))
{
   long item = mMacros->GetNextItem(-1,
                                    wxLIST_NEXT_ALL,
                                    wxLIST_STATE_SELECTED);
   if (item == -1) {
      return;
   }

   wxString name = mMacros->GetItemText(item);
   AudacityMessageDialog m(
      this,
      /*i18n-hint: %s will be replaced by the name of a file.*/
      XO("Are you sure you want to delete %s?").Format( name ),
      Verbatim( GetTitle() ),
      wxYES_NO | wxICON_QUESTION );
   if (m.ShowModal() == wxID_NO) {
      Raise();
      return;
   }
   Raise();

   mMacroCommands.DeleteMacro(name);

   item++;
   if (item >= (mMacros->GetItemCount() - 1) && item >= 0) {
      item--;
   }

   // Bug 2284.  The macro we have just removed might have been 
   // changed.  Since we've just deleted the macro, we should
   // forget about that change.
   mChanged = false;
   mSave->Enable( mChanged );
   mActiveMacro = mMacros->GetItemText(item);

   PopulateMacros();
   UpdateMenus();
}

///
void MacrosWindow::OnRename(wxCommandEvent & WXUNUSED(event))
{
   long item = mMacros->GetNextItem(-1,
                                    wxLIST_NEXT_ALL,
                                    wxLIST_STATE_SELECTED);
   if (item == -1) {
      return;
   }

   mMacros->EditLabel(item);
   UpdateMenus();
}

/// Reset a built in macro.
void MacrosWindow::OnRestore(wxCommandEvent & WXUNUSED(event))
{
   mMacroCommands.RestoreMacro(mActiveMacro);

   mChanged = true;
   mSave->Enable( mChanged );

   PopulateList();
}

///
void MacrosWindow::OnImport(wxCommandEvent & WXUNUSED(event))
{
   if (!ChangeOK()) {
      return;
   }

   long item = mMacros->GetNextItem(-1,
                                    wxLIST_NEXT_ALL,
                                    wxLIST_STATE_SELECTED);
   if (item == -1) {
      return;
   }

   wxString name = mMacros->GetItemText(item);

   name = mMacroCommands.ReadMacro({}, this);
   if (name == wxEmptyString) {
      return;
   }

   mActiveMacro = name;

   PopulateMacros();
   UpdateMenus();
}

///
void MacrosWindow::OnExport(wxCommandEvent & WXUNUSED(event))
{
   long item = mMacros->GetNextItem(-1,
                                    wxLIST_NEXT_ALL,
                                    wxLIST_STATE_SELECTED);
   if (item == -1) {
      return;
   }

   mMacroCommands.WriteMacro(mMacros->GetItemText(item), this);
}

void MacrosWindow::OnSave(wxCommandEvent & WXUNUSED(event))
{
   SaveChanges();
}


/// An item in the list has been selected.
/// Bring up a dialog to allow its parameters to be edited.
void MacrosWindow::OnCommandActivated(wxListEvent & WXUNUSED(event))
{
   wxCommandEvent dummy;
   OnEditCommandParams( dummy );
}

///
void MacrosWindow::OnInsert(wxCommandEvent & WXUNUSED(event))
{
   long item = mList->GetNextItem(-1,
                                  wxLIST_NEXT_ALL,
                                  wxLIST_STATE_SELECTED);
   if (item == -1) {
      item = mList->GetItemCount()-1;
   }
   InsertCommandAt( item );
}

void MacrosWindow::InsertCommandAt(int item)
{
   if (item == -1) {
      return;
   }

   MacroCommandDialog d(this, wxID_ANY, mProject);

   if (!d.ShowModal()) {
      Raise();
      return;
   }
   Raise();

   if(!d.mSelectedCommand.empty())
   {
      mMacroCommands.AddToMacro(d.mSelectedCommand,
                                d.mSelectedParameters,
                                item);
      mChanged = true;
      mSave->Enable( mChanged );

      mSelectedCommand = item + 1;
      PopulateList();
   }

}

void MacrosWindow::OnEditCommandParams(wxCommandEvent & WXUNUSED(event))
{
   int item = mList->GetNextItem( -1, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED );

   // LAST command in list is END.
   // If nothing selected, add at END.
   // If END selected, add at END.
   // When adding at end we use InsertCommandAt, so that a new command
   // can be chosen.
   int lastItem = mList->GetItemCount()-1;
   if( (item<0) || (item+1) == mList->GetItemCount() )
   {
      InsertCommandAt( lastItem );
      return;
   }

   // Just edit the parameters, and not the command.
   auto command = mMacroCommands.GetCommand(item);
   wxString params  = mMacroCommands.GetParams(item);
   wxString oldParams = params;

   params = MacroCommands::PromptForParamsFor(command, params, *this).Trim();
   Raise();

   if (oldParams == params)
      return; // They did not actually make any changes..

   mMacroCommands.DeleteFromMacro(item);
   mMacroCommands.AddToMacro(command,
      params,
      item);

   mChanged = true;
   mSave->Enable( mChanged );

   mSelectedCommand = item;
   PopulateList();
}

///
void MacrosWindow::OnDelete(wxCommandEvent & WXUNUSED(event))
{
   long item = mList->GetNextItem(-1,
                                  wxLIST_NEXT_ALL,
                                  wxLIST_STATE_SELECTED);
   if (item == -1 || item + 1 == mList->GetItemCount()) {
      return;
   }

   mMacroCommands.DeleteFromMacro(item);

   mChanged = true;
   mSave->Enable( mChanged );

   if (item >= (mList->GetItemCount() - 2) && item >= 0) {
      item--;
   }
   mSelectedCommand = item;
   PopulateList();
}

///
void MacrosWindow::OnUp(wxCommandEvent & WXUNUSED(event))
{
   long item = mList->GetNextItem(-1,
                                  wxLIST_NEXT_ALL,
                                  wxLIST_STATE_SELECTED);
   if (item == -1 || item == 0 || item + 1 == mList->GetItemCount()) {
      return;
   }

   mMacroCommands.AddToMacro(mMacroCommands.GetCommand(item),
                             mMacroCommands.GetParams(item),
                             item - 1);
   mMacroCommands.DeleteFromMacro(item + 1);

   mChanged = true;
   mSave->Enable( mChanged );

   mSelectedCommand = item - 1;
   PopulateList();
}

///
void MacrosWindow::OnDown(wxCommandEvent & WXUNUSED(event))
{
   long item = mList->GetNextItem(-1,
                                  wxLIST_NEXT_ALL,
                                  wxLIST_STATE_SELECTED);
   if (item == -1 || item + 2 >= mList->GetItemCount()) {
      return;
   }

   mMacroCommands.AddToMacro(mMacroCommands.GetCommand(item),
                             mMacroCommands.GetParams(item),
                             item + 2);
   mMacroCommands.DeleteFromMacro(item);

   mChanged = true;
   mSave->Enable( mChanged );

   mSelectedCommand = item + 1;
   PopulateList();
}

void MacrosWindow::OnApplyToProject(wxCommandEvent & event)
{
   if( !SaveChanges() )
      return;
   ApplyMacroDialog::OnApplyToProject( event );
}

void MacrosWindow::OnApplyToFiles(wxCommandEvent & event)
{
   if( !SaveChanges() )
      return;
   ApplyMacroDialog::OnApplyToFiles( event );
}

bool MacrosWindow::SaveChanges(){
   gPrefs->Write(wxT("/Batch/ActiveMacro"), mActiveMacro);
   gPrefs->Flush();

   if (mChanged) {
      if (!mMacroCommands.WriteMacro(mActiveMacro)) {
         return false;
      }
   }

   mChanged = false;
   if( mSave )
      mSave->Enable( mChanged );

   return true;
}

/// Send changed values back to Prefs, and update Audacity.
void MacrosWindow::OnOK(wxCommandEvent & WXUNUSED(event))
{
   if( !SaveChanges() )
      return;
   Hide();
   //EndModal(true);
}

///
void MacrosWindow::OnCancel(wxCommandEvent &WXUNUSED(event))
{
   bool bWasChanged = mChanged;
   if (!ChangeOK()) {
      return;
   }
   // If we've rejected a change, we need to restore the display
   // of the active macro.
   // That's because next time we open this dialog we want to see the 
   // unedited macro.
   if( bWasChanged )
      ShowActiveMacro();
   Hide();
}

///
void MacrosWindow::OnKeyDown(wxKeyEvent &event)
{
   if (event.GetKeyCode() == WXK_DELETE) {
      wxLogDebug(wxT("wxKeyEvent"));
   }

   event.Skip();
}

TranslatableString MacrosWindow::WindowTitle() const
{
   return mbExpanded ? ManageMacrosTitle : MacrosPaletteTitle;
}

// PrefsListener implementation
void MacrosWindow::UpdatePrefs()
{
   UpdateDisplay(mbExpanded);
}

// The rest of this file installs hooks

#include "CommonCommandFlags.h"
#include "commands/CommandContext.h"
#include "effects/EffectManager.h"
#include "ProjectWindows.h"
namespace {

AttachedWindows::RegisteredFactory sMacrosWindowKey{
   []( AudacityProject &parent ) -> wxWeakRef< wxWindow > {
      auto &window = ProjectWindow::Get( parent );
      return safenew MacrosWindow(
         &window, parent, true
      );
   }
};

void OnApplyMacroDirectlyByName(
   const CommandContext& context, const MacroID& Name);

void OnRepeatLastTool(const CommandContext& context)
{
   auto& menuManager = MenuManager::Get(context.project);
   switch (menuManager.mLastToolRegistration) {
     case MenuCreator::repeattypeplugin:
     {
        auto lastEffect = menuManager.mLastTool;
        if (!lastEffect.empty())
        {
           EffectUI::DoEffect(
              lastEffect, context, menuManager.mRepeatToolFlags);
        }
     }
       break;
     case MenuCreator::repeattypeunique:
        CommandManager::Get(context.project).DoRepeatProcess(context,
           menuManager.mLastToolRegisteredId);
        break;
     case MenuCreator::repeattypeapplymacro:
        OnApplyMacroDirectlyByName(context, menuManager.mLastTool);
        break;
   }
}

void OnManageMacros(const CommandContext &context )
{
   auto &project = context.project;
   CommandManager::Get(project).RegisterLastTool(context);  //Register Macros as Last Tool
   auto macrosWindow = &GetAttachedWindows(project)
      .AttachedWindows::Get< MacrosWindow >( sMacrosWindowKey );
   if (macrosWindow) {
      macrosWindow->Show();
      macrosWindow->Raise();
      macrosWindow->UpdateDisplay( true );
   }
}

void OnApplyMacrosPalette(const CommandContext &context )
{
   auto &project = context.project;
   CommandManager::Get(project).RegisterLastTool(context);  //Register Palette as Last Tool
   auto macrosWindow = &GetAttachedWindows(project)
      .AttachedWindows::Get< MacrosWindow >( sMacrosWindowKey );
   if (macrosWindow) {
      macrosWindow->Show();
      macrosWindow->Raise();
      macrosWindow->UpdateDisplay( false );
   }
}

void OnApplyMacroDirectly(const CommandContext &context )
{
   const MacroID& Name = context.parameter.GET();
   OnApplyMacroDirectlyByName(context, Name);
}

void OnApplyMacroDirectlyByName(const CommandContext& context, const MacroID& Name)

{
   auto &project = context.project;
   auto &window = ProjectWindow::Get( project );
   //wxLogDebug( "Macro was: %s", context.parameter);
   ApplyMacroDialog dlg( &window, project );
   //const auto &Name = context.parameter;

// We used numbers previously, but macros could get renumbered, making
// macros containing macros unpredictable.
#ifdef MACROS_BY_NUMBERS
   long item=0;
   // Take last three letters (of e.g. Macro007) and convert to a number.
   Name.Mid( Name.length() - 3 ).ToLong( &item, 10 );
   dlg.ApplyMacroToProject( item, false );
#else
   dlg.ApplyMacroToProject( Name, false );
#endif
   /* i18n-hint: %s will be the name of the macro which will be
    * repeated if this menu item is chosen */
   MenuManager::ModifyUndoMenuItems( project );

   TranslatableString desc;
   EffectManager& em = EffectManager::Get();
   auto shortDesc = em.GetCommandName(Name);
   auto& undoManager = UndoManager::Get(project);
   auto& commandManager = CommandManager::Get(project);
   int cur = undoManager.GetCurrentState();
   if (undoManager.UndoAvailable()) {
       undoManager.GetShortDescription(cur, &desc);
       commandManager.Modify(wxT("RepeatLastTool"), XXO("&Repeat %s")
          .Format(desc));
       auto& menuManager = MenuManager::Get(project);
       menuManager.mLastTool = Name;
       menuManager.mLastToolRegistration = MenuCreator::repeattypeapplymacro;
   }

}

MenuTable::BaseItemPtrs PopulateMacrosMenu( CommandFlag flags  )
{
   MenuTable::BaseItemPtrs result;
   auto names = MacroCommands::GetNames(); // these names come from filenames
   int i;

   // This finder scope may be redundant, but harmless
   for (i = 0; i < (int)names.size(); i++) {
      auto MacroID = ApplyMacroDialog::MacroIdOfName( names[i] );
      result.push_back( MenuTable::Command( MacroID,
         Verbatim( names[i] ), // file name verbatim
         OnApplyMacroDirectly,
         flags,
         CommandManager::Options{}.AllowInMacros()
      ) );
   }

   return result;
}

const ReservedCommandFlag&
   HasLastToolFlag() { static ReservedCommandFlag flag{
      [](const AudacityProject &project) {
      auto& menuManager = MenuManager::Get(project);
         if (menuManager.mLastToolRegistration == MenuCreator::repeattypeunique) return true;
         return !menuManager.mLastTool.empty();
      }
   }; return flag;
}
}

using namespace MenuTable;

BaseItemSharedPtr PluginMenuItems()
{
   using Options = CommandManager::Options;
   static BaseItemSharedPtr items{
   Items( "Macros",
      Section( "RepeatLast",
         // Delayed evaluation:
         [](AudacityProject &project)
         {
            const auto &lastTool = MenuManager::Get(project).mLastTool;
            TranslatableString buildMenuLabel;
            if (!lastTool.empty())
               buildMenuLabel = XO("Repeat %s")
                  .Format( EffectManager::Get().GetCommandName(lastTool) );
            else
               buildMenuLabel = XO("Repeat Last Tool");

            return Command( wxT("RepeatLastTool"), buildMenuLabel,
               OnRepeatLastTool,
               AudioIONotBusyFlag() |
                  HasLastToolFlag(),
               Options{}.IsGlobal() );
         }
      ),

      Command( wxT("ManageMacros"), XXO("&Macro Manager"),
         OnManageMacros, AudioIONotBusyFlag() ),

      Menu( wxT("Macros"), XXO("&Apply Macro"),
         // Palette has no access key to ensure first letter navigation of
         // sub menu
         Section( "",
            Command( wxT("ApplyMacrosPalette"), XXO("Palette..."),
               OnApplyMacrosPalette, AudioIONotBusyFlag() )
         ),

         Section( "",
            // Delayed evaluation:
            [](AudacityProject&)
            { return Items( wxEmptyString, PopulateMacrosMenu( AudioIONotBusyFlag() ) ); }
         )
      )
   ) };
   return items;
}

AttachedItem sAttachment1{
   wxT("Tools/Manage"),
   Shared( PluginMenuItems() )
};

BaseItemSharedPtr ExtraScriptablesIMenu()
{
   // These are the more useful to VI user Scriptables.
   static BaseItemSharedPtr menu{
   // i18n-hint: Scriptables are commands normally used from Python, Perl etc.
   Menu( wxT("Scriptables1"), XXO("Script&ables I") )
   };
   return menu;
}

AttachedItem sAttachment2{
   wxT("Optional/Extra/Part2"),
   Shared( ExtraScriptablesIMenu() )
};

BaseItemSharedPtr ExtraScriptablesIIMenu()
{
   // Less useful to VI users.
   static BaseItemSharedPtr menu{
   // i18n-hint: Scriptables are commands normally used from Python, Perl etc.
   Menu( wxT("Scriptables2"), XXO("Scripta&bles II") )
   };
   return menu;
}

AttachedItem sAttachment3{
   wxT("Optional/Extra/Part2"),
   Shared( ExtraScriptablesIIMenu() )
};
