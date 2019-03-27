/**********************************************************************

  Audacity: A Digital Audio Editor

  ApplyMacroDialog.cpp

  Dominic Mazzoni
  James Crook

*******************************************************************//*!

\class ApplyMacroDialog
\brief Shows progress in executing commands in MacroCommands.

*//*******************************************************************/

#include "Audacity.h"
#include "BatchProcessDialog.h"

#include <wx/setup.h> // for wxUSE_* macros
#include <wx/defs.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/filedlg.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/listctrl.h>
#include <wx/radiobut.h>
#include <wx/button.h>
#include <wx/imaglist.h>
#include <wx/settings.h>

#include "AudacityException.h"
#include "ShuttleGui.h"
#include "Menus.h"
#include "Prefs.h"
#include "Project.h"
#include "Internat.h"
#include "commands/CommandManager.h"
#include "commands/CommandContext.h"
#include "effects/Effect.h"
#include "../images/Arrow.xpm"
#include "../images/Empty9x16.xpm"
#include "BatchCommands.h"
#include "Track.h"
#include "UndoManager.h"

#include "Theme.h"
#include "AllThemeResources.h"

#include "FileDialog.h"
#include "FileNames.h"
#include "import/Import.h"
#include "widgets/ErrorDialog.h"
#include "widgets/HelpSystem.h"

#if wxUSE_ACCESSIBILITY
#include "widgets/WindowAccessible.h"
#endif

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
   EVT_BUTTON(wxID_HELP, ApplyMacroDialog::OnHelp)
END_EVENT_TABLE()

ApplyMacroDialog::ApplyMacroDialog(wxWindow * parent, bool bInherited):
   wxDialogWrapper(parent, wxID_ANY, _("Macros Palette"),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
   , mCatalog( GetActiveProject() )
{
   //AudacityProject * p = GetActiveProject();
   mAbort = false;
   mbExpanded = false;
   if( bInherited )
      return;
   SetLabel(_("Macros Palette"));         // Provide visual label
   SetName(_("Macros Palette"));          // Provide audible label
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
   S.StartStatic(_("Select Macro"), 1);
   {
      S.SetStyle(wxSUNKEN_BORDER | wxLC_REPORT | wxLC_HRULES | wxLC_VRULES |
                  wxLC_SINGLE_SEL);
      mMacros = S.Id(MacrosListID).Prop(1).AddListControlReportMode();
      mMacros->InsertColumn(0, _("Macro"), wxLIST_FORMAT_LEFT);
   }
   S.EndStatic();

   S.StartHorizontalLay(wxEXPAND, 0);
   {
      S.AddPrompt( _("Apply Macro to:") );
      wxButton* btn = S.Id(ApplyToProjectID).AddButton(_("&Project"));
#if wxUSE_ACCESSIBILITY
      // so that name can be set on a standard control
      btn->SetAccessible(safenew WindowAccessible(btn));
#endif
      btn->SetName(_("Apply macro to project"));

      btn = S.Id(ApplyToFilesID).AddButton(_("&Files..."));
#if wxUSE_ACCESSIBILITY
      // so that name can be set on a standard control
      btn->SetAccessible(safenew WindowAccessible(btn));
#endif
      btn->SetName(_("Apply macro to files..."));
   }
   S.EndHorizontalLay();

   S.StartHorizontalLay(wxEXPAND, 0);
   {
      /* i18n-hint: The Expand button makes the dialog bigger, with more in it */
      mResize = S.Id(ExpandID).AddButton(_("&Expand"));
      S.Prop(1).AddSpace( 10 );
      S.AddStandardButtons( eCancelButton | eHelpButton);
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
   wxString page = GetHelpPageName();
   HelpSystem::ShowHelp(this, page, true);
}

void ApplyMacroDialog::OnApplyToProject(wxCommandEvent & WXUNUSED(event))
{
   long item = mMacros->GetNextItem(-1,
                                    wxLIST_NEXT_ALL,
                                    wxLIST_STATE_SELECTED);

   if (item == -1) {
      AudacityMessageBox(_("No macro selected"));
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
   activityWin.SetName(activityWin.GetTitle());
   ShuttleGui S(&activityWin, eIsCreating);

   S.StartHorizontalLay(wxCENTER, false);
   {
      S.StartStatic( {}, false);   // deliberately not translated (!)
      {
         S.SetBorder(20);
         S.AddFixedText(wxString::Format(_("Applying '%s' to current project"),
                                         name));
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
      AudacityMessageBox(_("No macro selected"));
      return;
   }

   wxString name = mMacros->GetItemText(item);
   gPrefs->Write(wxT("/Batch/ActiveMacro"), name);
   gPrefs->Flush();

   AudacityProject *project = GetActiveProject();
   if (!project->GetTracks()->empty()) {
      AudacityMessageBox(_("Please save and close the current project first."));
      return;
   }

   wxString prompt =  _("Select file(s) for batch processing...");

   FormatList l;
   wxString filter;
   wxString all;

   Importer::Get().GetSupportedImportFormats(&l);
   for (const auto &format : l) {
      const Format *f = &format;

      wxString newfilter = f->formatName + wxT("|");
      for (size_t i = 0; i < f->formatExtensions.size(); i++) {
         if (!newfilter.Contains(wxT("*.") + f->formatExtensions[i] + wxT(";")))
            newfilter += wxT("*.") + f->formatExtensions[i] + wxT(";");
         if (!all.Contains(wxT("*.") + f->formatExtensions[i] + wxT(";")))
            all += wxT("*.") + f->formatExtensions[i] + wxT(";");
      }
      newfilter.RemoveLast(1);
      filter += newfilter;
      filter += wxT("|");
   }
   all.RemoveLast(1);
   filter.RemoveLast(1);

   wxString mask = _("All files|*|All supported files|") +
                   all + wxT("|") +
                   filter;

   wxString type = gPrefs->Read(wxT("/DefaultOpenType"),mask.BeforeFirst(wxT('|')));
   // Convert the type to the filter index
   int index = mask.First(type + wxT("|"));
   if (index == wxNOT_FOUND) {
      index = 0;
   }
   else {
      index = mask.Left(index).Freq(wxT('|')) / 2;
      if (index < 0) {
         index = 0;
      }
   }

   auto path = FileNames::FindDefaultPath(FileNames::Operation::Open);
   FileDialogWrapper dlog(this,
                   prompt,
                   path,
                   wxT(""),
                   mask,
                   wxFD_OPEN | wxFD_MULTIPLE | wxRESIZE_BORDER);

   dlog.SetFilterIndex(index);
   if (dlog.ShowModal() != wxID_OK) {
      Raise();
      return;
   }
   Raise();
   
   wxArrayString files;
   dlog.GetPaths(files);

   files.Sort();

   wxDialogWrapper activityWin(this, wxID_ANY, GetTitle());
   activityWin.SetName(activityWin.GetTitle());
   ShuttleGui S(&activityWin, eIsCreating);

   wxListCtrl * fileList = NULL;

   S.StartVerticalLay(false);
   {
      S.StartStatic(_("Applying..."), 1);
      {
         auto imageList = std::make_unique<wxImageList>(9, 16);
         imageList->Add(wxIcon(empty9x16_xpm));
         imageList->Add(wxIcon(arrow_xpm));

         S.SetStyle(wxSUNKEN_BORDER | wxLC_REPORT | wxLC_HRULES | wxLC_VRULES |
                    wxLC_SINGLE_SEL);
         fileList = S.Id(CommandsListID).AddListControlReportMode();
         // AssignImageList takes ownership
         fileList->AssignImageList(imageList.release(), wxIMAGE_LIST_SMALL);
         fileList->InsertColumn(0, _("File"), wxLIST_FORMAT_LEFT);
      }
      S.EndStatic();

      S.StartHorizontalLay(wxCENTER, false);
      {
         S.Id(wxID_CANCEL).AddButton(_("&Cancel"));
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

   int width = fileList->GetColumnWidth(0);
   wxSize sz = fileList->GetClientSize();
   if (width > sz.GetWidth() && width < 500) {
      sz.SetWidth(width);
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
   for (i = 0; i < (int)files.size(); i++) {
      wxWindowDisabler wd(&activityWin);
      if (i > 0) {
         //Clear the arrow in previous item.
         fileList->SetItemImage(i - 1, 0, 0);
      }
      fileList->SetItemImage(i, 1, 1);
      fileList->EnsureVisible(i);

      auto success = GuardedCall< bool >( [&] {
         project->Import(files[i]);
         project->ZoomAfterImport(nullptr);
         SelectActions::DoSelectAll(*project);
         if (!mMacroCommands.ApplyMacro(mCatalog))
            return false;

         if (!activityWin.IsShown() || mAbort)
            return false;

         return true;
      } );

      if (!success)
         break;
      
      project->ResetProjectToEmpty();
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
   ImportButtonID,
   ExportButtonID,
   DefaultsButtonID,
   InsertButtonID,
   EditButtonID,
   DeleteButtonID,
   UpButtonID,
   DownButtonID,
   RenameButtonID,
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
   EVT_BUTTON(ExpandID, MacrosWindow::OnExpand)
   EVT_BUTTON(ShrinkID, MacrosWindow::OnShrink)

   EVT_SIZE(MacrosWindow::OnSize)

   EVT_LIST_ITEM_ACTIVATED(CommandsListID, MacrosWindow::OnCommandActivated)
   EVT_BUTTON(InsertButtonID, MacrosWindow::OnInsert)
   EVT_BUTTON(EditButtonID, MacrosWindow::OnEditCommandParams)
   EVT_BUTTON(DeleteButtonID, MacrosWindow::OnDelete)
   EVT_BUTTON(UpButtonID, MacrosWindow::OnUp)
   EVT_BUTTON(DownButtonID, MacrosWindow::OnDown)
   EVT_BUTTON(DefaultsButtonID, MacrosWindow::OnDefaults)

   EVT_BUTTON(wxID_OK, MacrosWindow::OnOK)
   EVT_BUTTON(wxID_CANCEL, MacrosWindow::OnCancel)

   EVT_KEY_DOWN(MacrosWindow::OnKeyDown)
END_EVENT_TABLE()

enum {
   BlankColumn,
   ItemNumberColumn,
   ActionColumn,
   ParamsColumn,
};

/// Constructor
MacrosWindow::MacrosWindow(wxWindow * parent, bool bExpanded):
   ApplyMacroDialog(parent, true)
{
   mbExpanded = bExpanded;
   wxString Title = mbExpanded ? _("Manage Macros") : _("Macros Palette");
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
      S.StartStatic(_("Select Macro"),0);
      {
         S.StartHorizontalLay(wxEXPAND,1);
         {
            S.SetStyle(wxSUNKEN_BORDER | wxLC_REPORT | wxLC_HRULES | wxLC_SINGLE_SEL |
                        wxLC_EDIT_LABELS);
            mMacros = S.Id(MacrosListID).Prop(1).AddListControlReportMode();
            // i18n-hint: This is the heading for a column in the edit macros dialog
            mMacros->InsertColumn(0, _("Macro"), wxLIST_FORMAT_LEFT);
            S.StartVerticalLay(wxALIGN_TOP, 0);
            {
               S.Id(AddButtonID).AddButton(_("&New"));
               mRemove = S.Id(RemoveButtonID).AddButton(_("Remo&ve"));
               mRename = S.Id(RenameButtonID).AddButton(_("&Rename..."));
// Not yet ready for prime time.
#if 0
               S.Id(ImportButtonID).AddButton(_("I&mport..."))->Enable( false);
               S.Id(ExportButtonID).AddButton(_("E&xport..."))->Enable( false);
#endif
            }
            S.EndVerticalLay();
         }
         S.EndHorizontalLay();
      }
      S.EndStatic();

      S.StartStatic(_("Edit Steps"), true);
      {
         S.StartHorizontalLay(wxEXPAND,1);
         {
            
            S.SetStyle(wxSUNKEN_BORDER | wxLC_REPORT | wxLC_HRULES | wxLC_VRULES |
                        wxLC_SINGLE_SEL);
            mList = S.Id(CommandsListID).AddListControlReportMode();

            //An empty first column is a workaround - under Win98 the first column
            //can't be right aligned.
            mList->InsertColumn(BlankColumn, wxT(""), wxLIST_FORMAT_LEFT);
            /* i18n-hint: This is the number of the command in the list */
            mList->InsertColumn(ItemNumberColumn, _("Num"), wxLIST_FORMAT_RIGHT);
            mList->InsertColumn(ActionColumn, _("Command  "), wxLIST_FORMAT_RIGHT);
            mList->InsertColumn(ParamsColumn, _("Parameters"), wxLIST_FORMAT_LEFT);

            S.StartVerticalLay(wxALIGN_TOP, 0);
            {
               S.Id(InsertButtonID).AddButton(_("&Insert"), wxALIGN_LEFT);
               S.Id(EditButtonID).AddButton(_("&Edit..."), wxALIGN_LEFT);
               S.Id(DeleteButtonID).AddButton(_("De&lete"), wxALIGN_LEFT);
               S.Id(UpButtonID).AddButton(_("Move &Up"), wxALIGN_LEFT);
               S.Id(DownButtonID).AddButton(_("Move &Down"), wxALIGN_LEFT);
               mDefaults = S.Id(DefaultsButtonID).AddButton(_("De&faults"));
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
      mResize = S.Id(ShrinkID).AddButton(_("Shrin&k"));
      // Using variable text just to get the positioning options.
      S.Prop(0).AddVariableText( _("Apply Macro to:"), false, wxALL | wxALIGN_CENTRE_VERTICAL );
      wxButton* btn = S.Id(ApplyToProjectID).AddButton(_("&Project"));
#if wxUSE_ACCESSIBILITY
      // so that name can be set on a standard control
      btn->SetAccessible(safenew WindowAccessible(btn));
#endif
      btn->SetName(_("Apply macro to project"));

      btn = S.Id(ApplyToFilesID).AddButton(_("&Files..."));
#if wxUSE_ACCESSIBILITY
      // so that name can be set on a standard control
      btn->SetAccessible(safenew WindowAccessible(btn));
#endif
      btn->SetName(_("Apply macro to files..."));
      S.Prop(1).AddSpace( 10 );
      S.AddStandardButtons( eOkButton | eCancelButton | eHelpButton);
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
      ? entry->name.Translated()
      :
         // Expose an internal name to the user in default of any friendly name
         // -- AVOID THIS!
        Action;

   int i = mList->GetItemCount();

   mList->InsertItem(i, wxT(""));
   mList->SetItem(i, ItemNumberColumn, wxString::Format(wxT(" %02i"), i + 1));
   mList->SetItem(i, ActionColumn, friendlyName );
   mList->SetItem(i, ParamsColumn, Params );
}

void MacrosWindow::UpdateMenus()
{
   // OK even on mac, as dialog is modal.
   auto p = GetActiveProject();
   GetMenuManager(*p).RebuildMenuBar(*p);
}

void MacrosWindow::UpdateDisplay( bool bExpanded )
{
   if( bExpanded == mbExpanded )
      return;

   if( !SaveChanges() )
      return;

   mbExpanded = bExpanded;
   DestroyChildren();
   SetSizer( nullptr );
   
   mChanged = false;
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

   wxString Title = mbExpanded ? _("Manage Macros") : _("Macros Palette");
   SetLabel( Title );         // Provide visual label
   SetName( Title );          // Provide audible label
   SetTitle( Title );
}

void MacrosWindow::OnExpand(wxCommandEvent &WXUNUSED(event))
{  UpdateDisplay( true );}

void MacrosWindow::OnShrink(wxCommandEvent &WXUNUSED(event))
{  UpdateDisplay( false );}


bool MacrosWindow::ChangeOK()
{
   if (mChanged) {
      wxString title;
      wxString msg;
      int id;

      title.Printf(_("%s changed"), mActiveMacro);
      msg = _("Do you want to save the changes?");

      id = AudacityMessageBox(msg, title, wxYES_NO | wxCANCEL);
      if (id == wxCANCEL) {
         return false;
      }

      if (id == wxYES) {
         if (!mMacroCommands.WriteMacro(mActiveMacro)) {
            return false;
         }
      }

      mChanged = false;
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
   mMacroCommands.ReadMacro(mActiveMacro);
   if( !mbExpanded )
      return;
   
   if (mMacroCommands.IsFixed(mActiveMacro)) {
      mRemove->Disable();
      mRename->Disable();
      mDefaults->Enable();
   }
   else {
      mRemove->Enable();
      mRename->Enable();
      mDefaults->Disable();
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
   // Refrsh the layout and re-fit the columns.
   Layout();
   if( !mbExpanded )
      return;
   FitColumns();
}

void MacrosWindow::FitColumns()
{
   mList->SetColumnWidth(0, 0);  // First column width is zero, to hide it.

#if defined(__WXMAC__)
   // wxMac uses a hard coded width of 150 when wxLIST_AUTOSIZE_USEHEADER
   // is specified, so we calculate the width ourselves. This method may
   // work equally well on other platforms.
   for (size_t c = 1; c < mList->GetColumnCount(); c++) {
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
   mList->SetColumnWidth(1, wxLIST_AUTOSIZE_USEHEADER);
   mList->SetColumnWidth(2, wxLIST_AUTOSIZE_USEHEADER);
   mList->SetColumnWidth(3, wxLIST_AUTOSIZE);
#endif

   int bestfit = mList->GetColumnWidth(3);
   int clientsize = mList->GetClientSize().GetWidth();
   int col1 = mList->GetColumnWidth(1);
   int col2 = mList->GetColumnWidth(2);
   bestfit = (bestfit > clientsize-col1-col2)? bestfit : clientsize-col1-col2;
   mList->SetColumnWidth(3, bestfit);

}

///
void MacrosWindow::OnMacrosBeginEdit(wxListEvent &event)
{
   int itemNo = event.GetIndex();

   wxString macro = mMacros->GetItemText(itemNo);

   if (mMacroCommands.IsFixed(mActiveMacro)) {
      wxBell();
      event.Veto();
   }
}

///
void MacrosWindow::OnMacrosEndEdit(wxListEvent &event)
{
   if (event.IsEditCancelled()) {
      return;
   }

   wxString newname = event.GetLabel();

   mMacroCommands.RenameMacro(mActiveMacro, newname);

   mActiveMacro = newname;

   PopulateMacros();
}

///
void MacrosWindow::OnAdd(wxCommandEvent & WXUNUSED(event))
{
   while (true) {
      AudacityTextEntryDialog d(this,
                          _("Enter name of new macro"),
                          _("Name of new macro"));
      d.SetName(d.GetTitle());
      wxString name;

      if (d.ShowModal() == wxID_CANCEL) {
         Raise();
         return;
      }
      Raise();

      name = d.GetValue().Strip(wxString::both);

      if (name.length() == 0) {
         AudacityMessageBox(_("Name must not be blank"),
                      GetTitle(),
                      wxOK | wxICON_ERROR,
                      this);
         continue;
      }

      if (name.Contains(wxFILE_SEP_PATH) ||
          name.Contains(wxFILE_SEP_PATH_UNIX)) {
         /*i18n-hint: The %c will be replaced with 'forbidden characters', like '/' and '\'.*/
         AudacityMessageBox(wxString::Format(_("Names may not contain '%c' and '%c'"),
                      wxFILE_SEP_PATH, wxFILE_SEP_PATH_UNIX),
                      GetTitle(),
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
   AudacityMessageDialog m(this,
   /*i18n-hint: %s will be replaced by the name of a file.*/
                     wxString::Format(_("Are you sure you want to delete %s?"), name),
                     GetTitle(),
                     wxYES_NO | wxICON_QUESTION);
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

   MacroCommandDialog d(this, wxID_ANY);

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

   params = MacroCommands::PromptForParamsFor(command, params, this).Trim();
   Raise();

   mMacroCommands.DeleteFromMacro(item);
   mMacroCommands.AddToMacro(command,
                             params,
                             item);
   mChanged = true;
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

/// Select the empty Command macro.
void MacrosWindow::OnDefaults(wxCommandEvent & WXUNUSED(event))
{
   mMacroCommands.RestoreMacro(mActiveMacro);

   mChanged = true;

   PopulateList();
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
   if (!ChangeOK()) {
      return;
   }
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
