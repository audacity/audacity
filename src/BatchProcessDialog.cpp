/**********************************************************************

  Audacity: A Digital Audio Editor

  BatchProcessDialog.cpp

  Dominic Mazzoni
  James Crook

*******************************************************************//*!

\class BatchProcessDialog
\brief Shows progress in executing commands in BatchCommands.

*//*******************************************************************/

#include "Audacity.h"
#include "BatchProcessDialog.h"

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
#include <wx/msgdlg.h>
#include <wx/settings.h>

#include "ShuttleGui.h"
#include "Prefs.h"
#include "Project.h"
#include "Internat.h"
#include "commands/CommandManager.h"
#include "effects/Effect.h"
#include "../images/Arrow.xpm"
#include "../images/Empty9x16.xpm"
#include "BatchCommands.h"
#include "Track.h"
#include "UndoManager.h"

#include "Theme.h"
#include "AllThemeResources.h"

#include "FileDialog.h"
#include "import/Import.h"

#define ChainsListID       7001
#define CommandsListID     7002
#define ApplyToProjectID   7003
#define ApplyToFilesID     7004

BEGIN_EVENT_TABLE(BatchProcessDialog, wxDialogWrapper)
   EVT_BUTTON(ApplyToProjectID, BatchProcessDialog::OnApplyToProject)
   EVT_BUTTON(ApplyToFilesID, BatchProcessDialog::OnApplyToFiles)
   EVT_BUTTON(wxID_CANCEL, BatchProcessDialog::OnCancel)
END_EVENT_TABLE()

BatchProcessDialog::BatchProcessDialog(wxWindow * parent):
   wxDialogWrapper(parent, wxID_ANY, _("Apply Chain"),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
{
   //AudacityProject * p = GetActiveProject();

   SetLabel(_("Apply Chain"));         // Provide visual label
   SetName(_("Apply Chain"));          // Provide audible label
   Populate();

   mAbort = false;
}

BatchProcessDialog::~BatchProcessDialog()
{
}

void BatchProcessDialog::Populate()
{
   //------------------------- Main section --------------------
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

/// Defines the dialog and does data exchange with it.
void BatchProcessDialog::PopulateOrExchange(ShuttleGui &S)
{
   S.StartVerticalLay(true);
   {
      /*i18n-hint: A chain is a sequence of commands that can be applied
       * to one or more audio files.*/
      S.StartStatic(_("&Select Chain"), true);
      {
         S.SetStyle(wxSUNKEN_BORDER | wxLC_REPORT | wxLC_HRULES | wxLC_VRULES |
                     wxLC_SINGLE_SEL);
         mChains = S.Id(ChainsListID).AddListControlReportMode();
         mChains->InsertColumn(0, _("Chain"), wxLIST_FORMAT_LEFT);
      }
      S.EndStatic();

      S.StartHorizontalLay(wxALIGN_RIGHT, false);
      {
         S.SetBorder(10);
         S.Id(ApplyToProjectID).AddButton(_("Apply to Current &Project"));
         S.Id(ApplyToFilesID).AddButton(_("Apply to &Files..."));
         S.Id(wxID_CANCEL).AddButton(_("&Cancel"));
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();

   wxArrayString names = mBatchCommands.GetNames();
   for (int i = 0; i < (int)names.GetCount(); i++) {
      mChains->InsertItem(i, names[i]);
   }

   // Get and validate the currently active chain
   wxString name = gPrefs->Read(wxT("/Batch/ActiveChain"), wxT(""));

   int item = mChains->FindItem(-1, name);
   if (item == -1) {
      item = 0;
      name = mChains->GetItemText(0);
   }

   // Select the name in the list...this will fire an event.
   mChains->SetItemState(item, wxLIST_STATE_SELECTED, wxLIST_STATE_SELECTED);

   Layout();
   Fit();
   SetSizeHints(GetSize());
   Center();

   // Set the column size for the chains list.
   wxSize sz = mChains->GetClientSize();
   mChains->SetColumnWidth(0, sz.x);
}

void BatchProcessDialog::OnApplyToProject(wxCommandEvent & WXUNUSED(event))
{
   long item = mChains->GetNextItem(-1,
                                    wxLIST_NEXT_ALL,
                                    wxLIST_STATE_SELECTED);
   if (item == -1) {
      wxMessageBox(_("No chain selected"));
      return;
   }
   wxString name = mChains->GetItemText(item);

   wxDialog * pD = safenew wxDialogWrapper(this, wxID_ANY, GetTitle());
   pD->SetName(pD->GetTitle());
   ShuttleGui S(pD, eIsCreating);

   S.StartHorizontalLay(wxCENTER, false);
   {
      S.StartStatic(wxT(""), false);   // deliberately not translated (!)
      {
         S.SetBorder(20);
         S.AddFixedText(wxString::Format(_("Applying '%s' to current project"),
                                         name.c_str()));
      }
      S.EndStatic();
   }
   S.EndHorizontalLay();

   pD->Layout();
   pD->Fit();
   pD->CenterOnScreen();
   pD->Move(-1, 0);
   pD->Show();

   // The Hide() on the next line seems to tickle a bug in wx3,
   // giving rise to our Bug #1221.  The problem is that on Linux 
   // the 'Hide' converts us from a Modal into a regular dialog,
   // as far as closing is concerned.  On Linux we can't close with
   // EndModal() anymore after this.
   Hide();

   gPrefs->Write(wxT("/Batch/ActiveChain"), name);
   gPrefs->Flush();

   mBatchCommands.ReadChain(name);

   // The disabler must get deleted before the EndModal() call.  Otherwise,
   // the menus on OSX will remain disabled.
   bool success;
   {
      wxWindowDisabler wd(pD);
      success = mBatchCommands.ApplyChain();
   }

   if (!success) {
      Show();
      return;
   }

#if !defined(__WXMAC__)
   // Under Linux an EndModal() here crashes (Bug #1221).
   // But sending a close message instead is OK.
   wxCloseEvent Evt;
   Evt.SetId( wxID_OK );
   Evt.SetEventObject( this);
   ProcessWindowEvent( Evt );
#else
   EndModal(wxID_OK);
#endif

   // Raise myself again, and the parent window with me
   Show();
}

void BatchProcessDialog::OnApplyToFiles(wxCommandEvent & WXUNUSED(event))
{
   long item = mChains->GetNextItem(-1,
                                    wxLIST_NEXT_ALL,
                                    wxLIST_STATE_SELECTED);
   if (item == -1) {
      wxMessageBox(_("No chain selected"));
      return;
   }

   wxString name = mChains->GetItemText(item);
   gPrefs->Write(wxT("/Batch/ActiveChain"), name);
   gPrefs->Flush();

   AudacityProject *project = GetActiveProject();
   if (!project->GetIsEmpty()) {
      wxMessageBox(_("Please save and close the current project first."));
      return;
   }

   wxString path = gPrefs->Read(wxT("/DefaultOpenPath"), ::wxGetCwd());
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

   FileDialog dlog(this,
                   prompt,
                   path,
                   wxT(""),
                   mask,
                   wxFD_OPEN | wxFD_MULTIPLE | wxRESIZE_BORDER);

   dlog.SetFilterIndex(index);
   if (dlog.ShowModal() != wxID_OK) {
      return;
   }

   wxArrayString files;
   dlog.GetPaths(files);

   files.Sort();

   wxDialog * pD = safenew wxDialogWrapper(this, wxID_ANY, GetTitle());
   pD->SetName(pD->GetTitle());
   ShuttleGui S(pD, eIsCreating);

   S.StartVerticalLay(false);
   {
      S.StartStatic(_("Applying..."), 1);
      {
         auto imageList = std::make_unique<wxImageList>(9, 16);
         imageList->Add(wxIcon(empty9x16_xpm));
         imageList->Add(wxIcon(arrow_xpm));

         S.SetStyle(wxSUNKEN_BORDER | wxLC_REPORT | wxLC_HRULES | wxLC_VRULES |
                    wxLC_SINGLE_SEL);
         mList = S.Id(CommandsListID).AddListControlReportMode();
         // AssignImageList takes ownership
         mList->AssignImageList(imageList.release(), wxIMAGE_LIST_SMALL);
         mList->InsertColumn(0, _("File"), wxLIST_FORMAT_LEFT);
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
   for (i = 0; i < (int)files.GetCount(); i++ ) {
      mList->InsertItem(i, files[i], i == 0);
   }

   // Set the column size for the files list.
   mList->SetColumnWidth(0, wxLIST_AUTOSIZE);

   int width = mList->GetColumnWidth(0);
   wxSize sz = mList->GetClientSize();
   if (width > sz.GetWidth() && width < 500) {
      sz.SetWidth(width);
      mList->SetInitialSize(sz);
   }

   pD->Layout();
   pD->Fit();
   pD->SetSizeHints(pD->GetSize());
   pD->CenterOnScreen();
   pD->Move(-1, 0);
   pD->Show();
   Hide();

   mBatchCommands.ReadChain(name);
   for (i = 0; i < (int)files.GetCount(); i++) {
      wxWindowDisabler wd(pD);
      if (i > 0) {
         //Clear the arrow in previous item.
         mList->SetItemImage(i - 1, 0, 0);
      }
      mList->SetItemImage(i, 1, 1);
      mList->EnsureVisible(i);

      project->Import(files[i]);
      project->OnSelectAll();
      if (!mBatchCommands.ApplyChain()) {
         break;
      }

      if (!pD->IsShown() || mAbort) {
         break;
      }
      UndoManager *um = project->GetUndoManager();
      um->ClearStates();
      project->OnSelectAll();
      project->OnRemoveTracks();
   }
   project->OnRemoveTracks();

   // Under Linux an EndModal() here crashes (Bug #1221).
   // But sending a close message instead is OK.
#if !defined(__WXMAC__)
   wxCloseEvent Evt;
   Evt.SetId( wxID_OK );
   Evt.SetEventObject( this);
   ProcessWindowEvent( Evt );
#else
   EndModal(wxID_OK);
#endif 

   // Raise myself again, and the parent window with me
   Show();
}

void BatchProcessDialog::OnCancel(wxCommandEvent & WXUNUSED(event))
{
#if !defined(__WXMAC__)
   // It is possible that we could just do EndModal()
   // here even on Linux.  However, we know the alternative way of
   // closing works, if we are hidden, so we hide and then do that.
   Hide();
   // Under Linux an EndModal() here potentially crashes (Bug #1221).
   // But sending a close message instead is OK.
   wxCloseEvent Evt;
   Evt.SetId( wxID_CANCEL );
   Evt.SetEventObject( this);
   ProcessWindowEvent( Evt );
#else
   EndModal(wxID_CANCEL);
#endif
}

/////////////////////////////////////////////////////////////////////
#include <wx/textdlg.h>
#include "BatchCommandDialog.h"

enum {
// ChainsListID             7005
   AddButtonID = 10000,
   RemoveButtonID,
// CommandsListID,       7002
   ImportButtonID,
   ExportButtonID,
   DefaultsButtonID,
   InsertButtonID,
   DeleteButtonID,
   UpButtonID,
   DownButtonID,
   RenameButtonID
};

BEGIN_EVENT_TABLE(EditChainsDialog, wxDialogWrapper)
   EVT_LIST_ITEM_SELECTED(ChainsListID, EditChainsDialog::OnChainSelected)
   EVT_LIST_ITEM_SELECTED(CommandsListID, EditChainsDialog::OnListSelected)
   EVT_LIST_BEGIN_LABEL_EDIT(ChainsListID, EditChainsDialog::OnChainsBeginEdit)
   EVT_LIST_END_LABEL_EDIT(ChainsListID, EditChainsDialog::OnChainsEndEdit)
   EVT_BUTTON(AddButtonID, EditChainsDialog::OnAdd)
   EVT_BUTTON(RemoveButtonID, EditChainsDialog::OnRemove)
   EVT_BUTTON(RenameButtonID, EditChainsDialog::OnRename)
   EVT_SIZE(EditChainsDialog::OnSize)

   EVT_LIST_ITEM_ACTIVATED(CommandsListID, EditChainsDialog::OnCommandActivated)
   EVT_BUTTON(InsertButtonID, EditChainsDialog::OnInsert)
   EVT_BUTTON(DeleteButtonID, EditChainsDialog::OnDelete)
   EVT_BUTTON(UpButtonID, EditChainsDialog::OnUp)
   EVT_BUTTON(DownButtonID, EditChainsDialog::OnDown)
   EVT_BUTTON(DefaultsButtonID, EditChainsDialog::OnDefaults)

   EVT_BUTTON(wxID_OK, EditChainsDialog::OnOK)
   EVT_BUTTON(wxID_CANCEL, EditChainsDialog::OnCancel)

   EVT_KEY_DOWN(EditChainsDialog::OnKeyDown)
END_EVENT_TABLE()

enum {
   BlankColumn,
   ItemNumberColumn,
   ActionColumn,
   ParamsColumn,
};

/// Constructor
EditChainsDialog::EditChainsDialog(wxWindow * parent):
   wxDialogWrapper(parent, wxID_ANY, _("Edit Chains"),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
{
   SetLabel(_("Edit Chains"));         // Provide visual label
   SetName(_("Edit Chains"));          // Provide audible label

   mChanged = false;
   mSelectedCommand = 0;

   Populate();
}

EditChainsDialog::~EditChainsDialog()
{
}

/// Creates the dialog and its contents.
void EditChainsDialog::Populate()
{
   //------------------------- Main section --------------------
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------

   // Get and validate the currently active chain
   mActiveChain = gPrefs->Read(wxT("/Batch/ActiveChain"), wxT(""));

   // Go populate the chains list.
   PopulateChains();

   // We have a bare list.  We need to add columns and content.
   PopulateList();

   // Layout and set minimum size of window
   Layout();
   Fit();
   SetSizeHints(GetSize());

   // Size and place window
   SetSize(wxSystemSettings::GetMetric(wxSYS_SCREEN_X) * 3 / 4,
           wxSystemSettings::GetMetric(wxSYS_SCREEN_Y) * 4 / 5);
   Center();

   // Set the column size for the chains list.
   wxSize sz = mChains->GetClientSize();
   mChains->SetColumnWidth(0, sz.x);

   // Size columns properly
   FitColumns();
}

/// Defines the dialog and does data exchange with it.
void EditChainsDialog::PopulateOrExchange(ShuttleGui & S)
{
   S.StartHorizontalLay(wxEXPAND, 1);
   {
      S.StartStatic(_("&Chains"));
      {
         // JKC: Experimenting with an alternative way to get multiline
         // translated strings to work correctly without very long lines.
         // My appologies Alexandre if this way didn't work either.
         //
         // With this method:
         //   1) it compiles fine under windows unicode and normal mode.
         //   2) xgettext source code has handling for the trailing '\'
         //
         // It remains to see if linux and mac can cope and if xgettext
         // actually does do fine with strings presented like this.
         // If it doesn't work out, revert to all-on-one-line.
         S.SetStyle(wxSUNKEN_BORDER | wxLC_REPORT | wxLC_HRULES | wxLC_SINGLE_SEL |
                    wxLC_EDIT_LABELS);
         mChains = S.Id(ChainsListID).AddListControlReportMode();
         mChains->InsertColumn(0, wxT("Chain"), wxLIST_FORMAT_LEFT);
         S.StartHorizontalLay(wxCENTER, false);
         {
            S.Id(AddButtonID).AddButton(_("&Add"));
            mRemove = S.Id(RemoveButtonID).AddButton(_("&Remove"));
            mRename = S.Id(RenameButtonID).AddButton(_("Re&name"));
         }
         S.EndHorizontalLay();
      }
      S.EndStatic();

      S.StartStatic(_("C&hain (Double-Click or press SPACE to edit)"), true);
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

         S.StartHorizontalLay(wxCENTER, false);
         {
            S.Id(InsertButtonID).AddButton(_("&Insert"), wxALIGN_LEFT);
            S.Id(DeleteButtonID).AddButton(_("De&lete"), wxALIGN_LEFT);
            S.Id(UpButtonID).AddButton(_("Move &Up"), wxALIGN_LEFT);
            S.Id(DownButtonID).AddButton(_("Move &Down"), wxALIGN_LEFT);
            mDefaults = S.Id(DefaultsButtonID).AddButton(_("De&faults"));
         }
         S.EndHorizontalLay();
      }
      S.EndStatic();
   }
   S.EndHorizontalLay();

   S.AddStandardButtons();

   return;
}

/// This clears and updates the contents of mChains
void EditChainsDialog::PopulateChains()
{
   wxArrayString names = mBatchCommands.GetNames();
   int i;

   mChains->DeleteAllItems();
   for (i = 0; i < (int)names.GetCount(); i++) {
      mChains->InsertItem(i, names[i]);
   }

   int item = mChains->FindItem(-1, mActiveChain);
   if (item == -1) {
      item = 0;
      mActiveChain = mChains->GetItemText(0);
   }

   // Select the name in the list...this will fire an event.
   mChains->SetItemState(item, wxLIST_STATE_SELECTED, wxLIST_STATE_SELECTED);
}

/// This clears and updates the contents of mList
void EditChainsDialog::PopulateList()
{
   mList->DeleteAllItems();

   for (int i = 0; i < mBatchCommands.GetCount(); i++) {
      AddItem(mBatchCommands.GetCommand(i),
              mBatchCommands.GetParams(i));
   }
   /*i18n-hint: This is the last item in a list.*/
   AddItem(_("- END -"), wxT(""));

   // Select the name in the list...this will fire an event.
   if (mSelectedCommand >= (int)mList->GetItemCount()) {
      mSelectedCommand = 0;
   }
   mList->SetItemState(mSelectedCommand, wxLIST_STATE_SELECTED, wxLIST_STATE_SELECTED);
}

/// Add one item into mList
void EditChainsDialog::AddItem(const wxString &Action, const wxString &Params)
{
   int i = mList->GetItemCount();

   mList->InsertItem(i, wxT(""));
   mList->SetItem(i, ItemNumberColumn, wxString::Format(wxT(" %02i"), i + 1));
   mList->SetItem(i, ActionColumn, Action );
   mList->SetItem(i, ParamsColumn, Params );
}

bool EditChainsDialog::ChangeOK()
{
   if (mChanged) {
      wxString title;
      wxString msg;
      int id;

      title.Printf(_("%s changed"), mActiveChain.c_str());
      msg = _("Do you want to save the changes?");

      id = wxMessageBox(msg, title, wxYES_NO | wxCANCEL);
      if (id == wxCANCEL) {
         return false;
      }

      if (id == wxYES) {
         if (!mBatchCommands.WriteChain(mActiveChain)) {
            return false;
         }
      }

      mChanged = false;
   }

   return true;
}
/// An item in the chains list has been selected.
void EditChainsDialog::OnChainSelected(wxListEvent & event)
{
   if (!ChangeOK()) {
      event.Veto();
      return;
   }

   int item = event.GetIndex();

   mActiveChain = mChains->GetItemText(item);
   mBatchCommands.ReadChain(mActiveChain);

   if (mBatchCommands.IsFixed(mActiveChain)) {
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

/// An item in the chains list has been selected.
void EditChainsDialog::OnListSelected(wxListEvent & WXUNUSED(event))
{
   FitColumns();
}

/// The window has been resized.
void EditChainsDialog::OnSize(wxSizeEvent & WXUNUSED(event))
{
   // Refrsh the layout and re-fit the columns.
   Layout();
   FitColumns();
}

void EditChainsDialog::FitColumns()
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
void EditChainsDialog::OnChainsBeginEdit(wxListEvent &event)
{
   int itemNo = event.GetIndex();

   wxString chain = mChains->GetItemText(itemNo);

   if (mBatchCommands.IsFixed(mActiveChain)) {
      wxBell();
      event.Veto();
   }
}

///
void EditChainsDialog::OnChainsEndEdit(wxListEvent &event)
{
   if (event.IsEditCancelled()) {
      return;
   }

   wxString newname = event.GetLabel();

   mBatchCommands.RenameChain(mActiveChain, newname);

   mActiveChain = newname;

   PopulateChains();
}

///
void EditChainsDialog::OnAdd(wxCommandEvent & WXUNUSED(event))
{
   while (true) {
      wxTextEntryDialog d(this,
                          _("Enter name of new chain"),
                          _("Name of new chain"));
      d.SetName(d.GetTitle());
      wxString name;

      if (d.ShowModal() == wxID_CANCEL) {
         return;
      }

      name = d.GetValue().Strip(wxString::both);

      if (name.Length() == 0) {
         wxMessageBox(_("Name must not be blank"),
                      GetTitle(),
                      wxOK | wxICON_ERROR,
                      this);
         continue;
      }

      if (name.Contains(wxFILE_SEP_PATH) ||
          name.Contains(wxFILE_SEP_PATH_UNIX)) {
         /*i18n-hint: The %c will be replaced with 'forbidden characters', like '/' and '\'.*/
         wxMessageBox(wxString::Format(_("Names may not contain '%c' and '%c'"),
                      wxFILE_SEP_PATH, wxFILE_SEP_PATH_UNIX),
                      GetTitle(),
                      wxOK | wxICON_ERROR,
                      this);
         continue;
      }

      mBatchCommands.AddChain(name);

      mActiveChain = name;

      PopulateChains();

      break;
   }
}

///
void EditChainsDialog::OnRemove(wxCommandEvent & WXUNUSED(event))
{
   long item = mChains->GetNextItem(-1,
                                    wxLIST_NEXT_ALL,
                                    wxLIST_STATE_SELECTED);
   if (item == -1) {
      return;
   }

   wxString name = mChains->GetItemText(item);
   wxMessageDialog m(this,
   /*i18n-hint: %s will be replaced by the name of a file.*/
                     wxString::Format(_("Are you sure you want to delete %s?"), name.c_str()),
                     GetTitle(),
                     wxYES_NO | wxICON_QUESTION);
   if (m.ShowModal() == wxID_NO) {
      return;
   }

   mBatchCommands.DeleteChain(name);

   if (item >= (mChains->GetItemCount() - 1) && item >= 0) {
      item--;
   }

   mActiveChain = mChains->GetItemText(item);

   PopulateChains();
}

///
void EditChainsDialog::OnRename(wxCommandEvent & WXUNUSED(event))
{
   long item = mChains->GetNextItem(-1,
                                    wxLIST_NEXT_ALL,
                                    wxLIST_STATE_SELECTED);
   if (item == -1) {
      return;
   }

   mChains->EditLabel(item);
}

/// An item in the list has been selected.
/// Bring up a dialog to allow its parameters to be edited.
void EditChainsDialog::OnCommandActivated(wxListEvent &event)
{
   int item = event.GetIndex();

   BatchCommandDialog d(this, wxID_ANY);
   d.SetCommandAndParams(mBatchCommands.GetCommand(item),
                         mBatchCommands.GetParams(item));

   if (!d.ShowModal()) {
      return;
   }

   mBatchCommands.DeleteFromChain(item);
   mBatchCommands.AddToChain(d.mSelectedCommand,
                             d.mSelectedParameters,
                             item);

   mChanged = true;

   mSelectedCommand = item;

   PopulateList();
}

///
void EditChainsDialog::OnInsert(wxCommandEvent & WXUNUSED(event))
{
   long item = mList->GetNextItem(-1,
                                  wxLIST_NEXT_ALL,
                                  wxLIST_STATE_SELECTED);
   if (item == -1) {
      return;
   }

   BatchCommandDialog d(this, wxID_ANY);

   if (!d.ShowModal()) {
      return;
   }

   if(d.mSelectedCommand != wxT(""))
   {
      mBatchCommands.AddToChain(d.mSelectedCommand,
                                d.mSelectedParameters,
                                item);
      mChanged = true;

      mSelectedCommand = item + 1;

      PopulateList();
   }
}

///
void EditChainsDialog::OnDelete(wxCommandEvent & WXUNUSED(event))
{
   long item = mList->GetNextItem(-1,
                                  wxLIST_NEXT_ALL,
                                  wxLIST_STATE_SELECTED);
   if (item == -1 || item + 1 == mList->GetItemCount()) {
      return;
   }

   mBatchCommands.DeleteFromChain(item);

   mChanged = true;

   if (item >= (mList->GetItemCount() - 2) && item >= 0) {
      item--;
   }

   mSelectedCommand = item;

   PopulateList();
}

///
void EditChainsDialog::OnUp(wxCommandEvent & WXUNUSED(event))
{
   long item = mList->GetNextItem(-1,
                                  wxLIST_NEXT_ALL,
                                  wxLIST_STATE_SELECTED);
   if (item == -1 || item == 0 || item + 1 == mList->GetItemCount()) {
      return;
   }

   mBatchCommands.AddToChain(mBatchCommands.GetCommand(item),
                             mBatchCommands.GetParams(item),
                             item - 1);
   mBatchCommands.DeleteFromChain(item + 1);

   mChanged = true;

   mSelectedCommand = item - 1;

   PopulateList();
}

///
void EditChainsDialog::OnDown(wxCommandEvent & WXUNUSED(event))
{
   long item = mList->GetNextItem(-1,
                                  wxLIST_NEXT_ALL,
                                  wxLIST_STATE_SELECTED);
   if (item == -1 || item + 2 >= mList->GetItemCount()) {
      return;
   }

   mBatchCommands.AddToChain(mBatchCommands.GetCommand(item),
                             mBatchCommands.GetParams(item),
                             item + 2);
   mBatchCommands.DeleteFromChain(item);

   mChanged = true;

   mSelectedCommand = item + 1;

   PopulateList();
}

/// Select the empty Command chain.
void EditChainsDialog::OnDefaults(wxCommandEvent & WXUNUSED(event))
{
   mBatchCommands.RestoreChain(mActiveChain);

   mChanged = true;

   PopulateList();
}

/// Send changed values back to Prefs, and update Audacity.
void EditChainsDialog::OnOK(wxCommandEvent & WXUNUSED(event))
{
   gPrefs->Write(wxT("/Batch/ActiveChain"), mActiveChain);
   gPrefs->Flush();

   if (mChanged) {
      if (!mBatchCommands.WriteChain(mActiveChain)) {
         return;
      }
   }

   EndModal(true);
}

///
void EditChainsDialog::OnCancel(wxCommandEvent & WXUNUSED(event))
{
   if (!ChangeOK()) {
      return;
   }

   EndModal(false);
}

///
void EditChainsDialog::OnKeyDown(wxKeyEvent &event)
{
   if (event.GetKeyCode() == WXK_DELETE) {
      wxLogDebug(wxT("wxKeyEvent"));
   }

   event.Skip();
}
