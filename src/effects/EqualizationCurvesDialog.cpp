/**********************************************************************

   Audacity: A Digital Audio Editor

   EqualizationCurvesDialog.cpp

   Mitch Golden
   Vaughan Johnson (Preview)
   Martyn Shaw (FIR filters, response curve, graphic EQ)

   Paul Licameli split from Equalization.cpp

**********************************************************************/
#include "EqualizationCurvesDialog.h"

#include <wx/listctrl.h>
#include "ShuttleGui.h"
#include "AudacityTextEntryDialog.h"

BEGIN_EVENT_TABLE(EqualizationCurvesDialog, wxDialogWrapper)
   EVT_BUTTON(UpButtonID, EqualizationCurvesDialog::OnUp)
   EVT_BUTTON(DownButtonID, EqualizationCurvesDialog::OnDown)
   EVT_BUTTON(RenameButtonID, EqualizationCurvesDialog::OnRename)
   EVT_BUTTON(DeleteButtonID, EqualizationCurvesDialog::OnDelete)
   EVT_BUTTON(ImportButtonID, EqualizationCurvesDialog::OnImport)
   EVT_BUTTON(ExportButtonID, EqualizationCurvesDialog::OnExport)
   EVT_BUTTON(LibraryButtonID, EqualizationCurvesDialog::OnLibrary)
   EVT_BUTTON(DefaultsButtonID, EqualizationCurvesDialog::OnDefaults)
   EVT_BUTTON(wxID_OK, EqualizationCurvesDialog::OnOK)
   EVT_LIST_ITEM_SELECTED(CurvesListID,
                          EqualizationCurvesDialog::OnListSelectionChange)
   EVT_LIST_ITEM_DESELECTED(CurvesListID,
                          EqualizationCurvesDialog::OnListSelectionChange)
END_EVENT_TABLE()

EqualizationCurvesDialog::EqualizationCurvesDialog(wxWindow * parent,
   const TranslatableString &name, int options,
   EQCurveArray &curves, int position
)  : wxDialogWrapper(parent, wxID_ANY, XO("Manage Curves List"),
      wxDefaultPosition, wxDefaultSize,
      wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
   , mName{ name }
   , mOptions{ options }
   , mCurves{ curves }
{
   SetLabel(XO("Manage Curves"));         // Provide visual label
   SetName(XO("Manage Curves List"));     // Provide audible label
   mParent = parent;
   mPosition = position;
   // make a copy of curves here to muck about with.
   mEditCurves = curves;

   Populate();
   SetMinSize(GetSize());
}

EqualizationCurvesDialog::~EqualizationCurvesDialog()
{
}

/// Creates the dialog and its contents.
void EqualizationCurvesDialog::Populate()
{
   //------------------------- Main section --------------------
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

/// Defines the dialog and does data exchange with it.
void EqualizationCurvesDialog::PopulateOrExchange(ShuttleGui & S)
{
   S.StartHorizontalLay(wxEXPAND);
   {
      S.StartStatic(XO("&Curves"), 1);
      {
         mList = S.Id(CurvesListID)
            .Style(wxSUNKEN_BORDER | wxLC_REPORT | wxLC_HRULES | wxLC_VRULES )
            .AddListControlReportMode({
               { XO("Curve Name"), wxLIST_FORMAT_RIGHT }
            });
      }
      S.EndStatic();
      S.StartVerticalLay(0);
      {
         S.Id(UpButtonID).AddButton(XXO("Move &Up"), wxALIGN_LEFT);
         S.Id(DownButtonID).AddButton(XXO("Move &Down"), wxALIGN_LEFT);
         S.Id(RenameButtonID).AddButton(XXO("&Rename..."), wxALIGN_LEFT);
         S.Id(DeleteButtonID).AddButton(XXO("D&elete..."), wxALIGN_LEFT);
         S.Id(ImportButtonID).AddButton(XXO("I&mport..."), wxALIGN_LEFT);
         S.Id(ExportButtonID).AddButton(XXO("E&xport..."), wxALIGN_LEFT);
         S.Id(LibraryButtonID).AddButton(XXO("&Get More..."), wxALIGN_LEFT);
         S.Id(DefaultsButtonID).AddButton(XXO("De&faults"), wxALIGN_LEFT);
      }
      S.EndVerticalLay();
   }
   S.EndHorizontalLay();
   S.AddStandardButtons();
   S.StartStatic(XO("Help"));
   S.AddConstTextBox( {}, XO("Rename 'unnamed' to save a new entry.\n'OK' saves all changes, 'Cancel' doesn't."));
   S.EndStatic();
   PopulateList(mPosition);
   Fit();

   return;
}

void EqualizationCurvesDialog::PopulateList(int position)
{
   mList->DeleteAllItems();
   for (unsigned int i = 0; i < mEditCurves.size(); i++)
      mList->InsertItem(i, mEditCurves[i].Name);
   mList->SetColumnWidth(0, wxLIST_AUTOSIZE);
   int curvesWidth = mList->GetColumnWidth(0);
   mList->SetColumnWidth(0, wxLIST_AUTOSIZE_USEHEADER);
   int headerWidth = mList->GetColumnWidth(0);
   mList->SetColumnWidth(0, wxMax(headerWidth, curvesWidth));
   // use 'position' to set focus
   mList->EnsureVisible(position);
   mList->SetItemState(position, wxLIST_STATE_SELECTED|wxLIST_STATE_FOCUSED, wxLIST_STATE_SELECTED|wxLIST_STATE_FOCUSED);
}

void EqualizationCurvesDialog::OnUp(wxCommandEvent & WXUNUSED(event))
{
   long item = mList->GetNextItem(-1, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED);
   if ( item == -1 )
      return;  // no items selected
   if( item == 0 )
      item = mList->GetNextItem(item, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED); // top item selected, can't move up
   int state;
   while( item != -1 )
   {
      if ( item == mList->GetItemCount()-1)
      {  // 'unnamed' always stays at the bottom
         EQUtils::DoMessageBox(mName,
            XO("'unnamed' always stays at the bottom of the list"),
            XO("'unnamed' is special") );   // these could get tedious!
         return;
      }
      state = mList->GetItemState(item-1, wxLIST_STATE_SELECTED);
      if ( state != wxLIST_STATE_SELECTED )
      { // swap this with one above but only if it isn't selected
         EQCurve temp(wxT("temp"));
         temp.Name = mEditCurves[item].Name;
         temp.points = mEditCurves[item].points;
         mEditCurves[item].Name = mEditCurves[item-1].Name;
         mEditCurves[item].points = mEditCurves[item-1].points;
         mEditCurves[item-1].Name = temp.Name;
         mEditCurves[item-1].points = temp.points;
         wxString sTemp = mList->GetItemText(item);
         mList->SetItem(item, 0, mList->GetItemText(item-1));
         mList->SetItem(item-1, 0, sTemp);
         mList->SetItemState(item, 0, wxLIST_STATE_SELECTED);
         mList->SetItemState(item-1, wxLIST_STATE_SELECTED, wxLIST_STATE_SELECTED);
      }
      item = mList->GetNextItem(item, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED);
   }
}

void EqualizationCurvesDialog::OnDown(wxCommandEvent & WXUNUSED(event))
{  // looks harder than OnUp as we need to seek backwards up the list, hence GetPreviousItem
   long item = GetPreviousItem(mList->GetItemCount());
   if( item == -1 )
      return;  // nothing selected
   int state;
   while( item != -1 )
   {
      if( (item != mList->GetItemCount()-1) && (item != mList->GetItemCount()-2) )
      {  // can't move 'unnamed' down, or the one above it
         state = mList->GetItemState(item+1, wxLIST_STATE_SELECTED);
         if ( state != wxLIST_STATE_SELECTED )
         { // swap this with one below but only if it isn't selected
            EQCurve temp(wxT("temp"));
            temp.Name = mEditCurves[item].Name;
            temp.points = mEditCurves[item].points;
            mEditCurves[item].Name = mEditCurves[item+1].Name;
            mEditCurves[item].points = mEditCurves[item+1].points;
            mEditCurves[item+1].Name = temp.Name;
            mEditCurves[item+1].points = temp.points;
            wxString sTemp = mList->GetItemText(item);
            mList->SetItem(item, 0, mList->GetItemText(item+1));
            mList->SetItem(item+1, 0, sTemp);
            mList->SetItemState(item, 0, wxLIST_STATE_SELECTED);
            mList->SetItemState(item+1, wxLIST_STATE_SELECTED, wxLIST_STATE_SELECTED);
         }
      }
      item = GetPreviousItem(item);
   }
}

long EqualizationCurvesDialog::GetPreviousItem(long item)  // wx doesn't have this
{
   long lastItem = -1;
   long itemTemp = mList->GetNextItem(-1, wxLIST_NEXT_ALL,
      wxLIST_STATE_SELECTED);
   while( (itemTemp != -1) && (itemTemp < item) )
   {
      lastItem = itemTemp;
      itemTemp = mList->GetNextItem(itemTemp, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED);
   }
   return lastItem;
}

// Rename curve/curves
void EqualizationCurvesDialog::OnRename(wxCommandEvent & WXUNUSED(event))
{
   wxString name;
   int numCurves = mEditCurves.size();
   int curve = 0;

   // Setup list of characters that aren't allowed
   wxArrayStringEx exclude{
      wxT("<") ,
      wxT(">") ,
      wxT("'") ,
      wxT("\"") ,
   };

   // Get the first one to be renamed
   long item = mList->GetNextItem(-1, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED);
   long firstItem = item;  // for reselection with PopulateList
   while(item >= 0)
   {
      // Prompt the user until a valid name is enter or cancelled
      bool overwrite = false;
      bool bad = true;
      while( bad )   // Check for an unacceptable duplicate
      {   // Show the dialog and bail if the user cancels
         bad = false;
         // build the dialog
         AudacityTextEntryDialog dlg( this,
            XO("Rename '%s' to...").Format( mEditCurves[ item ].Name ),
            XO("Rename...") );
         dlg.SetTextValidator( wxFILTER_EXCLUDE_CHAR_LIST );
         dlg.SetName(
            wxString::Format( _("Rename '%s'"), mEditCurves[ item ].Name ) );
         wxTextValidator *tv = dlg.GetTextValidator();
         tv->SetExcludes( exclude );   // Tell the validator about excluded chars
         if( dlg.ShowModal() == wxID_CANCEL )
         {
            bad = true;
            break;
         }

         // Extract the name from the dialog
         name = dlg.GetValue();

         // Search list of curves for a duplicate name
         for( curve = 0; curve < numCurves; curve++ )
         {
            wxString temp = mEditCurves[ curve ].Name;
            if( name ==  mEditCurves[ curve ].Name ) // case sensitive
            {
               bad = true;
               if( curve == item )  // trying to rename a curve with the same name
               {
                  EQUtils::DoMessageBox(mName,
                     XO("Name is the same as the original one"),
                     XO("Same name"),
                     wxOK );
                  break;
               }
               int answer = EQUtils::DoMessageBox(mName,
                  XO("Overwrite existing curve '%s'?").Format( name ),
                  XO("Curve exists"),
                  wxYES_NO);
               if (answer == wxYES)
               {
                  bad = false;
                  overwrite = true; // we are going to overwrite the one with this name
                  break;
               }
            }
         }
         if( name.empty() || name == wxT("unnamed") )
            bad = true;
      }

      // if bad, we cancelled the rename dialog, so nothing to do.
      if( bad == true )
         ;
      else if(overwrite){
         // Overwrite another curve.
         // JKC: because 'overwrite' is true, 'curve' is the number of the curve that
         // we are about to overwrite.
         mEditCurves[ curve ].Name = name;
         mEditCurves[ curve ].points = mEditCurves[ item ].points;
         // if renaming the unnamed item, then select it,
         // otherwise get rid of the item we've renamed.
         if( item == (numCurves-1) )
            mList->SetItem(curve, 0, name);
         else
         {
            mEditCurves.erase( mEditCurves.begin() + item );
            numCurves--;
         }
      }
      else if( item == (numCurves-1) ) // renaming 'unnamed'
      {  // Create a NEW entry
         mEditCurves.push_back( EQCurve( wxT("unnamed") ) );
         // Copy over the points
         mEditCurves[ numCurves ].points = mEditCurves[ numCurves - 1 ].points;
         // Give the original unnamed entry the NEW name
         mEditCurves[ numCurves - 1 ].Name = name;
         numCurves++;
      }
      else  // just rename (the 'normal' case)
      {
         mEditCurves[ item ].Name = name;
         mList->SetItem(item, 0, name);
      }
      // get next selected item
      item = mList->GetNextItem(item, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED);
   }

   PopulateList(firstItem);  // Note: only saved to file when you OK out of the dialog
   return;
}

// Delete curve/curves
void EqualizationCurvesDialog::OnDelete(wxCommandEvent & WXUNUSED(event))
{
   // We could count them here
   // And then put in a 'Delete N items?' prompt.

#if 0 // 'one at a time' prompt code
   // Get the first one to be deleted
   long item = mList->GetNextItem(-1, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED);
   // Take care, mList and mEditCurves will get out of sync as curves are deleted
   int deleted = 0;
   long highlight = -1;

   while(item >= 0)
   {
      if(item == mList->GetItemCount()-1)   //unnamed
      {
         mEffect->Effect::MessageBox(
            XO("You cannot delete the 'unnamed' curve."),
            wxOK | wxCENTRE,
            XO("Can't delete 'unnamed'") );
      }
      else
      {
         // Create the prompt
         auto quest = XO("Delete '%s'?")
            .Format(mEditCurves[ item-deleted ].Name));

         // Ask for confirmation before removal
         int ans = mEffect->Effect::MessageBox(
            quest,
            wxYES_NO | wxCENTRE,
            XO("Confirm Deletion") );
         if( ans == wxYES )
         {  // Remove the curve from the array
            mEditCurves.RemoveAt( item-deleted );
            deleted++;
         }
         else
            highlight = item-deleted;  // if user presses 'No', select that curve
      }
      // get next selected item
      item = mList->GetNextItem(item, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED);
   }

   if(highlight == -1)
      PopulateList(mEditCurves.size()-1);   // set 'unnamed' as the selected curve
   else
      PopulateList(highlight);   // user said 'No' to deletion
#else // 'DELETE all N' code
   int count = mList->GetSelectedItemCount();
   long item = mList->GetNextItem(-1, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED);
   // Create the prompt
   TranslatableString quest;
   if( count > 1 )
      quest = XO("Delete %d items?").Format( count );
   else
      if( count == 1 )
         quest = XO("Delete '%s'?").Format( mEditCurves[ item ].Name );
      else
         return;
   // Ask for confirmation before removal
   int ans = EQUtils::DoMessageBox(mName,
      quest,
      XO("Confirm Deletion"),
      wxYES_NO | wxCENTRE );
   if( ans == wxYES )
   {  // Remove the curve(s) from the array
      // Take care, mList and mEditCurves will get out of sync as curves are deleted
      int deleted = 0;
      while(item >= 0)
      {
         // TODO: Migrate to the standard "Manage" dialog.
         if(item == mList->GetItemCount()-1)   //unnamed
         {
            EQUtils::DoMessageBox(mName,
               XO("You cannot delete the 'unnamed' curve, it is special."),
               XO("Can't delete 'unnamed'"));
         }
         else
         {
            mEditCurves.erase( mEditCurves.begin() + item - deleted );
            deleted++;
         }
         item = mList->GetNextItem(item, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED);
      }
      PopulateList(mEditCurves.size() - 1);   // set 'unnamed' as the selected curve
   }
#endif
}

static const FileNames::FileTypes &XMLtypes()
{
   static const FileNames::FileTypes results{
      FileNames::XMLFiles
   };
   return results;
}

void EqualizationCurvesDialog::OnImport( wxCommandEvent & WXUNUSED(event))
{
   FileDialogWrapper filePicker(
      this,
      XO("Choose an EQ curve file"), FileNames::DataDir(), wxT(""),
      XMLtypes() );
   wxString fileName;
   if( filePicker.ShowModal() == wxID_CANCEL)
      return;
   else
      fileName = filePicker.GetPath();
   EQCurveReader{ mEditCurves, mName, mOptions }
      .LoadCurves(fileName, true);
   PopulateList(0);  // update the EqualizationCurvesDialog dialog
   return;
}

void EqualizationCurvesDialog::OnExport( wxCommandEvent & WXUNUSED(event))
{
   FileDialogWrapper filePicker(this, XO("Export EQ curves as..."),
      FileNames::DataDir(), wxT(""),
      XMLtypes(),
      wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER); // wxFD_CHANGE_DIR?
   wxString fileName;
   if( filePicker.ShowModal() == wxID_CANCEL)
      return;
   else
      fileName = filePicker.GetPath();

   EQCurveArray exportCurves;   // Copy selected curves to export
   exportCurves.clear();
   long item = mList->GetNextItem(-1, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED);
   int i=0;
   while(item >= 0)
   {
      if(item != mList->GetItemCount()-1)   // not 'unnamed'
      {
         exportCurves.push_back(mEditCurves[item].Name);
         exportCurves[i].points = mEditCurves[item].points;
         i++;
      }
      else
         EQUtils::DoMessageBox(mName,
            XO("You cannot export 'unnamed' curve, it is special."),
            XO("Cannot Export 'unnamed'") );
      // get next selected item
      item = mList->GetNextItem(item, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED);
   }
   if(i>0)
   {
      EQCurveWriter{ exportCurves }.SaveCurves(fileName);
      auto message = XO("%d curves exported to %s").Format( i, fileName );
      EQUtils::DoMessageBox(mName,
         message,
         XO("Curves exported") );
   }
   else
      EQUtils::DoMessageBox(mName,
         XO("No curves exported"),
         XO("No curves exported") );
}

void EqualizationCurvesDialog::OnLibrary( wxCommandEvent & WXUNUSED(event))
{
   // full path to wiki.
   wxLaunchDefaultBrowser(wxT("https://wiki.audacityteam.org/wiki/EQCurvesDownload"));
}

void EqualizationCurvesDialog::OnDefaults( wxCommandEvent & WXUNUSED(event))
{
   // we expect this to fail in LoadCurves (due to a lack of path) and handle that there
   EQCurveReader{ mEditCurves, mName, mOptions }
      .LoadCurves( wxT("EQDefaultCurves.xml") );
   PopulateList(0);  // update the EqualizationCurvesDialog dialog
}

void EqualizationCurvesDialog::OnOK(wxCommandEvent & WXUNUSED(event))
{
   {
      // Make a backup of the current curves
      wxString backupPlace =
         wxFileName( FileNames::DataDir(), wxT("EQBackup.xml") ).GetFullPath();
      EQCurveWriter writer{ mCurves };
      writer.SaveCurves(backupPlace);
      // Load back into the main dialog
      mCurves = mEditCurves;
      // Save to default place
      writer.SaveCurves();
   } // scope of writer
   EQCurveReader{ mCurves, mName, mOptions }.LoadCurves();

   // Select something sensible
   long item = mList->GetNextItem(-1,
      wxLIST_NEXT_ALL,
      wxLIST_STATE_SELECTED);
   if (item == -1)
      item = mList->GetItemCount()-1;   // nothing selected, default to 'unnamed'
   mItem = item;
   EndModal(true);
}

void EqualizationCurvesDialog::OnListSelectionChange( wxListEvent & )
{
   const bool enable = mList->GetSelectedItemCount() > 0;
   static const int ids[] = {
      UpButtonID,
      DownButtonID,
      RenameButtonID,
      DeleteButtonID,
   };
   for (auto id : ids)
      FindWindowById(id, this)->Enable(enable);
}
