/**********************************************************************

  Audacity: A Digital Audio Editor

  ExtImportPrefs.cpp

  LRN

*******************************************************************//**

\class ExtImportPrefs
\brief A PrefsPanel used to select extended import filter options.

*//*******************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>

#include "../Prefs.h"
#include "../ShuttleGui.h"

#include "ExtImportPrefs.h"

#include <wx/arrimpl.cpp> // this is a magic incantation which must be done!

#define EXTIMPORT_MIME_SUPPORT 0

WX_DEFINE_OBJARRAY(ExtImportItems);

enum ExtImportPrefsControls
{
  EIPPluginList = 20000,
  EIPRuleTable,
  EIPAddRule,
  EIPDelRule
};

BEGIN_EVENT_TABLE(ExtImportPrefs, PrefsPanel)
   EVT_LIST_KEY_DOWN(EIPPluginList,ExtImportPrefs::OnPluginKeyDown)
   EVT_KEY_DOWN (ExtImportPrefs::OnRuleTableKeyDown) 
   EVT_GRID_EDITOR_HIDDEN (ExtImportPrefs::OnRuleTableEdit)
   EVT_GRID_SELECT_CELL (ExtImportPrefs::OnRuleTableSelect)
   EVT_GRID_RANGE_SELECT (ExtImportPrefs::OnRuleTableSelectRange)
   EVT_BUTTON(EIPAddRule,ExtImportPrefs::OnAddRule)
   EVT_BUTTON(EIPDelRule,ExtImportPrefs::OnDelRule)
END_EVENT_TABLE()

ExtImportPrefs::ExtImportPrefs(wxWindow * parent)
:   PrefsPanel(parent, _("Extended Import"))
{
   last_selected = -1;
   Populate();
}

ExtImportPrefs::~ExtImportPrefs()
{
}

/// Creates the dialog and its contents.
void ExtImportPrefs::Populate()
{
   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is 
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
#if EXTIMPORT_MIME_SUPPORT
   RuleTable->CreateGrid (0, 2, wxGrid::wxGridSelectRows);
#else
   RuleTable->CreateGrid (0, 1, wxGrid::wxGridSelectRows);
#endif
   RuleTable->DisableDragColMove ();
   RuleTable->DisableDragRowSize ();
   RuleTable->SetColLabelValue (0, _("File extensions"));
#if EXTIMPORT_MIME_SUPPORT
   RuleTable->SetColLabelValue (1, _("Mime-types"));
#endif
   RuleTable->SetRowLabelSize (0);
   RuleTable->SetSelectionMode (wxGrid::wxGridSelectRows);
   ExtImportItems *items = wxGetApp().mImporter->GetImportItems();

   for (int i = 0; i < items->Count(); i++)
      AddItemToTable (i, &(*items)[i]);
   // ----------------------- End of main section --------------
}

void ExtImportPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);

   S.StartStatic(_("Rules to choose import filters"), 1);
   {
      S.SetSizerProportion(1);
      S.StartHorizontalLay (wxEXPAND, 1);
	  {
	    RuleTable = S.Id(EIPRuleTable).AddGrid ();
	    RuleTable->SetWindowStyle (wxBORDER_SUNKEN);
		PluginList = S.Id(EIPPluginList).AddListControl ();
		PluginList->SetSingleStyle (wxLC_REPORT, true);
		PluginList->SetSingleStyle (wxLC_SINGLE_SEL, true);
		PluginList->InsertColumn (0, _("Importer"));
	  }
	  S.EndHorizontalLay();
	  S.StartHorizontalLay (wxSHRINK, 0);
	  {
        AddRule = S.Id (EIPAddRule).AddButton (_("Add new rule"));
        DelRule = S.Id (EIPDelRule).AddButton (_("Delete selected rule"));
	  }
	  S.EndHorizontalLay();
   }
   S.EndStatic();
   Layout();
   Fit();
   SetMinSize(GetSize());
}

bool ExtImportPrefs::Apply()
{  
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);    
   
   return true;
}

void ExtImportPrefs::OnPluginKeyDown(wxListEvent& event)
{
   for (int i = 0; i < 1; i++)
   {
#ifdef __WXMAC__
      if (!wxGetKeyState(WXK_COMMAND))
         break;
#else
      if (!wxGetKeyState(WXK_CONTROL))
         break;
#endif
         
      int code = event.GetKeyCode();

      if (code != WXK_UP && code != WXK_DOWN)
         break;

      long itemIndex = -1;
      itemIndex = PluginList->GetNextItem(itemIndex,
            wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED);
      if (itemIndex == -1)
         break;
      
      if (last_selected == -1)
         break;

      ExtImportItems *items = wxGetApp().mImporter->GetImportItems();
      ExtImportItem *item = &(*items)[last_selected];
      
      if (code == WXK_UP && itemIndex == 0)
         break;
      else if (code == WXK_DOWN && itemIndex == PluginList->GetItemCount() - 1)
         break;
      
      wxString t, t2;
      long d, d2;
      ImportPlugin *ip1, *ip2;
      if (code == WXK_UP)
      {
         t = PluginList->GetItemText (itemIndex);
         d = PluginList->GetItemData (itemIndex);
         d2 = PluginList->GetItemData (itemIndex - 1);
         PluginList->SetItemText (itemIndex, PluginList->GetItemText (itemIndex - 1));
         PluginList->SetItemText (itemIndex - 1, t);
         if (d == -1 || d2 == -1)
         {
            PluginList->SetItemData (itemIndex, PluginList->GetItemData (itemIndex - 1));
            PluginList->SetItemData (itemIndex - 1, d);
            if (d == -1)
            {
               item->divider--;
            }
            else if (d2 == -1)
            {
               item->divider++;
            }
         }
         else
         {
            ip1 = item->filter_objects[d];
            ip2 = item->filter_objects[d2];
            item->filter_objects[d] = ip2;
            item->filter_objects[d2] = ip1;
            t = item->filters[d];
            t2 = item->filters[d2];
            item->filters[d] = t2;
            item->filters[d2] = t;
         }
      }
      else if (code == WXK_DOWN)
      {
         t = PluginList->GetItemText (itemIndex);
         d = PluginList->GetItemData (itemIndex);
         d2 = PluginList->GetItemData (itemIndex + 1);
         PluginList->SetItemText (itemIndex, PluginList->GetItemText (itemIndex + 1));
         PluginList->SetItemText (itemIndex + 1, t);
         if (d == -1 || d2 == -1)
         {
            PluginList->SetItemData (itemIndex, PluginList->GetItemData (itemIndex + 1));
            PluginList->SetItemData (itemIndex + 1, d);
            if (d == -1)
            {
               item->divider++;
            }
            else if (d2 == -1)
            {
               item->divider--;
            }
         }
         else
         {
            ip1 = item->filter_objects[d];
            ip2 = item->filter_objects[d2];
            item->filter_objects[d] = ip2;
            item->filter_objects[d2] = ip1;
            t = item->filters[d];
            t2 = item->filters[d2];
            item->filters[d] = t2;
            item->filters[d2] = t;
         }
      }
      int fcount = item->filter_objects.Count();
      if (item->divider >= fcount)
      {
         item->divider = -1;
      }
      if (item->divider < -1)
         item->divider = item->filter_objects.Count() - 1;
   }
   event.Skip();
}

void ExtImportPrefs::OnRuleTableKeyDown(wxKeyEvent& event)
{
   int mods = event.GetModifiers();
   if (mods & wxMOD_CMD && (event.GetKeyCode() == WXK_UP || event.GetKeyCode() == WXK_DOWN))
   {
      ExtImportItems *items = wxGetApp().mImporter->GetImportItems();
      ExtImportItem *t1, *t2;
      int selrow = RuleTable->GetGridCursorRow ();
      wxString ts;
      if (event.GetKeyCode() == WXK_UP)
      {
         if (selrow == 0)
            return;
         t1 = items->Detach(selrow - 1);
         t2 = items->Detach(selrow - 1);
         items->Insert (t1, selrow - 1);
         items->Insert (t2, selrow - 1);
         for (int i = 0; i < RuleTable->GetNumberCols(); i++)
         {
            ts = RuleTable->GetCellValue (selrow, i);
            RuleTable->SetCellValue (selrow, i, RuleTable->GetCellValue (selrow - 1, i));
            RuleTable->SetCellValue (selrow - 1, i, ts);
         }
         RuleTable->MoveCursorUp (false);
      }
      else if (event.GetKeyCode() == WXK_DOWN)
      {
         if (selrow == RuleTable->GetNumberRows() - 1)
            return;
         t1 = items->Detach(selrow);
         t2 = items->Detach(selrow);
         items->Insert (t2, selrow);
         items->Insert (t1, selrow);
         for (int i = 0; i < RuleTable->GetNumberCols(); i++)
         {
            ts = RuleTable->GetCellValue (selrow, i);
            RuleTable->SetCellValue (selrow, i, RuleTable->GetCellValue (selrow + 1, i));
            RuleTable->SetCellValue (selrow + 1, i, ts);
         }
         RuleTable->MoveCursorDown (false);
      }
   }
   else
     event.Skip();
}

void ExtImportPrefs::OnRuleTableSelect (wxGridEvent& event)
{
   int toprow;
   if (!event.Selecting())
      return;
   toprow = event.GetRow();
   DoOnRuleTableSelect (toprow);
   event.Skip();
}

void ExtImportPrefs::OnRuleTableSelectRange (wxGridRangeSelectEvent& event)
{
   int toprow;
   if (!event.Selecting())
      return;
   toprow = event.GetTopRow();
   DoOnRuleTableSelect (toprow);
   event.Skip();
}

void ExtImportPrefs::DoOnRuleTableSelect (int toprow)
{
   ExtImportItems *items = wxGetApp().mImporter->GetImportItems();
   ExtImportItem *item = &(*items)[toprow];
   PluginList->DeleteAllItems();
   
   int fcount;
   fcount = item->filters.Count();
   int shift = 0;
   for (int i = 0; i < fcount; i++)
   {
      if (item->divider == i)
      {
         PluginList->InsertItem (i, _("Unused filters:"));
         PluginList->SetItemData (i, -1);
         shift = 1;
      }
      if (item->filter_objects[i] != NULL)
      {
         PluginList->InsertItem (i + shift, item->filter_objects[i]->GetPluginFormatDescription());
      }
      else
      {
         PluginList->InsertItem (i + shift, item->filters[i]);
      }
      PluginList->SetItemData (i + shift, i);
   }
   if (item->divider == -1)
   {
      PluginList->InsertItem (fcount, _("Unused filters:"));
      PluginList->SetItemData (fcount, -1);
   }
   PluginList->SetColumnWidth (0, wxLIST_AUTOSIZE);
   last_selected = toprow;
}

void ExtImportPrefs::OnRuleTableEdit (wxGridEvent& event)
{
   int row = event.GetRow();
   int col = event.GetCol();
   ExtImportItems *items = wxGetApp().mImporter->GetImportItems();
   ExtImportItem *item = &(*items)[row];
   RuleTable->SaveEditControlValue();
   wxString val = RuleTable->GetCellValue (row, col);
   switch (col)
   {
   case 0:
      item->extensions.Clear();
      wxGetApp().mImporter->StringToList (val, wxString(wxT(":")), item->extensions);
      break;
   case 1:
      item->mime_types.Clear();
      wxGetApp().mImporter->StringToList (val, wxString(wxT(":")), item->mime_types);
      break;
   }
   RuleTable->AutoSizeColumns ();
}

void ExtImportPrefs::AddItemToTable (int index, ExtImportItem *item)
{
   wxString extensions, mime_types;
   if (item->extensions.Count() > 0)
   {
      extensions.Append (item->extensions[0]);
      for (int i = 1; i < item->extensions.Count(); i++)
      {
         extensions.Append (wxT(":"));
         extensions.Append (item->extensions[i]);
      }
   }
   if (item->mime_types.Count() > 0)
   {
      mime_types.Append (item->mime_types[0]);
      for (int i = 1; i < item->mime_types.Count(); i++)
      {
         mime_types.Append (wxT(":"));
         mime_types.Append (item->mime_types[i]);
      }
   }
 
   RuleTable->InsertRows (index, 1);
   RuleTable->SetCellValue (index, 0, extensions);
#if EXTIMPORT_MIME_SUPPORT
   RuleTable->SetCellValue (index, 1, mime_types);
#endif
   RuleTable->AutoSizeColumns ();
}

void ExtImportPrefs::OnAddRule(wxCommandEvent& event)
{
   ExtImportItems *items = wxGetApp().mImporter->GetImportItems();
   ExtImportItem *item = wxGetApp().mImporter->CreateDefaultImportItem();
   items->Add (item);

   AddItemToTable (RuleTable->GetNumberRows (), item);   
}

void ExtImportPrefs::OnDelRule(wxCommandEvent& event)
{
   if (last_selected < 0)
      return;
   ExtImportItems *items = wxGetApp().mImporter->GetImportItems();
   RuleTable->DeleteRows (last_selected);
   items->RemoveAt (last_selected);
   RuleTable->AutoSizeColumns ();
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 427b9e64-3fc6-40ef-bbf8-e6fff1d442f0
