/**********************************************************************

  Audacity: A Digital Audio Editor

  ExtImportPrefs.cpp

  LRN

*******************************************************************//**

\class ExtImportPrefs
\brief A PrefsPanel used to select extended import filter options.

*//*******************************************************************/



#include "ExtImportPrefs.h"

#include <wx/defs.h>
#include <wx/listctrl.h>
#include <wx/dnd.h>

#include "Prefs.h"
#include "../ShuttleGui.h"
#include "../import/Import.h"
#include "../widgets/AudacityMessageBox.h"
#include "../widgets/Grid.h"

#define EXTIMPORT_MIME_SUPPORT 0

enum ExtImportPrefsControls
{
   EIPPluginList = 20000,
   EIPRuleTable,
   EIPAddRule,
   EIPDelRule,
   EIPMoveRuleUp,
   EIPMoveRuleDown,
   EIPMoveFilterUp,
   EIPMoveFilterDown
};

BEGIN_EVENT_TABLE(ExtImportPrefs, PrefsPanel)
   EVT_LIST_KEY_DOWN(EIPPluginList,ExtImportPrefs::OnPluginKeyDown)
   EVT_LIST_BEGIN_DRAG(EIPPluginList,ExtImportPrefs::OnPluginBeginDrag)
   EVT_KEY_DOWN (ExtImportPrefs::OnRuleTableKeyDown)
   EVT_GRID_CELL_LEFT_CLICK (ExtImportPrefs::OnRuleTableCellClick)
   EVT_GRID_EDITOR_HIDDEN (ExtImportPrefs::OnRuleTableEdit)
   EVT_GRID_SELECT_CELL (ExtImportPrefs::OnRuleTableSelect)
   EVT_GRID_RANGE_SELECT (ExtImportPrefs::OnRuleTableSelectRange)
   EVT_BUTTON(EIPAddRule,ExtImportPrefs::OnAddRule)
   EVT_BUTTON(EIPDelRule,ExtImportPrefs::OnDelRule)
   EVT_BUTTON(EIPMoveRuleUp,ExtImportPrefs::OnRuleMoveUp)
   EVT_BUTTON(EIPMoveRuleDown,ExtImportPrefs::OnRuleMoveDown)
   EVT_BUTTON(EIPMoveFilterUp,ExtImportPrefs::OnFilterMoveUp)
   EVT_BUTTON(EIPMoveFilterDown,ExtImportPrefs::OnFilterMoveDown)
END_EVENT_TABLE()

ExtImportPrefs::ExtImportPrefs(wxWindow * parent, wxWindowID winid)
/* i18n-hint:  Title of dialog governing "Extended", or "advanced,"
 * audio file import options */
:   PrefsPanel(parent, winid, XO("Extended Import")), RuleTable(NULL),
    PluginList(NULL), mCreateTable (false), mDragFocus (NULL),
    mFakeKeyEvent (false), mStopRecursiveSelection (false), last_selected (-1)
{
   Populate();

   // See bug #2315 for discussion
   // This should be reviewed and (possibly) removed after wx3.1.3.
   Bind(wxEVT_SHOW, &ExtImportPrefs::OnShow, this);
}
ExtImportPrefs::~ExtImportPrefs()
{
}

ComponentInterfaceSymbol ExtImportPrefs::GetSymbol()
{
   return EXT_IMPORT_PREFS_PLUGIN_SYMBOL;
}

TranslatableString ExtImportPrefs::GetDescription()
{
   return XO("Preferences for ExtImport");
}

ManualPageID ExtImportPrefs::HelpPageName()
{
   return "Extended_Import_Preferences";
}

/// Creates the dialog and its contents.
void ExtImportPrefs::Populate()
{
   // Ensure Importer has current items
   Importer::Get().ReadImportItems();

   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

void ExtImportPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);
   S.StartScroller();

   S.TieCheckBox(XXO("A&ttempt to use filter in OpenFile dialog first"),
         {wxT("/ExtendedImport/OverrideExtendedImportByOpenFileDialogChoice"),
          true});
   S.StartStatic(XO("Rules to choose import filters"), 1);
   {
      S.SetSizerProportion(1);
      S.StartHorizontalLay (wxEXPAND, 1);
      {
         bool fillRuleTable = false;
         if (RuleTable == NULL)
         {
            RuleTable = safenew Grid(S.GetParent(),EIPRuleTable);

            RuleTable->SetColLabelSize(RuleTable->GetDefaultRowSize());
#if EXTIMPORT_MIME_SUPPORT
            RuleTable->CreateGrid (0, 2, wxGrid::wxGridSelectRows);
#else
            RuleTable->CreateGrid (0, 1, wxGrid::wxGridSelectRows);
#endif
            RuleTable->DisableDragColMove ();
            RuleTable->DisableDragRowSize ();
            RuleTable->SetDefaultCellAlignment(wxALIGN_LEFT, wxALIGN_CENTER);
            RuleTable->SetColLabelValue (0, _("File extensions"));
#if EXTIMPORT_MIME_SUPPORT
            RuleTable->SetColLabelValue (1, _("Mime-types"));
#endif
            RuleTable->SetRowLabelSize (0);
            RuleTable->SetSelectionMode (wxGrid::wxGridSelectRows);
            // call SetMinSize to enable scrolling on large content
            RuleTable->Fit();
            RuleTable->SetMinSize(RuleTable->GetSize());

            ExtImportPrefsDropTarget *dragtarget1 {};
            RuleTable->SetDropTarget (
               dragtarget1 = safenew ExtImportPrefsDropTarget(
                  dragtext1 = safenew wxTextDataObject(wxT(""))
               )
            );
            dragtarget1->SetPrefs (this);

            RuleTable->EnableDragCell (true);
            fillRuleTable = true;
         }
         S.Position(wxEXPAND | wxALL)
            .AddWindow(RuleTable);

         PluginList = S.Id(EIPPluginList).AddListControl(
            { { XO("Importer order"), wxLIST_FORMAT_LEFT,
                wxLIST_AUTOSIZE_USEHEADER } },
            wxLC_REPORT | wxLC_SINGLE_SEL
         );

         if (fillRuleTable)
         {
            ExtImportPrefsDropTarget *dragtarget2 {};
            PluginList->SetDropTarget (
               dragtarget2 = safenew ExtImportPrefsDropTarget(
                  dragtext2 = safenew wxTextDataObject(wxT(""))
               )
            );
            dragtarget2->SetPrefs (this);

            auto &items = Importer::Get().GetImportItems();
            {
               int i = -1;
               for (const auto &item : items)
                  AddItemToTable (++i, item.get());
            }
            if (!items.empty())
            {
               RuleTable->SelectRow(0);
               RuleTable->SetGridCursor(0,0);
            }
         }
      }
      S.EndHorizontalLay();
      S.StartHorizontalLay (wxSHRINK, 0);
      {
          MoveRuleUp = S.Id (EIPMoveRuleUp).AddButton(XXO("Move rule &up"));
          MoveRuleDown = S.Id (EIPMoveRuleDown).AddButton(
             XXO("Move rule &down"));
          MoveFilterUp = S.Id (EIPMoveFilterUp).AddButton(
             XXO("Move f&ilter up"));
          MoveFilterDown = S.Id (EIPMoveFilterDown).AddButton(
             XXO("Move &filter down"));
      }
      S.EndHorizontalLay();
      S.StartHorizontalLay (wxSHRINK, 0);
      {
          AddRule = S.Id (EIPAddRule).AddButton(XXO("&Add new rule"));
          DelRule = S.Id (EIPDelRule).AddButton(XXO("De&lete selected rule"));
      }
      S.EndHorizontalLay();
   }
   S.EndStatic();
   S.EndScroller();

   Layout();
   Fit();
   SetMinSize(GetSize());
}

bool ExtImportPrefs::Commit()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   Importer::Get().WriteImportItems();

   return true;
}

// See bug #2315 for discussion. This should be reviewed
// and (possibly) removed after wx3.1.3.
void ExtImportPrefs::OnShow(wxShowEvent &event)
{
   event.Skip();
   if (event.IsShown())
   {
      RuleTable->Refresh();
      PluginList->Refresh();
   }
}

void ExtImportPrefs::OnPluginKeyDown(wxListEvent& event)
{
   for (int i = 0; i < 1; i++)
   {
#ifdef __WXMAC__
      if (!mFakeKeyEvent && !wxGetKeyState(WXK_COMMAND))
         break;
#else
      if (!mFakeKeyEvent && !wxGetKeyState(WXK_CONTROL))
         break;
#endif

      if (DoOnPluginKeyDown (event.GetKeyCode()))
         event.Skip();
   }
}

void ExtImportPrefs::SwapPluginRows (int row1, int row2)
{
   wxString t, t2;
   long d, d2;
   ImportPlugin *ip1, *ip2;

   auto &items = Importer::Get().GetImportItems();
   ExtImportItem *item = NULL;
   if( last_selected >= 0 )
      item = items[last_selected].get();

   t = PluginList->GetItemText (row1);
   d = PluginList->GetItemData (row1);
   d2 = PluginList->GetItemData (row2);
   PluginList->SetItemText (row1, PluginList->GetItemText (row2));
   PluginList->SetItemText (row2, t);
   if (d == -1 || d2 == -1)
   {
      PluginList->SetItemData (row1, PluginList->GetItemData (row2));
      PluginList->SetItemData (row2, d);
      if( !item )
         return;
      if (d == -1)
      {
         item->divider = row2;
      }
      else if (d2 == -1)
      {
         item->divider = row1;
      }
   }
   else
   {
      if( !item )
         return;
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

bool ExtImportPrefs::DoOnPluginKeyDown (int code)
{
   if (code != WXK_UP && code != WXK_DOWN)
         return false;

   long itemIndex = -1;
   long itemIndex2 = -1;
   itemIndex = PluginList->GetNextItem(itemIndex,
         wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED);
   if (itemIndex == -1)
         return false;

   if (last_selected == -1)
         return false;

   auto &items = Importer::Get().GetImportItems();
   ExtImportItem *item = items[last_selected].get();

   if (code == WXK_UP && itemIndex == 0)
      return false;
   else if (code == WXK_DOWN && itemIndex == PluginList->GetItemCount() - 1)
      return false;

   if (code == WXK_UP)
   {
      itemIndex2 = itemIndex - 1;
   }
   else if (code == WXK_DOWN)
   {
      itemIndex2 = itemIndex + 1;
   }
   SwapPluginRows (itemIndex, itemIndex2);
   if (mFakeKeyEvent)
   {
      PluginList->SetItemState (itemIndex, 0, wxLIST_STATE_SELECTED);
      PluginList->SetItemState (itemIndex2, wxLIST_STATE_SELECTED | wxLIST_STATE_FOCUSED,
         wxLIST_STATE_SELECTED | wxLIST_STATE_FOCUSED);
   }
   int fcount = item->filter_objects.size();
   if (item->divider >= fcount)
   {
      item->divider = -1;
   }
   if (item->divider < -1)
      item->divider = item->filter_objects.size() - 1;

   return true;
}


void ExtImportPrefs::SwapRows (int row1, int row2)
{
   int t;
   wxString ts;
   if (row1 == row2)
      return;
   if (row1 > row2)
   {
      t = row1;
      row1 = row2;
      row2 = t;
   }
   auto &items = Importer::Get().GetImportItems();

   auto &t1 = items[row1];
   auto &t2 = items[row2];
   std::swap(t1, t2);

   for (int i = 0; i < RuleTable->GetNumberCols(); i++)
   {
      ts = RuleTable->GetCellValue (row2, i);
      RuleTable->SetCellValue (row2, i, RuleTable->GetCellValue (row1, i));
      RuleTable->SetCellValue (row1, i, ts);
   }
}

void ExtImportPrefs::OnPluginBeginDrag(wxListEvent& WXUNUSED(event))
{
   wxDropSource dragSource(this);
   dragtext2->SetText(wxT(""));
   dragSource.SetData(*dragtext2);
   mDragFocus = PluginList;
   if( mDragFocus == NULL )
      return;
   wxDragResult result = dragSource.DoDragDrop(wxDrag_DefaultMove);
   mDragFocus = NULL;
   switch (result)
   {
      case wxDragCopy:
      case wxDragMove:
      case wxDragNone:
         return;
         break;
      default:
         break;
   }
}

void ExtImportPrefs::OnRuleTableKeyDown(wxKeyEvent& event)
{
   int mods = event.GetModifiers();
   if (mods & wxMOD_CMD && (event.GetKeyCode() == WXK_UP ||
          event.GetKeyCode() == WXK_DOWN))
   {
      DoOnRuleTableKeyDown (event.GetKeyCode());
   }
   else
   {
      event.Skip();
   }
}

void ExtImportPrefs::DoOnRuleTableKeyDown (int keycode)
{
   int selrow = RuleTable->GetGridCursorRow ();
   wxString ts;
   if (keycode == WXK_UP)
   {
      if (selrow <= 0)
         return;
      SwapRows (selrow - 1, selrow);
      RuleTable->MoveCursorUp (false);
      RuleTable->SelectRow (selrow - 1);
   }
   else if (keycode == WXK_DOWN)
   {
      if (selrow == RuleTable->GetNumberRows() - 1)
         return;
      SwapRows (selrow, selrow + 1);
      RuleTable->MoveCursorDown (false);
      RuleTable->SelectRow (selrow + 1);
   }
}

void ExtImportPrefs::OnRuleTableSelect (wxGridEvent& event)
{
   int toprow;
   event.Skip();
   if (!event.Selecting() || mStopRecursiveSelection)
      return;

   toprow = event.GetRow();
   if (toprow < 0)
      return;

   DoOnRuleTableSelect (toprow);
}

void ExtImportPrefs::OnRuleTableSelectRange (wxGridRangeSelectEvent& event)
{
   int toprow;
   event.Skip();
   if (!event.Selecting() || mStopRecursiveSelection)
      return;

   toprow = event.GetTopRow();
   if (toprow < 0)
      return;

   DoOnRuleTableSelect (toprow);
   mStopRecursiveSelection = true;
   RuleTable->SelectRow (toprow);
   mStopRecursiveSelection = false;
   RuleTable->SetGridCursor (toprow, 0);
}

void ExtImportPrefs::DoOnRuleTableSelect (int toprow)
{
   auto &items = Importer::Get().GetImportItems();

   if (toprow < 0 || toprow >= (int)items.size())
   {
      return;
   }

   ExtImportItem *item = items[toprow].get();
   PluginList->DeleteAllItems();

   int fcount;
   fcount = item->filters.size();
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
         PluginList->InsertItem (i + shift,
               item->filter_objects[i]->GetPluginFormatDescription().Translation());
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
   wxListItem info;
   info.SetId (0);
   info.SetColumn (0);
   info.SetStateMask (wxLIST_STATE_SELECTED | wxLIST_STATE_FOCUSED);
   info.SetState (wxLIST_STATE_SELECTED | wxLIST_STATE_FOCUSED);
   info.SetMask (wxLIST_MASK_STATE);
   PluginList->SetItem (info);
   PluginList->SetColumnWidth (0, wxLIST_AUTOSIZE);
   last_selected = toprow;
}

void ExtImportPrefs::OnRuleTableEdit (wxGridEvent& event)
{
   int row = event.GetRow();
   int col = event.GetCol();
   auto &items = Importer::Get().GetImportItems();
   ExtImportItem *item = items[row].get();
   RuleTable->SaveEditControlValue();

   wxString val = RuleTable->GetCellValue (row, col);
   int fixSpaces = wxNO;
   bool askedAboutSpaces = false;
   wxArrayString vals;
   wxString delims(wxT(":"));
   Importer::Get().StringToList (val, delims, vals);
   switch (col)
   {
   case 0:
      item->extensions.clear();
      break;
   case 1:
      item->mime_types.clear();
      break;
   }

   for (size_t i = 0; i < vals.size(); i++)
   {

      wxString trimmed = vals[i];
      trimmed.Trim().Trim(false);
      if (trimmed != vals[i])
      {
         if (!askedAboutSpaces)
         {
            fixSpaces = AudacityMessageBox(
               XO(
"There are space characters (spaces, newlines, tabs or linefeeds) in one of \
the items. They are likely to break the pattern matching. Unless you know \
what you are doing, it is recommended to trim spaces. Do you want \
Audacity to trim spaces for you?"),
               XO("Spaces detected"),
               wxYES_NO);
            askedAboutSpaces = true;
         }
         if (fixSpaces != wxYES)
         {
            trimmed = vals[i];
         }
         else
         {
            vals[i] = trimmed;
         }
      }
      switch (col)
      {
      case 0:
         item->extensions.push_back(trimmed);
         break;
      case 1:
         item->mime_types.push_back(trimmed);
         break;
      }
   }
   if (fixSpaces == wxYES)
   {
      wxString vals_as_string;
      for (size_t i = 0; i < vals.size(); i++)
      {
         if (i > 0)
            vals_as_string.Append (wxT(":"));
         vals_as_string.Append (vals[i]);
      }
      RuleTable->SetCellValue (row, col, vals_as_string);
   }

   RuleTable->AutoSizeColumns ();
}

void ExtImportPrefs::AddItemToTable (int index, const ExtImportItem *item)
{
   wxString extensions, mime_types;
   if (item->extensions.size() > 0)
   {
      extensions.Append (item->extensions[0]);
      for (unsigned int i = 1; i < item->extensions.size(); i++)
      {
         extensions.Append (wxT(":"));
         extensions.Append (item->extensions[i]);
      }
   }
   if (item->mime_types.size() > 0)
   {
      mime_types.Append (item->mime_types[0]);
      for (unsigned int i = 1; i < item->mime_types.size(); i++)
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

void ExtImportPrefs::OnAddRule(wxCommandEvent& WXUNUSED(event))
{
   auto &items = Importer::Get().GetImportItems();
   auto uitem = Importer::Get().CreateDefaultImportItem();
   auto item = uitem.get();
   items.push_back(std::move(uitem));
   AddItemToTable (RuleTable->GetNumberRows (), item);

   RuleTable->SelectRow(RuleTable->GetNumberRows () - 1);
   RuleTable->SetGridCursor (RuleTable->GetNumberRows () - 1, 0);
   RuleTable->SetFocus();
}

void ExtImportPrefs::OnDelRule(wxCommandEvent& WXUNUSED(event))
{
   if (last_selected < 0)
      return;
   auto &items = Importer::Get().GetImportItems();

   int msgres = AudacityMessageBox (
      XO("Do you really want to delete selected rule?"),
      XO("Rule deletion confirmation"),
      wxYES_NO,
      RuleTable);
   // Yes or no, there is no third!
   if (msgres != wxYES)
      return;

   PluginList->DeleteAllItems();
   items.erase (items.begin() + last_selected);
   DoOnRuleTableSelect (last_selected);
   // This will change last_selected
   RuleTable->DeleteRows (last_selected);
   RuleTable->AutoSizeColumns ();
   if (last_selected >= RuleTable->GetNumberRows ())
      last_selected = RuleTable->GetNumberRows () - 1;
   if (last_selected >= 0)
   {
      RuleTable->SelectRow(last_selected);
      RuleTable->SetGridCursor (last_selected, 0);
   }
}

void ExtImportPrefs::OnRuleMoveUp(wxCommandEvent& WXUNUSED(event))
{
   DoOnRuleTableKeyDown (WXK_UP);
}

void ExtImportPrefs::OnRuleMoveDown(wxCommandEvent& WXUNUSED(event))
{
   DoOnRuleTableKeyDown (WXK_DOWN);
}

void ExtImportPrefs::FakeOnPluginKeyDown (int keycode)
{
   wxListEvent fakeevent(wxEVT_COMMAND_LIST_KEY_DOWN, EIPPluginList);
   fakeevent.SetEventObject(this);
   fakeevent.m_code = keycode;
   mFakeKeyEvent = true;
   GetEventHandler()->ProcessEvent (fakeevent);
   mFakeKeyEvent = false;
}

void ExtImportPrefs::OnFilterMoveUp(wxCommandEvent& WXUNUSED(event))
{
   FakeOnPluginKeyDown (WXK_UP);
}

void ExtImportPrefs::OnFilterMoveDown(wxCommandEvent& WXUNUSED(event))
{
   FakeOnPluginKeyDown (WXK_DOWN);
}


void ExtImportPrefs::OnRuleTableCellClick (wxGridEvent& event)
{
   int row = event.GetRow();
   RuleTable->SelectRow (row, false);
   RuleTable->SetGridCursor (row, 0);

   wxDropSource dragSource(this);
   dragtext1->SetText(wxT(""));
   dragSource.SetData(*dragtext1);
   mDragFocus = RuleTable;
   wxDragResult result = dragSource.DoDragDrop(wxDrag_DefaultMove);
   mDragFocus = NULL;
   switch (result)
   {
      case wxDragCopy: /* copy the data */
      case wxDragMove:
      case wxDragNone:
         return;
         break;
      default:         /* do nothing */ break;
   }

   event.Skip();
}

ExtImportPrefsDropTarget::ExtImportPrefsDropTarget(wxDataObject *dataObject)
   : wxDropTarget(dataObject)
{
   mPrefs = NULL;
}

ExtImportPrefsDropTarget::~ExtImportPrefsDropTarget ()
{
}

void ExtImportPrefsDropTarget::SetPrefs (ExtImportPrefs *prefs)
{
   mPrefs = prefs;
}

wxDragResult ExtImportPrefsDropTarget::OnData(wxCoord  WXUNUSED(x), wxCoord  WXUNUSED(y),
      wxDragResult def)
{
   return def;
}

#if defined(__WXMSW__)
/* wxListCtrl::FindItem() in wxPoint()-taking mode works only for lists in
 * Small/Large-icon mode
 * wxListCtrl::HitTest() on Windows only hits item label rather than its whole
 * row, which makes it difficult to drag over items with short or non-existent
 * labels.
 */
long wxCustomFindItem(wxListCtrl *list, int x, int y)
{
   long count = list->GetItemCount();
   wxRect r;
   for (long i = 0; i < count; i++)
   {
      if (list->GetItemRect (i, r))
      {
         if (r.Contains (x, y))
            return i;
      }
   }
   return -1;
}
#endif

bool ExtImportPrefsDropTarget::OnDrop(wxCoord x, wxCoord y)
{
   if (mPrefs == NULL)
      return false;
   wxListCtrl *PluginList = mPrefs->GetPluginList();
   Grid *RuleTable = mPrefs->GetRuleTable();
   if (mPrefs->GetDragFocus() == RuleTable)
   {
      if (RuleTable->YToRow(
            RuleTable->CalcUnscrolledPosition(wxPoint(x, y)).y) == wxNOT_FOUND)
         return false;
   }
   else if (mPrefs->GetDragFocus() == PluginList)
   {
#if defined(__WXMSW__)
      long item = wxCustomFindItem (PluginList, x, y);
#else
      int flags = 0;
      long item = PluginList->HitTest (wxPoint (x, y), flags, NULL);
#endif
      if (item < 0)
         return false;
   }

   return true;
}

wxDragResult ExtImportPrefsDropTarget::OnEnter(wxCoord x, wxCoord y,
      wxDragResult def)
{
   return OnDragOver(x, y, def);
}

wxDragResult ExtImportPrefsDropTarget::OnDragOver(wxCoord x, wxCoord y,
      wxDragResult WXUNUSED(def))
{
   if (mPrefs == NULL)
      return wxDragNone;
   wxListCtrl *PluginList = mPrefs->GetPluginList();
   Grid *RuleTable = mPrefs->GetRuleTable();
   if (mPrefs->GetDragFocus() == RuleTable)
   {
      int row;
      row = RuleTable->YToRow(RuleTable->CalcUnscrolledPosition(wxPoint(x, y)).y);
      if (row == wxNOT_FOUND)
         return wxDragNone;


      int cRow = RuleTable->GetGridCursorRow ();
      if (row != cRow)
      {
         mPrefs->SwapRows (cRow, row);
         RuleTable->SetGridCursor (row, 0);
         RuleTable->SelectRow (row);
      }
   }
   else if (mPrefs->GetDragFocus() == PluginList)
   {
#if defined(__WXMSW__)
      long item = wxCustomFindItem (PluginList, x, y);
#else
      int flags = 0;
      long item = PluginList->HitTest (wxPoint (x, y), flags, NULL);
#endif
      if (item < 0)
         return wxDragNone;

      long selected = -1;
      selected = PluginList->GetNextItem(selected,
            wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED);
      if (selected == -1)
            return wxDragNone;

      if (item != selected)
      {
         mPrefs->SwapPluginRows(selected, item);
         PluginList->SetItemState (selected, 0, wxLIST_STATE_SELECTED);
         PluginList->SetItemState (item, wxLIST_STATE_SELECTED,
               wxLIST_STATE_SELECTED);
      }
   }
   return wxDragMove;
}

void ExtImportPrefsDropTarget::OnLeave()
{
}

namespace{
PrefsPanel::Registration sAttachment{ "ExtImport",
   [](wxWindow *parent, wxWindowID winid, AudacityProject *)
   {
      wxASSERT(parent); // to justify safenew
      return safenew ExtImportPrefs(parent, winid);
   },
   false,
   // Place as a lower level of the tree of pages:
   { "ImportExport" }
};
}
