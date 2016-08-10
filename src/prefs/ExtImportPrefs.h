/**********************************************************************

  Audacity: A Digital Audio Editor

  ExtImportPrefs.h

  LRN

**********************************************************************/

#ifndef __AUDACITY_EXT_IMPORT_PREFS__
#define __AUDACITY_EXT_IMPORT_PREFS__

#include <wx/defs.h>
#include <wx/dnd.h>
#include <wx/window.h>
#include "../widgets/Grid.h"

#include "PrefsPanel.h"

#include "../import/Import.h"
#include "../import/ImportPlugin.h"

class wxButton;
class wxListCtrl;
class wxListEvent;
class ExtImportPrefs;
class ShuttleGui;

class ExtImportPrefsDropTarget final : public wxDropTarget
{
public:
   // Takes ownership of the argument
   ExtImportPrefsDropTarget(wxDataObject* dataObject = nullptr);
   ~ExtImportPrefsDropTarget ();
   wxDragResult OnData(wxCoord x, wxCoord y, wxDragResult def);
   bool OnDrop(wxCoord x, wxCoord y);
   wxDragResult OnEnter(wxCoord x, wxCoord y, wxDragResult def);
   wxDragResult OnDragOver(wxCoord x, wxCoord y, wxDragResult def);
   void OnLeave();
   void SetPrefs (ExtImportPrefs *prefs);
private:
   ExtImportPrefs *mPrefs;
};

class ExtImportPrefs final : public PrefsPanel
{
 public:
   ExtImportPrefs(wxWindow * parent);
   ~ExtImportPrefs();
   bool Apply() override;

   void OnPluginKeyDown(wxListEvent& event);
   void OnPluginBeginDrag(wxListEvent& event);
   void OnRuleTableKeyDown(wxKeyEvent& event);
   void OnRuleTableSelect(wxGridEvent& event);
   void OnRuleTableEdit(wxGridEvent& event);
   void OnRuleTableSelectRange(wxGridRangeSelectEvent& event);
   void OnRuleTableCellClick(wxGridEvent& event);
   void OnAddRule(wxCommandEvent& event);
   void OnDelRule(wxCommandEvent& event);
   void OnRuleMoveUp(wxCommandEvent& event);
   void OnRuleMoveDown(wxCommandEvent& event);
   void OnFilterMoveUp(wxCommandEvent& event);
   void OnFilterMoveDown(wxCommandEvent& event);

   void OnNavKey (wxNavigationKeyEvent& event);

   void SwapRows (int row1, int row2);
   void SwapPluginRows (int row1, int row2);

   Grid *GetRuleTable() { return RuleTable; }
   wxListCtrl *GetPluginList() { return PluginList; }

   wxWindow *GetDragFocus() { return mDragFocus; }

 private:

   Grid *RuleTable;
   wxListCtrl *PluginList;

   wxButton *AddRule;
   wxButton *DelRule;
   wxButton *MoveRuleUp;
   wxButton *MoveRuleDown;
   wxButton *MoveFilterUp;
   wxButton *MoveFilterDown;

   wxTextDataObject *dragtext1 {};
   wxTextDataObject *dragtext2 {};

   bool mCreateTable;
   wxWindow *mDragFocus;
   bool mFakeKeyEvent;
   bool mStopRecursiveSelection;

   int last_selected;

   void FakeOnPluginKeyDown (int keycode);
   void DoOnRuleTableKeyDown (int keycode);
   bool DoOnPluginKeyDown (int code);
   void DoOnRuleTableSelect (int toprow);
   void AddItemToTable (int index, const ExtImportItem *item);
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
   DECLARE_EVENT_TABLE()
};


class ExtImportPrefsFactory final : public PrefsPanelFactory
{
public:
   PrefsPanel *Create(wxWindow *parent) override;
};
#endif
