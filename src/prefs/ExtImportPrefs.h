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

#include "../ShuttleGui.h"

#include "PrefsPanel.h"

#include "../import/Import.h"
#include "../import/ImportPlugin.h"

class wxListEvent;
class ExtImportPrefs;

class RuleTableDropTarget: public wxDropTarget
{
public:
   RuleTableDropTarget (wxDataObject *dataObject = 0);
   ~RuleTableDropTarget ();
   wxDragResult OnData(wxCoord x, wxCoord y, wxDragResult def);
   bool OnDrop(wxCoord x, wxCoord y);
   wxDragResult OnEnter(wxCoord x, wxCoord y, wxDragResult def);
   wxDragResult OnDragOver(wxCoord x, wxCoord y, wxDragResult def);
   void OnLeave();
   void SetDataObject(wxDataObject* data);
   void SetPrefs (ExtImportPrefs *prefs);
private:
   ExtImportPrefs *mPrefs;
};

class ExtImportPrefs:public PrefsPanel
{
 public:
   ExtImportPrefs(wxWindow * parent);
   ~ExtImportPrefs();
   virtual bool Apply();

   void OnPluginKeyDown(wxListEvent& event);
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
   
   Grid *RuleTable;
   
 private:
 
   wxListCtrl *PluginList;
   
   wxButton *AddRule;
   wxButton *DelRule;
   wxButton *MoveRuleUp;
   wxButton *MoveRuleDown;
   wxButton *MoveFilterUp;
   wxButton *MoveFilterDown;
   
   wxTextDataObject *dragtext;
   RuleTableDropTarget *dragtarget;

   bool mCreateTable;
   
   int last_selected;
   
   void DoOnRuleTableKeyDown (int keycode);
   bool DoOnPluginKeyDown (int code);
   void DoOnRuleTableSelect (int toprow);
   void AddItemToTable (int index, ExtImportItem *item);
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
   DECLARE_EVENT_TABLE()
};

#endif
