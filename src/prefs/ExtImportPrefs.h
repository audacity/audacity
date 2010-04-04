/**********************************************************************

  Audacity: A Digital Audio Editor

  ExtImportPrefs.h

  LRN

**********************************************************************/

#ifndef __AUDACITY_EXT_IMPORT_PREFS__
#define __AUDACITY_EXT_IMPORT_PREFS__

#include <wx/defs.h>

#include <wx/window.h>

#include "../ShuttleGui.h"

#include "PrefsPanel.h"

#include "../import/Import.h"
#include "../import/ImportPlugin.h"

class wxListEvent;

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
/*   void OnRuleTableKeyDown(wxListEvent& event);
   void OnRuleTableFocus(wxListEvent& event);
   void OnRuleTableActivate(wxListEvent& event);
   void OnRuleTableRightClick(wxListEvent& event);*/
   void OnAddRule(wxCommandEvent& event);
   void OnDelRule(wxCommandEvent& event);
   
 private:
 
   wxGrid *RuleTable;
   wxListCtrl *PluginList;
   
   wxButton *AddRule;
   wxButton *DelRule;
   
   int last_selected;

   void DoOnRuleTableSelect (int toprow);
   void AddItemToTable (int index, ExtImportItem *item);
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
   DECLARE_EVENT_TABLE()
};

#endif
