/**********************************************************************

  Audacity: A Digital Audio Editor

  ExtImportPrefs.h

  LRN

**********************************************************************/

#ifndef __AUDACITY_EXT_IMPORT_PREFS__
#define __AUDACITY_EXT_IMPORT_PREFS__

#include <wx/defs.h>
#include <wx/dnd.h> // to inherit wxDropTarget

#include "PrefsPanel.h"

#include "ImportPlugin.h"

class wxButton;
class wxGridEvent;
class wxGridRangeSelectEvent;
class wxListCtrl;
class wxListEvent;
class ExtImportItem;
class ExtImportPrefs;
class Grid;
class ShuttleGui;

#define EXT_IMPORT_PREFS_PLUGIN_SYMBOL ComponentInterfaceSymbol { XO("Ext Import") }

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
    void SetPrefs(ExtImportPrefs* prefs);
private:
    ExtImportPrefs* mPrefs;
};

class ExtImportPrefs final : public PrefsPanel
{
public:
    ExtImportPrefs(wxWindow* parent, wxWindowID winid);
    ~ExtImportPrefs();
    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;

    bool Commit() override;
    ManualPageID HelpPageName() override;
    void PopulateOrExchange(ShuttleGui& S) override;

    // See bug #2315 for discussion. This should be reviewed
    // and (possibly) removed after wx3.1.3.
    void OnShow(wxShowEvent& event);

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

    void OnNavKey(wxNavigationKeyEvent& event);

    void SwapRows(int row1, int row2);
    void SwapPluginRows(int row1, int row2);

    Grid* GetRuleTable() { return RuleTable; }
    wxListCtrl* GetPluginList() { return PluginList; }

    wxWindow* GetDragFocus() { return mDragFocus; }

private:

    Grid* RuleTable;
    wxListCtrl* PluginList;

    wxButton* AddRule;
    wxButton* DelRule;
    wxButton* MoveRuleUp;
    wxButton* MoveRuleDown;
    wxButton* MoveFilterUp;
    wxButton* MoveFilterDown;

    wxTextDataObject* dragtext1 {};
    wxTextDataObject* dragtext2 {};

    bool mCreateTable;
    wxWindow* mDragFocus;
    bool mFakeKeyEvent;
    bool mStopRecursiveSelection;

    int last_selected;

    void FakeOnPluginKeyDown(int keycode);
    void DoOnRuleTableKeyDown(int keycode);
    bool DoOnPluginKeyDown(int code);
    void DoOnRuleTableSelect(int toprow);
    void AddItemToTable(int index, const ExtImportItem* item);
    void Populate();
    DECLARE_EVENT_TABLE()
};

#endif
