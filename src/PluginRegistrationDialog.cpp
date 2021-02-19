/*!*********************************************************************

  Audacity: A Digital Audio Editor

  @file PluginRegistrationDialog.cpp

  Paul Licameli split from PluginManager.cpp

**********************************************************************/
#include "PluginRegistrationDialog.h"

#include "EffectInterface.h"
#include "IncompatiblePluginsDialog.h"
#include "ModuleManager.h"
#include "PluginManager.h"
#include "PluginStartupRegistration.h"
#include "ShuttleGui.h"
#include "widgets/AudacityMessageBox.h"
#include "widgets/ProgressDialog.h"

#include <set>
#include <wx/setup.h> // for wxUSE_* macros
#include <wx/app.h>
#include <wx/defs.h>
#include <wx/dir.h>
#include <wx/dynlib.h>
#include <wx/filename.h>
#include <wx/listctrl.h>
#include <wx/radiobut.h>
#include <wx/wfstream.h>
#include <wx/utils.h>

#define DISABLE_STATE_NEW

// ============================================================================
//
//
//
// ============================================================================
#if wxUSE_ACCESSIBILITY
#include "WindowAccessible.h"

class CheckListAx final : public WindowAccessible
{
public:
   CheckListAx(wxListCtrl * window);

   virtual ~ CheckListAx();

   // Retrieves the address of an IDispatch interface for the specified child.
   // All objects must support this property.
   wxAccStatus GetChild( int childId, wxAccessible **child ) override;

   // Gets the number of children.
   wxAccStatus GetChildCount( int *childCount ) override;

   // Gets the default action for this object (0) or > 0 (the action for a child).
   // Return wxACC_OK even if there is no action. actionName is the action, or the empty
   // string if there is no action.
   // The retrieved string describes the action that is performed on an object,
   // not what the object does as a result. For example, a toolbar button that prints
   // a document has a default action of "Press" rather than "Prints the current document."
   wxAccStatus GetDefaultAction( int childId, wxString *actionName ) override;

   // Returns the description for this object or a child.
   wxAccStatus GetDescription( int childId, wxString *description ) override;

   // Gets the window with the keyboard focus.
   // If childId is 0 and child is NULL, no object in
   // this subhierarchy has the focus.
   // If this object has the focus, child should be 'this'.
   wxAccStatus GetFocus( int *childId, wxAccessible **child ) override;

   // Returns help text for this object or a child, similar to tooltip text.
   wxAccStatus GetHelpText( int childId, wxString *helpText ) override;

   // Returns the keyboard shortcut for this object or child.
   // Return e.g. ALT+K
   wxAccStatus GetKeyboardShortcut( int childId, wxString *shortcut ) override;

   // Returns the rectangle for this object (id = 0) or a child element (id > 0).
   // rect is in screen coordinates.
   wxAccStatus GetLocation( wxRect& rect, int elementId ) override;

   // Gets the name of the specified object.
   wxAccStatus GetName( int childId, wxString *name ) override;

   // Returns a role constant.
   wxAccStatus GetRole( int childId, wxAccRole *role ) override;

   // Gets a variant representing the selected children
   // of this object.
   // Acceptable values:
   // - a null variant (IsNull() returns TRUE)
   // - a list variant (GetType() == wxT("list"))
   // - an integer representing the selected child element,
   //   or 0 if this object is selected (GetType() == wxT("long"))
   // - a "void*" pointer to a wxAccessible child object
   wxAccStatus GetSelections( wxVariant *selections ) override;

   // Returns a state constant.
   wxAccStatus GetState( int childId, long* state ) override;

   // Returns a localized string representing the value for the object
   // or child.
   wxAccStatus GetValue( int childId, wxString *strValue ) override;

   void SetSelected( int item, bool focused = true );

private:
   wxListCtrl *mParent;
   int mLastId;
};

CheckListAx::CheckListAx( wxListCtrl * window )
:  WindowAccessible( window )
{
   mParent = window;
   mLastId = -1;
}

CheckListAx::~CheckListAx()
{
}

void CheckListAx::SetSelected( int item, bool focused )
{
   if (mLastId != -1)
   {
      NotifyEvent( wxACC_EVENT_OBJECT_SELECTIONREMOVE,
               mParent,
               wxOBJID_CLIENT,
               mLastId );
      mLastId = -1;
   }

   if (item != -1)
   {
      if (focused)
      {
         NotifyEvent( wxACC_EVENT_OBJECT_FOCUS,
                     mParent,
                     wxOBJID_CLIENT,
                     item + 1 );
      }

      NotifyEvent( wxACC_EVENT_OBJECT_SELECTION,
                  mParent,
                  wxOBJID_CLIENT,
                  item + 1 );

      mLastId = item + 1;
   }
}

// Retrieves the address of an IDispatch interface for the specified child.
// All objects must support this property.
wxAccStatus CheckListAx::GetChild( int childId, wxAccessible** child )
{
   if( childId == wxACC_SELF )
   {
      *child = this;
   }
   else
   {
      *child = NULL;
   }

   return wxACC_OK;
}

// Gets the number of children.
wxAccStatus CheckListAx::GetChildCount( int *childCount )
{
   *childCount = mParent->GetItemCount();

   return wxACC_OK;
}

// Gets the default action for this object (0) or > 0 (the action for a child).
// Return wxACC_OK even if there is no action. actionName is the action, or the empty
// string if there is no action.
// The retrieved string describes the action that is performed on an object,
// not what the object does as a result. For example, a toolbar button that prints
// a document has a default action of "Press" rather than "Prints the current document."
wxAccStatus CheckListAx::GetDefaultAction( int WXUNUSED(childId), wxString *actionName )
{
   actionName->clear();

   return wxACC_OK;
}

// Returns the description for this object or a child.
wxAccStatus CheckListAx::GetDescription( int WXUNUSED(childId), wxString *description )
{
   description->clear();

   return wxACC_OK;
}

// Gets the window with the keyboard focus.
// If childId is 0 and child is NULL, no object in
// this subhierarchy has the focus.
// If this object has the focus, child should be 'this'.
wxAccStatus CheckListAx::GetFocus( int *childId, wxAccessible **child )
{
   *childId = 0;
   *child = this;

   return wxACC_OK;
}

// Returns help text for this object or a child, similar to tooltip text.
wxAccStatus CheckListAx::GetHelpText( int WXUNUSED(childId), wxString *helpText )
{
   helpText->clear();

   return wxACC_OK;
}

// Returns the keyboard shortcut for this object or child.
// Return e.g. ALT+K
wxAccStatus CheckListAx::GetKeyboardShortcut( int WXUNUSED(childId), wxString *shortcut )
{
   shortcut->clear();

   return wxACC_OK;
}

// Returns the rectangle for this object (id = 0) or a child element (id > 0).
// rect is in screen coordinates.
wxAccStatus CheckListAx::GetLocation( wxRect& rect, int elementId )
{
   if( elementId == wxACC_SELF )
   {
      rect = mParent->GetRect();
      rect.SetPosition( mParent->GetParent()->ClientToScreen( rect.GetPosition() ) );
   }
   else
   {
      if( elementId <= mParent->GetItemCount() )
      {
         mParent->GetItemRect( elementId - 1, rect, wxLIST_RECT_LABEL );
         rect.SetPosition( mParent->ClientToScreen( rect.GetPosition() ) );
      }
   }

   return wxACC_OK;
}

// Gets the name of the specified object.
wxAccStatus CheckListAx::GetName( int WXUNUSED(childId), wxString *name )
{
   *name = mParent->GetName();

   return wxACC_OK;
}

// Returns a role constant.
wxAccStatus CheckListAx::GetRole( int childId, wxAccRole *role )
{
   if( childId == wxACC_SELF )
   {
      *role = wxROLE_SYSTEM_LIST;
   }
   else
   {
      *role = wxROLE_SYSTEM_LISTITEM;
   }

   return wxACC_OK;
}

// Gets a variant representing the selected children
// of this object.
// Acceptable values:
// - a null variant (IsNull() returns TRUE)
// - a list variant (GetType() == wxT("list"))
// - an integer representing the selected child element,
//   or 0 if this object is selected (GetType() == wxT("long"))
// - a "void*" pointer to a wxAccessible child object
wxAccStatus CheckListAx::GetSelections( wxVariant * WXUNUSED(selections) )
{
   return wxACC_NOT_IMPLEMENTED;
}

// Returns a state constant.
wxAccStatus CheckListAx::GetState( int childId, long *pState )
{
   int flag = wxACC_STATE_SYSTEM_FOCUSABLE;

   if( childId == wxACC_SELF )
   {
      flag |= wxACC_STATE_SYSTEM_FOCUSED;
   }
   else
   {
      wxListItem item;

      item.SetId( childId - 1 );
      item.SetState( wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED );
      item.SetMask( wxLIST_MASK_STATE );

      if( mParent->GetItem( item ) )
      {
         flag |= wxACC_STATE_SYSTEM_SELECTABLE;

         long state = item.GetState();

         if( state & wxLIST_STATE_FOCUSED )
         {
            flag |= wxACC_STATE_SYSTEM_FOCUSED;
         }

         if( state & wxLIST_STATE_SELECTED )
         {
            flag |= wxACC_STATE_SYSTEM_SELECTED;
         }
      }
   }

   *pState = flag;

   return wxACC_OK;
}

// Returns a localized string representing the value for the object
// or child.
wxAccStatus CheckListAx::GetValue( int childId, wxString *strValue )
{
   if( childId == 0 )
   {
      return wxACC_OK;
   }
   else
   {
      *strValue = mParent->GetItemText( childId - 1 );
   }

   return wxACC_OK;
}

#endif
enum
{
   STATE_Enabled,
   STATE_Disabled,
#ifndef DISABLE_STATE_NEW
   STATE_New,
#endif

   STATE_COUNT
};

enum
{
   ID_ShowAll = 10000,
   ID_ShowEnabled,
   ID_ShowDisabled,
#ifndef DISABLE_STATE_NEW
   ID_ShowNew,
#endif
   ID_List,
   ID_ClearAll,
   ID_SelectAll,
   ID_Rescan,
   ID_Enable,
   ID_Disable,
};

enum
{
   COL_Name,
   COL_State,
   COL_Path,

   COL_COUNT
};

BEGIN_EVENT_TABLE(PluginRegistrationDialog, wxDialogWrapper)
   EVT_LIST_COL_CLICK(ID_List, PluginRegistrationDialog::OnSort)
   EVT_BUTTON(wxID_OK, PluginRegistrationDialog::OnOK)
   EVT_BUTTON(wxID_CANCEL, PluginRegistrationDialog::OnCancel)
   EVT_BUTTON(ID_Rescan, PluginRegistrationDialog::OnRescan)
   EVT_BUTTON(ID_ClearAll, PluginRegistrationDialog::OnClearAll)
   EVT_BUTTON(ID_SelectAll, PluginRegistrationDialog::OnSelectAll)
   EVT_BUTTON(ID_Enable, PluginRegistrationDialog::OnEnable)
   EVT_BUTTON(ID_Disable, PluginRegistrationDialog::OnDisable)
   EVT_RADIOBUTTON(ID_ShowAll, PluginRegistrationDialog::OnChangedVisibility)
   EVT_RADIOBUTTON(ID_ShowEnabled, PluginRegistrationDialog::OnChangedVisibility)
   EVT_RADIOBUTTON(ID_ShowDisabled, PluginRegistrationDialog::OnChangedVisibility)
#ifndef DISABLE_STATE_NEW
   EVT_RADIOBUTTON(ID_ShowNew, PluginRegistrationDialog::OnChangedVisibility)
#endif
END_EVENT_TABLE()

PluginRegistrationDialog::PluginRegistrationDialog(wxWindow *parent)
:  wxDialogWrapper(parent,
            wxID_ANY,
            XO("Manage Plugins"),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
{
   mEffects = NULL;
   SetName();

   mStates.resize(STATE_COUNT);
   mStates[STATE_Enabled] = _("Enabled");
   mStates[STATE_Disabled] = _("Disabled");
#ifndef DISABLE_STATE_NEW
   mStates[STATE_New] = _("New");
#endif

   mSortColumn = COL_Name;
   mSortDirection = 1;

   Populate();

   DoSort( mSortColumn );
}

void PluginRegistrationDialog::Populate()
{
   //------------------------- Main section --------------------
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

/// Defines the dialog and does data exchange with it.
void PluginRegistrationDialog::PopulateOrExchange(ShuttleGui &S)
{
   S.StartVerticalLay(true);
   {
      /*i18n-hint: The dialog shows a list of plugins with check-boxes
       beside each one.*/
//      S.StartStatic(XO("Effects"), true);
      S.StartVerticalLay();
      {
         S.StartHorizontalLay(wxEXPAND, 0);
         {
            S.StartHorizontalLay(wxALIGN_LEFT, 0);
            {
               S.AddPrompt(XXO("Select effects, click the Enable or Disable button, then click OK."));
            }
            S.EndHorizontalLay();

            S.StartHorizontalLay(wxCENTER, 1);
            {
               S.AddSpace(1);
            }
            S.EndHorizontalLay();

            S.StartHorizontalLay(wxALIGN_NOT | wxALIGN_LEFT, 0);
            {
               wxRadioButton *rb;

               /* i18n-hint: This is before radio buttons selecting which effects to show */
               S.AddPrompt(XXO("Show:"));
               rb = S.Id(ID_ShowAll)
                  /* i18n-hint: Radio button to show all effects */
                  .Name(XO("Show all"))
                  /* i18n-hint: Radio button to show all effects */
                  .AddRadioButton(XXO("&All"));
#if wxUSE_ACCESSIBILITY
               // so that name can be set on a standard control
               rb->SetAccessible(safenew WindowAccessible(rb));
#endif

               rb = S.Id(ID_ShowDisabled)
                  /* i18n-hint: Radio button to show just the currently disabled effects */
                  .Name(XO("Show disabled"))
                  /* i18n-hint: Radio button to show just the currently disabled effects */
                  .AddRadioButtonToGroup(XXO("D&isabled"));
#if wxUSE_ACCESSIBILITY
               // so that name can be set on a standard control
               rb->SetAccessible(safenew WindowAccessible(rb));
#endif

               rb = S.Id(ID_ShowEnabled)
                  /* i18n-hint: Radio button to show just the currently enabled effects */
                  .Name(XO("Show enabled"))
                  /* i18n-hint: Radio button to show just the currently enabled effects */
                  .AddRadioButtonToGroup(XXO("E&nabled"));
#if wxUSE_ACCESSIBILITY
               // so that name can be set on a standard control
               rb->SetAccessible(safenew WindowAccessible(rb));
#endif

#ifndef DISABLE_STATE_NEW
               rb = S.Id(ID_ShowNew)
                  /* i18n-hint: Radio button to show just the newly discovered effects */
                  .Name(XO("Show new"))
                  /* i18n-hint: Radio button to show just the newly discovered effects */
                  .AddRadioButtonToGroup(XXO("Ne&w"));
#if wxUSE_ACCESSIBILITY
               // so that name can be set on a standard control
               rb->SetAccessible(safenew WindowAccessible(rb));
#endif
#endif
            }
            S.EndHorizontalLay();
         }
         S.EndHorizontalLay();

         mEffects = S.Id(ID_List)
            .Style(wxSUNKEN_BORDER | wxLC_REPORT | wxLC_HRULES | wxLC_VRULES )
            .ConnectRoot(wxEVT_KEY_DOWN,
                      &PluginRegistrationDialog::OnListChar)
            .AddListControlReportMode({ XO("Name"), XO("State"), XO("Path") });
#if wxUSE_ACCESSIBILITY
         mEffects->SetAccessible(mAx = safenew CheckListAx(mEffects));
#endif

         S.StartHorizontalLay(wxALIGN_LEFT | wxEXPAND, 0);
         {
            S.Id(ID_SelectAll).AddButton(XXO("&Select All"));
            S.Id(ID_ClearAll).AddButton(XXO("C&lear All"));
            S.Id(ID_Rescan).AddButton(XXO("Rescan"));

            S.StartHorizontalLay(wxALIGN_CENTER);
            {
               S.AddSpace(1);
            }
            S.EndHorizontalLay();

            S.Id(ID_Enable).AddButton(XXO("&Enable"));
            S.Id(ID_Disable).AddButton(XXO("&Disable"));
         }
         S.EndHorizontalLay();
      }
//      S.EndStatic();
      S.EndVerticalLay();

      S.AddStandardButtons(eOkButton | eCancelButton);
   }
   S.EndVerticalLay();

   std::vector<int> colWidths;
   for (int i = 0, cnt = mEffects->GetColumnCount(); i < cnt; i++)
   {
      colWidths.push_back(0);
   }

   for (int i = 0, cnt = mStates.size(); i < cnt; i++)
   {
      int x;
      mEffects->GetTextExtent(mStates[i], &x, NULL);
      colWidths[COL_State] = wxMax(colWidths[COL_State], x + 4);  // 2 pixel margin on each side
   }

   PluginManager & pm = PluginManager::Get();
   PopulateItemsList(pm);

   for (const auto& item : mItems)
   {
      const auto& itemData = item.second;

      int x;
      mEffects->GetTextExtent(itemData.name, &x, NULL);
      colWidths[COL_Name] = wxMax(colWidths[COL_Name], x);

      mEffects->GetTextExtent(itemData.path, &x, NULL);
      if (x > colWidths[COL_Path])
      {
         mLongestPath = itemData.path;
      }
      colWidths[COL_Path] = wxMax(colWidths[COL_Path], x);
   }

   wxRect r = wxGetClientDisplayRect();

   int maxW = 0;
   for (int i = 0, cnt = mEffects->GetColumnCount(); i < cnt; i++)
   {
      int w = colWidths[i] + /* fudge */ 10;
      mEffects->SetColumnWidth(i, w);
      maxW += w;
   }

   // Keep dialog from getting too wide
   int w = r.GetWidth() - (GetClientSize().GetWidth() - mEffects->GetSize().GetWidth());
   mEffects->SetMinSize({ std::min(maxW, w), 200 });
   mEffects->SetMaxSize({ w, -1 });

   RegenerateEffectsList(ID_ShowAll);

   Layout();
   Fit();

   wxSize sz = GetSize();
   sz.SetWidth(wxMin(sz.GetWidth(), r.GetWidth()));
   sz.SetHeight(wxMin(sz.GetHeight(), r.GetHeight()));
   SetMinSize(sz);

   // Parent window is usually not there yet, so centre on screen rather than on parent.
   CenterOnScreen();

   if (mEffects->GetItemCount() > 0)
   {
      // Make sure first item is selected/focused.
      mEffects->SetFocus();
      mEffects->SetItemState(0, wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED, wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED);
#if wxUSE_ACCESSIBILITY
      mAx->SetSelected(0);
#endif
   }

}

void PluginRegistrationDialog::PopulateItemsList(PluginManager& pm)
{
   for (auto& plug : pm.AllPlugins())
   {
      PluginType plugType = plug.GetPluginType();
      if (plugType != PluginTypeEffect && plugType != PluginTypeStub)
         continue;

      const auto& path = plug.GetPath();
      ItemData& item = mItems[path];  // will create NEW entry
      item.plugs.push_back(&plug);
      item.path = path;
      item.state = plug.IsEnabled() ? STATE_Enabled : STATE_Disabled;
      item.valid = plug.IsValid();

      if (plugType == PluginTypeEffect)
      {
         item.name = plug.GetSymbol().Translation();
      }
      // This is not right and will not work when other plugin types are added.
      // But it's presumed that the plugin manager dialog will be fully developed
      // by then.
      else if (plugType == PluginTypeStub)
      {
         wxFileName fname{ path };
         item.name = fname.GetName().Trim(false).Trim(true);
#ifndef DISABLE_STATE_NEW
         if (!item.valid)
         {
            item.state = STATE_New;
         }
#endif
      }
   }
}

void PluginRegistrationDialog::RegenerateEffectsList(int filter)
{
   mFilter = filter;

   mEffects->DeleteAllItems();

   int i = 0;
   for (ItemDataMap::iterator iter = mItems.begin(); iter != mItems.end(); ++iter)
   {
      ItemData & item = iter->second;
      bool add = false;

      switch (mFilter)
      {
      case ID_ShowAll:
         add = true;
         break;
#ifndef DISABLE_STATE_NEW
      case ID_ShowNew:
         if (item.state == STATE_New)
         {
            add = true;
         }
         break;
#endif
      case ID_ShowEnabled:
         if (item.state == STATE_Enabled)
         {
            add = true;
         }
         break;
      case ID_ShowDisabled:
         if (item.state == STATE_Disabled)
         {
            add = true;
         }
         break;
      }

      if (add)
      {
         mEffects->InsertItem(i, item.name);
         mEffects->SetItem(i, COL_State, mStates[item.state]);
         mEffects->SetItem(i, COL_Path, item.path);
         mEffects->SetItemPtrData(i, (wxUIntPtr) &item);

         ++i;
      }
   }

   mEffects->SortItems(SortCompare, (wxUIntPtr) this);

   if (mEffects->GetItemCount() > 0)
   {
      // Make sure first item is selected/focused.
//      mEffects->SetFocus();
      mEffects->SetItemState(0, wxLIST_STATE_FOCUSED|wxLIST_STATE_SELECTED, wxLIST_STATE_FOCUSED|wxLIST_STATE_SELECTED);
#if wxUSE_ACCESSIBILITY
      mAx->SetSelected(0, false);
#endif
   }
}

void PluginRegistrationDialog::SetState(int i, bool toggle, bool state)
{
    wxListItem li;

   li.m_mask = wxLIST_MASK_DATA;
   li.m_itemId = i;

   mEffects->GetItem(li);

   ItemData *item = (ItemData *) li.m_data;

#ifndef DISABLE_STATE_NEW
   // If changing the state of a "New" (stub) entry, then we mark it as valid
   // since it will either be registered if "Enabled" or ignored if "Disabled".
   if (item->state == STATE_New)
   {
      item->valid = true;
   }
#endif

   if (toggle)
   {
      item->state = item->state == STATE_Enabled ? STATE_Disabled : STATE_Enabled;
   }
   else
   {
      item->state = state;
   }
   
#ifndef DISABLE_STATE_NEW
   if (mFilter == ID_ShowNew && item->state != STATE_New)
   {
      mEffects->DeleteItem(i);
   }
   else//if
#endif
   if (mFilter == ID_ShowDisabled && item->state != STATE_Disabled)
   {
      mEffects->DeleteItem(i);
   }
   else if (mFilter == ID_ShowEnabled && item->state != STATE_Enabled)
   {
      mEffects->DeleteItem(i);
   }
   else
   {
      mEffects->SetItem(i, COL_State, mStates[item->state]);
#if wxUSE_ACCESSIBILITY
      mAx->SetSelected(i);
#endif
   }
}

int wxCALLBACK PluginRegistrationDialog::SortCompare(wxIntPtr item1, wxIntPtr item2, wxIntPtr sortData)
{
   PluginRegistrationDialog *dlg = (PluginRegistrationDialog *) sortData;
   ItemData *i1 = (ItemData *) item1;
   ItemData *i2 = (ItemData *) item2;

   return dlg->SortCompare(i1, i2);
}

int PluginRegistrationDialog::SortCompare(ItemData *item1, ItemData *item2)
{
   // This function is a three-valued comparator

   wxString *str1;
   wxString *str2;

   switch (mSortColumn)
   {
   case COL_Name:
      str1 = &item1->name;
      str2 = &item2->name;
      break;
   case COL_State:
      str1 = &mStates[item1->state];
      str2 = &mStates[item2->state];
      break;
   case COL_Path:
      str1 = &item1->path;
      str2 = &item2->path;
      break;
   default:
      return 0;
   }

   return str2->CmpNoCase(*str1) * mSortDirection;
}

void PluginRegistrationDialog::OnChangedVisibility(wxCommandEvent & evt)
{
   // Go and show the relevant items.
   RegenerateEffectsList(evt.GetId());
}

void PluginRegistrationDialog::OnSort(wxListEvent & evt)
{
   int col = evt.GetColumn();
   DoSort( col );
}

void PluginRegistrationDialog::DoSort( int col )
{
   if (col != mSortColumn)
   {
      mSortDirection = 1;
   }
   else
   {
      mSortDirection *= -1;
   }

   mSortColumn = col;
   mEffects->SortItems(SortCompare, (wxUIntPtr) this);

   // Without a refresh, wxMac doesn't redisplay the list properly after a sort
   mEffects->Refresh();
}

void PluginRegistrationDialog::OnListChar(wxKeyEvent & evt)
{
   switch (evt.GetKeyCode())
   {
      case WXK_SPACE:
      {
         int item = mEffects->GetNextItem(-1, wxLIST_NEXT_ALL, wxLIST_STATE_FOCUSED);
         if (item != wxNOT_FOUND)
         {
            SetState(item, true);
         }
      }
      break;

      case WXK_RETURN:
         // Don't know why wxListCtrls prevent default dialog action,
         // but they do, so handle it.
         EmulateButtonClickIfPresent(GetAffirmativeId());
      break;

      default:
         evt.Skip();
      break;
   }
}

void PluginRegistrationDialog::OnSelectAll(wxCommandEvent & WXUNUSED(evt))
{
   for (int i = 0, cnt = mEffects->GetItemCount(); i < cnt; i++)
   {
      mEffects->SetItemState(i, wxLIST_STATE_SELECTED, wxLIST_STATE_SELECTED);
   }
}

void PluginRegistrationDialog::OnClearAll(wxCommandEvent & WXUNUSED(evt))
{
   for (int i = 0, cnt = mEffects->GetItemCount(); i < cnt; i++)
   {
      mEffects->SetItemState(i, 0, wxLIST_STATE_SELECTED);
   }
}

void PluginRegistrationDialog::OnRescan(wxCommandEvent& WXUNUSED(evt))
{
   mItems.clear();
   mEffects->DeleteAllItems();
   mEffects->Update();

   wxTheApp->CallAfter([this] {
      std::set<PluginPath> disabledPlugins;
      std::vector<wxString> failedPlugins;

      auto& pm = PluginManager::Get();

      // Record list of plugins that are currently disabled
      for (auto& plug : pm.AllPlugins())
      {
         PluginType plugType = plug.GetPluginType();
         if (plugType != PluginTypeEffect && plugType != PluginTypeStub)
            continue;

         if (!plug.IsEnabled())
            disabledPlugins.insert(plug.GetPath());
      }

      pm.ClearEffectPlugins();

      auto newPlugins = PluginManager::Get().CheckPluginUpdates();
      if (!newPlugins.empty())
      {
         PluginStartupRegistration reg(newPlugins);
         reg.Run();

         failedPlugins = reg.GetFailedPluginsPaths();
      }

      // Disable all plugins which were previously disabled
      for (auto& plug : pm.AllPlugins())
      {
         PluginType plugType = plug.GetPluginType();
         if (plugType != PluginTypeEffect && plugType != PluginTypeStub)
            continue;

         const auto& path = plug.GetPath();
         if (disabledPlugins.find(path) != disabledPlugins.end())
            plug.SetEnabled(false);
      }

      pm.Save();

      if (!failedPlugins.empty())
      {
         auto dialog = safenew IncompatiblePluginsDialog(this, wxID_ANY, ScanType::Manual, failedPlugins);
         dialog->ShowModal();
      }

      PopulateItemsList(pm);
      RegenerateEffectsList(mFilter);
   });
}

void PluginRegistrationDialog::OnEnable(wxCommandEvent & WXUNUSED(evt))
{
   std::vector<long> items;

   {
      long i = mEffects->GetNextItem(-1, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED);
      while (i != wxNOT_FOUND)
      {
         items.insert(items.begin(), i);
         i = mEffects->GetNextItem(i, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED);
      }
   }

   for (size_t i = 0, cnt = items.size(); i < cnt; i++)
   {
      SetState(items[i], false, STATE_Enabled);
   }
}

void PluginRegistrationDialog::OnDisable(wxCommandEvent & WXUNUSED(evt))
{
   std::vector<long> items;

   {
      long i = mEffects->GetNextItem(-1, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED);
      while (i != wxNOT_FOUND)
      {
         items.insert(items.begin(), i);
         i = mEffects->GetNextItem(i, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED);
      }
   }

   for (size_t i = 0, cnt = items.size(); i < cnt; i++)
   {
      SetState(items[i], false, STATE_Disabled);
   }
}

void PluginRegistrationDialog::OnOK(wxCommandEvent & WXUNUSED(evt))
{
   PluginManager & pm = PluginManager::Get();
   ModuleManager & mm = ModuleManager::Get();

   int enableCount = 0;
   for (ItemDataMap::iterator iter = mItems.begin(); iter != mItems.end(); ++iter)
   {
      ItemData & item = iter->second;
      wxString path = item.path;

      if (item.state == STATE_Enabled && item.plugs[0]->GetPluginType() == PluginTypeStub)
      {
         enableCount++;
      }
   }

   wxString last3 = mLongestPath + wxT("\n") +
                    mLongestPath + wxT("\n") +
                    mLongestPath + wxT("\n");

   auto msg = XO("Enabling effects or commands:\n\n%s").Format( last3 );

   // Make sure the progress dialog is deleted before we call EndModal() or
   // we will leave the project window in an unusable state on OSX.
   // See bug #1192.
   {
      ProgressDialog progress{
         Verbatim( GetTitle() ), msg, pdlgHideStopButton };
      progress.CenterOnParent();

      int i = 0;
      for (ItemDataMap::iterator iter = mItems.begin(); iter != mItems.end(); ++iter)
      {
         ItemData & item = iter->second;
         wxString path = item.path;

         if (item.state == STATE_Enabled && item.plugs[0]->GetPluginType() == PluginTypeStub)
         {
            last3 = last3.AfterFirst(wxT('\n')) + item.path + wxT("\n");
            auto status = progress.Update(++i, enableCount,
               XO("Enabling effect or command:\n\n%s").Format( last3 ));
            if (status == ProgressResult::Cancelled)
            {
               break;
            }

            TranslatableString errMsgs;

            // Try to register the plugin via each provider until one succeeds
            for (size_t j = 0, cntj = item.plugs.size(); j < cntj; j++)
            {
               TranslatableString errMsg;
               if (mm.RegisterEffectPlugin(item.plugs[j]->GetProviderID(), path,
                                     errMsg))
               {
                  for (auto plug : item.plugs)
                     pm.UnregisterPlugin(
                        plug->GetProviderID() + wxT("_") + path);
                  // Bug 1893.  We've found a provider that works.
                  // Error messages from any that failed are no longer useful.
                  errMsgs = {};
                  break;
               }
               else
               {
                  if (!errMsgs.empty())
                     errMsgs.Join( errMsg, '\n' );
                  else
                     errMsgs = errMsg;
               }
            }
            if (!errMsgs.empty())
               AudacityMessageBox(
                  XO("Effect or Command at %s failed to register:\n%s")
                     .Format( path, errMsgs ) );
         }
#ifndef DISABLE_STATE_NEW
         else if (item.state == STATE_New) {
            for (auto plug : item.plugs)
               plug->SetValid(false);
         }
#endif
         else {
            for (auto plug : item.plugs) {
               plug->SetEnabled(item.state == STATE_Enabled);
               plug->SetValid(item.valid);
            }
         }
      }

      pm.Save();
   }

   EndModal(wxID_OK);
}

void PluginRegistrationDialog::OnCancel(wxCommandEvent & WXUNUSED(evt))
{
   EndModal(wxID_CANCEL);
}
