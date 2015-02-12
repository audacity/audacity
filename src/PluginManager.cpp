/**********************************************************************

  Audacity: A Digital Audio Editor

  PluginManager.cpp

  Leland Lucius

*******************************************************************//*!

\file PluginManager.cpp
\brief

*//*******************************************************************/

#include <algorithm>

#include "Audacity.h"

#include <wx/defs.h>
#include <wx/dialog.h>
#include <wx/dir.h>
#include <wx/dynarray.h>
#include <wx/dynlib.h>
#include <wx/hashmap.h>
#include <wx/filename.h>
#include <wx/icon.h>
#include <wx/imaglist.h>
#include <wx/list.h>
#include <wx/listctrl.h>
#include <wx/log.h>
#include <wx/string.h>
#include <wx/tokenzr.h>
#include <wx/wfstream.h>

#include "audacity/EffectInterface.h"

#include "AudacityApp.h"
#include "effects/EffectManager.h"
#include "FileNames.h"
#include "ModuleManager.h"
#include "PlatformCompatibility.h"
#include "Prefs.h"
#include "ShuttleGui.h"
#include "xml/XMLFileReader.h"
#include "xml/XMLWriter.h"

#include "PluginManager.h"

#include <wx/arrimpl.cpp>

WX_DECLARE_STRING_HASH_MAP(wxArrayString, ProviderMap);

// ============================================================================
//
//
//
// ============================================================================
#if wxUSE_ACCESSIBILITY

class CheckListAx: public wxWindowAccessible
{
public:
   CheckListAx(wxListCtrl * window);

   virtual ~ CheckListAx();

   // Retrieves the address of an IDispatch interface for the specified child.
   // All objects must support this property.
   virtual wxAccStatus GetChild( int childId, wxAccessible **child );

   // Gets the number of children.
   virtual wxAccStatus GetChildCount( int *childCount );

   // Gets the default action for this object (0) or > 0 (the action for a child).
   // Return wxACC_OK even if there is no action. actionName is the action, or the empty
   // string if there is no action.
   // The retrieved string describes the action that is performed on an object,
   // not what the object does as a result. For example, a toolbar button that prints
   // a document has a default action of "Press" rather than "Prints the current document."
   virtual wxAccStatus GetDefaultAction( int childId, wxString *actionName );

   // Returns the description for this object or a child.
   virtual wxAccStatus GetDescription( int childId, wxString *description );

   // Gets the window with the keyboard focus.
   // If childId is 0 and child is NULL, no object in
   // this subhierarchy has the focus.
   // If this object has the focus, child should be 'this'.
   virtual wxAccStatus GetFocus( int *childId, wxAccessible **child );

   // Returns help text for this object or a child, similar to tooltip text.
   virtual wxAccStatus GetHelpText( int childId, wxString *helpText );

   // Returns the keyboard shortcut for this object or child.
   // Return e.g. ALT+K
   virtual wxAccStatus GetKeyboardShortcut( int childId, wxString *shortcut );

   // Returns the rectangle for this object (id = 0) or a child element (id > 0).
   // rect is in screen coordinates.
   virtual wxAccStatus GetLocation( wxRect& rect, int elementId );

   // Gets the name of the specified object.
   virtual wxAccStatus GetName( int childId, wxString *name );

   // Returns a role constant.
   virtual wxAccStatus GetRole( int childId, wxAccRole *role );

   // Gets a variant representing the selected children
   // of this object.
   // Acceptable values:
   // - a null variant (IsNull() returns TRUE)
   // - a list variant (GetType() == wxT("list"))
   // - an integer representing the selected child element,
   //   or 0 if this object is selected (GetType() == wxT("long"))
   // - a "void*" pointer to a wxAccessible child object
   virtual wxAccStatus GetSelections( wxVariant *selections );

   // Returns a state constant.
   virtual wxAccStatus GetState( int childId, long* state );

   // Returns a localized string representing the value for the object
   // or child.
   virtual wxAccStatus GetValue( int childId, wxString *strValue );

   void SetSelected( int item );

private:
   wxListCtrl *mParent;
   int mLastId;
};

CheckListAx::CheckListAx( wxListCtrl * window ):
   wxWindowAccessible( window )
{
   mParent = window;
   mLastId = -1;
}

CheckListAx::~CheckListAx()
{
}

void CheckListAx::SetSelected( int item )
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
      NotifyEvent( wxACC_EVENT_OBJECT_FOCUS,
                  mParent,
                  wxOBJID_CLIENT,
                  item + 1 );

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
   actionName->Clear();

   return wxACC_OK;
}

// Returns the description for this object or a child.
wxAccStatus CheckListAx::GetDescription( int WXUNUSED(childId), wxString *description )
{
   description->Clear();

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
   helpText->Clear();

   return wxACC_OK;
}

// Returns the keyboard shortcut for this object or child.
// Return e.g. ALT+K
wxAccStatus CheckListAx::GetKeyboardShortcut( int WXUNUSED(childId), wxString *shortcut )
{
   shortcut->Clear();

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
wxAccStatus CheckListAx::GetState( int childId, long *state )
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
      item.SetMask( wxLIST_MASK_IMAGE | wxLIST_MASK_STATE );

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

         if( item.GetImage() != 0 )
         {
            flag |= wxACC_STATE_SYSTEM_CHECKED;
         }
      }
   }

   *state = flag;

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

// ============================================================================
//
//
//
// ============================================================================

#include "../images/Unchecked.xpm"
#include "../images/Checked.xpm"
#include "../images/Arrow15x15.xpm"

#define EffectListID       7001
#define EffectClearAllID   7002
#define EffectSelectAllID  7003

int wxCALLBACK SortCompare(long item1, long item2, long WXUNUSED(sortData))
{
   wxString *str1 = (wxString *) item1;
   wxString *str2 = (wxString *) item2;

#if defined(__WXMAC__)   
   return str2->Cmp(*str1);
#else
   return str1->Cmp(*str2);
#endif
}

class PluginRegistrationDialog : public wxDialog
{
public:
   // constructors and destructors
   PluginRegistrationDialog(ProviderMap & map);
   virtual ~PluginRegistrationDialog();

private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);

   void OnOK(wxCommandEvent & evt);
   void OnCancel(wxCommandEvent & evt);
   void OnListChar(wxKeyEvent & evt);
   void OnListMouseDown(wxMouseEvent & evt);
   void OnSelectAll(wxCommandEvent & evt);
   void OnClearAll(wxCommandEvent & evt);

   void SetBoldOrRegular(int i);
   void SetState(int i, int state);
   void ToggleItem(int i);

private:
   ModuleInterface *mMod;

#if wxUSE_ACCESSIBILITY
   CheckListAx *mAx;
#endif

   wxListCtrl *mEffects;
   PluginIDList mProvs;
   wxArrayString mPaths;
   wxArrayInt miState;

   bool mCancelClicked;

   ProviderMap & mMap;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(PluginRegistrationDialog, wxDialog)
   EVT_BUTTON(wxID_OK, PluginRegistrationDialog::OnOK)
   EVT_BUTTON(wxID_CANCEL, PluginRegistrationDialog::OnCancel)
   EVT_BUTTON(EffectClearAllID, PluginRegistrationDialog::OnClearAll)
   EVT_BUTTON(EffectSelectAllID, PluginRegistrationDialog::OnSelectAll)
END_EVENT_TABLE()

PluginRegistrationDialog::PluginRegistrationDialog(ProviderMap & map)
:  wxDialog(wxGetApp().GetTopWindow(),
            wxID_ANY,
            _("Register Effects"),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER),
   mMap(map)            
{
   mEffects = NULL;
   SetLabel(_("Register Effects"));         // Provide visual label
   SetName(_("Register Effects"));          // Provide audible label
   Populate();
   SetReturnCode(wxID_OK);
}

PluginRegistrationDialog::~PluginRegistrationDialog()
{
   mEffects->Disconnect(wxEVT_LEFT_DOWN,
                        wxMouseEventHandler(PluginRegistrationDialog::OnListMouseDown),
                        NULL,
                        this);
   mEffects->Disconnect(wxEVT_KEY_DOWN,
                        wxKeyEventHandler(PluginRegistrationDialog::OnListChar),
                        NULL,
                        this);

   for (int i = 0, cnt = mEffects->GetItemCount(); i < cnt; i++)
   {
      wxString *str = (wxString *) mEffects->GetItemData(i);
      if (str)
      {
         delete str;
      }
   }
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
   wxImageList * pImageList = new wxImageList( 15, 15 );

#define SHOW_UNCHECKED (0)
#define SHOW_CHECKED (1)
#define SHOW_ARROW (2)

#define COL_NAME (0)
#define COL_PATH (1)

   pImageList->Add(wxBitmap(unchecked_xpm));
   pImageList->Add(wxBitmap(checked_xpm));
   pImageList->Add(wxBitmap(arrow15x15_xpm));

   S.StartVerticalLay(true);
   {
      /*i18n-hint: The dialog shows a list of plugins with check-boxes 
       beside each one.*/
      S.StartStatic(_("&Select Plug-ins to Install or press ENTER to Install All"), true);
      {
         S.SetStyle(wxSUNKEN_BORDER | wxLC_REPORT | wxLC_SINGLE_SEL | wxLC_HRULES | wxLC_VRULES );
         mEffects = S.Id(EffectListID).AddListControlReportMode();
         mEffects->Connect(wxEVT_LEFT_DOWN,
                           wxMouseEventHandler(PluginRegistrationDialog::OnListMouseDown),
                           NULL,
                           this);
         mEffects->Connect(wxEVT_KEY_DOWN,
                           wxKeyEventHandler(PluginRegistrationDialog::OnListChar),
                           NULL,
                           this);
#if wxUSE_ACCESSIBILITY
         mAx = new CheckListAx(mEffects);
         mEffects->SetAccessible(mAx);
#endif
         mEffects->AssignImageList( pImageList, wxIMAGE_LIST_SMALL );
         mEffects->InsertColumn(COL_NAME, _("Plug-in Name"));
         mEffects->InsertColumn(COL_PATH, _("Path"));
      }
      S.EndStatic();

      S.StartHorizontalLay(wxALIGN_LEFT | wxEXPAND, false);
      {
         S.SetBorder(10);
         S.StartHorizontalLay(wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);
         {
            S.AddSpace(12);
            S.SetBorder(6);
            S.Id(EffectSelectAllID).AddButton(_("Select &All"));
            S.Id(EffectClearAllID).AddButton(_("Clea&r All"));
         }
         S.EndHorizontalLay();

         S.StartHorizontalLay(wxALIGN_CENTER | wxEXPAND);
         {
            S.AddSpace(1);
         }
         S.EndHorizontalLay();

         S.AddStandardButtons(eOkButton | eCancelButton);
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();

   // The dc is used to compute the text width in pixels.
   // FIXME: That works fine for PC, but apparently comes out too small for wxMAC.
   // iLen is minimum width in pixels shown for the file names.  200 is reasonable.
   int iNameLen = 0;
   int iPathLen = 0;
   int x, y;
   wxRect iconrect;

   int i = 0;
   for (ProviderMap::iterator iter = mMap.begin(); iter != mMap.end(); iter++, i++)
   {
      miState.Add( SHOW_CHECKED );

      wxFileName fname = iter->first;
      wxString name = fname.GetName();
      wxString path = iter->first;

      mEffects->InsertItem(i, name, SHOW_CHECKED);
      mEffects->SetItemPtrData(i, (wxUIntPtr) new wxString(name));
      mEffects->SetItem(i, COL_PATH, path);

      // Only need to get the icon width once 
      if (i == 0)
      {
#if defined(__WXMAC__)
         // wxMac doesn't return the ICON rectangle.  It returns the
         // rectangle for the first column and that even comes back
         // with negative numbers sometimes.
         // 
         // So, just guess.
         wxIcon i1(unchecked_xpm);
         wxIcon i2(checked_xpm);
         wxIcon i3(arrow15x15_xpm);
         iconrect.x = 4;
         iconrect.width = wxMax(wxMax(i1.GetWidth(), i2.GetWidth()), i3.GetWidth());
#else
         mEffects->GetItemRect( i, iconrect, wxLIST_RECT_ICON );
#endif
      }
      mEffects->GetTextExtent(name, &x, &y);
      iNameLen = wxMax(iNameLen, x + iconrect.width + (iconrect.x * 2));
      mEffects->GetTextExtent(path, &x, &y );
      iPathLen = wxMax(iPathLen, x + iconrect.width + (iconrect.x * 2));
   }

   mEffects->SortItems(SortCompare, 0);

   mEffects->SetColumnWidth(COL_NAME, iNameLen + /* fudge */ 5);
   mEffects->SetColumnWidth(COL_PATH, iPathLen + /* fudge */ 5);

   mEffects->SetSizeHints(iNameLen + iPathLen + /* fudge */ 15, 200);
   if (mPaths.size() > 0)
   {
      // Make sure first item is selected/focused.
      mEffects->SetFocus();
      mEffects->SetItemState(0, wxLIST_STATE_FOCUSED|wxLIST_STATE_SELECTED, wxLIST_STATE_FOCUSED|wxLIST_STATE_SELECTED);
#if wxUSE_ACCESSIBILITY
      mAx->SetSelected(0);
#endif
   }
   Layout();
   Fit();
   SetSizeHints(GetSize());
   // Parent window is usually not there yet, so centre on screen rather than on parent.
   CenterOnScreen();

}

void PluginRegistrationDialog::OnListMouseDown(wxMouseEvent & evt)
{
   wxPoint p = evt.GetPosition();
   int flags = wxLIST_HITTEST_ONITEM;
   int item = mEffects->HitTest(p, flags);

   if (item != wxNOT_FOUND)
   {
      ToggleItem(item);
   }

   evt.Skip();
}

void PluginRegistrationDialog::OnListChar(wxKeyEvent & evt)
{
   switch (evt.GetKeyCode())
   {
      case WXK_SPACE:
      {
         int iItem = mEffects->GetNextItem( -1, wxLIST_NEXT_ALL, wxLIST_STATE_FOCUSED);

         if (iItem != wxNOT_FOUND)
         {
            ToggleItem(iItem);
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

void PluginRegistrationDialog::SetBoldOrRegular(int i)
{
   wxFont Font = mEffects->GetItemFont(i);
   Font.SetWeight( (miState[i] == SHOW_CHECKED) ? wxFONTWEIGHT_BOLD : wxFONTWEIGHT_NORMAL);
   mEffects->SetItemFont(i, Font);
}

// We can't capture mouse clicks, only selected and deselected.
// Clicking on a selected item does not generate any event.
// Therefore our workaround solution is to NEVER actually select.
// So whenever the code tries to , we cancel the selection.
// That way we continue to get events.
void PluginRegistrationDialog::SetState(int i, int state)
{
   miState[i] = state;
   mEffects->SetItemImage(i, miState[i]);
#if wxUSE_ACCESSIBILITY
   mAx->SetSelected(i);
#endif
}

void PluginRegistrationDialog::ToggleItem(int i)
{
   SetState(i, miState[i] == SHOW_CHECKED ? SHOW_UNCHECKED : SHOW_CHECKED);
}

void PluginRegistrationDialog::OnSelectAll(wxCommandEvent & WXUNUSED(evt))
{
   for (size_t i = 0, cnt = miState.size(); i < cnt; i++)
   {
      SetState(i, SHOW_CHECKED);
   }
}

void PluginRegistrationDialog::OnClearAll(wxCommandEvent & WXUNUSED(evt))
{
   for (size_t i = 0, cnt = miState.size(); i < cnt; i++)
   {
      SetState(i, SHOW_UNCHECKED);
   }
}

void PluginRegistrationDialog::OnOK(wxCommandEvent & WXUNUSED(evt))
{
   mCancelClicked = false;
   FindWindowById(EffectListID)->Disable();
   FindWindowById(wxID_OK)->Disable();
   FindWindowById(EffectListID)->Disable();
   FindWindowById(EffectClearAllID)->Disable();
   FindWindowById(EffectSelectAllID)->Disable();

   PluginManager & pm = PluginManager::Get();
   ModuleManager & mm = ModuleManager::Get();

   wxListItem li;
   li.Clear();
   for (int i = 0, cnt = mEffects->GetItemCount(); i < cnt && !mCancelClicked; i++)
   {
      mEffects->EnsureVisible(i);
      li.SetId(i);
      li.SetColumn(COL_PATH);
      li.SetMask(wxLIST_MASK_TEXT);
      mEffects->GetItem(li);
      wxString path = li.GetText();

      // Create a placeholder descriptor to show we've seen this plugin before and not
      // to show it as new the next time Audacity starts.
      //
      // Placeholder descriptors have a plugin type of PluginTypeNone and the ID is the
      // path.
      PluginDescriptor & plug = pm.mPlugins[path];

      plug.SetID(path);
      plug.SetPath(path);
      plug.SetEnabled(false);
      plug.SetValid(false);

      if (miState[i] == SHOW_CHECKED)
      {
         mEffects->SetItemImage(i, SHOW_ARROW);
         wxArrayString providers = mMap[path];
         for (size_t j = 0, cnt = providers.GetCount(); j < cnt; j++)
         {
            if (mm.RegisterPlugin(providers[j], path))
            {
               break;
            }
         }
         mEffects->SetItemImage(i, SHOW_CHECKED);
      }
      wxYield();
   }

   EndModal(mCancelClicked ? wxID_CANCEL : wxID_OK);
}

void PluginRegistrationDialog::OnCancel(wxCommandEvent & WXUNUSED(evt))
{
   mCancelClicked = true;

   EndModal(mCancelClicked ? wxID_CANCEL : wxID_OK);
}



///////////////////////////////////////////////////////////////////////////////
//
// Plugindescriptor
//
///////////////////////////////////////////////////////////////////////////////

PluginDescriptor::PluginDescriptor()
{
   mPluginType = PluginTypeNone;
   mEnabled = false;
   mInstance = NULL;

   mEffectType = EffectTypeNone;
   mEffectInteractive = false;
   mEffectDefault = false;
   mEffectLegacy = false;
   mEffectRealtime = false;
   mEffectAutomatable = false;
}

PluginDescriptor::~PluginDescriptor()
{
   if (mInstance)
   {
      ModuleManager::Get().DeleteInstance(GetProviderID(), mInstance);
   }

   return;
}

bool PluginDescriptor::IsInstantiated()
{
   return mInstance != NULL;
}

IdentInterface *PluginDescriptor::GetInstance()
{
   if (!mInstance)
   {
      if (GetPluginType() == PluginTypeModule)
      {
         mInstance = ModuleManager::Get().CreateProviderInstance(GetID(), GetPath());
      }
      else
      {
         mInstance = ModuleManager::Get().CreateInstance(GetProviderID(), GetPath());
      }
   }

   return mInstance;
}

void PluginDescriptor::SetInstance(IdentInterface *instance)
{
   mInstance = instance;

   return;
}

PluginType PluginDescriptor::GetPluginType() const
{
   return mPluginType;
}

const PluginID & PluginDescriptor::GetID() const
{
   return mID;
}

const PluginID & PluginDescriptor::GetProviderID() const
{
   return mProviderID;
}

const wxString & PluginDescriptor::GetPath() const
{
   return mPath;
}

const wxString & PluginDescriptor::GetSymbol() const
{
   if (mSymbol.IsEmpty())
   {
      return mName;
   }

   return mSymbol;
}

wxString PluginDescriptor::GetName() const
{
   return wxGetTranslation(mName);
}

wxString PluginDescriptor::GetVersion() const
{
   return wxGetTranslation(mVersion);
}

wxString PluginDescriptor::GetVendor() const
{
   return wxGetTranslation(mVendor);
}

wxString PluginDescriptor::GetDescription() const
{
   return wxGetTranslation(mDescription);
}

bool PluginDescriptor::IsEnabled() const
{
   return mEnabled;
}

bool PluginDescriptor::IsValid() const
{
   return mValid;
}

void PluginDescriptor::SetPluginType(PluginType type)
{
   mPluginType = type;
}

void PluginDescriptor::SetID(const PluginID & ID)
{
   mID = ID;
}

void PluginDescriptor::SetProviderID(const PluginID & providerID)
{
   mProviderID = providerID;
}

void PluginDescriptor::SetPath(const wxString & path)
{
   mPath = path;
}

void PluginDescriptor::SetSymbol(const wxString & symbol)
{
   mSymbol = symbol;
}

void PluginDescriptor::SetName(const wxString & name)
{
   mName = name;
}

void PluginDescriptor::SetVersion(const wxString & version)
{
   mVersion = version;
}

void PluginDescriptor::SetVendor(const wxString & vendor)
{
   mVendor = vendor;
}

void PluginDescriptor::SetDescription(const wxString & description)
{
   mDescription = description;
}

void PluginDescriptor::SetEnabled(bool enable)
{
   mEnabled = enable;
}

void PluginDescriptor::SetValid(bool valid)
{
   mValid = valid;
}

// Effects

const wxString & PluginDescriptor::GetEffectFamily() const
{
   return mEffectFamily;
}

EffectType PluginDescriptor::GetEffectType() const
{
   return mEffectType;
}

bool PluginDescriptor::IsEffectInteractive() const
{
   return mEffectInteractive;
}

bool PluginDescriptor::IsEffectDefault() const
{
   return mEffectDefault;
}

bool PluginDescriptor::IsEffectLegacy() const
{
   return mEffectLegacy;
}

bool PluginDescriptor::IsEffectRealtime() const
{
   return mEffectRealtime;
}

bool PluginDescriptor::IsEffectAutomatable() const
{
   return mEffectAutomatable;
}

void PluginDescriptor::SetEffectFamily(const wxString & family)
{
   mEffectFamily = family;
}

void PluginDescriptor::SetEffectType(EffectType type)
{
   mEffectType = type;
}

void PluginDescriptor::SetEffectInteractive(bool interactive)
{
   mEffectInteractive = interactive;
}

void PluginDescriptor::SetEffectDefault(bool dflt)
{
   mEffectDefault = dflt;
}

void PluginDescriptor::SetEffectLegacy(bool legacy)
{
   mEffectLegacy = legacy;
}

void PluginDescriptor::SetEffectRealtime(bool realtime)
{
   mEffectRealtime = realtime;
}

void PluginDescriptor::SetEffectAutomatable(bool automatable)
{
   mEffectAutomatable = automatable;
}

// Importer

const wxString & PluginDescriptor::GetImporterIdentifier() const
{
   return mImporterIdentifier;
}

void PluginDescriptor::SetImporterIdentifier(const wxString & identifier)
{
   mImporterIdentifier = identifier;
}

const wxString & PluginDescriptor::GetImporterFilterDescription() const
{
   return mImporterFilterDesc;
}

void PluginDescriptor::SetImporterFilterDescription(const wxString & filterDesc)
{
   mImporterFilterDesc = filterDesc;
}

const wxArrayString & PluginDescriptor::GetImporterExtensions() const
{
   return mImporterExtensions;
}

void PluginDescriptor::SetImporterExtensions(const wxArrayString & extensions)
{
   mImporterExtensions = extensions;
}

///////////////////////////////////////////////////////////////////////////////
//
// PluginManager
//
///////////////////////////////////////////////////////////////////////////////

#define REGVERKEY wxString(wxT("/pluginregistryversion"))
#define REGVERCUR wxString(wxT("1.0"))
#define REGROOT wxString(wxT("/pluginregistry/"))

#define SETVERKEY wxString(wxT("/pluginsettingsversion"))
#define SETVERCUR wxString(wxT("1.0"))
#define SETROOT wxString(wxT("/pluginsettings/"))

#define KEY_ID                         wxT("ID")
#define KEY_PATH                       wxT("Path")
#define KEY_SYMBOL                     wxT("Symbol")
#define KEY_NAME                       wxT("Name")
#define KEY_VENDOR                     wxT("Vendor")
#define KEY_VERSION                    wxT("Version")
#define KEY_DESCRIPTION                wxT("Description")
#define KEY_LASTUPDATED                wxT("LastUpdated")
#define KEY_ENABLED                    wxT("Enabled")
#define KEY_VALID                      wxT("Valid")
#define KEY_PROVIDERID                 wxT("ProviderID")
#define KEY_EFFECTTYPE                 wxT("EffectType")
#define KEY_EFFECTFAMILY               wxT("EffectFamily")
#define KEY_EFFECTDEFAULT              wxT("EffectDefault")
#define KEY_EFFECTINTERACTIVE          wxT("EffectInteractive")
#define KEY_EFFECTREALTIME             wxT("EffectRealtime")
#define KEY_EFFECTAUTOMATABLE          wxT("EffectAutomatable")
#define KEY_EFFECTTYPE_NONE            wxT("None")
#define KEY_EFFECTTYPE_ANALYZE         wxT("Analyze")
#define KEY_EFFECTTYPE_GENERATE        wxT("Generate")
#define KEY_EFFECTTYPE_PROCESS         wxT("Process")
#define KEY_IMPORTERIDENT              wxT("ImporterIdent")
#define KEY_IMPORTERFILTER             wxT("ImporterFilter")
#define KEY_IMPORTEREXTENSIONS         wxT("ImporterExtensions")

// ============================================================================
//
// PluginManagerInterface implementation
//
// ============================================================================

const PluginID & PluginManager::RegisterModulePlugin(ModuleInterface *module)
{
   PluginDescriptor & plug = CreatePlugin(GetID(module), module, PluginTypeModule);

   plug.SetEnabled(true);
   plug.SetValid(true);

   return plug.GetID();
}

const PluginID & PluginManager::RegisterEffectPlugin(ModuleInterface *provider, EffectIdentInterface *effect)
{
   PluginDescriptor & plug = CreatePlugin(GetID(effect), effect, PluginTypeEffect);

   plug.SetProviderID(PluginManager::GetID(provider));

   plug.SetEffectType(effect->GetType());
   plug.SetEffectFamily(effect->GetFamily());
   plug.SetEffectInteractive(effect->IsInteractive());
   plug.SetEffectDefault(effect->IsDefault());
   plug.SetEffectRealtime(effect->SupportsRealtime());
   plug.SetEffectAutomatable(effect->SupportsAutomation());

   plug.SetEnabled(true);
   plug.SetValid(true);

   return plug.GetID();
}

const PluginID & PluginManager::RegisterImporterPlugin(ModuleInterface *provider, ImporterInterface *importer)
{
   PluginDescriptor & plug = CreatePlugin(GetID(importer), importer, PluginTypeImporter);

   plug.SetProviderID(PluginManager::GetID(provider));

   plug.SetImporterIdentifier(importer->GetPluginStringID());
   plug.SetImporterFilterDescription(importer->GetPluginFormatDescription());
   plug.SetImporterExtensions(importer->GetSupportedExtensions());

   return plug.GetID();
}

void PluginManager::FindFilesInPathList(const wxString & pattern,
                                        const wxArrayString & pathList,
                                        wxArrayString & files,
                                        bool directories)
{
   
   wxLogNull nolog;

   // Why bother...
   if (pattern.IsEmpty())
   {
      return;
   }

   // TODO:  We REALLY need to figure out the "Audacity" plug-in path(s)

   wxFileName f;
   wxArrayString paths;

   // Add the "per-user" plug-ins directory
   f = FileNames::PlugInDir();
   paths.Add(f.GetFullPath());
 
   // Add the "Audacity" plug-ins directory
   f = PlatformCompatibility::GetExecutablePath();
#if defined(__WXMAC__)
   f.RemoveLastDir();
   f.RemoveLastDir();
   f.RemoveLastDir();
#endif
   f.AppendDir(wxT("plug-ins"));
   paths.Add(f.GetPath());

   // Weed out duplicates
   for (size_t i = 0, cnt = pathList.size(); i < cnt; i++)
   {
      f = pathList[i];
      wxString path = f.GetFullPath();
      if (paths.Index(path, wxFileName::IsCaseSensitive()) == wxNOT_FOUND)
      {
         paths.Add(path);
      }
   }

   // Find all matching files in each path
   for (size_t i = 0, cnt = paths.GetCount(); i < cnt; i++)
   {
      f = paths[i] + wxFILE_SEP_PATH + pattern;
      wxDir::GetAllFiles(f.GetPath(), &files, f.GetFullName(), directories ? wxDIR_DEFAULT : wxDIR_FILES);
   }

   return;
}

bool PluginManager::GetSharedConfigSubgroups(const PluginID & ID, const wxString & group, wxArrayString & subgroups)
{
   return GetSubgroups(SharedGroup(ID, group), subgroups);
}

bool PluginManager::GetSharedConfig(const PluginID & ID, const wxString & group, const wxString & key, wxString & value, const wxString & defval)
{
   return GetConfig(SharedKey(ID, group, key), value, defval);
}

bool PluginManager::GetSharedConfig(const PluginID & ID, const wxString & group, const wxString & key, int & value, int defval)
{
   return GetConfig(SharedKey(ID, group, key), value, defval);
}

bool PluginManager::GetSharedConfig(const PluginID & ID, const wxString & group, const wxString & key, bool & value, bool defval)
{
   return GetConfig(SharedKey(ID, group, key), value, defval);
}

bool PluginManager::GetSharedConfig(const PluginID & ID, const wxString & group, const wxString & key, float & value, float defval)
{
   return GetConfig(SharedKey(ID, group, key), value, defval);
}

bool PluginManager::GetSharedConfig(const PluginID & ID, const wxString & group, const wxString & key, double & value, double defval)
{
   return GetConfig(SharedKey(ID, group, key), value, defval);
}

bool PluginManager::GetSharedConfig(const PluginID & ID, const wxString & group, const wxString & key, sampleCount & value, sampleCount defval)
{
   return GetConfig(SharedKey(ID, group, key), value, defval);
}

bool PluginManager::SetSharedConfig(const PluginID & ID, const wxString & group, const wxString & key, const wxString & value)
{
   return SetConfig(SharedKey(ID, group, key), value);
}

bool PluginManager::SetSharedConfig(const PluginID & ID, const wxString & group, const wxString & key, const int & value) 
{
   return SetConfig(SharedKey(ID, group, key), value);
}

bool PluginManager::SetSharedConfig(const PluginID & ID, const wxString & group, const wxString & key, const bool & value)
{
   return SetConfig(SharedKey(ID, group, key), value);
}

bool PluginManager::SetSharedConfig(const PluginID & ID, const wxString & group, const wxString & key, const float & value)
{
   return SetConfig(SharedKey(ID, group, key), value);
}

bool PluginManager::SetSharedConfig(const PluginID & ID, const wxString & group, const wxString & key, const double & value)
{
   return SetConfig(SharedKey(ID, group, key), value);
}

bool PluginManager::SetSharedConfig(const PluginID & ID, const wxString & group, const wxString & key, const sampleCount & value)
{
   return SetConfig(SharedKey(ID, group, key), value);
}

bool PluginManager::RemoveSharedConfigSubgroup(const PluginID & ID, const wxString & group)
{
   bool result = GetSettings()->DeleteGroup(SharedGroup(ID, group));
   if (result)
   {
      GetSettings()->Flush();
   }

   return result;
}

bool PluginManager::RemoveSharedConfig(const PluginID & ID, const wxString & group, const wxString & key)
{
   bool result = GetSettings()->DeleteEntry(SharedKey(ID, group, key));
   if (result)
   {
      GetSettings()->Flush();
   }

   return result;
}

bool PluginManager::GetPrivateConfigSubgroups(const PluginID & ID, const wxString & group, wxArrayString & subgroups)
{
   return GetSubgroups(PrivateGroup(ID, group), subgroups);
}

bool PluginManager::GetPrivateConfig(const PluginID & ID, const wxString & group, const wxString & key, wxString & value, const wxString & defval)
{
   return GetConfig(PrivateKey(ID, group, key), value, defval);
}

bool PluginManager::GetPrivateConfig(const PluginID & ID, const wxString & group, const wxString & key, int & value, int defval)
{
   return GetConfig(PrivateKey(ID, group, key), value, defval);
}

bool PluginManager::GetPrivateConfig(const PluginID & ID, const wxString & group, const wxString & key, bool & value, bool defval)
{
   return GetConfig(PrivateKey(ID, group, key), value, defval);
}

bool PluginManager::GetPrivateConfig(const PluginID & ID, const wxString & group, const wxString & key, float & value, float defval)
{
   return GetConfig(PrivateKey(ID, group, key), value, defval);
}

bool PluginManager::GetPrivateConfig(const PluginID & ID, const wxString & group, const wxString & key, double & value, double defval)
{
   return GetConfig(PrivateKey(ID, group, key), value, defval);
}

bool PluginManager::GetPrivateConfig(const PluginID & ID, const wxString & group, const wxString & key, sampleCount & value, sampleCount defval)
{
   return GetConfig(PrivateKey(ID, group, key), value, defval);
}

bool PluginManager::SetPrivateConfig(const PluginID & ID, const wxString & group, const wxString & key, const wxString & value)
{
   return SetConfig(PrivateKey(ID, group, key), value);
}

bool PluginManager::SetPrivateConfig(const PluginID & ID, const wxString & group, const wxString & key, const int & value) 
{
   return SetConfig(PrivateKey(ID, group, key), value);
}

bool PluginManager::SetPrivateConfig(const PluginID & ID, const wxString & group, const wxString & key, const bool & value)
{
   return SetConfig(PrivateKey(ID, group, key), value);
}

bool PluginManager::SetPrivateConfig(const PluginID & ID, const wxString & group, const wxString & key, const float & value)
{
   return SetConfig(PrivateKey(ID, group, key), value);
}

bool PluginManager::SetPrivateConfig(const PluginID & ID, const wxString & group, const wxString & key, const double & value)
{
   return SetConfig(PrivateKey(ID, group, key), value);
}

bool PluginManager::SetPrivateConfig(const PluginID & ID, const wxString & group, const wxString & key, const sampleCount & value)
{
   return SetConfig(PrivateKey(ID, group, key), value);
}

bool PluginManager::RemovePrivateConfigSubgroup(const PluginID & ID, const wxString & group)
{
   bool result = GetSettings()->DeleteGroup(PrivateGroup(ID, group));
   if (result)
   {
      GetSettings()->Flush();
   }

   return result;
}

bool PluginManager::RemovePrivateConfig(const PluginID & ID, const wxString & group, const wxString & key)
{
   bool result = GetSettings()->DeleteEntry(PrivateKey(ID, group, key));
   if (result)
   {
      GetSettings()->Flush();
   }

   return result;
}

// ============================================================================
//
// PluginManager
//
// ============================================================================

// The one and only PluginManager
PluginManager *PluginManager::mInstance = NULL;

// ----------------------------------------------------------------------------
// Creation/Destruction
// ----------------------------------------------------------------------------

PluginManager::PluginManager()
{
   mSettings = NULL;
}

PluginManager::~PluginManager()
{
   if (mSettings)
   {
      delete mSettings;
   }
}

// ----------------------------------------------------------------------------
// PluginManager implementation
// ----------------------------------------------------------------------------

// ============================================================================
//
// Return reference to singleton
//
// (Thread-safe...no active threading during construction or after destruction)
// ============================================================================

PluginManager & PluginManager::Get()
{
   if (!mInstance)
   {
      mInstance = new PluginManager();
   }

   return *mInstance;
}

void PluginManager::Destroy()
{
   if (mInstance)
   {
      delete mInstance;
   }
}

void PluginManager::Initialize()
{
   // Always load the registry first
   Load();

   // Then look for providers (they may autoregister plugins)
   ModuleManager::Get().DiscoverProviders();

   // And finally check for updates
   CheckForUpdates();
}

void PluginManager::Terminate()
{
   // Get rid of all non-module plugins first
   PluginMap::iterator iter = mPlugins.begin();
   while (iter != mPlugins.end())
   {
      PluginDescriptor & plug = iter->second;
      if (plug.GetPluginType() == PluginTypeEffect)
      {
         mPlugins.erase(iter++);
         continue;
      }

      ++iter;
   }

   // Now get rid of the modules
   iter = mPlugins.begin();
   while (iter != mPlugins.end())
   {
      mPlugins.erase(iter++);
   }
}

void PluginManager::Load()
{
   // Get the full scan setting
   bool doRescan;
   gPrefs->Read(wxT("/Plugins/Rescan"), &doRescan, true);

   // Create/Open the registry
   mRegistry = new wxFileConfig(wxEmptyString, wxEmptyString, FileNames::PluginRegistry());

   // If this group doesn't exist then we have something that's not a registry.
   // We should probably warn the user, but it's pretty unlikely that this will happen.
   if (!mRegistry->HasGroup(REGROOT) || doRescan)
   {
      // Must start over
      mRegistry->DeleteAll();
      delete mRegistry;
      return;
   }

   // Check for a registry version that we can understand
   wxString regver = mRegistry->Read(REGVERKEY);
   if (regver < REGVERCUR )
   {
      // This is where we'd put in conversion code when the
      // registry version changes.
      //
      // Should also check for a registry file that is newer than
      // what we can understand.
   }

   // Load all provider plugins first
   LoadGroup(PluginTypeModule);

   // Now the rest
   LoadGroup(PluginTypeEffect);
   LoadGroup(PluginTypeExporter);
   LoadGroup(PluginTypeImporter);

   LoadGroup(PluginTypeNone);

   delete mRegistry;

   return;
}

void PluginManager::LoadGroup(PluginType type)
{
   wxString strVal;
   bool boolVal;
   wxString groupName;
   long groupIndex;
   wxString group = GetPluginTypeString(type);
   wxString cfgPath = REGROOT + group + wxCONFIG_PATH_SEPARATOR;

   mRegistry->SetPath(cfgPath);
   for (bool cont = mRegistry->GetFirstGroup(groupName, groupIndex);
        cont;
        mRegistry->SetPath(cfgPath),
        cont = mRegistry->GetNextGroup(groupName, groupIndex))
   {
      PluginDescriptor plug;

      mRegistry->SetPath(groupName);

      groupName = ConvertID(groupName);

      // Bypass group if the ID is already in use
      if (mPlugins.find(groupName) != mPlugins.end())
      {
         mRegistry->SetPath(wxT(".."));

         continue;
      }

      // Set the ID and type
      plug.SetID(groupName);
      plug.SetPluginType(type);

      // Get the provider ID and bypass group if not found
      if (!mRegistry->Read(KEY_PROVIDERID, &strVal, wxEmptyString))
      {
         // Bypass group if the provider isn't valid
         if (!strVal.IsEmpty() && mPlugins.find(strVal) == mPlugins.end())
         {
            continue;
         }
      }
      plug.SetProviderID(PluginID(strVal));

      // Get the path (optional)
      mRegistry->Read(KEY_PATH, &strVal, wxEmptyString);
      plug.SetPath(strVal);

      // Get the name and bypass group if not found
      if (!mRegistry->Read(KEY_NAME, &strVal))
      {
         continue;
      }
      plug.SetName(strVal);

      // Get the symbol...use name if not found
      if (!mRegistry->Read(KEY_SYMBOL, &strVal))
      {
         strVal = plug.GetName();
      }
      plug.SetSymbol(strVal);

      // Get the version and bypass group if not found
      if (!mRegistry->Read(KEY_VERSION, &strVal))
      {
         continue;
      }
      plug.SetVersion(strVal);

      // Get the vendor and bypass group if not found
      if (!mRegistry->Read(KEY_VENDOR, &strVal))
      {
         continue;
      }
      plug.SetVendor(strVal);

      // Get the description and bypass group if not found
      if (!mRegistry->Read(KEY_DESCRIPTION, &strVal))
      {
         continue;
      }
      plug.SetDescription(strVal);

      // Is it enabled...default to no if not found
      mRegistry->Read(KEY_ENABLED, &boolVal, false);
      plug.SetEnabled(boolVal);

      // Is it valid...default to no if not found
      mRegistry->Read(KEY_VALID, &boolVal, false);
      plug.SetValid(boolVal);

      switch (type)
      {
         case PluginTypeModule:
         {
            // Nothing to do here yet
         }
         break;

         case PluginTypeEffect:
         {
            // Get the effect type and bypass group if not found
            if (!mRegistry->Read(KEY_EFFECTTYPE, &strVal))
            {
               continue;
            }

            if (strVal.IsSameAs(KEY_EFFECTTYPE_NONE))
            {
               plug.SetEffectType(EffectTypeNone);
            }
            else if (strVal.IsSameAs(KEY_EFFECTTYPE_ANALYZE))
            {
               plug.SetEffectType(EffectTypeAnalyze);
            }
            else if (strVal.IsSameAs(KEY_EFFECTTYPE_GENERATE))
            {
               plug.SetEffectType(EffectTypeGenerate);
            }
            else if (strVal.IsSameAs(KEY_EFFECTTYPE_PROCESS))
            {
               plug.SetEffectType(EffectTypeProcess);
            }
            else
            {
               continue;
            }

            // Get the effect family and bypass group if not found
            if (!mRegistry->Read(KEY_EFFECTFAMILY, &strVal))
            {
               continue;
            }
            plug.SetEffectFamily(strVal);

            // Is it a default (above the line) effect and bypass group if not found
            if (!mRegistry->Read(KEY_EFFECTDEFAULT, &boolVal))
            {
               continue;
            }
            plug.SetEffectDefault(boolVal);

            // Is it an interactive effect and bypass group if not found
            if (!mRegistry->Read(KEY_EFFECTINTERACTIVE, &boolVal))
            {
               continue;
            }
            plug.SetEffectInteractive(boolVal);

            // Is it a realtime capable effect and bypass group if not found
            if (!mRegistry->Read(KEY_EFFECTREALTIME, &boolVal))
            {
               continue;
            }
            plug.SetEffectRealtime(boolVal);

            // Does the effect support automation...bypass group if not found
            if (!mRegistry->Read(KEY_EFFECTAUTOMATABLE, &boolVal))
            {
               continue;
            }
            plug.SetEffectAutomatable(boolVal);
         }
         break;

         case PluginTypeImporter:
         {
            // Get the importer identifier and bypass group if not found
            if (!mRegistry->Read(KEY_IMPORTERIDENT, &strVal))
            {
               continue;
            }
            plug.SetImporterIdentifier(strVal);

            // Get the importer filter description and bypass group if not found
            if (!mRegistry->Read(KEY_IMPORTERFILTER, &strVal))
            {
               continue;
            }
            plug.SetImporterFilterDescription(strVal);

            // Get the importer extensions and bypass group if not found
            if (!mRegistry->Read(KEY_IMPORTEREXTENSIONS, &strVal))
            {
               continue;
            }
            wxArrayString extensions;
            wxStringTokenizer tkr(strVal, wxT(":"));
            while (tkr.HasMoreTokens())
            {
               extensions.Add(tkr.GetNextToken());
            }
            plug.SetImporterExtensions(extensions);
         }
         break;

         case PluginTypeNone:
         {
            // Used for placeholder groups
         }
         break;

         default:
         {
            continue;
         }
      }

      // Everything checked out...accept the plugin
      mPlugins[groupName] = plug;
   }

   return;
}

void PluginManager::Save()
{
   // Create/Open the registry
   mRegistry = new wxFileConfig(wxEmptyString, wxEmptyString, FileNames::PluginRegistry());

   // Write the version string
   mRegistry->Write(REGVERKEY, REGVERCUR);

   // Save the individual groups
   SaveGroup(PluginTypeEffect);
   SaveGroup(PluginTypeExporter);
   SaveGroup(PluginTypeImporter);
   SaveGroup(PluginTypeNone);

   // And now the providers
   SaveGroup(PluginTypeModule);

   // Just to be safe
   mRegistry->Flush();

   delete mRegistry;
}

void PluginManager::SaveGroup(PluginType type)
{
   wxString group = GetPluginTypeString(type);
   for (PluginMap::iterator iter = mPlugins.begin(); iter != mPlugins.end(); iter++)
   {
      PluginDescriptor & plug = iter->second;

      if (plug.GetPluginType() != type)
      {
         continue;
      }

      mRegistry->SetPath(REGROOT + group + wxCONFIG_PATH_SEPARATOR + ConvertID(plug.GetID()));

      mRegistry->Write(KEY_PATH, plug.GetPath());
      mRegistry->Write(KEY_SYMBOL, plug.GetSymbol());
      mRegistry->Write(KEY_NAME, plug.GetName());
      mRegistry->Write(KEY_VERSION, plug.GetVersion());
      mRegistry->Write(KEY_VENDOR, plug.GetVendor());
      mRegistry->Write(KEY_DESCRIPTION, plug.GetDescription());
      mRegistry->Write(KEY_PROVIDERID, plug.GetProviderID());
      mRegistry->Write(KEY_ENABLED, plug.IsEnabled());
      mRegistry->Write(KEY_VALID, plug.IsValid());

      switch (type)
      {
         case PluginTypeModule:
         break;

         case PluginTypeEffect:
         {
            EffectType etype = plug.GetEffectType();
            wxString stype;
            if (etype == EffectTypeNone)
            {
               stype = KEY_EFFECTTYPE_NONE;
            }
            else if (etype == EffectTypeAnalyze)
            {
               stype = KEY_EFFECTTYPE_ANALYZE;
            }
            else if (etype == EffectTypeGenerate)
            {
               stype = KEY_EFFECTTYPE_GENERATE;
            }
            else if (etype == EffectTypeProcess)
            {
               stype = KEY_EFFECTTYPE_PROCESS;
            }
            mRegistry->Write(KEY_EFFECTTYPE, stype);
            mRegistry->Write(KEY_EFFECTFAMILY, plug.GetEffectFamily());
            mRegistry->Write(KEY_EFFECTDEFAULT, plug.IsEffectDefault());
            mRegistry->Write(KEY_EFFECTINTERACTIVE, plug.IsEffectInteractive());
            mRegistry->Write(KEY_EFFECTREALTIME, plug.IsEffectRealtime());
            mRegistry->Write(KEY_EFFECTAUTOMATABLE, plug.IsEffectAutomatable());
         }
         break;

         case PluginTypeImporter:
         {
            mRegistry->Write(KEY_IMPORTERIDENT, plug.GetImporterIdentifier());
            mRegistry->Write(KEY_IMPORTERFILTER, plug.GetImporterFilterDescription());
            const wxArrayString & extensions = plug.GetImporterExtensions();
            wxString strExt;
            for (size_t i = 0, cnt = extensions.size(); i < cnt; i++)
            {
               strExt += extensions[i] + wxT(":");
            }
            strExt.RemoveLast(1);
            mRegistry->Write(KEY_IMPORTEREXTENSIONS, strExt);
         }
         break;

         default:
         break;
      }
   }

   return;
}

void PluginManager::CheckForUpdates()
{
   // Get ModuleManager reference
   ModuleManager & mm = ModuleManager::Get();

   // Get the full scan and check for update settings
   bool doRescan;
   bool doCheck;
   gPrefs->Read(wxT("/Plugins/Rescan"), &doRescan, true);
   gPrefs->Read(wxT("/Plugins/CheckForUpdates"), &doCheck, true);

   ProviderMap map;

   // Always check for and disable missing plugins
   // 
   // Since the user's saved presets are in the registery, never delete them.  That is
   // a job for the plugin manager UI (once it is written)
   // Check for plugins that are no longer valid
   PluginMap::iterator iter = mPlugins.begin();
   while (iter != mPlugins.end())
   {
      PluginDescriptor & plug = iter->second;
      const PluginID & plugID = plug.GetID();
      const wxString & plugPath = plug.GetPath();

      if (plug.GetPluginType() == PluginTypeModule)
      {
         if (!mm.IsProviderValid(plugID, plugPath))
         {
            plug.SetValid(false);
         }
         else
         {
            // Only collect plugin paths if we're doing a full scan or checking for updates
            if (doRescan || doCheck)
            {
               wxArrayString paths = mm.FindPluginsForProvider(plugID, plugPath);
               for (size_t i = 0, cnt = paths.GetCount(); i < cnt; i++)
               {
                  map[paths[i]].Add(plugID);
               }
            }
         }
      }
      else
      {
         plug.SetValid(mm.IsPluginValid(plug.GetProviderID(), plugPath));
      }

      iter++;
   }

   // If we're only checking for new plugins, then remove all of the known ones
   if (doCheck && !doRescan)
   {
      for (PluginMap::iterator iter = mPlugins.begin(); iter != mPlugins.end(); iter++)
      {
         PluginDescriptor & plug = iter->second;
         const wxString & plugPath = plug.GetPath();
         ProviderMap::iterator mapiter = map.find(plugPath);
         if (mapiter != map.end())
         {
            map.erase(mapiter);
         }
      }
   }

   // Allow the user to choose which ones to enable
   if (map.size() != 0)
   {
      PluginRegistrationDialog dlg(map);
      if (dlg.ShowModal() == wxID_OK)
      {
         gPrefs->Write(wxT("/Plugins/Rescan"), false);
      }
   }

   Save();

   return;
}

int PluginManager::GetPluginCount(PluginType type)
{
   int num = 0;

   for (PluginMap::iterator iter = mPlugins.begin(); iter != mPlugins.end(); iter++)
   {
      if (iter->second.GetPluginType() == type)
      {
         num++;
      }
   }

   return num;
}

const PluginDescriptor *PluginManager::GetPlugin(const PluginID & ID)
{
   if (mPlugins.find(ID) == mPlugins.end())
   {
      return NULL;
   }

   return &mPlugins[ID];
}

const PluginDescriptor *PluginManager::GetFirstPlugin(PluginType type)
{
   for (mPluginsIter = mPlugins.begin(); mPluginsIter != mPlugins.end(); mPluginsIter++)
   {
      PluginDescriptor & plug = mPluginsIter->second;
      bool familyEnabled = true;
      if (type == PluginTypeEffect)
      {
         gPrefs->Read(plug.GetEffectFamily() + wxT("/Enable"), &familyEnabled, true);
      }
      if (plug.IsValid() && plug.IsEnabled() && plug.GetPluginType() == type && familyEnabled)
      {
         return &mPluginsIter->second;
      }
   }

   return NULL;
}

const PluginDescriptor *PluginManager::GetNextPlugin(PluginType type)
{
   while (++mPluginsIter != mPlugins.end())
   {
      PluginDescriptor & plug = mPluginsIter->second;
      bool familyEnabled = true;
      if (type == PluginTypeEffect)
      {
         gPrefs->Read(plug.GetEffectFamily() + wxT("/Enable"), &familyEnabled, true);
      }
      if (plug.IsValid() && plug.IsEnabled() && plug.GetPluginType() == type && familyEnabled)
      {
         return &mPluginsIter->second;
      }
   }

   return NULL;
}

const PluginDescriptor *PluginManager::GetFirstPluginForEffectType(EffectType type)
{
   for (mPluginsIter = mPlugins.begin(); mPluginsIter != mPlugins.end(); mPluginsIter++)
   {
      PluginDescriptor & plug = mPluginsIter->second;

      bool familyEnabled;
      gPrefs->Read(plug.GetEffectFamily() + wxT("/Enable"), &familyEnabled, true);
      if (plug.IsValid() && plug.IsEnabled() && plug.GetEffectType() == type && familyEnabled)
      {
         return &plug;
      }
   }

   return NULL;
}

const PluginDescriptor *PluginManager::GetNextPluginForEffectType(EffectType type)
{
   while (++mPluginsIter != mPlugins.end())
   {
      PluginDescriptor & plug = mPluginsIter->second;
      bool familyEnabled;
      gPrefs->Read(plug.GetEffectFamily() + wxT("/Enable"), &familyEnabled, true);
      if (plug.IsValid() && plug.IsEnabled() && plug.GetEffectType() == type && familyEnabled)
      {
         return &plug;
      }
   }

   return NULL;
}

bool PluginManager::IsRegistered(const PluginID & ID)
{
   if (mPlugins.find(ID) == mPlugins.end())
   {
      return false;
   }

   return true;
}

const PluginID & PluginManager::RegisterLegacyEffectPlugin(EffectIdentInterface *effect)
{
   PluginDescriptor & plug = CreatePlugin(GetID(effect), effect, PluginTypeEffect);

   plug.SetEffectType(effect->GetType());
   plug.SetEffectFamily(effect->GetFamily());
   plug.SetEffectInteractive(effect->IsInteractive());
   plug.SetEffectDefault(effect->IsDefault());
   plug.SetEffectRealtime(effect->SupportsRealtime());
   plug.SetEffectAutomatable(effect->SupportsAutomation());

   plug.SetInstance(effect);
   plug.SetEffectLegacy(true);
   plug.SetEnabled(true);
   plug.SetValid(true);

   return plug.GetID();
}

bool PluginManager::IsPluginEnabled(const PluginID & ID)
{
   if (mPlugins.find(ID) == mPlugins.end())
   {
      return false;
   }

   return mPlugins[ID].IsEnabled();
}

void PluginManager::EnablePlugin(const PluginID & ID, bool enable)
{
   if (mPlugins.find(ID) == mPlugins.end())
   {
      return;
   }

   return mPlugins[ID].SetEnabled(enable);
}

const wxString & PluginManager::GetSymbol(const PluginID & ID)
{
   if (mPlugins.find(ID) == mPlugins.end())
   {
      static wxString empty;
      return empty;
   }

   return mPlugins[ID].GetSymbol();
}

wxString PluginManager::GetName(const PluginID & ID)
{
   if (mPlugins.find(ID) == mPlugins.end())
   {
      return wxEmptyString;
   }

   return mPlugins[ID].GetName();
}

IdentInterface *PluginManager::GetInstance(const PluginID & ID)
{
   if (mPlugins.find(ID) == mPlugins.end())
   {
      return NULL;
   }

   PluginDescriptor & plug = mPlugins[ID];

   // If not dealing with legacy effects, make sure the provider is loaded 
   if (!plug.IsEffectLegacy())
   {
      const PluginID & prov = plug.GetProviderID();
      if (mPlugins.find(prov) == mPlugins.end())
      {
         return NULL;
      }
      mPlugins[prov].GetInstance();
   }

   return plug.GetInstance();
}

// TODO:  This goes away when all effects have been converted
void PluginManager::SetInstance(const PluginID & ID, IdentInterface *instance)
{
   if (mPlugins.find(ID) == mPlugins.end())
   {
      return;
   }

   return mPlugins[ID].SetInstance(instance);
}

PluginID PluginManager::GetID(ModuleInterface *module)
{
   return wxString::Format(wxT("%s_%s_%s_%s_%s"),
                           GetPluginTypeString(PluginTypeModule).c_str(),
                           wxEmptyString,
                           module->GetVendor().c_str(),
                           module->GetName().c_str(),
                           module->GetPath().c_str());
}

PluginID PluginManager::GetID(EffectIdentInterface *effect)
{
   return wxString::Format(wxT("%s_%s_%s_%s_%s"),
                           GetPluginTypeString(PluginTypeEffect).c_str(),
                           effect->GetFamily().c_str(),
                           effect->GetVendor().c_str(),
                           effect->GetName().c_str(),
                           effect->GetPath().c_str());
}

PluginID PluginManager::GetID(ImporterInterface *importer)
{
   return wxString::Format(wxT("%s_%s_%s_%s_%s"),
                           GetPluginTypeString(PluginTypeImporter).c_str(),
                           wxEmptyString,
                           importer->GetVendor().c_str(),
                           importer->GetName().c_str(),
                           importer->GetPath().c_str());
}

wxString PluginManager::GetPluginTypeString(PluginType type)
{
   wxString str;

   switch (type)
   {
   case PluginTypeNone:
      str = wxT("Placeholder");
      break;
   case PluginTypeEffect:
      str = wxT("Effect");
      break;
   case PluginTypeExporter:
      str = wxT("Exporter");
      break;
   case PluginTypeImporter:
      str = wxT("Importer");
      break;
   case PluginTypeModule:
      str = wxT("Module");
      break;
   }

   return str;
}

PluginDescriptor & PluginManager::CreatePlugin(const PluginID & id,
                                               IdentInterface *ident,
                                               PluginType type)
{
   // This will either create a new entry or replace an existing entry
   PluginDescriptor & plug = mPlugins[id];

   plug.SetPluginType(type);

   plug.SetID(id);
   plug.SetPath(ident->GetPath());
   plug.SetSymbol(ident->GetSymbol());
   plug.SetName(ident->GetName());
   plug.SetVendor(ident->GetVendor());
   plug.SetVersion(ident->GetVersion());
   plug.SetDescription(ident->GetDescription());

   return plug;
}

wxFileConfig *PluginManager::GetSettings()
{
   if (!mSettings)
   {
      mSettings = new wxFileConfig(wxEmptyString, wxEmptyString, FileNames::PluginSettings());

      // Check for a settings version that we can understand
      if (mSettings->HasEntry(SETVERKEY))
      {
         wxString setver = mSettings->Read(SETVERKEY, SETVERKEY);
         if (setver < SETVERCUR )
         {
            // This is where we'd put in conversion code when the
            // settings version changes.
            //
            // Should also check for a settings file that is newer than
            // what we can understand.
         }
      }
      else
      {
         // Make sure is has a version string
         mSettings->Write(SETVERKEY, SETVERCUR);
         mSettings->Flush();
      }
   }

   return mSettings;
}

bool PluginManager::GetSubgroups(const wxString & group, wxArrayString & subgroups)
{
   bool result = false;

   if (!group.IsEmpty())
   {
      wxString name = wxEmptyString;
      long index = 0;
      wxString path = GetSettings()->GetPath();
      GetSettings()->SetPath(group);

      if (GetSettings()->GetFirstGroup(name, index))
      {
         do
         {
            subgroups.Add(name);
         } while (GetSettings()->GetNextGroup(name, index));
      }

      GetSettings()->SetPath(path);
   }

   return result;
}

bool PluginManager::GetConfig(const wxString & key, int & value, int defval)
{
   bool result = false;

   if (!key.IsEmpty())
   {
      result = GetSettings()->Read(key, &value, defval);
   }

   return result;
}

bool PluginManager::GetConfig(const wxString & key, wxString & value, const wxString & defval)
{
   bool result = false;

   if (!key.IsEmpty())
   {
      wxString wxval = wxEmptyString;

      result = GetSettings()->Read(key, &wxval, defval);

      value = wxval;
   }

   return result;
}

bool PluginManager::GetConfig(const wxString & key, bool & value, bool defval)
{
   bool result = false;

   if (!key.IsEmpty())
   {
      result = GetSettings()->Read(key, &value, defval);
   }

   return result;
}

bool PluginManager::GetConfig(const wxString & key, float & value, float defval)
{
   bool result = false;

   if (!key.IsEmpty())
   {
      double dval = 0.0;

      result = GetSettings()->Read(key, &dval, (double) defval);

      value = (float) dval;
   }

   return result;
}

bool PluginManager::GetConfig(const wxString & key, double & value, double defval)
{
   bool result = false;

   if (!key.IsEmpty())
   {
      result = GetSettings()->Read(key, &value, defval);
   }

   return result;
}

bool PluginManager::GetConfig(const wxString & key, sampleCount & value, sampleCount defval)
{
   bool result = false;

   if (!key.IsEmpty())
   {
      wxString wxval = wxEmptyString;
      wxString wxdef;
      wchar_t *endptr;
      wxdef.Printf(wxT("%Ld"), defval);

      result = GetSettings()->Read(key, &wxval, wxdef);
      value = wxStrtoll(wxval.c_str(), &endptr, 10);
   }
   
   return result;
}

bool PluginManager::SetConfig(const wxString & key, const wxString & value)
{
   bool result = false;

   if (!key.IsEmpty())
   {
      wxString wxval = value.c_str();
      result = GetSettings()->Write(key, wxval);
      if (result)
      {
         result = GetSettings()->Flush();
      }
   }

   return result;
}

bool PluginManager::SetConfig(const wxString & key, const int & value) 
{
   bool result = false;

   if (!key.IsEmpty())
   {
      result = GetSettings()->Write(key, value);
      if (result)
      {
         result = GetSettings()->Flush();
      }
   }

   return result;
}

bool PluginManager::SetConfig(const wxString & key, const bool & value)
{
   bool result = false;

   if (!key.IsEmpty())
   {
      result = GetSettings()->Write(key, value);
      if (result)
      {
         result = GetSettings()->Flush();
      }
   }

   return result;
}

bool PluginManager::SetConfig(const wxString & key, const float & value)
{
   bool result = false;

   if (!key.IsEmpty())
   {
      result = GetSettings()->Write(key, value);
      if (result)
      {
         result = GetSettings()->Flush();
      }
   }

   return result;
}

bool PluginManager::SetConfig(const wxString & key, const double & value)
{
   bool result = false;

   if (!key.IsEmpty())
   {
      result = GetSettings()->Write(key, value);
      if (result)
      {
         result = GetSettings()->Flush();
      }
   }

   return result;
}

bool PluginManager::SetConfig(const wxString & key, const sampleCount & value)
{
   bool result = false;

   if (!key.IsEmpty())
   {
      result = GetSettings()->Write(key, wxString::Format(wxT("%d"), (int) value));
      if (result)
      {
         result = GetSettings()->Flush();
      }
   }

   return result;
}

wxString PluginManager::SettingsID(const PluginID & ID)
{
   if (mPlugins.find(ID) == mPlugins.end())
   {
      return wxEmptyString;
   }

   const PluginDescriptor & plug = mPlugins[ID];

   return wxString::Format(wxT("%s_%s_%s_%s"),
                           GetPluginTypeString(plug.GetPluginType()).c_str(),
                           plug.GetEffectFamily().c_str(), // is empty for non-Effects
                           plug.GetVendor().c_str(),
                           plug.GetName().c_str());
}

wxString PluginManager::SharedGroup(const PluginID & ID, const wxString & group)
{
   wxString settingsID = SettingsID(ID);

   wxString path = SETROOT +
                   ConvertID(settingsID) +
                   wxCONFIG_PATH_SEPARATOR +
                   wxT("shared") +
                   wxCONFIG_PATH_SEPARATOR;

   wxFileName f(group);
   if (!f.GetName().IsEmpty())
   {
      path += f.GetFullPath(wxPATH_UNIX) + wxCONFIG_PATH_SEPARATOR;
   }

   return path;
}

wxString PluginManager::SharedKey(const PluginID & ID, const wxString & group, const wxString & key)
{
   wxString path = SharedGroup(ID, group);
   if (path.IsEmpty())
   {
      return path;
   }

   return path + key;
}

wxString PluginManager::PrivateGroup(const PluginID & ID, const wxString & group)
{
   wxString settingsID = SettingsID(ID);

   wxString path = SETROOT +
                  ConvertID(settingsID) +
                  wxCONFIG_PATH_SEPARATOR +
                  wxT("private") +
                  wxCONFIG_PATH_SEPARATOR;

   wxFileName f(group);
   if (!f.GetName().IsEmpty())
   {
      path += f.GetFullPath(wxPATH_UNIX) + wxCONFIG_PATH_SEPARATOR;
   }

   return path;
}

wxString PluginManager::PrivateKey(const PluginID & ID, const wxString & group, const wxString & key)
{
   wxString path = PrivateGroup(ID, group);
   if (path.IsEmpty())
   {
      return path;
   }

   return path + key;
}

// Sanitize the ID...not the best solution, but will suffice until this
// is converted to XML.  We use base64 encoding to preserve case.
wxString PluginManager::ConvertID(const PluginID & ID)
{
   if (ID.StartsWith(wxT("base64:")))
   {
      wxString id = ID.Mid(7);
      char *buf = new char[id.Length() / 4 * 3];
      id =  wxString::FromUTF8(buf, b64decode(id, buf));
      delete [] buf;
      return id;
   }

   const wxCharBuffer & buf = ID.ToUTF8();
   return wxT("base64:") + b64encode(buf, strlen(buf));
}

////////////////////////////////////////////////////////////////////////////////
// Base64 en/decoding
//
// Original routines marked as public domain and found at:
//
// http://en.wikibooks.org/wiki/Algorithm_implementation/Miscellaneous/Base64
//
////////////////////////////////////////////////////////////////////////////////

// Lookup table for encoding
const static wxChar cset[] = wxT("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/");
const static char padc = wxT('=');

wxString PluginManager::b64encode(const void *in, int len)
{
   unsigned char *p = (unsigned char *) in;
   wxString out;

   unsigned long temp;
   for (int i = 0; i < len / 3; i++)
   {
      temp  = (*p++) << 16; //Convert to big endian
      temp += (*p++) << 8;
      temp += (*p++);
      out += cset[(temp & 0x00FC0000) >> 18];
      out += cset[(temp & 0x0003F000) >> 12];
      out += cset[(temp & 0x00000FC0) >> 6];
      out += cset[(temp & 0x0000003F)];
   }

   switch (len % 3)
   {
      case 1:
         temp  = (*p++) << 16; //Convert to big endian
         out += cset[(temp & 0x00FC0000) >> 18];
         out += cset[(temp & 0x0003F000) >> 12];
         out += padc;
         out += padc;
      break;

      case 2:
         temp  = (*p++) << 16; //Convert to big endian
         temp += (*p++) << 8;
         out += cset[(temp & 0x00FC0000) >> 18];
         out += cset[(temp & 0x0003F000) >> 12];
         out += cset[(temp & 0x00000FC0) >> 6];
         out += padc;
      break;
   }

   return out;
}

int PluginManager::b64decode(wxString in, void *out)
{
   int len = in.length();
   unsigned char *p = (unsigned char *) out;

   if (len % 4)  //Sanity check
   {
      return 0;
   }

   int padding = 0;
   if (len)
   {
      if (in[len - 1] == padc)
      {
         padding++;
      }

      if (in[len - 2] == padc)
      {
         padding++;
      }
   }

   //const char *a = in.mb_str();
   //Setup a vector to hold the result
   unsigned long temp = 0; //Holds decoded quanta
   int i = 0;
   while (i < len)
   {
      for (int quantumPosition = 0; quantumPosition < 4; quantumPosition++)
      {
         unsigned char c = in[i];
         temp <<= 6;

         if (c >= 0x41 && c <= 0x5A)
         {
            temp |= c - 0x41;
         }
         else if (c >= 0x61 && c <= 0x7A)
         {
            temp |= c - 0x47;
         }
         else if (c >= 0x30 && c <= 0x39)
         {
            temp |= c + 0x04;
         }
         else if (c == 0x2B)
         {
            temp |= 0x3E;
         }
         else if (c == 0x2F)
         {
            temp |= 0x3F;
         }
         else if (c == padc)
         {
            switch (len - i)
            {
               case 1: //One pad character
                  *p++ = (temp >> 16) & 0x000000FF;
                  *p++ = (temp >> 8) & 0x000000FF;
                  return p - (unsigned char *) out;
               case 2: //Two pad characters
                  *p++ = (temp >> 10) & 0x000000FF;
                  return p - (unsigned char *) out;
            }
         }
         i++;
      }
      *p++ = (temp >> 16) & 0x000000FF;
      *p++ = (temp >> 8) & 0x000000FF;
      *p++ = temp & 0x000000FF;
   }

   return p - (unsigned char *) out;
}
