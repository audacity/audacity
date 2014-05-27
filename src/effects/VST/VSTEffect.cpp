/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTEffect.cpp

  Dominic Mazzoni
  
  This class implements a VST Plug-in effect.  The plug-in must be
  loaded in a platform-specific way and passed into the constructor,
  but from here this class handles the interfacing.  VST plug-ins
  are used in Cubase and other Steinberg products, and all of those
  files and the information within is copyrighted by Steinberg.

**********************************************************************/

// *******************************************************************
// WARNING:  This is NOT 64-bit safe
// *******************************************************************

#include "../../Audacity.h"

#if USE_VST

#include <wx/defs.h>
#include <wx/button.h>
#include <wx/combobox.h>
#include <wx/dialog.h>
#include <wx/filename.h>
#include <wx/frame.h>
#include <wx/msgdlg.h>
#include <wx/process.h>
#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/scrolwin.h>
#include <wx/stattext.h>
#include <wx/statbox.h>
#include <wx/stopwatch.h>
#include <wx/utils.h>
#include <wx/dcclient.h>
#include <wx/imaglist.h>
#include <wx/listctrl.h>

#if defined(__WXMAC__)
#include <dlfcn.h>
#include <wx/mac/private.h>
#else
#include <wx/dynlib.h>
#endif

#if defined(__WXMSW__)
   #include <wx/msw/seh.h>
   #include <shlwapi.h>
   #pragma comment(lib, "shlwapi")
#endif

#include "FileDialog.h"

#include "../../AudacityApp.h"
#include "../../FileNames.h"
#include "../../Internat.h"
#include "../../PlatformCompatibility.h"
#include "../../PluginManager.h"
#include "../../Prefs.h"
#include "../../xml/XMLFileReader.h"
#include "../../xml/XMLWriter.h"
#include "../../Theme.h"
#include "../../widgets/valnum.h"
#include "../EffectManager.h"
#include "../images/Arrow.xpm"

#include "VSTEffect.h"

///////////////////////////////////////////////////////////////////////////////
//
// RegisterVSTEffects
//
///////////////////////////////////////////////////////////////////////////////

void RegisterVSTEffects()
{
   PluginManager & pm = PluginManager::Get();

   pm.Open();

   bool bRescanRequired = false;
   if (gPrefs->Read(wxT("/VST/Rescan"), (long)true) != false) {
      bRescanRequired = true;
      pm.PurgeType(VSTPLUGINTYPE);
   }

   if (!pm.HasType(VSTPLUGINTYPE)) {
      pm.Close();
      if( bRescanRequired ) {
         if( VSTEffect::Scan() != wxID_CANCEL ) {
            gPrefs->Write(wxT("/VST/Rescan"), false);
            gPrefs->Flush();
         }
      }
      pm.Open();
   }

   EffectManager & em = EffectManager::Get();

   wxString path = pm.GetFirstPlugin(VSTPLUGINTYPE);
   while (!path.IsEmpty()) {
#if defined(__WXMAC__)
      if (wxDirExists(path)) {
#else
      if (wxFileExists(path)) {
#endif
         em.RegisterEffect(new VSTEffect(path));
      }
      
      path = pm.GetNextPlugin(VSTPLUGINTYPE);
   }

   pm.Close();
}

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
#if 0
   NotifyEvent(wxACC_EVENT_OBJECT_REORDER,
               mParent,
               wxOBJID_CLIENT,
               0);
#endif
#if 1
   if (mLastId != -1) {
      NotifyEvent(wxACC_EVENT_OBJECT_SELECTIONREMOVE,
               mParent,
               wxOBJID_CLIENT,
               mLastId);
      mLastId = -1;
   }

   if (item != -1)
   {
      NotifyEvent(wxACC_EVENT_OBJECT_FOCUS,
                  mParent,
                  wxOBJID_CLIENT,
                  item + 1);

      NotifyEvent(wxACC_EVENT_OBJECT_SELECTION,
                  mParent,
                  wxOBJID_CLIENT,
                  item + 1);

      mLastId = item + 1;
   }
#endif
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

class PluginRegistrationDialog:public wxDialog {
 public:
   // constructors and destructors
   PluginRegistrationDialog(wxWindow * parent, const wxArrayString & files);
   virtual ~PluginRegistrationDialog();
 public:
   void Populate();
   void PopulateOrExchange( ShuttleGui & S );

   void OnApply(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);
   void OnListChar(wxKeyEvent & event);
   void OnListMouseDown(wxMouseEvent & event);
   void OnSelectAll(wxCommandEvent & event);
   void OnClearAll(wxCommandEvent & event);

   void SetBoldOrRegular( int i );
   void SetState(int i, int state);
   void ToggleItem(int i);

#if wxUSE_ACCESSIBILITY
   CheckListAx *mAx;
#endif

   wxListCtrl *mPlugins;
   wxArrayString mFiles;
   wxArrayInt miState;

   bool mCancelClicked;

   DECLARE_EVENT_TABLE()
};


#define PluginListID       7001
#define PluginClearAllID   7002
#define PluginSelectAllID  7003

BEGIN_EVENT_TABLE(PluginRegistrationDialog, wxDialog)
   EVT_BUTTON(wxID_OK, PluginRegistrationDialog::OnApply)
   EVT_BUTTON(wxID_CANCEL, PluginRegistrationDialog::OnCancel)
   EVT_BUTTON(PluginClearAllID, PluginRegistrationDialog::OnClearAll)
   EVT_BUTTON(PluginSelectAllID, PluginRegistrationDialog::OnSelectAll)
END_EVENT_TABLE()

PluginRegistrationDialog::PluginRegistrationDialog(wxWindow * parent, const wxArrayString & files):
   mFiles( files ),
   wxDialog(parent, wxID_ANY, _("Install VST Effects"),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
{
   mPlugins = NULL;
   SetLabel(_("Install VST Effects"));         // Provide visual label
   SetName(_("Install VST Effects"));          // Provide audible label
   Populate();
   SetReturnCode( wxID_OK);
}

PluginRegistrationDialog::~PluginRegistrationDialog()
{
   mPlugins->Disconnect(wxEVT_LEFT_DOWN,
                        wxMouseEventHandler(PluginRegistrationDialog::OnListMouseDown),
                        NULL,
                        this);
   mPlugins->Disconnect(wxEVT_KEY_DOWN,
                        wxKeyEventHandler(PluginRegistrationDialog::OnListChar),
                        NULL,
                        this);
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
   wxImageList * pImageList = new wxImageList( 16, 16 );

#define SHOW_UNCHECKED (0)
#define SHOW_CHECKED (1)
#define SHOW_ARROW (2)

#define COL_NAME (0)
#define COL_PATH (1)

   pImageList->Add(wxIcon(unchecked_xpm));
   pImageList->Add(wxIcon(checked_xpm));
   pImageList->Add(wxIcon(arrow15x15_xpm));

   S.StartVerticalLay(true);
   {
      /*i18n-hint: The dialog shows a list of plugins with check-boxes 
       beside each one.*/
      S.StartStatic(_("&Select Plug-ins to Install or press ENTER to Install All"), true);
      {
         S.SetStyle(wxSUNKEN_BORDER | wxLC_REPORT | wxLC_SINGLE_SEL | wxLC_HRULES | wxLC_VRULES );
         mPlugins = S.Id(PluginListID).AddListControlReportMode();
         mPlugins->Connect(wxEVT_LEFT_DOWN,
                           wxMouseEventHandler(PluginRegistrationDialog::OnListMouseDown),
                           NULL,
                           this);
         mPlugins->Connect(wxEVT_KEY_DOWN,
                           wxKeyEventHandler(PluginRegistrationDialog::OnListChar),
                           NULL,
                           this);
#if wxUSE_ACCESSIBILITY
         mAx = new CheckListAx(mPlugins);
         mPlugins->SetAccessible(mAx);
#endif
         mPlugins->AssignImageList( pImageList, wxIMAGE_LIST_SMALL );
         mPlugins->InsertColumn(COL_NAME, _("Plug-in Name"));
         mPlugins->InsertColumn(COL_PATH, _("Path"));
      }
      S.EndStatic();

      S.StartHorizontalLay(wxALIGN_LEFT | wxEXPAND, false);
      {
         S.SetBorder(10);
         S.StartHorizontalLay(wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);
         {
            S.AddSpace(12);
            S.SetBorder(6);
            S.Id(PluginSelectAllID).AddButton(_("Select &All"));
            S.Id(PluginClearAllID).AddButton(_("Clea&r All"));
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
   for (int i = 0; i < (int)mFiles.GetCount(); i++)
   {
      miState.Add( SHOW_CHECKED );

      wxFileName fn(mFiles[i]);
      wxString name( fn.GetName() );
      wxString path( fn.GetFullPath() );

      mPlugins->InsertItem( i, name, SHOW_CHECKED );
      mPlugins->SetItem( i, COL_PATH, path );

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
         mPlugins->GetItemRect( i, iconrect, wxLIST_RECT_ICON );
#endif
      }
      mPlugins->GetTextExtent( name, &x, &y );
      iNameLen = wxMax( iNameLen, x + iconrect.width + (iconrect.x * 2) );
      mPlugins->GetTextExtent( path, &x, &y );
      iPathLen = wxMax( iPathLen, x + iconrect.width + (iconrect.x * 2) );
   }

   mPlugins->SetColumnWidth(COL_NAME, iNameLen + /* fudge */ 5);
   mPlugins->SetColumnWidth(COL_PATH, iPathLen + /* fudge */ 5);

   //SetBoldOrRegular( miSelected );
   mPlugins->SetSizeHints( iNameLen + iPathLen + /* fudge */ 15 , 200 );
   if( mFiles.GetCount() > 0 )
   {
      // Make sure first item is selected/focused.
      mPlugins->SetFocus();
      mPlugins->SetItemState( 0, wxLIST_STATE_FOCUSED|wxLIST_STATE_SELECTED, wxLIST_STATE_FOCUSED|wxLIST_STATE_SELECTED);
#if wxUSE_ACCESSIBILITY
      mAx->SetSelected( 0 );
#endif
   }
   Layout();
   Fit();
   SetSizeHints(GetSize());
   // Parent window is usually not there yet, so centre on screen rather than on parent.
   CenterOnScreen();

}

void PluginRegistrationDialog::OnListMouseDown( wxMouseEvent & event )
{
   wxPoint p = event.GetPosition();
   int flags = wxLIST_HITTEST_ONITEM;
   int item = mPlugins->HitTest( p, flags );

   if( item != wxNOT_FOUND )
   {
      ToggleItem( item );
   }

   event.Skip();
}

void PluginRegistrationDialog::OnListChar( wxKeyEvent & event )
{
   switch( event.GetKeyCode() )
   {
      case WXK_SPACE:
      {
         int iItem = mPlugins->GetNextItem( -1, wxLIST_NEXT_ALL, wxLIST_STATE_FOCUSED);

         if( iItem != wxNOT_FOUND )
         {
            ToggleItem( iItem );
         }
      }
      break;

      case WXK_RETURN:
         // Don't know why wxListCtrls prevent default dialog action,
         // but they do, so handle it.
         EmulateButtonClickIfPresent( GetAffirmativeId() );
      break;

      default:
         event.Skip();
      break;
   }
}

void PluginRegistrationDialog::SetBoldOrRegular( int i )
{
   wxFont Font = mPlugins->GetItemFont( i );
   Font.SetWeight( (miState[i]==SHOW_CHECKED)? wxFONTWEIGHT_BOLD : wxFONTWEIGHT_NORMAL );
   mPlugins->SetItemFont( i, Font );
}

// We can't capture mouse clicks, only selected and deselected.
// Clicking on a selected item does not generate any event.
// Therefore our workaround solution is to NEVER actually select.
// So whenever the code tries to , we cancel the selection.
// That way we continue to get events.
void PluginRegistrationDialog::SetState(int i, int state)
{
   miState[ i ] = state;
   mPlugins->SetItemImage( i, miState[i] );
#if wxUSE_ACCESSIBILITY
   mAx->SetSelected( i );
#endif
}

void PluginRegistrationDialog::ToggleItem(int i)
{
   SetState( i, miState[ i ] == SHOW_CHECKED ? SHOW_UNCHECKED : SHOW_CHECKED );
}

void PluginRegistrationDialog::OnSelectAll(wxCommandEvent & WXUNUSED(event))
{
   for( size_t i = 0, cnt = miState.GetCount(); i < cnt; i++ )
   {
      SetState(i, SHOW_CHECKED);
   }
}

void PluginRegistrationDialog::OnClearAll(wxCommandEvent & WXUNUSED(event))
{
   for( size_t i = 0, cnt = miState.GetCount(); i < cnt; i++ )
   {
      SetState(i, SHOW_UNCHECKED);
   }
}

void PluginRegistrationDialog::OnApply(wxCommandEvent & WXUNUSED(event))
{
   mCancelClicked = false;
   FindWindowById(wxID_OK)->Disable();

   size_t cnt = mFiles.GetCount();
   for (size_t i = 0; i < cnt && !mCancelClicked; i++) {
      wxString file = mFiles[i];

      mPlugins->EnsureVisible( i );
      if( miState[ i ] == SHOW_CHECKED )
      {
         mPlugins->SetItemImage( i, SHOW_ARROW );
         VSTEffect::ScanOnePlugin( file );
         mPlugins->SetItemImage( i, SHOW_CHECKED );
      }
      wxYield();
   }

   EndModal(mCancelClicked ? wxID_CANCEL : wxID_OK);
}

void PluginRegistrationDialog::OnCancel(wxCommandEvent & WXUNUSED(event))
{
   mCancelClicked = true;

   EndModal(mCancelClicked ? wxID_CANCEL : wxID_OK);
}

///////////////////////////////////////////////////////////////////////////////
//
// VSTEffectSettingsDialog
//
///////////////////////////////////////////////////////////////////////////////

class VSTEffectSettingsDialog:public wxDialog
{
 public:
   VSTEffectSettingsDialog(wxWindow * parent);
   virtual ~VSTEffectSettingsDialog();

   void PopulateOrExchange(ShuttleGui & S);

   void OnOk(wxCommandEvent & evt);

 private:
    int mBufferSize;

    DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(VSTEffectSettingsDialog, wxDialog)
   EVT_BUTTON(wxID_OK, VSTEffectSettingsDialog::OnOk)
END_EVENT_TABLE()

VSTEffectSettingsDialog::VSTEffectSettingsDialog(wxWindow * parent)
:  wxDialog(parent, wxID_ANY, wxString(_("VST Effect Settings")))
{
   gPrefs->Read(wxT("/VST/BufferSize"), &mBufferSize, 8192);

   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
}

VSTEffectSettingsDialog::~VSTEffectSettingsDialog()
{
}

void VSTEffectSettingsDialog::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(5);
   S.StartHorizontalLay(wxEXPAND, 1);
   {
      S.StartVerticalLay(false);
      {
         S.StartStatic(_("Buffer Specification"));
         {
            wxIntegerValidator<int> vld(&mBufferSize);
            vld.SetRange(8, 1048576 * 1);
   
            S.AddVariableText(wxString() +
               _("The buffer size controls the number of samples sent to the effect ") +
               _("on each iteration. Smaller values will cause slower processing and ") +
               _("some effects require 8192 samples or less to work properly. However ") +
               _("most effects can accept large buffers and using them will greatly ") +
               _("reduce processing time."))->Wrap(650);
   
            S.StartHorizontalLay(wxALIGN_LEFT);
            {
               wxTextCtrl *t;
               t = S.TieNumericTextBox(_("&Buffer Size (8 to 1048576 samples):"),
                                       wxT("/VST/BufferSize"),
                                       wxT(""), 12);
               t->SetMinSize(wxSize(100, -1));
               t->SetValidator(vld);
            }
            S.EndHorizontalLay();
         }
         S.EndStatic();
   
         S.StartStatic(_("Buffer Delay Compensation"));
         {
            S.AddVariableText(wxString() +
               _("As part of their processing, some VST effects must delay returning ") +
               _("audio to Audacity. When not compensating for this delay, you will ") +
               _("notice that small silences have been inserted into the audio. ") +
               _("Enabling this setting will provide that compensation, but it may ") +
               _("not work for all VST effects."))->Wrap(650);
   
            S.StartHorizontalLay(wxALIGN_LEFT);
            {
               S.TieCheckBox(_("Enable &compensation"), wxT("/VST/UseBufferDelay"), true);
            }
            S.EndHorizontalLay();
         }
         S.EndStatic();
   
         S.StartStatic(_("Presentation Method"));
         {
            S.AddVariableText(wxString() +
               _("Most VST effects provide a graphical interface for setting the ") +
               _("parameter values. However, a basic text only method is also ") +
               _("available.  Reopen the effect for this to take affect."))->Wrap(650);
            S.TieCheckBox(_("Enable &graphical interface"), wxT("/VST/GUI"), true);
         }
         S.EndStatic();
   
         S.StartStatic(_("Effect Refresh"));
         {
            S.AddVariableText(wxString() +
               _("To improve Audacity startup, a search for VST effects is performed ") +
               _("once and relevant information is recorded. When you add VST effects ") +
               _("to your system, you need to tell Audacity to rescan so the new ") +
               _("information can be recorded."))->Wrap(650);
            S.TieCheckBox(_("&Rescan effects on next launch"), wxT("/VST/Rescan"), false);
         }
         S.EndStatic();
      }
      S.EndVerticalLay();
   }
   S.EndHorizontalLay();

   S.AddStandardButtons();

   Layout();
   Fit();
   Center();
}

void VSTEffectSettingsDialog::OnOk(wxCommandEvent & WXUNUSED(evt))
{
   if (!Validate()) {
      return;
   }

   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   EndModal(wxID_OK);
}

///////////////////////////////////////////////////////////////////////////////
//
// VSTEffectDialog
//
///////////////////////////////////////////////////////////////////////////////
DECLARE_LOCAL_EVENT_TYPE(EVT_SIZEWINDOW, -1);
DEFINE_LOCAL_EVENT_TYPE(EVT_SIZEWINDOW);
DECLARE_LOCAL_EVENT_TYPE(EVT_UPDATEDISPLAY, -1);
DEFINE_LOCAL_EVENT_TYPE(EVT_UPDATEDISPLAY);

class VSTEffectDialog:public wxDialog, XMLTagHandler
{
 public:
   VSTEffectDialog(wxWindow * parent,
                   const wxString & title,
                   VSTEffect *effect,
                   AEffect *aeffect);
   virtual ~VSTEffectDialog();

   void RemoveHandler();

   void OnProgram(wxCommandEvent & evt);
   void OnProgramText(wxCommandEvent & evt);
   void OnLoad(wxCommandEvent & evt);
   void OnSave(wxCommandEvent & evt);
   void OnSettings(wxCommandEvent & evt);

   void OnSlider(wxCommandEvent &event);

   void OnOk(wxCommandEvent & evt);
   void OnCancel(wxCommandEvent & evt);
   void OnClose(wxCloseEvent & evt);
   void OnPreview(wxCommandEvent & evt);

   void OnSizeWindow(wxCommandEvent & evt);
   void OnUpdateDisplay(wxCommandEvent & evt);

private: 

   void BuildPlain();
   void BuildFancy();
   wxSizer *BuildProgramBar();
   void RefreshParameters(int skip = -1);

   virtual bool HandleXMLTag(const wxChar *tag, const wxChar **attrs);
   virtual void HandleXMLEndTag(const wxChar *tag);
   virtual void HandleXMLContent(const wxString & content);
   virtual XMLTagHandler *HandleXMLChild(const wxChar *tag);
   wxString b64encode(const void *in, int len);
   int b64decode(wxString in, void *out);

   VSTEffect *mEffect;
   AEffect *mAEffect;

   bool mGui;

#if defined(__WXMAC__)
   wxSizerItem *mContainer;
#elif defined(__WXMSW__)
   wxSizerItem *mContainer;
#else
#endif

   wxComboBox *mProgram;
   wxStaticText **mNames;
   wxSlider **mSliders;
   wxStaticText **mDisplays;
   wxStaticText **mLabels;

   bool mInChunk;
   wxString mChunk;

#if defined(__WXMAC__)
   static pascal OSStatus OverlayEventHandler(EventHandlerCallRef handler, EventRef event, void *data);
   OSStatus OnOverlayEvent(EventHandlerCallRef handler, EventRef event);
   static pascal OSStatus WindowEventHandler(EventHandlerCallRef handler, EventRef event, void *data);
   OSStatus OnWindowEvent(EventHandlerCallRef handler, EventRef event);

   WindowRef mOverlayRef;
   EventHandlerUPP mOverlayEventHandlerUPP;
   EventHandlerRef mOverlayEventHandlerRef;
   bool mOverlayEventsBlocked;

   WindowRef mWindowRef;
   EventHandlerUPP mWindowEventHandlerUPP;
   EventHandlerRef mWindowEventHandlerRef;

   WindowRef mPreviousRef;

#endif

   DECLARE_EVENT_TABLE()
};

enum
{
   ID_VST_PROGRAM = 11000,
   ID_VST_LOAD,
   ID_VST_SAVE,
   ID_VST_SLIDERS,
   ID_VST_SETTINGS
};

BEGIN_EVENT_TABLE(VSTEffectDialog, wxDialog)
   EVT_BUTTON(wxID_OK, VSTEffectDialog::OnOk)
   EVT_BUTTON(wxID_CANCEL, VSTEffectDialog::OnCancel)
   EVT_BUTTON(ID_EFFECT_PREVIEW, VSTEffectDialog::OnPreview)

   EVT_COMBOBOX(ID_VST_PROGRAM, VSTEffectDialog::OnProgram)
   EVT_TEXT(ID_VST_PROGRAM, VSTEffectDialog::OnProgramText)
   EVT_BUTTON(ID_VST_LOAD, VSTEffectDialog::OnLoad)
   EVT_BUTTON(ID_VST_SAVE, VSTEffectDialog::OnSave)
   EVT_BUTTON(ID_VST_SETTINGS, VSTEffectDialog::OnSettings)

   EVT_SLIDER(wxID_ANY, VSTEffectDialog::OnSlider)

   // Events from the audioMaster callback
   EVT_COMMAND(wxID_ANY, EVT_SIZEWINDOW, VSTEffectDialog::OnSizeWindow)
   EVT_COMMAND(wxID_ANY, EVT_UPDATEDISPLAY, VSTEffectDialog::OnUpdateDisplay)
END_EVENT_TABLE()

#if defined(__WXMAC__)

// ----------------------------------------------------------------------------
// Most of the following is used to deal with VST effects that create an overlay
// window on top of ours.  This is usually done because Cocoa is being used
// instead of Carbon.
//
// That works just fine...usually.  But, we display the effect in a modal dialog
// box and, since that overlay window is just another window in the application,
// the modality of the dialog causes the overlay to be disabled and the user
// can't interact with the effect.
//
// Examples of these effects would be BlueCat's Freeware Pack and GRM Tools,
// though I'm certain there are other's out there.  Anything JUCE based would
// affected...that's what GRM Tools uses.
//
// So, to work around the problem (without moving to Cocoa or wxWidgets 3.x),
// we install an event handler if the overlay is detected.  This handler and
// the companion handler on our window use the kEventWindowGetClickModality
// event to tell the system that events can be passed to our window and the
// overlay window.
//
// In addition, there's some window state management that must be dealt with
// to keep our window from becoming unhightlighted when the floater is clicked.
// ----------------------------------------------------------------------------

// Events to be captured in the overlay window
static const EventTypeSpec OverlayEventList[] =
{
   { kEventClassWindow, kEventWindowGetClickModality },
   { kEventClassWindow, kEventWindowHandleActivate },
   { kEventClassWindow, kEventWindowActivated },
   { kEventClassMouse,  kEventMouseDown },
   { kEventClassMouse,  kEventMouseUp },
   { kEventClassMouse,  kEventMouseMoved },
   { kEventClassMouse,  kEventMouseDragged },
   { kEventClassMouse,  kEventMouseEntered },
   { kEventClassMouse,  kEventMouseExited },
   { kEventClassMouse,  kEventMouseWheelMoved },
};

// Overlay window event handler callback thunk
pascal OSStatus VSTEffectDialog::OverlayEventHandler(EventHandlerCallRef handler, EventRef event, void *data)
{
   return ((VSTEffectDialog *)data)->OnOverlayEvent(handler, event);
}

// Overlay window event handler
OSStatus VSTEffectDialog::OnOverlayEvent(EventHandlerCallRef handler, EventRef event)
{
   // Get the current window in from of all the rest non-floaters.
   WindowRef frontwin = FrontNonFloatingWindow();

   // Get the target window of the event
   WindowRef evtwin = 0;
   GetEventParameter(event,
                     kEventParamDirectObject,
                     typeWindowRef,
                     NULL,
                     sizeof(evtwin),
                     NULL,
                     &evtwin);

#if defined(DEBUG_VST)
   int cls = GetEventClass(event);
   printf("WINDOW class %4.4s kind %d ewin %p owin %p mwin %p anf %p fnf %p\n",
      &cls,
      GetEventKind(event),
      evtwin,
      mOverlayRef,
      mWindowRef,
      ActiveNonFloatingWindow(),
      frontwin);
#endif

   // We must block mouse events because plugins still act on mouse
   // movement and drag events, even if they are supposed to be disabled
   // due to other modal dialogs (like when Load or Settings are clicked).
   if (GetEventClass(event) == kEventClassMouse) {
      if (mOverlayEventsBlocked) {
         return noErr;
      }
   }

   // Only kEventClassWindow event at this poine
   switch (GetEventKind(event))
   {
      // The system is asking if the target of an upcoming event
      // should be passed to the overlay window or not.
      //
      // We allow it when the overlay window or our window is the
      // curret top window.  Any other windows would mean that a
      // modal dialog box has been open on top and we should block.
      case kEventWindowGetClickModality:
      {
         HIModalClickResult res = kHIModalClickIsModal;
         WindowRef ref = mWindowRef;

         // Allow it to pass?
         if (frontwin == mWindowRef | frontwin == mOverlayRef) {
            res |= kHIModalClickAllowEvent; // | kHIModalClickRaiseWindow;
            mOverlayEventsBlocked = false;
         }
         // No, block it
         else {
            res |= kHIModalClickAnnounce;
            ref = frontwin;
            mOverlayEventsBlocked = true;
         }

         // Get the kind of modal block from the modal window
         WindowModality kind;
         GetWindowModality(ref, &kind, NULL);

         // Set the return parameters
         SetEventParameter(event,
                           kEventParamWindowModality,
                           typeWindowRef,
                           sizeof(kind),
                           &kind);

         SetEventParameter(event,
                           kEventParamModalWindow,
                           typeWindowRef,
                           sizeof(ref),
                           &ref);

         SetEventParameter(event,
                           kEventParamModalClickResult,
                           typeModalClickResult,
                           sizeof(res),
                           &res);

         return noErr;
      }
      break;

      // Ignore the activate events to (sort of) keep our window as
      // the active one
      case kEventWindowHandleActivate:
      case kEventWindowActivated:
      {
         return noErr;
      }
      break;
   }

   return eventNotHandledErr;
}

// Events to be captured in the our window
static const EventTypeSpec WindowEventList[] =
{
   { kEventClassWindow, kEventWindowShown },
   { kEventClassWindow, kEventWindowClose },
   { kEventClassWindow, kEventWindowGetClickModality },
   { kEventClassWindow, kEventWindowHandleDeactivate },
   { kEventClassWindow, kEventWindowDeactivated },
};

// Our window event handler callback thunk
pascal OSStatus VSTEffectDialog::WindowEventHandler(EventHandlerCallRef handler, EventRef event, void *data)
{
   return ((VSTEffectDialog *)data)->OnWindowEvent(handler, event);
}

// Our window event handler
OSStatus VSTEffectDialog::OnWindowEvent(EventHandlerCallRef handler, EventRef event)
{
   // Get the current window in from of all the rest non-floaters.
   WindowRef frontwin = FrontNonFloatingWindow();

   // Get the target window of the event
   WindowRef evtwin = 0;
   GetEventParameter(event,
                     kEventParamDirectObject,
                     typeWindowRef,
                     NULL,
                     sizeof(evtwin),
                     NULL,
                     &evtwin);

#if defined(DEBUG_VST)
   int cls = GetEventClass(event);
   printf("WINDOW class %4.4s kind %d ewin %p owin %p mwin %p anf %p fnf %p\n",
      &cls,
      GetEventKind(event),
      evtwin,
      mOverlayRef,
      mWindowRef,
      ActiveNonFloatingWindow(),
      frontwin);
#endif

   // Only kEventClassWindow event at this poine
   switch (GetEventKind(event))
   {
      // If we don't capture the close event Audacity, will crash attermination
      // since the window is still on the wxWidgets toplevel window lists, but
      // it's already gone.
      case kEventWindowClose:
      {
         RemoveHandler();
         Close();
         return noErr;
      }
      break;

      // This is where we determine if the effect has created a window above
      // ours.  Since the overlay is created on top of our window, we look at
      // the topmost window to see if it is different that ours.  If so, then
      // we assume an overlay has been created and install the event handler
      // on the overlay.
      case kEventWindowShown:
      {
         // Have an overlay?
         mOverlayRef = GetPreviousWindow(mWindowRef);
         if (mOverlayRef == mPreviousRef) {
            mOverlayRef = mWindowRef;
         }
            
         if (mOverlayRef != mWindowRef)
         {
            // Try again
            
            // Install the handler
            mOverlayEventHandlerUPP = NewEventHandlerUPP(OverlayEventHandler);
            InstallWindowEventHandler(mOverlayRef,
                                      mOverlayEventHandlerUPP,
                                      GetEventTypeCount(OverlayEventList),
                                      OverlayEventList,
                                      this,
                                      &mOverlayEventHandlerRef);
         }
      }
      break;

      // The system is asking if the target of an upcoming event
      // should be passed to the overlay window or not.
      //
      // We allow it when the overlay window or our window is the
      // curret top window.  Any other windows would mean that a
      // modal dialog box has been open on top and we should block.
      case kEventWindowGetClickModality:
      {
         // No overlay present, so leave this up to the system
         if (mOverlayRef == mWindowRef) {
            break;
         }

         HIModalClickResult res = kHIModalClickIsModal;
         WindowRef ref = mWindowRef;

         // Allow it to pass?
         if (frontwin == mWindowRef | frontwin == mOverlayRef) {
            res |= kHIModalClickAllowEvent; // | kHIModalClickRaiseWindow;
         }
         // No, block it
         else {
            res |= kHIModalClickAnnounce;
            ref = frontwin;
         }

         // Get the kind of modal block from the modal window
         WindowModality kind;
         GetWindowModality(ref, &kind, NULL);

         // Set the return parameters
         SetEventParameter(event,
                           kEventParamWindowModality,
                           typeWindowRef,
                           sizeof(kind),
                           &kind);

         SetEventParameter(event,
                           kEventParamModalWindow,
                           typeWindowRef,
                           sizeof(ref),
                           &ref);

         SetEventParameter(event,
                           kEventParamModalClickResult,
                           typeModalClickResult,
                           sizeof(res),
                           &res);

         // The mouse click will activate the our window, but we want any
         // control underneath the mouse to get the click instead.  Activating
         // the main window here will allow the click to pass through to the
         // control.
         ActivateWindow(ref, TRUE);

         return noErr;
      }
      break;

      // Instead of deactivating our window, we actually do the opposite by
      // activate it.  This helps to ensure it says highlighted and raised.
      case kEventWindowHandleDeactivate:
      case kEventWindowDeactivated:
      {
         // No overlay present, so leave this up to the system
         if (mOverlayRef == mWindowRef) {
            break;
         }

         ActivateWindow(mWindowRef, TRUE);
         return noErr;
      }
      break;
   }

   return eventNotHandledErr;
}
#endif

VSTEffectDialog::VSTEffectDialog(wxWindow *parent,
                                 const wxString & title,
                                 VSTEffect *effect,
                                 AEffect *aeffect)
:  wxDialog(parent, wxID_ANY, title),
   mEffect(effect),
   mAEffect(aeffect)
{
   mNames = NULL;
   mSliders = NULL;
   mDisplays = NULL;
   mLabels = NULL;
   mContainer = NULL;

#if defined(__WXMAC__)
   mOverlayRef = 0;
   mOverlayEventHandlerUPP = 0;
   mOverlayEventHandlerRef = 0;
   mOverlayEventsBlocked = true;

   mWindowRef = 0;
   mWindowEventHandlerUPP = 0;
   mWindowEventHandlerRef = 0;
#endif

   // Determine if the VST editor is supposed to be used or not
   mGui = (gPrefs->Read(wxT("/VST/GUI"), (long) true) != 0) &&
          mAEffect->flags & effFlagsHasEditor;

   // Build the appropriate dialog type
   if (mGui) {
      BuildFancy();
   }
   else {
      BuildPlain();
   }
}

VSTEffectDialog::~VSTEffectDialog()
{
   RemoveHandler();
   
   if (mNames) {
      delete [] mNames;	
   }

   if (mSliders) {
      delete [] mSliders;
   }

   if (mDisplays) {
      delete [] mDisplays;
   }

   if (mLabels) {
      delete [] mLabels;
   }
}

void VSTEffectDialog::RemoveHandler()
{
#if defined(__WXMAC__)
   if (mOverlayEventHandlerRef) {
      ::RemoveEventHandler(mOverlayEventHandlerRef);
      mOverlayEventHandlerRef = 0;
   }

   if (mOverlayEventHandlerUPP) {
      DisposeEventHandlerUPP(mOverlayEventHandlerUPP);
      mOverlayEventHandlerUPP = 0;
   }

   if (mWindowEventHandlerRef) {
      ::RemoveEventHandler(mWindowEventHandlerRef);
      mWindowEventHandlerRef = 0;
      MacInstallTopLevelWindowEventHandler();
   }

   if (mWindowEventHandlerUPP) {
      DisposeEventHandlerUPP(mWindowEventHandlerUPP);
      mWindowEventHandlerUPP = 0;
   }
#endif
}

void VSTEffectDialog::BuildFancy()
{
   struct
   {
      short top, left, bottom, right;
   } *rect;

   // Some effects like to have us get their rect before opening them.
   mEffect->callDispatcher(effEditGetRect, 0, 0, &rect, 0.0);

#if defined(__WXMAC__)

   // Retrieve the current window and the one above it.  The window list
   // is kept in top-most to bottom-most order, so we'll use that to
   // determine if another window was opened above ours.
   mWindowRef = (WindowRef) MacGetWindowRef();
   mPreviousRef = GetPreviousWindow(mWindowRef);

   // Install the event handler on our window
   mWindowEventHandlerUPP = NewEventHandlerUPP(WindowEventHandler);
   InstallWindowEventHandler(mWindowRef,
                             mWindowEventHandlerUPP,
                             GetEventTypeCount(WindowEventList),
                             WindowEventList,
                             this,
                             &mWindowEventHandlerRef);

   // Find the content view within our window
   HIViewRef view;
   HIViewFindByID(HIViewGetRoot(mWindowRef), kHIViewWindowContentID, &view);

   // And ask the effect to add it's GUI
   mEffect->callDispatcher(effEditOpen, 0, 0, mWindowRef, 0.0);

   // Get the subview it created
   HIViewRef subview = HIViewGetFirstSubview(view);
   if (subview == NULL) {
      // Doesn't seem the effect created the subview, so switch
      // to the plain dialog
      mEffect->callDispatcher(effEditClose, 0, 0, mWindowRef, 0.0);
      mGui = false;
      RemoveHandler();
      BuildPlain();
      return;
   }

#elif defined(__WXMSW__)

   wxWindow *w = new wxPanel(this, wxID_ANY);
   mEffect->callDispatcher(effEditOpen, 0, 0, w->GetHWND(), 0.0);

#else
#endif

   // Get the final bounds of the effect GUI
   mEffect->callDispatcher(effEditGetRect, 0, 0, &rect, 0.0);

   // Build our display now
   wxBoxSizer *vs = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer *hs = new wxBoxSizer(wxHORIZONTAL);

   // Add the program bar at the top
   vs->Add(BuildProgramBar(), 0, wxCENTER | wxEXPAND);

   // Reserve space for the effect GUI
#if defined(__WXMAC__)

   mContainer = hs->Add(rect->right - rect->left, rect->bottom - rect->top);

#elif defined(__WXMSW__)

   mContainer = hs->Add(w, 1, wxCENTER | wxEXPAND);
   mContainer->SetMinSize(rect->right - rect->left, rect->bottom - rect->top);

#else
#endif

   vs->Add(hs, 0, wxCENTER);

   // Add the standard button bar at the bottom
   vs->Add(CreateStdButtonSizer(this, ePreviewButton|eCancelButton|eOkButton), 0, wxEXPAND);
   SetSizerAndFit(vs);

   // Found out where the reserved space wound up
   wxPoint pos = mContainer->GetPosition();

#if defined(__WXMAC__)
   // Reposition the subview into the reserved space
   HIViewPlaceInSuperviewAt(subview, pos.x, pos.y);

   // Some VST effects do not work unless the default handler is removed since
   // it captures many of the events that the plugins need.  But, it must be
   // done last since proper window sizing will not occur otherwise.
   ::RemoveEventHandler((EventHandlerRef)MacGetEventHandler());

#elif defined(__WXMSW__)
#else
#endif
}

void VSTEffectDialog::BuildPlain()
{
   mNames = new wxStaticText *[mAEffect->numParams];
   mSliders = new wxSlider *[mAEffect->numParams];
   mDisplays = new wxStaticText *[mAEffect->numParams];
   mLabels = new wxStaticText *[mAEffect->numParams];

   wxBoxSizer *vSizer = new wxBoxSizer(wxVERTICAL);
   vSizer->Add(BuildProgramBar(), 0,  wxALIGN_CENTER | wxEXPAND);

   wxScrolledWindow *sw = new wxScrolledWindow(this,
                                               wxID_ANY,
                                               wxDefaultPosition,
                                               wxDefaultSize,
                                               wxVSCROLL | wxTAB_TRAVERSAL);

   // Try to give the window a sensible default/minimum size
   wxSize sz = GetParent()->GetSize();
   sw->SetMinSize(wxSize(wxMax(600, sz.GetWidth() * 2 / 3), sz.GetHeight() / 2));
                                              
   sw->SetScrollRate(0, 20);
   vSizer->Add(sw, 1, wxEXPAND | wxALL, 5);

   // Preview, OK, & Cancel buttons
   vSizer->Add(CreateStdButtonSizer(this, ePreviewButton|eCancelButton|eOkButton), 0, wxEXPAND);

   SetSizer(vSizer);

   wxSizer *paramSizer = new wxStaticBoxSizer(wxVERTICAL, sw, _("Effect Settings"));

   wxFlexGridSizer *gridSizer = new wxFlexGridSizer(4, 0, 0);
   gridSizer->AddGrowableCol(1);

   // Find the longest parameter name.
   int namew = 0;
   int w;
   int h;
   for (int i = 0; i < mAEffect->numParams; i++) {
      wxString text = mEffect->GetString(effGetParamName, i);
      if (text.Right(1) != wxT(':')) {
         text += wxT(':');
      }
      GetTextExtent(text, &w, &h);
      if (w > namew) {
         namew = w;
      }
   }

   GetTextExtent(wxT("HHHHHHHH"), &w, &h);

   for (int i = 0; i < mAEffect->numParams; i++) {
      mNames[i] = new wxStaticText(sw,
                                    wxID_ANY,
                                    wxEmptyString,
                                    wxDefaultPosition,
                                    wxSize(namew, -1),
                                    wxALIGN_RIGHT | wxST_NO_AUTORESIZE);
      gridSizer->Add(mNames[i], 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);

      mSliders[i] = new wxSlider(sw,
                                 ID_VST_SLIDERS + i,
                                 0,
                                 0,
                                 1000,
                                 wxDefaultPosition,
                                 wxSize(200, -1));
      gridSizer->Add(mSliders[i], 0, wxALIGN_CENTER_VERTICAL | wxEXPAND | wxALL, 5);

      mDisplays[i] = new wxStaticText(sw,
                                      wxID_ANY,
                                      wxEmptyString,
                                      wxDefaultPosition,
                                      wxSize(w, -1),
                                      wxALIGN_RIGHT | wxST_NO_AUTORESIZE);
      gridSizer->Add(mDisplays[i], 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);

      mLabels[i] = new wxStaticText(sw,
                                     wxID_ANY,
                                     wxEmptyString,
                                     wxDefaultPosition,
                                     wxSize(w, -1),
                                     wxALIGN_LEFT | wxST_NO_AUTORESIZE);
      gridSizer->Add(mLabels[i], 0, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 5);
   }

   paramSizer->Add(gridSizer, 1, wxEXPAND | wxALL, 5);
   sw->SetSizer(paramSizer);

   Layout();
   Fit();
   SetSizeHints(GetSize());
   RefreshParameters();

   mSliders[0]->SetFocus();
}

wxSizer *VSTEffectDialog::BuildProgramBar()
{
   wxArrayString progs;

   // Some plugins, like Guitar Rig 5, only report 128 programs while they have hundreds.  While
   // I was able to come up with a hack in the Guitar Rig case to gather all of the program names,
   // it would not let me set a program outside of the first 128.
   for (int i = 0; i < mAEffect->numPrograms; i++) {
      progs.Add(mEffect->GetString(effGetProgramNameIndexed, i));
   }

   if (progs.GetCount() == 0) {
      progs.Add(_("None"));
   }

   wxString val;
   int progn = mEffect->callDispatcher(effGetProgram, 0, 0, NULL, 0.0);

   // An unset program is perfectly valid, do not force a default.
   if (progn >= 0) {
      val = progs[progn];
   }

   wxBoxSizer *hs = new wxBoxSizer(wxHORIZONTAL);

   wxStaticText *st = new wxStaticText(this, wxID_ANY, _("Presets:"));
   hs->Add(st, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);

   mProgram = new wxComboBox(this,
                             ID_VST_PROGRAM,
                             val,
                             wxDefaultPosition,
                             wxSize(200, -1),
                             progs);
   mProgram->SetName(_("Presets"));
   hs->Add(mProgram, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);

   wxButton *bt = new wxButton(this, ID_VST_LOAD, _("&Load"));
   hs->Add(bt, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);

   bt = new wxButton(this, ID_VST_SAVE, _("&Save"));
   hs->Add(bt, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);

   hs->AddStretchSpacer();

   bt = new wxButton(this, ID_VST_SETTINGS, _("S&ettings..."));
   hs->Add(bt, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 5);

   return hs;
}

void VSTEffectDialog::RefreshParameters(int skip)
{
   if (!mGui) {
      for (int i = 0; i < mAEffect->numParams; i++) {
         wxString text = mEffect->GetString(effGetParamName, i).Trim(true).Trim(false);
         wxString name = text;

         if (text.Right(1) != wxT(':')) {
            text += wxT(':');
         }
         mNames[i]->SetLabel(text);

         // For some parameters types like on/off, setting the slider value has
         // a side effect that causes it to only move when the parameter changes
         // from off to on.  However, this prevents changing the value using the
         // keyboard, so we skip the active slider if any.
         if (i != skip) {
            mSliders[i]->SetValue(mEffect->callGetParameter(i) * 1000);
         }
         name = text;

         text = mEffect->GetString(effGetParamDisplay, i);
         if (text.IsEmpty()) {
            text.Printf(wxT("%.5g"),mEffect->callGetParameter(i));
         }
         mDisplays[i]->SetLabel(wxString::Format(wxT("%8s"), text.c_str()));
         name += wxT(' ') + text;

         text = mEffect->GetString(effGetParamDisplay, i);
         if (!text.IsEmpty()) {
            text.Printf(wxT("%-8s"), mEffect->GetString(effGetParamLabel, i).c_str());
            mLabels[i]->SetLabel(wxString::Format(wxT("%8s"), text.c_str()));
            name += wxT(' ') + text;
         }

         mSliders[i]->SetName(name);
      }
   }
}

void VSTEffectDialog::OnUpdateDisplay(wxCommandEvent & evt)
{
   int i;

   Freeze();

   // Refresh the program list since some effects change the available programs based
   // on the users activity.
   mProgram->Clear();
   for (i = 0; i < mAEffect->numPrograms; i++) {
      mProgram->Append(mEffect->GetString(effGetProgramNameIndexed, i));
   }

   // The new list may not have the previously selected program or the user may have
   // changed it
   i = mEffect->callDispatcher(effGetProgram, 0, 0, NULL, 0.0);
   if (i >= 0) {
      mProgram->SetSelection(i);
   }

   Thaw();
}

void VSTEffectDialog::OnSizeWindow(wxCommandEvent & evt)
{
   if (!mContainer) {
      return;
   }

   mContainer->SetMinSize(evt.GetInt(), (int) evt.GetExtraLong());
   Fit();
   Layout();
}

void VSTEffectDialog::OnSlider(wxCommandEvent & evt)
{
   wxSlider *s = (wxSlider *) evt.GetEventObject();
   int i = s->GetId() - ID_VST_SLIDERS;

   mEffect->callSetParameter(i, s->GetValue() / 1000.0);

   RefreshParameters(i);
}

void VSTEffectDialog::OnProgram(wxCommandEvent & evt)
{
   mEffect->callDispatcher(effSetProgram, 0, evt.GetInt(), NULL, 0.0);
   RefreshParameters();
}

void VSTEffectDialog::OnProgramText(wxCommandEvent & WXUNUSED(event))
{
   int i = mEffect->callDispatcher(effGetProgram, 0, 0, NULL, 0.0);

   // Bail if nothing is selected
   if (i < 0) {
      return;
   }

   wxString name = mProgram->GetValue();
   int ip = mProgram->GetInsertionPoint();

   // Limit the length of the string, max 24 + 1 for null terminator
   if (name.Length() > 24) {
      name = name.Left(24);
   }

   mEffect->SetString(effSetProgramName, name, i);

   // Some effects do not allow you to change the name and you can't always trust the
   // return value, so just get ask for the name again.
   name = mEffect->GetString(effGetProgramNameIndexed, i);

   mProgram->SetString(i, name);

   // On Windows, must reselect after doing a SetString()...at least that's
   // what seems to be required.
   mProgram->SetStringSelection(name);

   // Which also means we have to reposition the caret.
   if (ip >= 0) {
      mProgram->SetInsertionPoint(ip);
   }
   
   RefreshParameters();
}

void VSTEffectDialog::OnLoad(wxCommandEvent & WXUNUSED(event))
{
   wxString fn;

   // Ask the user for the real name
   fn = FileSelector(_("Load VST Preset:"),
                     FileNames::DataDir(),
                     wxEmptyString,
                     wxT("xml"),
                     wxT("VST preset files (*.fxp; *.xml)|*.fxp;*.xml"),
                     wxFD_OPEN | wxRESIZE_BORDER,
                     this);

   // User canceled...
   if (fn.IsEmpty()) {
      return;
   }

   size_t len = fn.Len();
   if (len > 4 && wxStricmp(fn.Mid(len - 4), wxT(".fxp")) == 0) {
      //
      // FXP specification from VST SDK 2.4 (vstfxstore.h)
      //
      int i;
      wxInt32 buffer[8];
      char *buf = NULL;
      bool error = false;

      // read VST program file (FXP)
      wxFFile fxpFile(fn, wxT("rb"));
      if (!fxpFile.IsOpened()) {
         wxMessageBox(wxString::Format(_("Could not open file: \"%s\""), fn.c_str()),
                      _("Error Loading VST Presets"),
                      wxOK | wxCENTRE,
                      this);
         return;
      }
      if (!error) error = !fread(buffer, 28, 1, fxpFile.fp());

      // VST always uses Big Endian, convert first
      for (i = 0; i < 7; i++)
         buffer[i] = wxINT32_SWAP_ON_LE(buffer[i]);

      if (!error) error = (buffer[0] != CCONST('C', 'c', 'n', 'K'));    ///< 'CcnK'
      //VstInt32 byteSize;			                                    ///< size of this chunk, excl. magic + byteSize
      if (!error) {               
         if (mAEffect->flags & effFlagsProgramChunks)
            error = (buffer[2] != CCONST('F', 'P', 'C', 'h'));          ///< 'FxCk' (regular) or 'FPCh' (opaque chunk)
         else
            error = (buffer[2] != CCONST('F', 'x', 'C', 'k'));
      }
      if (!error) error = (buffer[3] != 1);                             ///< format version (currently 1)
      if (!error) error = (buffer[4] != mAEffect->uniqueID);            ///< fx unique ID    (could try to load instead, as XML does)
      //if (!error) error = (buffer[5] !=                               ///< fx version      (currently ignored)
      //                     mEffect->callDispatcher(effGetVendorVersion, 0, 0, NULL, 0.0));
      if (!error) error = (buffer[6] != mAEffect->numParams);           ///< number of parameters

      buffer[7] = 0; // ensure trailing NUL
      if (!error) error = !fread(buffer, 28, 1, fxpFile.fp());          ///< program name (null-terminated ASCII string)
      if (!error) fn = wxConvLocal.cMB2WC((char*)buffer, 29, &len);

      if (mAEffect->flags & effFlagsProgramChunks) {
         if (!error) error = !fread(buffer, 4, 1, fxpFile.fp());
         i = wxINT32_SWAP_ON_LE(buffer[0]);
         if (!error) error = (i < 1);

         if (!error) {
            buf = new char[i];
            error = !buf;
         }
         if (!error) error = !fread(buf, i, 1, fxpFile.fp());
         if (!error) mEffect->callDispatcher(effSetChunk, 1, i, buf, 0.0);

         delete [] buf;
      }
      else {
         float val;

         if (!error) {
            buf = new char[mAEffect->numParams << 2];
            error = !buf;
         }
         if (!error) error = !fread(buf, mAEffect->numParams << 2, 1, fxpFile.fp());

         i = -1;
         while (!error && (++i < mAEffect->numParams)) {
            *((wxInt32*)&val) = wxINT32_SWAP_ON_LE(((wxInt32*)buf)[i]);
            if (!error) error = (val < 0.0 || val > 1.0);
            if (!error) mEffect->callSetParameter(i, val);
         }

         delete [] buf;
      }

      // set program name
      if (!error && !fn.IsEmpty()) {
         i = mProgram->GetCurrentSelection();
         if (i < 0) i = 0;   // default to first program
         mProgram->SetString(i, fn);
         mProgram->SetValue(fn);
         mEffect->SetString(effSetProgramName, fn, i);
      }

      if (error) {
         wxMessageBox(_("Could not load file or incompatible content."),
                      _("Error Loading VST Presets"),
                      wxOK | wxCENTRE,
                      this);
      }
      fxpFile.Close();
   }
   else {
      // default to read as XML file
      // Load the program
      XMLFileReader reader;
      if (!reader.Parse(this, fn)) {
         // Inform user of load failure
         wxMessageBox(reader.GetErrorStr(),
                      _("Error Loading VST Presets"),
                      wxOK | wxCENTRE,
                      this);
      }
   }

   RefreshParameters();

   return;
}

void VSTEffectDialog::OnSave(wxCommandEvent & WXUNUSED(event))
{
   int i = mProgram->GetCurrentSelection();
   wxString fn;

   // Ask the user for the real name
   FileDialog fd(this,
                 _("Save VST Preset As:"),
                 FileNames::DataDir(),
                 mProgram->GetValue(),
                 wxT("Standard VST preset file (*.fxp)|*.fxp|Audacity VST preset file (*.xml)|*.xml"),
                 wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER);

   // User canceled...
   if (fd.ShowModal() == wxID_CANCEL) {
      return;
   }

   fn = fd.GetPath();
   if (fd.GetFilterIndex() == 0) {
      //
      // FXP specification from VST SDK 2.4 (vstfxstore.h)
      //
      int i;
      wxInt32 buffer[8], chunkSize;
      char *buf = NULL;
      bool error = false;

      // Create/Open the file
      wxFFile fxpFile(fn, wxT("wb"));
      if (!fxpFile.IsOpened()) {
         wxMessageBox(wxString::Format(_("Could not open file: \"%s\""), fn.c_str()),
                      _("Error Saving VST Presets"),
                      wxOK | wxCENTRE,
                      this);
         return;
      }
      
      buffer[0] = CCONST('C', 'c', 'n', 'K'); // VstInt32 chunkMagic;   ///< 'CcnK'
      buffer[1] = 48; // VstInt32 byteSize;			                    ///< size of this chunk, excl. magic + byteSize
      if (mAEffect->flags & effFlagsProgramChunks) {                    ///< 'FxCk' (regular) or 'FPCh' (opaque chunk)
         buffer[2] = CCONST('F', 'P', 'C', 'h');
         chunkSize = mEffect->callDispatcher(effGetChunk, 1, 0, &buf, 0.0);
         buffer[1] += 4;
         buffer[1] += chunkSize;
      }
      else {
         buffer[2] = CCONST('F', 'x', 'C', 'k');
         buffer[1] += (mAEffect->numParams << 2);
      }
      buffer[3] = 1;                                                    ///< format version (currently 1)
      buffer[4] = mAEffect->uniqueID;                                   ///< fx unique ID
      buffer[5] = mEffect->callDispatcher(effGetVendorVersion, 0, 0, NULL, 0.0);      ///< fx version
      buffer[6] = mAEffect->numParams;                      

      // VST always uses Big Endian, convert first
      for (i = 0; i < 7; i++)
         buffer[i] = wxINT32_SWAP_ON_LE(buffer[i]);

      if (!error) error = !fwrite(buffer, 28, 1, fxpFile.fp());
      memset(buffer, 0, 28);
      wxConvLocal.FromWChar((char*)buffer, 27, mProgram->GetValue());
      if (!error) error = (fwrite(buffer, 1, 28, fxpFile.fp()) < 28);   ///< program name (null-terminated ASCII string)
 
      if (mAEffect->flags & effFlagsProgramChunks) {
         buffer[0] = wxINT32_SWAP_ON_LE(chunkSize);
         if (!error) error = !fwrite(buffer, 4, 1, fxpFile.fp());
         if (!error) error = !fwrite(buf, chunkSize, 1, fxpFile.fp());
      }
      else {
         float val;
         if (!error) {
            buf = new char[mAEffect->numParams << 2];
            error = !buf;
         }
         for (i = 0; i < mAEffect->numParams; i++) {
            val = mEffect->callGetParameter(i);
            ((wxInt32*)buf)[i] = wxINT32_SWAP_ON_LE(*((wxInt32*)&val));
         }
         if (!error) error = !fwrite(buf, (mAEffect->numParams << 2), 1, fxpFile.fp());
         delete [] buf;
      }

      fxpFile.Close();
   }
   else {
      XMLFileWriter xmlFile;

      // Create/Open the file
      xmlFile.Open(fn, wxT("wb"));

      xmlFile.StartTag(wxT("vstprogrampersistence"));
      xmlFile.WriteAttr(wxT("version"), wxT("1"));

      i = mEffect->callDispatcher(effGetVendorVersion, 0, 0, NULL, 0.0);
      xmlFile.StartTag(wxT("effect"));
      xmlFile.WriteAttr(wxT("name"), mEffect->GetEffectIdentifier());
      xmlFile.WriteAttr(wxT("version"), i);

      xmlFile.StartTag(wxT("program"));
      xmlFile.WriteAttr(wxT("name"), mProgram->GetValue());

      int clen = 0;
      if (mAEffect->flags & effFlagsProgramChunks) {
         void *chunk = NULL;

         clen = (int) mEffect->callDispatcher(effGetChunk, 1, 0, &chunk, 0.0);
         if (clen != 0) {
            xmlFile.StartTag(wxT("chunk"));
            xmlFile.WriteSubTree(b64encode(chunk, clen) + wxT('\n'));
            xmlFile.EndTag(wxT("chunk"));
         }
      }

      if (clen == 0) {
         for (i = 0; i < mAEffect->numParams; i++) {
            xmlFile.StartTag(wxT("param"));

            xmlFile.WriteAttr(wxT("index"), i);
            xmlFile.WriteAttr(wxT("name"),
                              mEffect->GetString(effGetParamName, i));
            xmlFile.WriteAttr(wxT("value"),
                              wxString::Format(wxT("%f"),
                              mEffect->callGetParameter(i)));

            xmlFile.EndTag(wxT("param"));
         }
      }

      xmlFile.EndTag(wxT("program"));

      xmlFile.EndTag(wxT("effect"));

      xmlFile.EndTag(wxT("vstprogrampersistence"));

      // Close the file
      xmlFile.Close();

   }
}

void VSTEffectDialog::OnSettings(wxCommandEvent & WXUNUSED(event))
{
   VSTEffectSettingsDialog dlg(this);
   dlg.ShowModal();
}

void VSTEffectDialog::OnClose(wxCloseEvent & WXUNUSED(event))
{
   EndModal(false);
}

void VSTEffectDialog::OnPreview(wxCommandEvent & WXUNUSED(event))
{
   mEffect->Preview();
}

void VSTEffectDialog::OnOk(wxCommandEvent & WXUNUSED(event))
{
   // Hide the dialog before closing the effect to prevent a brief empty dialog
   Show(false);

   if (mGui) {
      mEffect->callDispatcher(effEditClose, 0, 0, NULL, 0.0);
   }

   EndModal(true);
}

void VSTEffectDialog::OnCancel(wxCommandEvent & WXUNUSED(event))
{
   // Hide the dialog before closing the effect to prevent a brief empty dialog
   Show(false);

   if (mGui) {
      mEffect->callDispatcher(effEditClose, 0, 0, NULL, 0.0);
   }

   EndModal(false);
}

bool VSTEffectDialog::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   if (wxStrcmp(tag, wxT("vstprogrampersistence")) == 0) {
      while (*attrs) {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;
         
         if (!value) {
            break;
         }

         const wxString strValue = value;

         if (wxStrcmp(attr, wxT("version")) == 0) {
            if (!XMLValueChecker::IsGoodInt(strValue)) {
               return false;
            }
            // Nothing to do with it for now
         }
         else {
            return false;
         }
      }

      return true;
   }

   if (wxStrcmp(tag, wxT("effect")) == 0) {
      while (*attrs) {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;
         
         if (!value) {
            break;
         }

         const wxString strValue = value;

         if (wxStrcmp(attr, wxT("name")) == 0) {
            if (!XMLValueChecker::IsGoodString(strValue)) {
               return false;
            }

            if (value != mEffect->GetEffectIdentifier()) {
               wxString msg;
               msg.Printf(_("This parameter file was saved from %s.  Continue?"), value);
               int result = wxMessageBox(msg, wxT("Confirm"), wxYES_NO, this);
               if (result == wxNO) {
                  return false;
               }
            }
         }
         else if (wxStrcmp(attr, wxT("version")) == 0) {
            if (!XMLValueChecker::IsGoodInt(strValue)) {
               return false;
            }
            // Nothing to do with it for now
         }
         else {
            return false;
         }
      }

      return true;
   }
      
   if (wxStrcmp(tag, wxT("program")) == 0) {
      while (*attrs) {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;
         
         if (!value) {
            break;
         }

         const wxString strValue = value;

         if (wxStrcmp(attr, wxT("name")) == 0) {
            if (!XMLValueChecker::IsGoodString(strValue)) {
               return false;
            }

            if (strValue.Length() > 24) {
               return false;
            }

            int ndx = mProgram->GetCurrentSelection();
            if (ndx == wxNOT_FOUND)
            {
               ndx = 0;
            }

            mProgram->SetString(ndx, strValue);
            mProgram->SetValue(strValue);

            mEffect->SetString(effSetProgramName, strValue, ndx);
         }
         else {
            return false;
         }
      }

      mInChunk = false;

      return true;
   }

   if (wxStrcmp(tag, wxT("param")) == 0) {
      long ndx = -1;
      double val = -1.0;
      while (*attrs) {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;

         if (!value) {
            break;
         }

         const wxString strValue = value;

         if (wxStrcmp(attr, wxT("index")) == 0) {
            if (!XMLValueChecker::IsGoodInt(strValue) || !strValue.ToLong(&ndx)) {
               return false;
            }

            if (ndx < 0 || ndx >= mAEffect->numParams) {
               // Could be a different version of the effect...probably should
               // tell the user
               return false;
            }
         }
         else if (wxStrcmp(attr, wxT("name")) == 0) {
            if (!XMLValueChecker::IsGoodString(strValue)) {
               return false;
            }
            // Nothing to do with it for now
         }
         else if (wxStrcmp(attr, wxT("value")) == 0) {
            if (!XMLValueChecker::IsGoodInt(strValue) ||
               !Internat::CompatibleToDouble(strValue, &val)) {
               return false;
            }

            if (val < 0.0 || val > 1.0) {
               return false;
            }
         }
      }

      if (ndx == -1 || val == -1.0) {
         return false;
      }

      mEffect->callSetParameter(ndx, val);

      return true;
   }

   if (wxStrcmp(tag, wxT("chunk")) == 0) {
      mInChunk = true;
      return true;
   }

   return false;
}

void VSTEffectDialog::HandleXMLEndTag(const wxChar *tag)
{
   if (wxStrcmp(tag, wxT("chunk")) == 0) {
      if (mChunk.Length()) {
         char *buf = new char[mChunk.Length() / 4 * 3];

         int len = b64decode(mChunk, buf);
         if (len) {
            mEffect->callDispatcher(effSetChunk, 1, len, buf, 0.0);
         }

         delete [] buf;
         mChunk.Clear();
      }
      mInChunk = false;
   }
}

void VSTEffectDialog::HandleXMLContent(const wxString & content)
{
   if (mInChunk) {
      mChunk += wxString(content).Trim(true).Trim(false);
   }
}

XMLTagHandler *VSTEffectDialog::HandleXMLChild(const wxChar *tag)
{
   if (wxStrcmp(tag, wxT("vstprogrampersistence")) == 0) {
      return this;
   }

   if (wxStrcmp(tag, wxT("effect")) == 0) {
      return this;
   }

   if (wxStrcmp(tag, wxT("program")) == 0) {
      return this;
   }

   if (wxStrcmp(tag, wxT("param")) == 0) {
      return this;
   }

   if (wxStrcmp(tag, wxT("chunk")) == 0) {
      return this;
   }

   return NULL;
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

wxString VSTEffectDialog::b64encode(const void *in, int len)
{
   unsigned char *p = (unsigned char *) in;
   wxString out;
   
   unsigned long temp;
   for (int i = 0; i < len / 3; i++) {
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

int VSTEffectDialog::b64decode(wxString in, void *out)
{
   int len = in.Length();
   unsigned char *p = (unsigned char *) out;

   if (len % 4) { //Sanity check
      return 0;
   }

   int padding = 0;
   if (len) {
      if (in[len - 1] == padc) {
         padding++;
      }

      if (in[len - 2] == padc) {
         padding++;
      }
   }

   //const char *a = in.mb_str();
   //Setup a vector to hold the result
   unsigned long temp = 0; //Holds decoded quanta
   int i = 0;
   while (i < len) {
      for (int quantumPosition = 0; quantumPosition < 4; quantumPosition++) {
         unsigned char c = in[i];
         temp <<= 6;

         if (c >= 0x41 && c <= 0x5A) {
            temp |= c - 0x41;
         }
         else if (c >= 0x61 && c <= 0x7A) {
            temp |= c - 0x47;
         }
         else if (c >= 0x30 && c <= 0x39) {
            temp |= c + 0x04;
         }
         else if (c == 0x2B) {
            temp |= 0x3E;
         }
         else if (c == 0x2F) {
            temp |= 0x3F;
         }
         else if (c == padc) {
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

///////////////////////////////////////////////////////////////////////////////
//
// VSTEffect
//
///////////////////////////////////////////////////////////////////////////////

typedef AEffect *(*vstPluginMain)(audioMasterCallback audioMaster);

static intptr_t audioMaster(AEffect * effect,
                            int32_t opcode,
                            int32_t index,
                            intptr_t value,
                            void * ptr,
                            float opt)
{
   VSTEffect *vst = (effect ? (VSTEffect *) effect->user : NULL);

   // Handles operations during initialization...before VSTEffect has had a
   // chance to set its instance pointer.
   switch (opcode)
   {
      case audioMasterVersion:
         return (intptr_t) 2400;

      case audioMasterCurrentId:
         return (intptr_t) audacityVSTID;

      case audioMasterGetVendorString:
         strcpy((char *) ptr, "Audacity Team");    // Do not translate, max 64 + 1 for null terminator
         return 1;

      case audioMasterGetProductString:
         strcpy((char *) ptr, "Audacity");         // Do not translate, max 64 + 1 for null terminator
         return 1;

      case audioMasterGetVendorVersion:
         return (intptr_t) (AUDACITY_VERSION << 24 |
                            AUDACITY_RELEASE << 16 |
                            AUDACITY_REVISION << 8 |
                            AUDACITY_MODLEVEL);

      // Let the effect know if a pin (channel in our case) is connected
      case audioMasterPinConnected:
         if (vst) {
            return (intptr_t) (index < vst->GetChannels() ? 0 : 1);
         }
         return 0;

      // Some (older) effects depend on an effIdle call when requested.  An
      // example is the Antress Modern plugins which uses the call to update
      // the editors display when the program (preset) changes.
      case audioMasterNeedIdle:
         if (vst) {
            return vst->NeedIdle();
         }
         return 0;

      // We would normally get this if the effect editor is dipslayed and something "major"
      // has changed (like a program change) instead of multiple automation calls.
      // Since we don't do anything with the parameters while the editor is displayed,
      // there's no need for us to do anything.
      case audioMasterUpdateDisplay:
         if (vst) {
            vst->UpdateDisplay();
            return 1;
         }
         return 0;

      // Return the current time info.
      case audioMasterGetTime:
         if (vst) {
            return (intptr_t) vst->GetTimeInfo();
         }
         return 0;

      // Inputs, outputs, or initial delay has changed...all we care about is initial delay.
      case audioMasterIOChanged:
         if (vst) {
            vst->SetBufferDelay(effect->initialDelay);
            return 1;
         }
         return 0;

      case audioMasterGetSampleRate:
         if (vst) {
            return (intptr_t) vst->GetSampleRate();
         }
         return 0;

      case audioMasterIdle:
         wxYieldIfNeeded();
         return 1;
      
      case audioMasterGetCurrentProcessLevel:
         if (vst) {
            return vst->GetProcessLevel();
         }
         return 0;

      case audioMasterGetLanguage:
         return kVstLangEnglish;

      // We always replace, never accumulate
      case audioMasterWillReplaceOrAccumulate:
         return 1;

      // Resize the window to accommodate the effect size
      case audioMasterSizeWindow:
         if (vst) {
            vst->SizeWindow(index, value);
         }
         return 1;

      case audioMasterCanDo:
      {
#if defined(__WXDEBUG__)
#if defined(__WXMSW__)
         wxLogDebug(wxT("VST canDo: %s"), wxString::FromAscii((char *)ptr).c_str());
#else
         wxPrintf(wxT("VST canDo: %s\n"), wxString::FromAscii((char *)ptr).c_str());
#endif
#endif
         char *s = (char *) ptr;
         if (strcmp(s, "acceptIOChanges") == 0 ||
            strcmp(s, "sizeWindow") == 0) {
            return 1;
         }

#if defined(__WXDEBUG__)
#if defined(__WXMSW__)
         wxLogDebug(wxT("VST canDo: %s"), wxString::FromAscii((char *)ptr).c_str());
#else
         wxPrintf(wxT("VST canDo: %s\n"), wxString::FromAscii((char *)ptr).c_str());
#endif
#endif

         return 0;
      }

      // These are not needed since we don't need the parameter values until after the editor
      // has already been closed.  If we did realtime effects, then we'd need these.
      case audioMasterBeginEdit:
      case audioMasterEndEdit:
      case audioMasterAutomate:

      // We don't do MIDI yet
      case audioMasterWantMidi:
      case audioMasterProcessEvents:

         // Don't need to see any messages about these
         return 0;
   }

#if defined(__WXDEBUG__)
#if defined(__WXMSW__)
   wxLogDebug(wxT("vst: %p opcode: %d index: %d value: %d ptr: %p opt: %f user: %p"),
              effect, opcode, index, value, ptr, opt, vst);
#else
   wxPrintf(wxT("vst: %p opcode: %d index: %d value: %d ptr: %p opt: %f user: %p\n"),
            effect, opcode, index, value, ptr, opt, vst);
#endif
#endif

   return 0;
}

class VSTEffectTimer : public wxTimer
{
public:
   VSTEffectTimer(VSTEffect *effect)
   :  wxTimer(),
      mEffect(effect)
   {
   }

   ~VSTEffectTimer()
   {
   }

   void Notify()
   {
      // Call the effect
      if (!mEffect->callDispatcher(effIdle, 0, 0, NULL, 0.0)) {
         // No more idle calls
         Stop();
      }
   }

private:
   VSTEffect *mEffect;
};

VSTEffect::VSTEffect(const wxString & path)
:  mPath(path)
{
   mModule = NULL;
   mAEffect = NULL;
   mInBuffer = NULL;
   mOutBuffer = NULL;
   mDlg = NULL;
   mTimer = NULL;
   mInputs = 0;
   mOutputs = 0;
   mChannels = 0;
   mBlockSize = 0;
   mProcessLevel = 1;         // in GUI thread

   memset(&mTimeInfo, 0, sizeof(mTimeInfo));
   mTimeInfo.samplePos = 0.0;
   mTimeInfo.sampleRate = 44100.0;  // this is a bogus value, but it's only for the display
   mTimeInfo.nanoSeconds = wxGetLocalTimeMillis().ToDouble();
   mTimeInfo.tempo = 120.0;
   mTimeInfo.timeSigNumerator = 4;
   mTimeInfo.timeSigDenominator = 4;
   mTimeInfo.flags = kVstTempoValid | kVstNanosValid;

   PluginManager & pm = PluginManager::Get();

   if (pm.IsRegistered(VSTPLUGINTYPE, mPath)) {
      mName = pm.Read(wxT("Name"), wxEmptyString);
      mVendor = pm.Read(wxT("Vendor"), wxEmptyString);
      mInputs = pm.Read(wxT("Inputs"), 0L);
      mOutputs = pm.Read(wxT("Outputs"), 0L);
   }
   else if (Load()) {
      pm.RegisterPlugin(VSTPLUGINTYPE, mPath);
      pm.Write(wxT("Name"), mName);
      pm.Write(wxT("Vendor"), mVendor);
      pm.Write(wxT("Inputs"), mInputs);
      pm.Write(wxT("Outputs"), mOutputs);
   }

   if (mVendor.IsEmpty()) {
      mVendor = VSTPLUGINTYPE;
   }

   if (mName.IsEmpty()) {
      wxFileName fn(mPath);
      mName = fn.GetName();
   }

   int flags = PLUGIN_EFFECT;
   if (mInputs == 0) {
      flags |= INSERT_EFFECT;
   }
   else if (mOutputs == 0) {
      flags |= ANALYZE_EFFECT;
   }
   else {
      flags |= PROCESS_EFFECT;
   }

   SetEffectFlags(flags);
}

VSTEffect::~VSTEffect()
{
   Unload();
}

wxString VSTEffect::GetEffectName()
{
   if (mVendor.IsEmpty()) {
      return mName + wxT("...");
   }

   return mVendor + wxT(": ") + mName + wxT("...");
}

wxString VSTEffect::GetEffectIdentifier()
{
   return mName;
}

std::set<wxString> VSTEffect::GetEffectCategories()
{
   return std::set<wxString>();
}

wxString VSTEffect::GetEffectAction()
{
   return _("Performing Effect: ") + mName;
}

bool VSTEffect::Init()
{
   if (!mAEffect) {
      Load();
   }

   if (!mAEffect) {
      return false;
   }

   mBlockSize = 0;

   TrackListIterator iter(mOutputTracks);
   WaveTrack *left = (WaveTrack *) iter.First();
   while (left) {
      sampleCount lstart;
      sampleCount llen;

      GetSamples(left, &lstart, &llen);
      
      if (left->GetLinked()) {
         WaveTrack *right = (WaveTrack *) iter.Next();
         sampleCount rstart;
         sampleCount rlen;

         GetSamples(right, &rstart, &rlen);         

         if (left->GetRate() != right->GetRate()) {
            wxMessageBox(_("Both channels of a stereo track must be the same sample rate."));
            return false;
         }

         if (llen != rlen) {
            wxMessageBox(_("Both channels of a stereo track must be the same length."));
            return false;
         }
      }
      
      left = (WaveTrack *) iter.Next();
   }

   return true;
}

//
// Some history...
//
// Before we ran into the Antress plugin problem with buffer size limitations,
// (see below) we just had a plain old effect loop...get the input samples, pass
// them to the effect, save the output samples.
//
// But, the hack I put in to limit the buffer size to only 8k (normally 512k or so)
// severely impacted performance.  So, Michael C. added some intermediate buffering
// that sped things up quite a bit and this is how things have worked for quite a
// while.  It still didn't get the performance back to the pre-hack stage, but it
// was a definite benefit.
//
// History over...
//
// I've recently (May 2014) tried newer versions of the Antress effects and they
// no longer seem to have a problem with buffer size.  So, I've made a bit of a
// compromise...I've made the buffer size user configurable.  Should have done this
// from the beginning.  I've left the default 8k, just in case, but now the user
// can set the buffering based on their specific setup and needs.
//
// And at the same time I added buffer delay compensation, which allows Audacity
// to account for latency introduced by some effects.  This is based on information
// provided by the effect, so it will not work with all effects since they don't
// allow provide the information (kn0ck0ut is one).
//
bool VSTEffect::PromptUser()
{
   mProcessLevel = 1;      // in GUI thread

   mDlg = new VSTEffectDialog(mParent, mName, this, mAEffect);
   mDlg->CentreOnParent();
   mDlg->ShowModal();

   bool ret = mDlg->GetReturnCode() != 0;

   mDlg->Destroy();
   return ret;
}

bool VSTEffect::Process()
{
   mProcessLevel = 2;      // in (simulated) audio thread

   CopyInputTracks();
   bool bGoodResult = true;

   // Some VST effects (Antress Modern is an example), do not like
   // overly large block sizes.  Unfortunately, I have not found a
   // way to determine if the effect has a maximum it will support,
   // so just limit to small value for now.  This will increase
   // processing time and, it's a shame, because most plugins seem
   // to be able to handle much larger sizes.
   //
   // NOTE:  This no longer seems to apply to more recent versions
   //        of Antress plugins, but leaving comment and 8192 default
   //        just in case.
   gPrefs->Read(wxT("/VST/BufferSize"), &mBufferSize, 8192);

   gPrefs->Read(wxT("/VST/UseBufferDelay"), &mUseBufferDelay, true);
   mBufferDelay = 0;

   mInBuffer = NULL;
   mOutBuffer = NULL;

   TrackListIterator iter(mOutputTracks);
   int count = 0;
   bool clear = false;
   WaveTrack *left = (WaveTrack *) iter.First();
   while (left) {
      WaveTrack *right;
      sampleCount len;
      sampleCount lstart;
      sampleCount rstart;

      GetSamples(left, &lstart, &len);

      mChannels = 1;

      right = NULL;
      rstart = 0;
      if (left->GetLinked() && mInputs > 1) {
         right = (WaveTrack *) iter.Next();         
         GetSamples(right, &rstart, &len);
         clear = false;
         mChannels = 2;
      }

      if (mBlockSize == 0) {
         mBlockSize = mWTBlockSize = left->GetMaxBlockSize() * 2;

         // Limit the buffer size to the user specified value since they may
         // have wanted a smaller value for a reason.
         if (mBlockSize > mBufferSize) {
            mBlockSize = mBufferSize;
         }

         mInBuffer = new float *[mInputs];
         for (int i = 0; i < mInputs; i++) {
            mInBuffer[i] = new float[mBlockSize];
         }

         //Process 2 audacity blockfiles per WaveTrack::Set independently of mBlockSize
         //because it is extremely slow to do multiple Set()s per blockfile.
         mOutBuffer = new float *[mOutputs];
         for (int i = 0; i < mOutputs; i++) {
            mOutBuffer[i] = new float[mWTBlockSize + mBlockSize];
         }

         // Turn the power off
         callDispatcher(effMainsChanged, 0, 0, NULL, 0.0);

         // Set processing parameters
         callDispatcher(effSetSampleRate, 0, 0, NULL, left->GetRate());
         callDispatcher(effSetBlockSize, 0, mBlockSize, NULL, 0.0);
      }

      // Clear unused input buffers
      if (!right && !clear) {
         for (int i = 1; i < mInputs; i++) {
            for (int j = 0; j < mBlockSize; j++) {
               mInBuffer[i][j] = 0.0;
            }
         }
         clear = true;
      }

      bGoodResult = ProcessStereo(count, left, right, lstart, rstart, len);
      if (!bGoodResult) {
         break;
      }

      left = (WaveTrack *) iter.Next();
      count++;
   }

   if (mOutBuffer) {
      for (int i = 0; i < mOutputs; i++) {
         delete mOutBuffer[i];
      }
      delete [] mOutBuffer;
      mOutBuffer = NULL;
   }

   if (mInBuffer) {
      for (int i = 0; i < mInputs; i++) {
         delete mInBuffer[i];
      }
      delete [] mInBuffer;
      mInBuffer = NULL;
   }

   ReplaceProcessedTracks(bGoodResult); 
   return bGoodResult;
}

bool VSTEffect::ProcessStereo(int count,
                              WaveTrack *left, WaveTrack *right,
                              sampleCount lstart, sampleCount rstart,
                              sampleCount len)
{
   bool rc = true;
   //sampleCount amountLeft = 0;

   // Initialize time info
   mTimeInfo.samplePos = 0.0;
   mTimeInfo.sampleRate = left->GetRate();
   mTimeInfo.flags |= kVstTransportPlaying;

   // Turn the power on
   callDispatcher(effMainsChanged, 0, 1, NULL, 0.0);

   // Tell effect we're starting to process
   callDispatcher(effStartProcess, 0, 0, NULL, 0.0);

   // Get the initial latency
   SetBufferDelay(mAEffect->initialDelay);

   // LLL:
   //
   // Some explanation to what this mess is all about.
   // (see history above)
   //
   // For each input block of samples, we pass it to the VST effect along with a
   // variable output location.  This output location is simply a pointer into a
   // much larger buffer.  This reduces the number of calls required to add the
   // samples to the output track which was Michael's speed up mentioned above.
   //
   // The buffer delay compensation adds even more complexitity...
   // 
   // Upon return from the effect, the output samples are "moved to the left" by
   // the number of samples in the current delay setting, effectively removing the
   // delay introduced by the effect.
   //
   // At the same time the total number of delayed samples are gathered and when the
   // there is no further input data to process, the loop continues to call the
   // effect with an empty input buffer until the effect has had a chance to 
   // return all of the remaining delayed samples.
   //
   // Please note, that this process has next to no documetation on how it should
   // work, so a lot of this was from trial and error.  It appears to be correct
   // though since it has worked with every plugin I've found that adds latency,
   // with the exception of kn0ck0ut.  I'm sure there are other effects out there
   // that add latency but do not provide the delay information, so be wary. :-)
   sampleCount originalLen = len;
   sampleCount ls = lstart;
   sampleCount rs = rstart;
   sampleCount outls = lstart;
   sampleCount outrs = rstart;
   sampleCount outBufferCursor = 0;
   float **outBufSegment = new float *[mOutputs];
   sampleCount delay = 0;
   sampleCount delayed = 0;
   bool cleared = false;

   // Call the effect until we run out of input or delayed samples
   while (len || delayed) {
      sampleCount block = mBlockSize;

      // As long as we have input samples, use those
      if (len) {
         // At the end if we don't have enough left for a whole block
         if (block > len) {
            block = len;
         }

         // Get the samples into our buffer
         left->Get((samplePtr)mInBuffer[0], floatSample, ls, block);
         if (right) {
            right->Get((samplePtr)mInBuffer[1], floatSample, rs, block);
         }
      }
      // We've reached the end of the input samples, so start processing
      // delayed ones if there are any
      else if (delayed) {
         // At the end if we don't have enough left for a whole block
         if (block > delayed) {
            block = delayed;
         }

         // Clear the input buffer so that we only pass zeros to the effect.
         if (!cleared) {
            for (int i = 1; i < mInputs; i++) {
               for (int j = 0; j < mBlockSize; j++) {
                  mInBuffer[i][j] = 0.0;
               }
            }
            cleared = true;
         }
      }

      // Set current output pointer
      for (int i = 0; i < mOutputs; i++) {
         outBufSegment[i] = mOutBuffer[i] + outBufferCursor;
      }

      // Go let the effect moleste the samples
      callProcessReplacing(mInBuffer, outBufSegment, block);

      // Get the current number of delayed samples and accumulate
      delay += mBufferDelay;
      delayed += mBufferDelay;

      // Reset...the effect will set this again if it has a further
      // need to delay samples...some effects only set the value once
      // at the start of processing.
      mBufferDelay = 0;

      // If the effect has delayed the output by more samples than our
      // current block size, then we leave the output pointers where they
      // are.  This will effectively remove those delayed samples from the
      // output buffer.
      if (delay >= block) {
         delay -= block;
      }
      // We have some delayed samples, at the beginning of the output samples,
      // so overlay them by shifting the remaining output samples.
      else if (delay > 0) {
         sampleCount oblock = block - delay;
         memmove(outBufSegment[0], outBufSegment[0] + delay, SAMPLE_SIZE(floatSample) * oblock);
         memmove(outBufSegment[1], outBufSegment[1] + delay, SAMPLE_SIZE(floatSample) * oblock);
         delay = 0;
         outBufferCursor += oblock;
      }
      // no delay, just bump to the new output location
      else {
         outBufferCursor += block;
      }

      // Process 2 audacity blockfiles per WaveTrack::Set independently of mBlockSize
      // because it is extremely slow to do multiple Set()s per blockfile due to Undo History
      // If we do more optimization we should probably align the Sets to blockfile boundries.
      if (outBufferCursor >= mWTBlockSize) {
         left->Set((samplePtr)mOutBuffer[0], floatSample, outls, mWTBlockSize);
         if (right) {
            right->Set((samplePtr)mOutBuffer[1], floatSample, outrs, mWTBlockSize);
         }
         if (outBufferCursor >= mWTBlockSize) {
            //snake the buffer down
            memmove(mOutBuffer[0], mOutBuffer[0] + mWTBlockSize, SAMPLE_SIZE(floatSample) * (outBufferCursor - mWTBlockSize));
            memmove(mOutBuffer[1], mOutBuffer[1] + mWTBlockSize, SAMPLE_SIZE(floatSample) * (outBufferCursor - mWTBlockSize));
         }
         outBufferCursor -= mWTBlockSize;
         outls += mWTBlockSize;
         outrs += mWTBlockSize;
      }

      // Still processing input samples
      if (len) {
         len -= block;
      }
      // Or maybe we're working on delayed samples
      else if (delayed) {
         delayed -= block;
      }

      // "ls" and "rs" serve as the input sample index for the left and
      // right channels when processing the input samples.  If we flip
      // over to processing delayed samples, the simply become counters
      // for the progress display.
      ls += block;
      rs += block;
      mTimeInfo.samplePos += ((double) block / mTimeInfo.sampleRate);

      if (mInputs > 1) {      
         if (TrackGroupProgress(count, (ls - lstart) / (double)originalLen)) {
            rc = false;
            break;
         }
      }
      else {
         if (TrackProgress(count, (ls - lstart) / (double)originalLen)) {
            rc = false;
            break;
         }
      }
   }

   // Finish taking the remainder
   if (outBufferCursor) {
     left->Set((samplePtr)mOutBuffer[0], floatSample, outls, outBufferCursor);
     if (right) {
         right->Set((samplePtr)mOutBuffer[1], floatSample, outrs, outBufferCursor);
      }
   }

   // Tell effect we're done
   callDispatcher(effStopProcess, 0, 0, NULL, 0.0);

   // Turn the power off
   callDispatcher(effMainsChanged, 0, 0, NULL, 0.0);

   // No longer playing
   mTimeInfo.samplePos = 0.0;
   mTimeInfo.sampleRate = 44100.0;
   mTimeInfo.tempo = 120.0;
   mTimeInfo.timeSigNumerator = 4;
   mTimeInfo.timeSigDenominator = 4;
   mTimeInfo.flags = kVstTempoValid | kVstNanosValid;

   return rc;
}

void VSTEffect::End()
{
}

bool VSTEffect::Load()
{
   vstPluginMain pluginMain;
   bool success = false;

   mModule = NULL;
   mAEffect = NULL;

#if defined(__WXMAC__)
   // Start clean
   mBundleRef = NULL;

   // Don't really know what this should be initialize to
   mResource = -1;

   // Convert the path to a CFSTring
   wxMacCFStringHolder path(mPath);

   // Convert the path to a URL
   CFURLRef urlRef =
      CFURLCreateWithFileSystemPath(kCFAllocatorDefault,
                                    path,
                                    kCFURLPOSIXPathStyle,
                                    true);
   if (urlRef == NULL) {
      return false;
   }

   // Create the bundle using the URL
   CFBundleRef bundleRef = CFBundleCreate(kCFAllocatorDefault, urlRef);

   // Done with the URL
   CFRelease(urlRef);

   // Bail if the bundle wasn't created
   if (bundleRef == NULL) {
      return false;
   }

   // Retrieve a reference to the executable
   CFURLRef exeRef = CFBundleCopyExecutableURL(bundleRef);
   if (exeRef == NULL) {
      CFRelease(bundleRef);
      return false;
   }

   // Convert back to path
   UInt8 exePath[PLATFORM_MAX_PATH];
   Boolean good = CFURLGetFileSystemRepresentation(exeRef, true, exePath, sizeof(exePath));

   // Done with the executable reference
   CFRelease(exeRef);

   // Bail if we couldn't resolve the executable path
   if (good == FALSE) {
      CFRelease(bundleRef);
      return false;
   }

   // Attempt to open it
   mModule = dlopen((char *) exePath, RTLD_NOW | RTLD_LOCAL);
   if (mModule == NULL) {
      CFRelease(bundleRef);
      return false;
   }

   // Try to locate the new plugin entry point
   pluginMain = (vstPluginMain) dlsym(mModule, "VSTPluginMain");

   // If not found, try finding the old entry point
   if (pluginMain == NULL) {
      pluginMain = (vstPluginMain) dlsym(mModule, "main_macho");
   }

   // Must not be a VST plugin
   if (pluginMain == NULL) {
      dlclose(mModule);
      mModule = NULL;
      CFRelease(bundleRef);
      return false;
   }

   // Need to keep the bundle reference around so we can map the
   // resources.
   mBundleRef = bundleRef;

   // Open the resource map ... some plugins (like GRM Tools) need this.
   mResource = (int) CFBundleOpenBundleResourceMap(bundleRef);

#else

   {
      wxLogNull nolog;

      // Try to load the library
      wxDynamicLibrary *lib = new wxDynamicLibrary(mPath);
      if (!lib) {
         return false;
      }

      // Bail if it wasn't successful
      if (!lib->IsLoaded()) {
         delete lib;
         return false;
      }

      // Try to find the entry point, while suppressing error messages
      pluginMain = (vstPluginMain) lib->GetSymbol(wxT("VSTPluginMain"));
      if (pluginMain == NULL) {
         pluginMain = (vstPluginMain) lib->GetSymbol(wxT("main"));
         if (pluginMain == NULL) {
            delete lib;
            return false;
         }
      }

      // Save the library reference
      mModule = lib;
   }

#endif

   // Initialize the plugin
   mAEffect = pluginMain(audioMaster);

   // Was it successful?
   if (mAEffect) {
      //
      mAEffect->user = this;

      //
      callDispatcher(effOpen, 0, 0, NULL, 0.0);

      // Ensure that it looks like a plugin and can deal with ProcessReplacing
      // calls.  Also exclude synths for now.
      if (mAEffect->magic == kEffectMagic &&
         !(mAEffect->flags & effFlagsIsSynth) &&
         mAEffect->flags & effFlagsCanReplacing) {

         mVendor = GetString(effGetVendorString);
         mName = GetString(effGetEffectName);
         mInputs = mAEffect->numInputs;
         mOutputs = mAEffect->numOutputs;
         
         // We could even go so far as to run a small test here.

         success = true;
      }
   }

   if (!success) {
      Unload();
   }

   return success;
}

void VSTEffect::Unload()
{
   if (mAEffect) {
      callDispatcher(effClose, 0, 0, NULL, 0.0);
   }

   if (mModule) {
#if defined(__WXMAC__)
      if (mResource != -1) {
         CFBundleCloseBundleResourceMap((CFBundleRef) mBundleRef, mResource);
         mResource = -1;
      }

      if (mBundleRef != NULL) {
         CFRelease((CFBundleRef) mBundleRef);
         mBundleRef = NULL;
      }

      dlclose(mModule);
#else
      delete (wxDynamicLibrary *) mModule;
#endif

      mModule = NULL;
      mAEffect = NULL;
   }
}

void VSTEffect::ScanOnePlugin( const wxString & file )
{
   const wxChar * argv[4];
   argv[0] = PlatformCompatibility::GetExecutablePath().c_str();
   argv[1] = VSTCMDKEY;
   argv[2] = file.c_str();
   argv[3] = NULL;
   // ToDo: do we need a try--catch around this in case a bad plug-in 
   // fails? (JKC Nov09)
   wxExecute((wxChar **) argv, wxEXEC_SYNC | wxEXEC_NODISABLE, NULL);
}

int VSTEffect::ShowPluginListDialog( const wxArrayString & files )
{
   PluginRegistrationDialog d( wxGetApp().GetTopWindow(), files );
   return d.ShowModal();
}

void VSTEffect::ShowProgressDialog( const wxString & longest, const wxArrayString & files )
{
   ProgressDialog *progress = new ProgressDialog(_("Scanning VST Plugins"),
                                                 longest,
                                                 pdlgHideStopButton);
//   progress->SetSize(wxSize(500, -1));
   progress->CenterOnScreen();

   size_t cnt = files.GetCount();
   for (size_t i = 0; i < cnt; i++) {
      wxString file = files[i];
      int status = progress->Update(wxLongLong(i),
                                    wxLongLong(cnt),
                                    wxString::Format(_("Checking %s"), file.c_str()));
      if (status != eProgressSuccess) {
         break;
      }
      ScanOnePlugin( file );
   }

   delete progress;   
}

/* static */
int VSTEffect::Scan()
{
   wxArrayString audacityPathList = wxGetApp().audacityPathList;
   wxArrayString pathList;
   wxArrayString files;

   // Check for the VST_PATH environment variable
   wxString vstpath = wxGetenv(wxT("VST_PATH"));
   if (!vstpath.IsEmpty()) {
      wxGetApp().AddUniquePathToPathList(vstpath, pathList);
   }

   // Add Audacity specific paths
   for (size_t i = 0; i < audacityPathList.GetCount(); i++) {
      wxString prefix = audacityPathList[i] + wxFILE_SEP_PATH;
      wxGetApp().AddUniquePathToPathList(prefix + VSTPLUGINTYPE,
                                         pathList);
      wxGetApp().AddUniquePathToPathList(prefix + wxT("plugins"),
                                         pathList);
      wxGetApp().AddUniquePathToPathList(prefix + wxT("plug-ins"),
                                         pathList);
   }

#if defined(__WXMAC__)
#define VSTPATH wxT("/Library/Audio/Plug-Ins/VST")

   // Look in /Library/Audio/Plug-Ins/VST and $HOME/Library/Audio/Plug-Ins/VST
   wxGetApp().AddUniquePathToPathList(VSTPATH, pathList);
   wxGetApp().AddUniquePathToPathList(wxString(wxGetenv(wxT("HOME"))) + VSTPATH,
                                      pathList);

   // Recursively search all paths for Info.plist files.  This will identify all
   // bundles.
   wxGetApp().FindFilesInPathList(wxT("Info.plist"), pathList, files, wxDIR_DEFAULT);

   // Remove the 'Contents/Info.plist' portion of the names
   for (size_t i = 0; i < files.GetCount(); i++) {
      files[i] = wxPathOnly(wxPathOnly(files[i]));
      if (!files[i].EndsWith(wxT(".vst"))) {
         files.RemoveAt(i--);
      }
   }
   
#elif defined(__WXMSW__)

   TCHAR dpath[MAX_PATH];
   TCHAR tpath[MAX_PATH];
   DWORD len;

   // Try HKEY_CURRENT_USER registry key first
   len = sizeof(tpath) / sizeof(TCHAR);
   if (SHRegGetUSValue(wxT("Software\\VST"),
                       wxT("VSTPluginsPath"),
                       NULL,
                       tpath,
                       &len,
                       FALSE,
                       NULL,
                       0) == ERROR_SUCCESS) {
      tpath[len] = 0;
      dpath[0] = 0;
      ExpandEnvironmentStrings(tpath, dpath, WXSIZEOF(dpath));
      wxGetApp().AddUniquePathToPathList(LAT1CTOWX(dpath), pathList);
   }

   // Then try HKEY_LOCAL_MACHINE registry key
   len = sizeof(tpath) / sizeof(TCHAR);
   if (SHRegGetUSValue(wxT("Software\\VST"),
                       wxT("VSTPluginsPath"),
                       NULL,
                       tpath,
                       &len,
                       TRUE,
                       NULL,
                       0) == ERROR_SUCCESS) {
      tpath[len] = 0;
      dpath[0] = 0;
      ExpandEnvironmentStrings(tpath, dpath, WXSIZEOF(dpath));
      wxGetApp().AddUniquePathToPathList(LAT1CTOWX(dpath), pathList);
   }

   // Add the default path last
   dpath[0] = 0;
   ExpandEnvironmentStrings(wxT("%ProgramFiles%\\Steinberg\\VSTPlugins"),
                            dpath,
                            WXSIZEOF(dpath));
   wxGetApp().AddUniquePathToPathList(LAT1CTOWX(dpath), pathList);

   // Recursively scan for all DLLs
   wxGetApp().FindFilesInPathList(wxT("*.dll"), pathList, files, wxDIR_DEFAULT);

#else

   // Recursively scan for all shared objects
   wxGetApp().FindFilesInPathList(wxT("*.so"), pathList, files);

#endif

   files.Sort();

   // This is a hack to allow for long paths in the progress dialog.  The
   // progress dialog should really truncate the message if it's too wide
   // for the dialog.
   size_t cnt = files.GetCount();
   wxString longest;

   // JKC: Let's not show the progress dialog if there are no 
   // files to test.
   if( cnt <= 0 )
      return wxID_OK;

   for (size_t i = 0; i < cnt; i++) {
      if (files[i].Length() > longest.Length()) {
         longest = files[i];
      }
   }
   //Choose the first for the original version which scans them all
   //The second to selectively scan.
   //ShowProgressDialog( longest, files );
   return ShowPluginListDialog(  files );
}

/* static */
void VSTEffect::Check(const wxChar *fname)
{
   PluginManager & pm = PluginManager::Get();

   pm.Open();

   VSTEffect *e = new VSTEffect(fname);

   pm.Close();

   if (e) {
      delete e;
   }
}

int VSTEffect::NeedIdle()
{
   int ret = callDispatcher(effIdle, 0, 0, NULL, 0.0);

   // Effect wants continuous idle calls
   if (ret) {
      if (!mTimer) {
         mTimer = new VSTEffectTimer(this);
      }
      if (mTimer) {
         mTimer->Start(100);
      }
   }
   else {
      if (mTimer) {
         mTimer->Stop();
         delete mTimer;
         mTimer = NULL;
      }
   }

   return ret;
}

int VSTEffect::GetChannels()
{
   return mChannels;
}

VstTimeInfo *VSTEffect::GetTimeInfo()
{
   mTimeInfo.nanoSeconds = wxGetLocalTimeMillis().ToDouble();
   return &mTimeInfo;
}

float VSTEffect::GetSampleRate()
{
   return mTimeInfo.sampleRate;
}

int VSTEffect::GetProcessLevel()
{
   return mProcessLevel;
}

void VSTEffect::SizeWindow(int w, int h)
{
   // Queue the event to make the resizes smoother
   if (mDlg) {
      wxCommandEvent sw(EVT_SIZEWINDOW);
      sw.SetInt(w);
      sw.SetExtraLong(h);
      mDlg->AddPendingEvent(sw);
   }

   return;
}

void VSTEffect::UpdateDisplay()
{
   // Tell the dialog to refresh effect information
   if (mDlg) {
      wxCommandEvent ud(EVT_UPDATEDISPLAY);
      mDlg->AddPendingEvent(ud);
   }
}

void VSTEffect::SetBufferDelay(int samples)
{
   // We do not support negative delay
   if (samples >= 0 && mUseBufferDelay) {
      mBufferDelay = samples;
   }
}

int VSTEffect::GetString(wxString & outstr, int opcode, int index)
{
   char buf[256];

   memset(buf, 0, sizeof(buf));

   int ret = callDispatcher(opcode, index, 0, buf, 0.0);
   if (ret) {
      outstr = LAT1CTOWX(buf);
   }

   return ret;
}

wxString VSTEffect::GetString(int opcode, int index)
{
   wxString str;

   GetString(str, opcode, index);

   return str;
}

void VSTEffect::SetString(int opcode, const wxString & str, int index)
{
   char buf[256];

   strcpy(buf, str.Left(255).mb_str());

   callDispatcher(opcode, index, 0, buf, 0.0);
}

intptr_t VSTEffect::callDispatcher(int opcode,
                                   int index, intptr_t value, void *ptr, float opt)
{
   return mAEffect->dispatcher(mAEffect, opcode, index, value, ptr, opt);
}

void VSTEffect::callProcess(float **inputs, float **outputs, int sampleframes)
{
   mAEffect->process(mAEffect, inputs, outputs, sampleframes);
}

void VSTEffect::callProcessReplacing(float **inputs,
                                     float **outputs, int sampleframes)
{
   mAEffect->processReplacing(mAEffect, inputs, outputs, sampleframes);
}

void VSTEffect::callSetParameter(int index, float parameter)
{
   mAEffect->setParameter(mAEffect, index, parameter);
}

float VSTEffect::callGetParameter(int index)
{
   return mAEffect->getParameter(mAEffect, index);
}

#endif // USE_VST
