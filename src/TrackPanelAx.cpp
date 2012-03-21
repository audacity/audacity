/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackPanelAx.cpp

  Leland Lucius
  and lots of other contributors

******************************************************************//*!

\class TrackPanelAx
\brief Helper to TrackPanel to give accessibility.

*//*******************************************************************/

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifndef WX_PRECOMP
// Include your minimal set of headers here, or wx.h
#include <wx/wx.h>
#endif


#include "Audacity.h"
#include "TrackPanelAx.h"

#include <wx/intl.h>

TrackPanelAx::TrackPanelAx( wxWindow *window )
#if wxUSE_ACCESSIBILITY
   :wxWindowAccessible( window )
#endif
{
   mTrackPanel = wxDynamicCast( window, TrackPanel );
   mFocusedTrack = NULL;
}

TrackPanelAx::~TrackPanelAx()
{
}

// Returns currently focused track or first one if none focused
Track *TrackPanelAx::GetFocus()
{
   if( !mFocusedTrack )
   {
      SetFocus( NULL );
   }

   if( !TrackNum( mFocusedTrack ) )
   {
      mFocusedTrack = NULL;
   }

   return( mFocusedTrack );
}

// Changes focus to a specified track
void TrackPanelAx::SetFocus( Track *track )
{
#if wxUSE_ACCESSIBILITY
   if( mFocusedTrack != NULL )
   {
      NotifyEvent( wxACC_EVENT_OBJECT_SELECTIONREMOVE,
                   mTrackPanel,
                   wxOBJID_CLIENT,
                   TrackNum( mFocusedTrack ) );
   }
#endif

   if( track == NULL )
   {
      TrackListIterator iter( mTrackPanel->mTracks );
      track = iter.First();
   }

   mFocusedTrack = track;

#if wxUSE_ACCESSIBILITY
   if( mFocusedTrack != NULL )
   {
      int num = TrackNum( mFocusedTrack );

      NotifyEvent( wxACC_EVENT_OBJECT_FOCUS,
                   mTrackPanel,
                   wxOBJID_CLIENT,
                   num );

      if( mFocusedTrack->GetSelected() )
      {
         NotifyEvent( wxACC_EVENT_OBJECT_SELECTION,
                      mTrackPanel,
                      wxOBJID_CLIENT,
                      num );
      }
   }
#endif

   return;
}

// Returns TRUE if passed track has the focus
bool TrackPanelAx::IsFocused( Track *track )
{
   if( !mFocusedTrack )
   {
      SetFocus( NULL );
   }

   if( ( track == mFocusedTrack ) ||
       ( track == mFocusedTrack->GetLink() ) )
   {
      return true;
   }

   return false;
}

int TrackPanelAx::TrackNum( Track *target )
{
   TrackListIterator iter( mTrackPanel->mTracks );
   Track *t = iter.First();
   int ndx = 0;

   while( t != NULL )
   {
      ndx++;
      if( t == target )
      {
         return ndx;
      }

      t = iter.Next( true );
   }

   return 0;
}

Track *TrackPanelAx::FindTrack( int num )
{
   TrackListIterator iter( mTrackPanel->mTracks );
   Track *t = iter.First();
   int ndx = 0;

   while( t != NULL )
   {
      ndx++;
      if( ndx == num )
      {
         break;
      }

      t = iter.Next( true );
   }

   return t;
}

void TrackPanelAx::Updated()
{
#if wxUSE_ACCESSIBILITY
   Track *t = GetFocus();
   NotifyEvent(wxACC_EVENT_OBJECT_NAMECHANGE,
               mTrackPanel,
               wxOBJID_CLIENT,
               TrackNum(t));
   SetFocus(t);
#endif
}

#if wxUSE_ACCESSIBILITY

// Retrieves the address of an IDispatch interface for the specified child.
// All objects must support this property.
wxAccStatus TrackPanelAx::GetChild( int childId, wxAccessible** child )
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
wxAccStatus TrackPanelAx::GetChildCount( int* childCount )
{
   TrackListIterator iter( mTrackPanel->mTracks );
   Track *t = iter.First();
   int cnt = 0;

   while( t != NULL )
   {
      cnt++;

      if( t->GetLink() != NULL )
      {
         t = iter.Next();
      }

      t = iter.Next();
   }

   *childCount = cnt;

   return wxACC_OK;
}

// Gets the default action for this object (0) or > 0 (the action for a child).
// Return wxACC_OK even if there is no action. actionName is the action, or the empty
// string if there is no action.
// The retrieved string describes the action that is performed on an object,
// not what the object does as a result. For example, a toolbar button that prints
// a document has a default action of "Press" rather than "Prints the current document."
wxAccStatus TrackPanelAx::GetDefaultAction( int childId, wxString *actionName )
{
   actionName->Clear();

   return wxACC_OK;
}

// Returns the description for this object or a child.
wxAccStatus TrackPanelAx::GetDescription( int childId, wxString *description )
{
   description->Clear();

   return wxACC_OK;
}

// Returns help text for this object or a child, similar to tooltip text.
wxAccStatus TrackPanelAx::GetHelpText( int childId, wxString *helpText )
{
   helpText->Clear();

   return wxACC_OK;
}

// Returns the keyboard shortcut for this object or child.
// Return e.g. ALT+K
wxAccStatus TrackPanelAx::GetKeyboardShortcut( int childId, wxString *shortcut )
{
   shortcut->Clear();

   return wxACC_OK;
}

// Returns the rectangle for this object (id = 0) or a child element (id > 0).
// rect is in screen coordinates.
wxAccStatus TrackPanelAx::GetLocation( wxRect& rect, int elementId )
{
   wxRect client;

   if( elementId == wxACC_SELF )
   {
      rect = mTrackPanel->GetRect();
   }
   else
   {
      Track *t = FindTrack( elementId );

      if( t == NULL )
      {
         return wxACC_FAIL;
      }

      rect = mTrackPanel->FindTrackRect( t, true );
   }

   rect.SetPosition( mTrackPanel->GetParent()->ClientToScreen( rect.GetPosition() ) );

   return wxACC_OK;
}

// Gets the name of the specified object.
wxAccStatus TrackPanelAx::GetName( int childId, wxString* name )
{
#if defined(__WXMSW__)
   if( childId == wxACC_SELF )
   {
      *name = _( "TrackView" );
   }
   else
   {
      Track *t = FindTrack( childId );

      if( t == NULL )
      {
         return wxACC_FAIL;
      }
      else
      {
         *name = t->GetName();
         if( *name == t->GetDefaultName() )
         {
            /* i18n-hint: The %d is replaced by th enumber of the track.*/
            name->Printf(_("Track %d"), TrackNum( t ) );
         }

         // LLL: Remove these during "refactor"
         if( t->GetMute() )
         {
            *name->Append( _( " Mute On" ) );
         }
         
         if( t->GetSolo() )
         {
            *name->Append( _( " Solo On" ) );
         }
         if( t->GetSelected() )
         {
            *name->Append( _( " Select On" ) );
         }
      }
   }

   return wxACC_OK;
#endif

#if defined(__WXMAC__)
   return wxACC_NOT_IMPLEMENTED;
#endif
}

// Returns a role constant.
wxAccStatus TrackPanelAx::GetRole( int childId, wxAccRole* role )
{
#if defined(__WXMSW__)
   if( childId == wxACC_SELF )
   {
      *role = wxROLE_SYSTEM_TABLE;
   }
   else
   {
      *role = wxROLE_SYSTEM_ROW;
   }
#endif

#if defined(__WXMAC__)
   if( childId == wxACC_SELF )
   {
      *role = wxROLE_SYSTEM_PANE;
   }
   else
   {
      *role = wxROLE_SYSTEM_STATICTEXT;
   }
#endif

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
wxAccStatus TrackPanelAx::GetSelections( wxVariant *selections )
{
   return wxACC_NOT_IMPLEMENTED;
}

// Returns a state constant.
wxAccStatus TrackPanelAx::GetState( int childId, long* state )
{
  *state = wxACC_STATE_SYSTEM_FOCUSABLE | wxACC_STATE_SYSTEM_SELECTABLE;

   if( childId > 0 )
   {
      Track *t = FindTrack( childId );

      if (t)
      {
         if( t == mFocusedTrack )
         {
            *state |= wxACC_STATE_SYSTEM_FOCUSED;
         }

         if( t->GetSelected() )
         {
            *state |= wxACC_STATE_SYSTEM_SELECTED;
         }
      }
   }

   return wxACC_OK;
}

// Returns a localized string representing the value for the object
// or child.
wxAccStatus TrackPanelAx::GetValue( int childId, wxString* strValue )
{
#if defined(__WXMSW__)
   return wxACC_NOT_IMPLEMENTED;
#endif

#if defined(__WXMAC__)
   if( childId == wxACC_SELF )
   {
      *strValue = _( "TrackView" );
   }
   else
   {
      Track *t = FindTrack( childId );

      if( t == NULL )
      {
         return wxACC_FAIL;
      }
      else
      {
         *strValue = t->GetName();
         if( *strValue == t->GetDefaultName() )
         {
            strValue->Printf(_("Track %d"), TrackNum( t ) );
         }

         // LLL: Remove these during "refactor"
         if( t->GetMute() )
         {
            strValue->Append( _( " Mute On" ) );
         }
         
         if( t->GetSolo() )
         {
            strValue->Append( _( " Solo On" ) );
         }
         if( t->GetSelected() )
         {
            strValue->Append( _( " Select On" ) );
         }
      }
   }
   return wxACC_OK;
#endif
}

// Gets the window with the keyboard focus.
// If childId is 0 and child is NULL, no object in
// this subhierarchy has the focus.
// If this object has the focus, child should be 'this'.
wxAccStatus TrackPanelAx::GetFocus( int *childId, wxAccessible **child )
{
#if defined(__WXMSW__)
   if( *childId == wxACC_SELF )
   {
      if( mFocusedTrack )
      {
         *child = this;
      }
   }
   else
   {
      *child = NULL;
   }

   return wxACC_OK;
#endif

#if defined(__WXMAC__)
   if( GetWindow() == wxWindow::FindFocus() )
   {
      if( mFocusedTrack )
      {
         *childId = TrackNum( mFocusedTrack );
      }
      else
      {
         *childId = wxACC_SELF;
}

      return wxACC_OK;
   }

   return wxACC_NOT_IMPLEMENTED;
#endif
}

#endif // wxUSE_ACCESSIBILITY

// Indentation settings for Vim and Emacs.
// Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
