/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackPanelAx.cpp

  Leland Lucius
  and lots of other contributors

******************************************************************//*!

\class TrackPanelAx
\brief Helper to TrackPanel to give accessibility.

*//*******************************************************************/

#include "Audacity.h"
#include "TrackPanelAx.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifndef WX_PRECOMP
// Include your minimal set of headers here, or wx.h
#include <wx/wx.h>
#endif


#include <wx/intl.h>

#include "Track.h"
#include "Internat.h"

TrackPanelAx::TrackPanelAx( wxWindow *window )
#if wxUSE_ACCESSIBILITY
   :wxWindowAccessible( window )
#endif
{
   mTrackPanel = wxDynamicCast( window, TrackPanel );

   mTrackName = true;
   mMessageCount = 0;
}

TrackPanelAx::~TrackPanelAx()
{
}

// Returns currently focused track or first one if none focused
std::shared_ptr<Track> TrackPanelAx::GetFocus()
{
   auto focusedTrack = mFocusedTrack.lock();
   if( !focusedTrack ) {
      TrackListIterator iter( mTrackPanel->GetTracks() );
      focusedTrack = Track::Pointer( iter.First() );
      // only call SetFocus if the focus has changed to avoid
      // unnecessary focus events
      if (focusedTrack) 
         focusedTrack = SetFocus();
   }

   if( !TrackNum( focusedTrack ) )
   {
      mFocusedTrack.reset();
      return {};
   }

   return( focusedTrack );
}

// Changes focus to a specified track
std::shared_ptr<Track> TrackPanelAx::SetFocus( std::shared_ptr<Track> track )
{
   mTrackName = true;

#if wxUSE_ACCESSIBILITY

   auto focusedTrack = mFocusedTrack.lock();
   if( focusedTrack && !focusedTrack->GetSelected() )
   {
      NotifyEvent( wxACC_EVENT_OBJECT_SELECTIONREMOVE,
                   mTrackPanel,
                   wxOBJID_CLIENT,
                   TrackNum( focusedTrack ) );
   }
#endif

   if( !track )
   {
      TrackListIterator iter( mTrackPanel->GetTracks() );
      track = Track::Pointer( iter.First() );
   }

   mFocusedTrack = track;

#if wxUSE_ACCESSIBILITY
   if( track )
   {
      int num = TrackNum( track );

      NotifyEvent( wxACC_EVENT_OBJECT_FOCUS,
                   mTrackPanel,
                   wxOBJID_CLIENT,
                   num );

      if( track->GetSelected() )
      {
         NotifyEvent( wxACC_EVENT_OBJECT_SELECTION,
                      mTrackPanel,
                      wxOBJID_CLIENT,
                      num );
      }
   }
   else
   {
      NotifyEvent(wxACC_EVENT_OBJECT_FOCUS,
         mTrackPanel,
         wxOBJID_CLIENT,
         wxACC_SELF);
   }

#endif

   return track;
}

// Returns TRUE if passed track has the focus
bool TrackPanelAx::IsFocused( Track *track )
{
   auto focusedTrack = mFocusedTrack.lock();
   if( !focusedTrack )
      focusedTrack = SetFocus();

   // Remap track pointer if there are oustanding pending updates
   auto origTrack =
      mTrackPanel->GetTracks()->FindById( track->GetId() );
   if (origTrack)
      track = origTrack;

   if( ( track == focusedTrack.get() ) ||
       ( focusedTrack && track == focusedTrack->GetLink() ) )
   {
      return true;
   }

   return false;
}

int TrackPanelAx::TrackNum( const std::shared_ptr<Track> &target )
{
   // Find 1-based position of the target in the visible tracks, or 0 if not
   // found
   TrackListIterator iter( mTrackPanel->GetTracks() );
   Track *t = iter.First();
   int ndx = 0;

   while( t != NULL )
   {
      ndx++;
      if( t == target.get() )
      {
         return ndx;
      }

      t = iter.Next( true );
   }

   return 0;
}

std::shared_ptr<Track> TrackPanelAx::FindTrack( int num )
{
   TrackListIterator iter( mTrackPanel->GetTracks() );
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

   return Track::Pointer( t );
}

void TrackPanelAx::Updated()
{
#if wxUSE_ACCESSIBILITY
   auto t = GetFocus();
   mTrackName = true;

   // logically, this should be an OBJECT_NAMECHANGE event, but Window eyes 9.1
   // does not read out the name with this event type, hence use OBJECT_FOCUS.
   NotifyEvent(wxACC_EVENT_OBJECT_FOCUS,
               mTrackPanel,
               wxOBJID_CLIENT,
               TrackNum(t));
#endif
}

void TrackPanelAx::MessageForScreenReader(const wxString& message)
{
#if wxUSE_ACCESSIBILITY
   if (mTrackPanel == wxWindow::FindFocus())
   {
      auto t = GetFocus();
      int childId = t ? TrackNum(t) : 0;

      mMessage = message;

      // append \a alernatively, so that the string is never the same as the previous string.
      // This ensures that screen readers read it.
      if (mMessageCount % 2 == 0)
         mMessage.Append('\a');
      mMessageCount++;

      mTrackName = false;
      NotifyEvent(wxACC_EVENT_OBJECT_FOCUS,
               mTrackPanel,
               wxOBJID_CLIENT,
               childId);
   }

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
   TrackListIterator iter( mTrackPanel->GetTracks() );
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
wxAccStatus TrackPanelAx::GetDefaultAction( int WXUNUSED(childId), wxString *actionName )
{
   actionName->Clear();

   return wxACC_OK;
}

// Returns the description for this object or a child.
wxAccStatus TrackPanelAx::GetDescription( int WXUNUSED(childId), wxString *description )
{
   description->Clear();

   return wxACC_OK;
}

// Returns help text for this object or a child, similar to tooltip text.
wxAccStatus TrackPanelAx::GetHelpText( int WXUNUSED(childId), wxString *helpText )
{
   helpText->Clear();

   return wxACC_OK;
}

// Returns the keyboard shortcut for this object or child.
// Return e.g. ALT+K
wxAccStatus TrackPanelAx::GetKeyboardShortcut( int WXUNUSED(childId), wxString *shortcut )
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
      auto t = FindTrack( elementId );

      if( t == NULL )
      {
         return wxACC_FAIL;
      }

      rect = mTrackPanel->FindTrackRect( t.get(), false );
      // Inflate the screen reader's rectangle so it overpaints Audacity's own
      // yellow focus rectangle.
#ifdef __WXMAC__
      const int dx = 2;
#else
      const int dx = 1;
#endif
      rect.Inflate(dx, dx);
   }

   rect.SetPosition( mTrackPanel->GetParent()->ClientToScreen( rect.GetPosition() ) );

   return wxACC_OK;
}

// Gets the name of the specified object.
wxAccStatus TrackPanelAx::GetName( int childId, wxString* name )
{
#if defined(__WXMSW__) || defined(__WXMAC__)
   if (mTrackName)
   {
      if( childId == wxACC_SELF )
      {
         *name = _( "TrackView" );
      }
      else
      {
         auto t = FindTrack( childId );

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

            if (t->GetKind() == Track::Label)
            {
               /* i18n-hint: This is for screen reader software and indicates that
                  this is a Label track.*/
               name->Append( wxT(" ") + wxString(_("Label Track")));
            }
            else if (t->GetKind() == Track::Time)
            {
               /* i18n-hint: This is for screen reader software and indicates that
                  this is a Time track.*/
               name->Append( wxT(" ") + wxString(_("Time Track")));
            }
#ifdef USE_MIDI
            else if (t->GetKind() == Track::Note)
            {
               /* i18n-hint: This is for screen reader software and indicates that
                  this is a Note track.*/
               name->Append( wxT(" ") + wxString(_("Note Track")));
            }
#endif

            // LLL: Remove these during "refactor"
            auto pt = dynamic_cast<PlayableTrack *>(t.get());
            if( pt && pt->GetMute() )
            {
               // The following comment also applies to the solo, selected,
               // and synclockselected states.
               // Many of translations of the strings with a leading space omitted
               // the leading space. Therefore a space has been added using wxT(" ").
               // Because screen readers won't be affected by multiple spaces, the
               // leading spaces have not been removed, so that no NEW translations are needed.
               /* i18n-hint: This is for screen reader software and indicates that
                  on this track mute is on.*/
               name->Append( wxT(" ") + wxString(_( " Mute On" )) );
            }

            if( pt && pt->GetSolo() )
            {
               /* i18n-hint: This is for screen reader software and indicates that
                  on this track solo is on.*/
               name->Append( wxT(" ") + wxString(_( " Solo On" )) );
            }
            if( t->GetSelected() )
            {
               /* i18n-hint: This is for screen reader software and indicates that
                  this track is selected.*/
               name->Append( wxT(" ") + wxString(_( " Select On" )) );
            }
            if( t->IsSyncLockSelected() )
            {
               /* i18n-hint: This is for screen reader software and indicates that
                  this track is shown with a sync-locked icon.*/
               // The absence of a dash between Sync and Locked is deliberate -
               // if present, Jaws reads it as "dash".
               name->Append( wxT(" ") + wxString(_( " Sync Lock Selected" )) );
            }
         }
      }
   }
   else
   {
      *name = mMessage;
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
   if (mTrackName)
   {
      if( childId == wxACC_SELF )
      {
         *role = wxROLE_SYSTEM_TABLE;
      }
      else
      {
         *role = wxROLE_SYSTEM_ROW;
      }
   }
   else
   {
      *role = wxROLE_NONE;
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
wxAccStatus TrackPanelAx::GetSelections( wxVariant * WXUNUSED(selections) )
{
   return wxACC_NOT_IMPLEMENTED;
}

// Returns a state constant.
wxAccStatus TrackPanelAx::GetState( int childId, long* state )
{
#if defined(__WXMSW__)
   if( childId > 0 )
   {
      auto t = FindTrack( childId );

      *state = wxACC_STATE_SYSTEM_FOCUSABLE | wxACC_STATE_SYSTEM_SELECTABLE;
      if (t)
      {
         if( t == mFocusedTrack.lock() )
         {
            *state |= wxACC_STATE_SYSTEM_FOCUSED;
         }

         if( t->GetSelected() && mTrackName )
         {
            *state |= wxACC_STATE_SYSTEM_SELECTED;
         }
      }
   }
   else     // childId == wxACC_SELF
   {
      *state = wxACC_STATE_SYSTEM_FOCUSABLE + wxACC_STATE_SYSTEM_FOCUSED;
   }
#endif

#if defined(__WXMAC__)
   *state = wxACC_STATE_SYSTEM_FOCUSABLE | wxACC_STATE_SYSTEM_SELECTABLE;

   if( childId > 0 )
   {
      auto t = FindTrack( childId );

      if (t)
      {
         if( t == mFocusedTrack.lock() )
         {
            *state |= wxACC_STATE_SYSTEM_FOCUSED;
         }

         if( t->GetSelected() )
         {
            *state |= wxACC_STATE_SYSTEM_SELECTED;
         }
      }
   }
#endif

   return wxACC_OK;
}

// Returns a localized string representing the value for the object
// or child.
#if defined(__WXMAC__)
wxAccStatus TrackPanelAx::GetValue( int childId, wxString* strValue )
#else
wxAccStatus TrackPanelAx::GetValue( int WXUNUSED(childId), wxString* WXUNUSED(strValue) )
#endif
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
      auto t = FindTrack( childId );

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
         auto pt = dynamic_cast<PlayableTrack *>(t.get());
         if( pt && pt->GetMute() )
         {
            strValue->Append( _( " Mute On" ) );
         }

         if( pt && pt->GetSolo() )
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

   if (mTrackPanel == wxWindow::FindFocus())
   {
      auto focusedTrack = mFocusedTrack.lock();
      if (focusedTrack)
      {
         *childId = TrackNum(focusedTrack);
      }
      else
      {
         *child = this;
      }
   }

   return wxACC_OK;
#endif

#if defined(__WXMAC__)
   if( GetWindow() == wxWindow::FindFocus() )
   {
      auto focusedTrack = mFocusedTrack.lock();
      if( focusedTrack )
      {
         *childId = TrackNum( focusedTrack );
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

// Navigates from fromId to toId/toObject
wxAccStatus TrackPanelAx::Navigate(wxNavDir navDir, int fromId, int* toId, wxAccessible** toObject)
{
   int childCount;
   GetChildCount( &childCount );

   if (fromId > childCount)
      return wxACC_FAIL;

   switch (navDir) {
   case wxNAVDIR_FIRSTCHILD:
      if (fromId == CHILDID_SELF && childCount > 0 )
         *toId = 1;
      else
         return wxACC_FALSE;
      break;

   case wxNAVDIR_LASTCHILD:
      if (fromId == CHILDID_SELF && childCount > 0 )
         *toId = childCount;
      else
         return wxACC_FALSE;
      break;

   case wxNAVDIR_NEXT:
   case wxNAVDIR_DOWN:
      if (fromId != CHILDID_SELF) {
         *toId = fromId + 1;
         if (*toId > childCount)
            return wxACC_FALSE;
      }
      else
         return wxACC_NOT_IMPLEMENTED;
      break;

   case wxNAVDIR_PREVIOUS:
   case wxNAVDIR_UP:
      if (fromId != CHILDID_SELF) {
         *toId = fromId - 1;
         if (*toId < 1)
            return wxACC_FALSE;
      }
      else
         return wxACC_NOT_IMPLEMENTED;
      break;

   case wxNAVDIR_LEFT:
   case wxNAVDIR_RIGHT:
      if (fromId != CHILDID_SELF)
         return wxACC_FALSE;
      else
         return wxACC_NOT_IMPLEMENTED;
      break;
   }

   *toObject = nullptr;
   return wxACC_OK;
}

#endif // wxUSE_ACCESSIBILITY
