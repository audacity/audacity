/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackPanelAx.cpp

  Leland Lucius
  and lots of other contributors

******************************************************************//*!

\class TrackPanelAx
\brief Helper to TrackPanel to give accessibility.

*//*******************************************************************/

#include "Audacity.h" // for USE_* macros
#include "TrackPanelAx.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#include <wx/setup.h> // for wxUSE_* macros

#ifndef WX_PRECOMP
// Include your minimal set of headers here, or wx.h
#include <wx/wx.h>
#endif


#include <wx/intl.h>

#include "Project.h"
#include "Track.h"


wxDEFINE_EVENT(EVT_TRACK_FOCUS_CHANGE, wxCommandEvent);

TrackPanelAx::TrackPanelAx( AudacityProject &project )
   :
#if wxUSE_ACCESSIBILITY
     WindowAccessible( nullptr ) // window pointer must be set after construction
   ,
#endif
     mProject{ project }
{
   mTrackName = true;
   mMessageCount = 0;
   mNumFocusedTrack = 0;
}

TrackPanelAx::~TrackPanelAx()
{
}

TrackList &TrackPanelAx::GetTracks()
{
   return TrackList::Get( mProject );
}

// Returns currently focused track
// if that track no longer exists, if there is a track at
// the same position, use that, else if there is a first
// track, use that.
std::shared_ptr<Track> TrackPanelAx::GetFocus()
{
   auto focusedTrack = mFocusedTrack.lock();
   if( !focusedTrack ) {
      if (mNumFocusedTrack >=1) {
         // This prevents the focus from being unnecessarily set to track 1
         // when effects are applied. (Applying an effect can change
         // the pointers of the selected tracks.)
         focusedTrack = FindTrack(mNumFocusedTrack);
      }
      if (!focusedTrack) {
         focusedTrack =
            Track::SharedPointer( *GetTracks().Any().first );
         // only call SetFocus if the focus has changed to avoid
         // unnecessary focus events
         if (focusedTrack) 
            focusedTrack = SetFocus();
      }
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
                   GetWindow(),
                   wxOBJID_CLIENT,
                   TrackNum( focusedTrack ) );
   }
#endif

   if( !track )
      track = Track::SharedPointer( *GetTracks().Any().begin() );

   if ( mFocusedTrack.lock() != track ) {
      mFocusedTrack = track;
      mProject.QueueEvent( safenew wxCommandEvent{ EVT_TRACK_FOCUS_CHANGE } );
   }
   mNumFocusedTrack = TrackNum(track);

#if wxUSE_ACCESSIBILITY
   if( track )
   {
      if (GetWindow() == wxWindow::FindFocus())
      {
         NotifyEvent( wxACC_EVENT_OBJECT_FOCUS,
                      GetWindow(),
                      wxOBJID_CLIENT,
                      mNumFocusedTrack );
      }

      if( track->GetSelected() )
      {
         NotifyEvent( wxACC_EVENT_OBJECT_SELECTION,
                      GetWindow(),
                      wxOBJID_CLIENT,
                      mNumFocusedTrack );
      }
   }
   else
   {
      NotifyEvent(wxACC_EVENT_OBJECT_FOCUS,
         GetWindow(),
         wxOBJID_CLIENT,
         wxACC_SELF);
   }

#endif

   return track;
}

// Returns TRUE if passed track has the focus
bool TrackPanelAx::IsFocused( const Track *track )
{
   auto focusedTrack = mFocusedTrack.lock();
   if( !focusedTrack )
      focusedTrack = SetFocus();

   // Remap track pointer if there are outstanding pending updates
   auto origTrack =
      GetTracks().FindById( track->GetId() );
   if (origTrack)
      track = origTrack;

   return focusedTrack
      ? TrackList::Channels(focusedTrack.get()).contains(track)
      : !track;
}

int TrackPanelAx::TrackNum( const std::shared_ptr<Track> &target )
{
   // Find 1-based position of the target in the visible tracks, or 0 if not
   // found
   int ndx = 0;

   for ( auto t : GetTracks().Leaders() )
   {
      ndx++;
      if( t == target.get() )
      {
         return ndx;
      }
   }

   return 0;
}

std::shared_ptr<Track> TrackPanelAx::FindTrack( int num )
{
   int ndx = 0;

   for ( auto t : GetTracks().Leaders() )
   {
      ndx++;
      if( ndx == num )
         return t->SharedPointer();
   }

   return {};
}

void TrackPanelAx::Updated()
{
#if wxUSE_ACCESSIBILITY
   auto t = GetFocus();
   mTrackName = true;

   // The object_focus event is only needed by Window-Eyes
   // and can be removed when we cease to support this screen reader.
   NotifyEvent(wxACC_EVENT_OBJECT_FOCUS,
               GetWindow(),
               wxOBJID_CLIENT,
               TrackNum(t));

   NotifyEvent(wxACC_EVENT_OBJECT_NAMECHANGE,
      GetWindow(),
      wxOBJID_CLIENT,
      TrackNum(t));
#endif
}

void TrackPanelAx::MessageForScreenReader(const TranslatableString& message)
{
#if wxUSE_ACCESSIBILITY
   if (GetWindow() == wxWindow::FindFocus())
   {
      auto t = GetFocus();
      int childId = t ? TrackNum(t) : 0;

      mMessage = message.Translation();

      // append \a alternatively, so that the string is never the same as the previous string.
      // This ensures that screen readers read it.
      if (mMessageCount % 2 == 0)
         mMessage.Append('\a');
      mMessageCount++;

      mTrackName = false;
      NotifyEvent(wxACC_EVENT_OBJECT_NAMECHANGE,
               GetWindow(),
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
   *childCount = GetTracks().Leaders().size();
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
   actionName->clear();

   return wxACC_OK;
}

// Returns the description for this object or a child.
wxAccStatus TrackPanelAx::GetDescription( int WXUNUSED(childId), wxString *description )
{
   description->clear();

   return wxACC_OK;
}

// Returns help text for this object or a child, similar to tooltip text.
wxAccStatus TrackPanelAx::GetHelpText( int WXUNUSED(childId), wxString *helpText )
{
   helpText->clear();

   return wxACC_OK;
}

// Returns the keyboard shortcut for this object or child.
// Return e.g. ALT+K
wxAccStatus TrackPanelAx::GetKeyboardShortcut( int WXUNUSED(childId), wxString *shortcut )
{
   shortcut->clear();

   return wxACC_OK;
}

// Returns the rectangle for this object (id = 0) or a child element (id > 0).
// rect is in screen coordinates.
wxAccStatus TrackPanelAx::GetLocation( wxRect& rect, int elementId )
{
   wxRect client;

   if( elementId == wxACC_SELF )
   {
      rect = GetWindow()->GetRect();
   }
   else
   {
      auto t = FindTrack( elementId );

      if( t == NULL )
      {
         return wxACC_FAIL;
      }

      rect = mFinder ? mFinder( *t ) : wxRect{};
      // Inflate the screen reader's rectangle so it overpaints Audacity's own
      // yellow focus rectangle.
#ifdef __WXMAC__
      const int dx = 2;
#else
      const int dx = 1;
#endif
      rect.Inflate(dx, dx);
   }

   rect.SetPosition( GetWindow()->GetParent()->ClientToScreen( rect.GetPosition() ) );

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
               /* i18n-hint: The %d is replaced by the number of the track.*/
               name->Printf(_("Track %d"), TrackNum( t ) );
            }

            t->TypeSwitch(
               [&](const LabelTrack *) {
                  /* i18n-hint: This is for screen reader software and indicates that
                     this is a Label track.*/
                  name->Append( wxT(" ") + wxString(_("Label Track")));
               },
               [&](const TimeTrack *) {
                  /* i18n-hint: This is for screen reader software and indicates that
                     this is a Time track.*/
                  name->Append( wxT(" ") + wxString(_("Time Track")));
               }
#ifdef USE_MIDI
                ,
               [&](const NoteTrack *) {
                  /* i18n-hint: This is for screen reader software and indicates that
                     this is a Note track.*/
                  name->Append( wxT(" ") + wxString(_("Note Track")));
               }
#endif
            );

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
                  this track is muted. (The mute button is on.)*/
               name->Append( wxT(" ") + wxString(_( " Muted" )) );
            }

            if( pt && pt->GetSolo() )
            {
               /* i18n-hint: This is for screen reader software and indicates that
                  this track is soloed. (The Solo button is on.)*/
               name->Append( wxT(" ") + wxString(_( " Soloed" )) );
            }
            if( t->GetSelected() )
            {
               /* i18n-hint: This is for screen reader software and indicates that
                  this track is selected.*/
               name->Append( wxT(" ") + wxString(_( " Selected" )) );
            }
            if( t->IsSyncLockSelected() )
            {
               /* i18n-hint: This is for screen reader software and indicates that
                  this track is shown with a sync-locked icon.*/
               // The absence of a dash between Sync and Locked is deliberate -
               // if present, Jaws reads it as "dash".
               name->Append( wxT(" ") + wxString(_( " Sync Locked" )) );
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

   if (GetWindow() == wxWindow::FindFocus())
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
      if (fromId == wxACC_SELF && childCount > 0 )
         *toId = 1;
      else
         return wxACC_FALSE;
      break;

   case wxNAVDIR_LASTCHILD:
      if (fromId == wxACC_SELF && childCount > 0 )
         *toId = childCount;
      else
         return wxACC_FALSE;
      break;

   case wxNAVDIR_NEXT:
   case wxNAVDIR_DOWN:
      if (fromId != wxACC_SELF) {
         *toId = fromId + 1;
         if (*toId > childCount)
            return wxACC_FALSE;
      }
      else
         return wxACC_NOT_IMPLEMENTED;
      break;

   case wxNAVDIR_PREVIOUS:
   case wxNAVDIR_UP:
      if (fromId != wxACC_SELF) {
         *toId = fromId - 1;
         if (*toId < 1)
            return wxACC_FALSE;
      }
      else
         return wxACC_NOT_IMPLEMENTED;
      break;

   case wxNAVDIR_LEFT:
   case wxNAVDIR_RIGHT:
      if (fromId != wxACC_SELF)
         return wxACC_FALSE;
      else
         return wxACC_NOT_IMPLEMENTED;
      break;
   }

   *toObject = nullptr;
   return wxACC_OK;
}

// Modify focus or selection
wxAccStatus TrackPanelAx::Select(int childId, wxAccSelectionFlags selectFlags)
{
   // Only support change of focus
   if (selectFlags != wxACC_SEL_TAKEFOCUS)
      return wxACC_NOT_IMPLEMENTED;
   
   if (childId != wxACC_SELF) {
      int childCount;
      GetChildCount( &childCount );
      if (childId > childCount)
           return wxACC_FAIL;

      Track* t = FindTrack(childId).get();
      if (t) {
         SetFocus( t->SharedPointer() );
         t->EnsureVisible();
      }
   }
   else
      return wxACC_NOT_IMPLEMENTED;

   return wxACC_OK;
}

#endif // wxUSE_ACCESSIBILITY

static const AudacityProject::AttachedObjects::RegisteredFactory key{
   []( AudacityProject &parent ){
      return std::make_shared< TrackFocus >( parent );
   }
};

TrackFocus &TrackFocus::Get( AudacityProject &project )
{
   return project.AttachedObjects::Get< TrackFocus >( key );
}

const TrackFocus &TrackFocus::Get( const AudacityProject &project )
{
   return Get( const_cast< AudacityProject & >( project ) );
}

TrackFocus::TrackFocus( AudacityProject &project )
   : mProject{ project }
{
}

TrackFocus::~TrackFocus()
{
}

Track *TrackFocus::Get()
{
   if (mAx)
      return mAx->GetFocus().get();
   return nullptr;
}

void TrackFocus::Set( Track *pTrack )
{
   if (mAx) {
      pTrack = *TrackList::Get( mProject ).FindLeader( pTrack );
      mAx->SetFocus( Track::SharedPointer( pTrack ) );
   }
}

bool TrackFocus::IsFocused( const Track *pTrack )
{
   if (mAx)
      return mAx->IsFocused( pTrack );
   return false;
}

void TrackFocus::SetAccessible(
   wxWindow &owner,
   std::unique_ptr< TrackPanelAx > pAx
)
{
#if wxUSE_ACCESSIBILITY
   // wxWidgets owns the accessible object
   owner.SetAccessible(mAx = pAx.release());
#else
   // wxWidgets does not own the object, but we need to retain it
   mAx = std::move(pAx);
#endif
}

void TrackFocus::MessageForScreenReader(const TranslatableString& message)
{
   if (mAx)
      mAx->MessageForScreenReader( message );
}

void TrackFocus::UpdateAccessibility()
{
   if (mAx)
      mAx->Updated();
}
