/**********************************************************************

  Audacity: A Digital Audio Editor

  Clipboard.cpp

*//*******************************************************************/

#include "Clipboard.h"
#include "Track.h"

wxDEFINE_EVENT( EVT_CLIPBOARD_CHANGE, wxCommandEvent);

Clipboard::Clipboard()
: mTracks { TrackList::Create( nullptr ) }
{
}

Clipboard::~Clipboard() = default;

Clipboard &Clipboard::Get()
{
   static Clipboard instance;
   return instance;
}

//static
const TrackList &Clipboard::GetTracks() const
{
   return *mTracks;
}

void Clipboard::Clear()
{
   mT0 = 0.0;
   mT1 = 0.0;
   mProject = nullptr;
   mTracks->Clear();

   // Emit an event for listeners
   AddPendingEvent( wxCommandEvent{ EVT_CLIPBOARD_CHANGE } );
}

void Clipboard::Assign( TrackList && newContents,
   double t0, double t1, AudacityProject *pProject )
{
   newContents.Swap( *mTracks );
   newContents.Clear();
   
   mT0 = t0;
   mT1 = t1;
   mProject = pProject;

   // Emit an event for listeners
   AddPendingEvent( wxCommandEvent{ EVT_CLIPBOARD_CHANGE } );
}
