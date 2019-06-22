/**********************************************************************

Audacity: A Digital Audio Editor

CommonTrackPanelCell.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "CommonTrackPanelCell.h"

#include <wx/cursor.h>

#include "../../HitTestResult.h"
#include "../../RefreshCode.h"
#include "../../TrackPanelMouseEvent.h"

namespace {
   CommonTrackPanelCell::Hook &GetHook()
   {
      static CommonTrackPanelCell::Hook theHook;
      return theHook;
   }
}

auto CommonTrackPanelCell::InstallMouseWheelHook( const Hook &hook )
   -> Hook
{
   auto &theHook = GetHook();
   auto result = theHook;
   theHook = hook;
   return result;
}

CommonTrackPanelCell::~CommonTrackPanelCell()
{
}

HitTestPreview CommonTrackPanelCell::DefaultPreview
(const TrackPanelMouseState &, const AudacityProject *)
{
   static wxCursor defaultCursor{ wxCURSOR_ARROW };
   return { {}, &defaultCursor, {} };
}

unsigned CommonTrackPanelCell::HandleWheelRotation
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   auto hook = GetHook();
   return hook ? hook( evt, pProject ) : RefreshCode::Cancelled;
}

CommonTrackCell::CommonTrackCell( const std::shared_ptr< Track > &parent )
   : mwTrack { parent }
{}

CommonTrackCell::~CommonTrackCell() = default;

void CommonTrackCell::CopyTo( Track& ) const
{
}

void CommonTrackCell::Reparent( const std::shared_ptr<Track> &parent )
{
   mwTrack = parent;
}

std::shared_ptr<Track> CommonTrackCell::DoFindTrack()
{
   return mwTrack.lock();
}

void CommonTrackCell::WriteXMLAttributes( XMLWriter & ) const
{
}

bool CommonTrackCell::HandleXMLAttribute( const wxChar *, const wxChar * )
{
   return false;
}
