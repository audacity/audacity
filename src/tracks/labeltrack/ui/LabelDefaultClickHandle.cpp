/**********************************************************************

Audacity: A Digital Audio Editor

LabelDefaultClickHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../Audacity.h"
#include "LabelDefaultClickHandle.h"

#include "../../../HitTestResult.h"
#include "../../../LabelTrack.h"
#include "../../../Project.h"
#include "../../../RefreshCode.h"
#include "../../../TrackPanelMouseEvent.h"
#include "../../../ViewInfo.h"

LabelDefaultClickHandle::LabelDefaultClickHandle()
{
}

LabelDefaultClickHandle::~LabelDefaultClickHandle()
{
}

struct LabelDefaultClickHandle::LabelState {
   std::vector< std::pair< std::weak_ptr<LabelTrack>, LabelTrack::Flags > > mPairs;
};

void LabelDefaultClickHandle::SaveState( AudacityProject *pProject )
{
   mLabelState = std::make_shared<LabelState>();
   auto &pairs = mLabelState->mPairs;
   TrackList *const tracks = pProject->GetTracks();

   for (auto lt : tracks->Any<LabelTrack>())
      pairs.push_back( std::make_pair(
         lt->SharedPointer<LabelTrack>(), lt->SaveFlags() ) );
}

void LabelDefaultClickHandle::RestoreState( AudacityProject *pProject )
{
   if ( mLabelState ) {
      for ( const auto &pair : mLabelState->mPairs )
         if (auto pLt = pProject->GetTracks()->Lock(pair.first))
            pLt->RestoreFlags( pair.second );
      mLabelState.reset();
   }
}

UIHandle::Result LabelDefaultClickHandle::Click
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   using namespace RefreshCode;
   // Redraw to show the change of text box selection status
   UIHandle::Result result = RefreshAll;

   if (evt.event.LeftDown())
   {
      SaveState( pProject );

      const auto pLT = evt.pCell.get();
      for (auto lt : pProject->GetTracks()->Any<LabelTrack>()) {
         if (pLT != lt) {
            lt->ResetFlags();
            lt->Unselect();
         }
      }
   }

   return result;
}

UIHandle::Result LabelDefaultClickHandle::Drag
(const TrackPanelMouseEvent &WXUNUSED(evt), AudacityProject *WXUNUSED(pProject))
{
   return RefreshCode::RefreshNone;
}

UIHandle::Result LabelDefaultClickHandle::Release
(const TrackPanelMouseEvent &WXUNUSED(evt), AudacityProject *WXUNUSED(pProject),
 wxWindow *WXUNUSED(pParent))
{
   mLabelState.reset();
   return RefreshCode::RefreshNone;
}

UIHandle::Result LabelDefaultClickHandle::Cancel(AudacityProject *pProject)
{
   UIHandle::Result result = RefreshCode::RefreshNone;
   RestoreState( pProject );
   return result;
}
