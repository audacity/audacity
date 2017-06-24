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

LabelDefaultClickHandle &LabelDefaultClickHandle::Instance()
{
   static LabelDefaultClickHandle instance;
   return instance;
}

LabelDefaultClickHandle::~LabelDefaultClickHandle()
{
}

struct LabelDefaultClickHandle::LabelState {
   std::vector< std::pair< std::weak_ptr<LabelTrack>, LabelTrack::Flags > > mPairs;
};

void LabelDefaultClickHandle::SaveState( AudacityProject *pProject )
{
   mLabelState = std::make_unique<LabelState>();
   auto &pairs = mLabelState->mPairs;
   TrackList *const tracks = pProject->GetTracks();
   TrackListIterator iter(tracks);
   Track *n = iter.First();

   while (n) {
      if (n->GetKind() == Track::Label) {
         LabelTrack *const lt = static_cast<LabelTrack*>(n);
         pairs.push_back( std::make_pair(
            Track::Pointer<LabelTrack>( lt ),
            lt->SaveFlags() )
         );
      }
      n = iter.Next();
   }
}

void LabelDefaultClickHandle::RestoreState( AudacityProject *pProject )
{
   if ( mLabelState ) {
      for ( const auto &pair : mLabelState->mPairs )
         if (auto pLt = pair.first.lock())
            pLt->RestoreFlags( pair.second );
      mLabelState.reset();
   }
}

void LabelDefaultClickHandle::DoClick
(const wxMouseEvent &event, AudacityProject *pProject, TrackPanelCell *pCell)
{
   LabelTrack *pLT = static_cast<LabelTrack*>(pCell);

   if (event.LeftDown())
   {
      SaveState( pProject );

      TrackList *const tracks = pProject->GetTracks();
      TrackListIterator iter(tracks);
      Track *n = iter.First();

      while (n) {
         if (n->GetKind() == Track::Label && pCell != n) {
            LabelTrack *const lt = static_cast<LabelTrack*>(n);
            lt->ResetFlags();
            lt->Unselect();
         }
         n = iter.Next();
      }
   }
}

UIHandle::Result LabelDefaultClickHandle::Click
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   using namespace RefreshCode;
   // Redraw to show the change of text box selection status
   UIHandle::Result result = RefreshAll;

   DoClick(evt.event, pProject, evt.pCell);

   if (mpForward)
      result |= mpForward->Click(evt, pProject);
   else
      // No drag or release follows
      result |= Cancelled;

   return result;
}

UIHandle::Result LabelDefaultClickHandle::Drag
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   if (mpForward)
      return mpForward->Drag(evt, pProject);
   else
      return RefreshCode::RefreshNone;
}

HitTestPreview LabelDefaultClickHandle::Preview
(const TrackPanelMouseEvent &evt, const AudacityProject *pProject)
{
   if (mpForward)
      return mpForward->Preview(evt, pProject);
   else
      return {};
}

UIHandle::Result LabelDefaultClickHandle::Release
(const TrackPanelMouseEvent &evt, AudacityProject *pProject,
 wxWindow *pParent)
{
   mLabelState.reset();
   if (mpForward)
      return mpForward->Release(evt, pProject, pParent);
   else
      return RefreshCode::RefreshNone;
}

UIHandle::Result LabelDefaultClickHandle::Cancel(AudacityProject *pProject)
{
   UIHandle::Result result = RefreshCode::RefreshNone;
   if (mpForward)
      result |= mpForward->Cancel(pProject);
   RestoreState( pProject );
   return result;
}

void LabelDefaultClickHandle::DrawExtras
(DrawingPass pass,
 wxDC * dc, const wxRegion &updateRegion, const wxRect &panelRect)
{
   UIHandle::DrawExtras(pass, dc, updateRegion, panelRect);
   if (mpForward)
      mpForward->DrawExtras(pass, dc, updateRegion, panelRect);
}

bool LabelDefaultClickHandle::StopsOnKeystroke()
{
   return
      (mpForward && mpForward->StopsOnKeystroke()) ||
      UIHandle::StopsOnKeystroke();
}

void LabelDefaultClickHandle::OnProjectChange(AudacityProject *pProject)
{
   if (mpForward)
      return mpForward->OnProjectChange(pProject);
   UIHandle::OnProjectChange(pProject);
}
