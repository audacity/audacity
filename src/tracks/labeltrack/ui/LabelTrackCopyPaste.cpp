// Install cut-copy-paste hooks
#include "../../../EditUtilities.h"
#include "../../../LabelTrack.h"
#include "ProjectHistory.h"
#include "../../../ProjectWindow.h"
#include "../../../ProjectWindows.h"
#include "ViewInfo.h"
#include "LabelTrackView.h"

namespace {

struct LabelCopyPasteMethods final : CopyPasteMethods {

~LabelCopyPasteMethods() override = default;
   
// Handle text paste (into active label), if any. Return true if did paste.
// (This was formerly the first part of overly-long OnPaste.)
bool DoPaste(AudacityProject &project) override
{
   auto &tracks = TrackList::Get( project );
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;
   auto &window = ProjectWindow::Get( project );

   for (auto pLabelTrack : tracks.Any<LabelTrack>())
   {
      // Does this track have an active label?
      if (LabelTrackView::Get( *pLabelTrack ).GetTextEditIndex(project) != -1) {

         // Yes, so try pasting into it
         auto &view = LabelTrackView::Get( *pLabelTrack );
         if (view.PasteSelectedText( project, selectedRegion.t0(),
                                            selectedRegion.t1() ))
         {
            ProjectHistory::Get( project )
               .PushState(XO("Pasted text from the clipboard"), XO("Paste"));

            // Make sure caret is in view
            int x;
            if (view.CalcCursorX( project, &x )) {
               window.ScrollIntoView(x);
            }

            return true;
         }
      }
   }
   return false;
}
 
bool DoCut(AudacityProject &project) override
{
   // This doesn't handle cutting labels, it handles
   // cutting the _text_ inside of labels, i.e. if you're
   // in the middle of editing the label text and select "Cut".

   auto &trackPanel = GetProjectPanel(project);
   auto &tracks = TrackList::Get(project);
   for (auto lt : tracks.Selected< LabelTrack >()) {
      auto &view = LabelTrackView::Get( *lt );
      if (view.CutSelectedText( project )) {
         trackPanel.Refresh(false);
         return true;
      }
   }
   return false;
}

bool DoCopy(AudacityProject &project) override
{
   auto &tracks = TrackList::Get(project);
   for (auto lt : tracks.Selected< LabelTrack >()) {
      auto &view = LabelTrackView::Get( *lt );
      if (view.CopySelectedText( project )) {
         //trackPanel.Refresh(false);
         return true;
      }
   }
   return false;
}

bool Enable(const AudacityProject &project) override
{
   auto range = TrackList::Get( project ).Any<const LabelTrack>()
      + [&](const LabelTrack *pTrack){
         return LabelTrackView::Get( *pTrack ).IsTextSelected(
            // unhappy const_cast because track focus might be set
            const_cast<AudacityProject&>(project)
         );
      };
   return ( !range.empty() );
}

};

RegisterCopyPasteMethods regMethods{
   std::make_unique<LabelCopyPasteMethods>()
};

}
