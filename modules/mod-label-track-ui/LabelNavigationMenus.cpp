#include "CommonCommandFlags.h"
#include "LabelTrack.h"
#include "ProjectAudioIO.h"
#include "ProjectAudioManager.h"
#include "ProjectWindow.h"
#include "TrackPanelAx.h"
#include "TransportUtilities.h"
#include "ViewInfo.h"
#include "commands/CommandContext.h"
#include "commands/CommandManager.h"

// Menu handler functions
namespace{

void DoMoveToLabel(AudacityProject &project, bool next)
{
   auto &tracks = TrackList::Get( project );
   auto &trackFocus = TrackFocus::Get( project );
   auto &window = ProjectWindow::Get( project );
   auto &projectAudioManager = ProjectAudioManager::Get(project);

   // Find the number of label tracks, and ptr to last track found
   auto trackRange = tracks.Any<LabelTrack>();
   auto lt = *trackRange.rbegin();
   auto nLabelTrack = trackRange.size();

   if (nLabelTrack == 0 ) {
      trackFocus.MessageForScreenReader(XO("no label track"));
   }
   else if (nLabelTrack > 1) {
      // find first label track, if any, starting at the focused track
      lt =
         *tracks.Find(trackFocus.Get()).Filter<LabelTrack>();
      if (!lt)
         trackFocus.MessageForScreenReader(
            XO("no label track at or below focused track"));
   }

   // If there is a single label track, or there is a label track at or below
   // the focused track
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;
   if (lt) {
      int i;
      if (next)
         i = lt->FindNextLabel(selectedRegion);
      else
         i = lt->FindPrevLabel(selectedRegion);

      if (i >= 0) {
         const LabelStruct* label = lt->GetLabel(i);
         bool newDefault = projectAudioManager.Looping();
         if (ProjectAudioIO::Get( project ).IsAudioActive()) {
            TransportUtilities::DoStopPlaying(project);
            selectedRegion = label->selectedRegion;
            window.RedrawProject();
            TransportUtilities::DoStartPlaying(project, newDefault);
         }
         else {
            selectedRegion = label->selectedRegion;
            window.ScrollIntoView(selectedRegion.t0());
            window.RedrawProject();
         }
         /* i18n-hint:
            String is replaced by the name of a label,
            first number gives the position of that label in a sequence
            of labels,
            and the last number is the total number of labels in the sequence.
         */
         auto message = XO("%s %d of %d")
            .Format( label->title, i + 1, lt->GetNumLabels() );
         trackFocus.MessageForScreenReader(message);
      }
      else {
         trackFocus.MessageForScreenReader(XO("no labels in label track"));
      }
   }
}


void OnMoveToPrevLabel(const CommandContext &context)
{
   auto &project = context.project;
   DoMoveToLabel(project, false);
}

void OnMoveToNextLabel(const CommandContext &context)
{
   auto &project = context.project;
   DoMoveToLabel(project, true);
}

using namespace MenuTable;

BaseItemSharedPtr ExtraSelectionItems()
{
   using Options = CommandManager::Options;
   static BaseItemSharedPtr items{
   Items(wxT("MoveToLabel"),
      Command(wxT("MoveToPrevLabel"), XXO("Move to Pre&vious Label"),
         OnMoveToPrevLabel,
         CaptureNotBusyFlag() | TrackPanelHasFocus(), wxT("Alt+Left")),
      Command(wxT("MoveToNextLabel"), XXO("Move to Ne&xt Label"),
         OnMoveToNextLabel,
         CaptureNotBusyFlag() | TrackPanelHasFocus(), wxT("Alt+Right"))
   ) };
   return items;
}

AttachedItem sAttachmentt{
  { wxT("Optional/Extra/Part1/Select"), { OrderingHint::End, {} } },
  Shared(ExtraSelectionItems())
};

}
