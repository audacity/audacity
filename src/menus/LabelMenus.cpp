#include "../AudioIO.h"
#include "../LabelTrack.h"
#include "../Menus.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../TrackPanel.h"
#include "../WaveTrack.h"
#include "../commands/CommandContext.h"
#include "../commands/CommandManager.h"

// private helper classes and functions
namespace {

//Adds label and returns index of label in labeltrack.
int DoAddLabel(
   AudacityProject &project, const SelectedRegion &region,
   bool preserveFocus = false)
{
   auto tracks = project.GetTracks();
   auto trackPanel = project.GetTrackPanel();
   auto trackFactory = project.GetTrackFactory();

   wxString title;      // of label

   bool useDialog;
   gPrefs->Read(wxT("/GUI/DialogForNameNewLabel"), &useDialog, false);
   if (useDialog) {
      if (LabelTrack::DialogForLabelName(
         project, region, wxEmptyString, title) == wxID_CANCEL)
         return -1;     // index
   }

   // If the focused track is a label track, use that
   Track *const pFocusedTrack = trackPanel->GetFocusedTrack();

   // Look for a label track at or after the focused track
   auto iter = pFocusedTrack
      ? tracks->Find(pFocusedTrack)
      : tracks->Any().begin();
   auto lt = * iter.Filter< LabelTrack >();

   // If none found, start a NEW label track and use it
   if (!lt) {
      lt = static_cast<LabelTrack*>
         (tracks->Add(trackFactory->NewLabelTrack()));
   }

// LLL: Commented as it seemed a little forceful to remove users
//      selection when adding the label.  This does not happen if
//      you select several tracks and the last one of those is a
//      label track...typing a label will not clear the selections.
//
//   SelectNone();
   lt->SetSelected(true);

   int focusTrackNumber;
   if (useDialog) {
      focusTrackNumber = -2;
   }
   else {
      focusTrackNumber = -1;
      if (pFocusedTrack && preserveFocus) {
         // Must remember the track to re-focus after finishing a label edit.
         // do NOT identify it by a pointer, which might dangle!  Identify
         // by position.
         focusTrackNumber = pFocusedTrack->GetIndex();
      }
   }

   int index = lt->AddLabel(region, title, focusTrackNumber);

   project.PushState(_("Added label"), _("Label"));

   project.RedrawProject();
   if (!useDialog) {
      trackPanel->EnsureVisible(lt);
   }
   trackPanel->SetFocus();

   return index;
}

}

namespace LabelEditActions {

// exported helper functions
// none

// Menu handler functions

struct Handler : CommandHandlerObject {

void OnEditLabels(const CommandContext &context)
{
   auto &project = context.project;
   LabelTrack::DoEditLabels(project);
}

void OnAddLabel(const CommandContext &context)
{
   auto &project = context.project;
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   DoAddLabel(project, selectedRegion);
}

void OnAddLabelPlaying(const CommandContext &context)
{
   auto &project = context.project;
   auto token = project.GetAudioIOToken();

   if (token > 0 &&
       gAudioIO->IsStreamActive(token)) {
      double indicator = gAudioIO->GetStreamTime();
      DoAddLabel(project, SelectedRegion(indicator, indicator), true);
   }
}

// Creates a NEW label in each selected label track with text from the system
// clipboard
void OnPasteNewLabel(const CommandContext &context)
{
   auto &project = context.project;
   auto tracks = project.GetTracks();
   auto trackFactory = project.GetTrackFactory();
   auto trackPanel = project.GetTrackPanel();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   bool bPastedSomething = false;

   {
      auto trackRange = tracks->Selected< const LabelTrack >();
      if (trackRange.empty())
      {
         // If there are no selected label tracks, try to choose the first label
         // track after some other selected track
         Track *t = *tracks->Selected().begin()
            .Filter( &Track::Any )
            .Filter<LabelTrack>();

         // If no match found, add one
         if (!t) {
            t = tracks->Add(trackFactory->NewLabelTrack());
         }

         // Select this track so the loop picks it up
         t->SetSelected(true);
      }
   }

   LabelTrack *plt = NULL; // the previous track
   for ( auto lt : tracks->Selected< LabelTrack >() )
   {
      // Unselect the last label, so we'll have just one active label when
      // we're done
      if (plt)
         plt->Unselect();

      // Add a NEW label, paste into it
      // Paul L:  copy whatever defines the selected region, not just times
      lt->AddLabel(selectedRegion);
      if (lt->PasteSelectedText(selectedRegion.t0(),
                                selectedRegion.t1()))
         bPastedSomething = true;

      // Set previous track
      plt = lt;
   }

   // plt should point to the last label track pasted to -- ensure it's visible
   // and set focus
   if (plt) {
      trackPanel->EnsureVisible(plt);
      trackPanel->SetFocus();
   }

   if (bPastedSomething) {
      project.PushState(
         _("Pasted from the clipboard"), _("Paste Text to New Label"));

      // Is this necessary? (carried over from former logic in OnPaste())
      project.RedrawProject();
   }
}

void OnToggleTypeToCreateLabel(const CommandContext &WXUNUSED(context) )
{
   bool typeToCreateLabel;
   gPrefs->Read(wxT("/GUI/TypeToCreateLabel"), &typeToCreateLabel, true);
   gPrefs->Write(wxT("/GUI/TypeToCreateLabel"), !typeToCreateLabel);
   gPrefs->Flush();
   MenuManager::ModifyAllProjectToolbarMenus();
}

void OnCutLabels(const CommandContext &context)
{
   auto &project = context.project;
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   if( selectedRegion.isPoint() )
     return;

   // Because of grouping the copy may need to operate on different tracks than
   // the clear, so we do these actions separately.
   project.EditClipboardByLabel( &WaveTrack::CopyNonconst );

   if( gPrefs->Read( wxT( "/GUI/EnableCutLines" ), ( long )0 ) )
      project.EditByLabel( &WaveTrack::ClearAndAddCutLine, true );
   else
      project.EditByLabel( &WaveTrack::Clear, true );

   AudacityProject::msClipProject = &project;

   selectedRegion.collapseToT0();

   project.PushState(
   /* i18n-hint: (verb) past tense.  Audacity has just cut the labeled audio
      regions.*/
      _( "Cut labeled audio regions to clipboard" ),
   /* i18n-hint: (verb)*/
      _( "Cut Labeled Audio" ) );

   project.RedrawProject();
}

void OnDeleteLabels(const CommandContext &context)
{
   auto &project = context.project;
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   if( selectedRegion.isPoint() )
      return;

   project.EditByLabel( &WaveTrack::Clear, true );

   selectedRegion.collapseToT0();

   project.PushState(
      /* i18n-hint: (verb) Audacity has just deleted the labeled audio regions*/
      _( "Deleted labeled audio regions" ),
      /* i18n-hint: (verb)*/
      _( "Delete Labeled Audio" ) );

   project.RedrawProject();
}

void OnSplitCutLabels(const CommandContext &context)
{
   auto &project = context.project;
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   if( selectedRegion.isPoint() )
      return;

   project.EditClipboardByLabel( &WaveTrack::SplitCut );

   AudacityProject::msClipProject = &project;

   project.PushState(
      /* i18n-hint: (verb) Audacity has just split cut the labeled audio
         regions*/
      _( "Split Cut labeled audio regions to clipboard" ),
      /* i18n-hint: (verb) Do a special kind of cut on the labels*/
      _( "Split Cut Labeled Audio" ) );

   project.RedrawProject();
}

void OnSplitDeleteLabels(const CommandContext &context)
{
   auto &project = context.project;
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   if( selectedRegion.isPoint() )
      return;

   project.EditByLabel( &WaveTrack::SplitDelete, false );

   project.PushState(
      /* i18n-hint: (verb) Audacity has just done a special kind of DELETE on
         the labeled audio regions */
      _( "Split Deleted labeled audio regions" ),
      /* i18n-hint: (verb) Do a special kind of DELETE on labeled audio
         regions */
      _( "Split Delete Labeled Audio" ) );

   project.RedrawProject();
}

void OnSilenceLabels(const CommandContext &context)
{
   auto &project = context.project;
   auto trackPanel = project.GetTrackPanel();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   if( selectedRegion.isPoint() )
      return;

   project.EditByLabel( &WaveTrack::Silence, false );

   project.PushState(
      /* i18n-hint: (verb)*/
      _( "Silenced labeled audio regions" ),
      /* i18n-hint: (verb)*/
      _( "Silence Labeled Audio" ) );

   trackPanel->Refresh( false );
}

void OnCopyLabels(const CommandContext &context)
{
   auto &project = context.project;
   auto trackPanel = project.GetTrackPanel();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   if( selectedRegion.isPoint() )
      return;

   project.EditClipboardByLabel( &WaveTrack::CopyNonconst );

   AudacityProject::msClipProject = &project;

   project.PushState( _( "Copied labeled audio regions to clipboard" ),
   /* i18n-hint: (verb)*/
      _( "Copy Labeled Audio" ) );

   trackPanel->Refresh( false );
}

void OnSplitLabels(const CommandContext &context)
{
   auto &project = context.project;

   project.EditByLabel( &WaveTrack::Split, false );

   project.PushState(
      /* i18n-hint: (verb) past tense.  Audacity has just split the labeled
         audio (a point or a region)*/
      _( "Split labeled audio (points or regions)" ),
      /* i18n-hint: (verb)*/
      _( "Split Labeled Audio" ) );

   project.RedrawProject();
}

void OnJoinLabels(const CommandContext &context)
{
   auto &project = context.project;
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   if( selectedRegion.isPoint() )
      return;

   project.EditByLabel( &WaveTrack::Join, false );

   project.PushState(
      /* i18n-hint: (verb) Audacity has just joined the labeled audio (points or
         regions) */
      _( "Joined labeled audio (points or regions)" ),
      /* i18n-hint: (verb) */
      _( "Join Labeled Audio" ) );

   project.RedrawProject();
}

void OnDisjoinLabels(const CommandContext &context)
{
   auto &project = context.project;
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   if( selectedRegion.isPoint() )
      return;

   project.EditByLabel( &WaveTrack::Disjoin, false );

   project.PushState(
      /* i18n-hint: (verb) Audacity has just detached the labeled audio regions.
      This message appears in history and tells you about something
      Audacity has done.*/
      _( "Detached labeled audio regions" ),
      /* i18n-hint: (verb)*/
      _( "Detach Labeled Audio" ) );

   project.RedrawProject();
}

}; // struct Handler

} // namespace

static CommandHandlerObject &findCommandHandler(AudacityProject &) {
   // Handler is not stateful.  Doesn't need a factory registered with
   // AudacityProject.
   static LabelEditActions::Handler instance;
   return instance;
};

// Menu definitions

#define FN(X) findCommandHandler, \
   static_cast<CommandFunctorPointer>(& LabelEditActions::Handler :: X)
#define XXO(X) _(X), wxString{X}.Contains("...")

MenuTable::BaseItemPtr LabelEditMenus( AudacityProject &project )
{
   using namespace MenuTable;
   using Options = CommandManager::Options;

   static const auto checkOff = Options{}.CheckState( false );

   constexpr auto NotBusyLabelsAndWaveFlags =
      AudioIONotBusyFlag |
      LabelsSelectedFlag | WaveTracksExistFlag | TimeSelectedFlag;

   // Returns TWO menus.
   
   return Items(

   Menu( _("&Labels"),
      Command( wxT("EditLabels"), XXO("&Edit Labels..."), FN(OnEditLabels),
                 AudioIONotBusyFlag ),

      Separator(),

      Command( wxT("AddLabel"), XXO("Add Label at &Selection"),
         FN(OnAddLabel), AlwaysEnabledFlag, wxT("Ctrl+B") ),
      Command( wxT("AddLabelPlaying"),
         XXO("Add Label at &Playback Position"),
         FN(OnAddLabelPlaying), AudioIOBusyFlag,
#ifdef __WXMAC__
         wxT("Ctrl+.")
#else
         wxT("Ctrl+M")
#endif
      ),
      Command( wxT("PasteNewLabel"), XXO("Paste Te&xt to New Label"),
         FN(OnPasteNewLabel),
         AudioIONotBusyFlag, wxT("Ctrl+Alt+V") ),

      Separator(),

      Command( wxT("TypeToCreateLabel"),
         XXO("&Type to Create a Label (on/off)"),
         FN(OnToggleTypeToCreateLabel), AlwaysEnabledFlag, checkOff )
   ), // first menu

   /////////////////////////////////////////////////////////////////////////////

   Menu( _("La&beled Audio"),
      /* i18n-hint: (verb)*/
      Command( wxT("CutLabels"), XXO("&Cut"), FN(OnCutLabels),
         AudioIONotBusyFlag | LabelsSelectedFlag | WaveTracksExistFlag |
            TimeSelectedFlag | IsNotSyncLockedFlag,
            Options{ wxT("Alt+X"), _("Label Cut") } ),
      Command( wxT("DeleteLabels"), XXO("&Delete"), FN(OnDeleteLabels),
         AudioIONotBusyFlag | LabelsSelectedFlag | WaveTracksExistFlag |
            TimeSelectedFlag | IsNotSyncLockedFlag,
         Options{ wxT("Alt+K"), _("Label Delete") } ),

      Separator(),

      /* i18n-hint: (verb) A special way to cut out a piece of audio*/
      Command( wxT("SplitCutLabels"), XXO("&Split Cut"),
         FN(OnSplitCutLabels), NotBusyLabelsAndWaveFlags,
         Options{ wxT("Alt+Shift+X"), _("Label Split Cut") } ),
      Command( wxT("SplitDeleteLabels"), XXO("Sp&lit Delete"),
         FN(OnSplitDeleteLabels), NotBusyLabelsAndWaveFlags,
         Options{ wxT("Alt+Shift+K"), _("Label Split Delete") } ),

      Separator(),

      Command( wxT("SilenceLabels"), XXO("Silence &Audio"),
         FN(OnSilenceLabels), NotBusyLabelsAndWaveFlags,
         Options{ wxT("Alt+L"), _("Label Silence") } ),
      /* i18n-hint: (verb)*/
      Command( wxT("CopyLabels"), XXO("Co&py"), FN(OnCopyLabels),
         NotBusyLabelsAndWaveFlags,
         Options{ wxT("Alt+Shift+C"), _("Label Copy") } ),

      Separator(),

      /* i18n-hint: (verb)*/
      Command( wxT("SplitLabels"), XXO("Spli&t"), FN(OnSplitLabels),
         AudioIONotBusyFlag | LabelsSelectedFlag | WaveTracksExistFlag,
         Options{ wxT("Alt+I"), _("Label Split") } ),
      /* i18n-hint: (verb)*/
      Command( wxT("JoinLabels"), XXO("&Join"), FN(OnJoinLabels),
         NotBusyLabelsAndWaveFlags,
         Options{ wxT("Alt+J"), _("Label Join") } ),
      Command( wxT("DisjoinLabels"), XXO("Detac&h at Silences"),
         FN(OnDisjoinLabels), NotBusyLabelsAndWaveFlags,
         wxT("Alt+Shift+J") )
   ) // second menu

   ); // two menus
}

#undef XXO
#undef FN
