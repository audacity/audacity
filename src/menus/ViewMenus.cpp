#include "../Audacity.h"
#include "../Experimental.h"

#include "../HistoryWindow.h"
#include "../LyricsWindow.h"
#include "../Menus.h"
#include "../MixerBoard.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../TrackPanel.h"
#include "../commands/CommandContext.h"
#include "../commands/CommandManager.h"
#include "../prefs/TracksPrefs.h"

#ifdef EXPERIMENTAL_EFFECTS_RACK
#include "../effects/EffectManager.h"
#endif

#include <wx/scrolbar.h>

// private helper classes and functions
namespace {

double GetZoomOfSelection( const AudacityProject &project )
{
   const auto &viewInfo = project.GetViewInfo();
   const auto &trackPanel = *project.GetTrackPanel();

   const double lowerBound =
      std::max(viewInfo.selectedRegion.t0(),
         project.ScrollingLowerBoundTime());
   const double denom =
      viewInfo.selectedRegion.t1() - lowerBound;
   if (denom <= 0.0)
      return viewInfo.GetZoom();

   // LL:  The "-1" is just a hack to get around an issue where zooming to
   //      selection doesn't actually get the entire selected region within the
   //      visible area.  This causes a problem with scrolling at end of playback
   //      where the selected region may be scrolled off the left of the screen.
   //      I know this isn't right, but until the real rounding or 1-off issue is
   //      found, this will have to work.
   // PRL:  Did I fix this?  I am not sure, so I leave the hack in place.
   //      Fixes might have resulted from commits
   //      1b8f44d0537d987c59653b11ed75a842b48896ea and
   //      e7c7bb84a966c3b3cc4b3a9717d5f247f25e7296
   int width;
   trackPanel.GetTracksUsableArea(&width, NULL);
   return (width - 1) / denom;
}

double GetZoomOfPreset( const AudacityProject &project, int preset )
{

   // Sets a limit on how far we will zoom out as a factor over zoom to fit.
   const double maxZoomOutFactor = 4.0;
   // Sets how many pixels we allow for one uint, such as seconds.
   const double pixelsPerUnit = 5.0;

   double result = 1.0;
   double zoomToFit = ViewActions::GetZoomOfToFit( project );
   switch( preset ){
      default:
      case WaveTrack::kZoomDefault:
         result = ZoomInfo::GetDefaultZoom();
         break;
      case WaveTrack::kZoomToFit:
         result = zoomToFit;
         break;
      case WaveTrack::kZoomToSelection:
         result = GetZoomOfSelection( project );
         break;
      case WaveTrack::kZoomMinutes:
         result = pixelsPerUnit * 1.0/60;
         break;
      case WaveTrack::kZoomSeconds:
         result = pixelsPerUnit * 1.0;
         break;
      case WaveTrack::kZoom5ths:
         result = pixelsPerUnit * 5.0;
         break;
      case WaveTrack::kZoom10ths:
         result = pixelsPerUnit * 10.0;
         break;
      case WaveTrack::kZoom20ths:
         result = pixelsPerUnit * 20.0;
         break;
      case WaveTrack::kZoom50ths:
         result = pixelsPerUnit * 50.0;
         break;
      case WaveTrack::kZoom100ths:
         result = pixelsPerUnit * 100.0;
         break;
      case WaveTrack::kZoom500ths:
         result = pixelsPerUnit * 500.0;
         break;
      case WaveTrack::kZoomMilliSeconds:
         result = pixelsPerUnit * 1000.0;
         break;
      case WaveTrack::kZoomSamples:
         result = 44100.0;
         break;
      case WaveTrack::kZoom4To1:
         result = 44100.0 * 4;
         break;
      case WaveTrack::kMaxZoom:
         result = ZoomInfo::GetMaxZoom();
         break;
   };
   if( result < (zoomToFit/maxZoomOutFactor) )
      result = zoomToFit / maxZoomOutFactor;
   return result;
}

}

namespace ViewActions {

// exported helper functions

double GetZoomOfToFit( const AudacityProject &project )
{
   const auto &tracks = *project.GetTracks();
   const auto &viewInfo = project.GetViewInfo();
   const auto &trackPanel = *project.GetTrackPanel();

   const double end = tracks.GetEndTime();
   const double start = viewInfo.bScrollBeyondZero
      ? std::min( tracks.GetStartTime(), 0.0)
      : 0;
   const double len = end - start;

   if (len <= 0.0)
      return viewInfo.GetZoom();

   int w;
   trackPanel.GetTracksUsableArea(&w, NULL);
   w -= 10;
   return w/len;
}

void DoZoomFit(AudacityProject &project)
{
   auto &viewInfo = project.GetViewInfo();
   auto tracks = project.GetTracks();

   const double start = viewInfo.bScrollBeyondZero
      ? std::min(tracks->GetStartTime(), 0.0)
      : 0;

   project.Zoom( GetZoomOfToFit( project ) );
   project.TP_ScrollWindow(start);
}

void DoZoomFitV(AudacityProject &project)
{
   auto trackPanel = project.GetTrackPanel();
   auto tracks = project.GetTracks();

   // Only nonminimized audio tracks will be resized
   auto range = tracks->Any<AudioTrack>() - &Track::GetMinimized;
   auto count = range.size();
   if (count == 0)
      return;

   // Find total height to apportion
   int height;
   trackPanel->GetTracksUsableArea(NULL, &height);
   height -= 28;
   
   // The height of minimized and non-audio tracks cannot be apportioned
   height -=
      tracks->Any().sum( &Track::GetHeight ) - range.sum( &Track::GetHeight );
   
   // Give each resized track the average of the remaining height
   height = height / count;
   height = std::max( (int)TrackInfo::MinimumTrackHeight(), height );

   for (auto t : range)
      t->SetHeight(height);
}

// Menu handler functions

struct Handler : CommandHandlerObject {

void OnZoomIn(const CommandContext &context)
{
   auto &project = context.project;
   project.ZoomInByFactor( 2.0 );
}

void OnZoomNormal(const CommandContext &context)
{
   auto &project = context.project;
   auto trackPanel = project.GetTrackPanel();

   project.Zoom(ZoomInfo::GetDefaultZoom());
   trackPanel->Refresh(false);
}

void OnZoomOut(const CommandContext &context)
{
   auto &project = context.project;
   project.ZoomOutByFactor( 1 /2.0 );
}

void OnZoomSel(const CommandContext &context)
{
   auto &project = context.project;
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   project.Zoom( GetZoomOfSelection( project ) );
   project.TP_ScrollWindow(selectedRegion.t0());
}

void OnZoomToggle(const CommandContext &context)
{
   auto &project = context.project;
   auto &viewInfo = project.GetViewInfo();
   auto trackPanel = project.GetTrackPanel();

//   const double origLeft = viewInfo.h;
//   const double origWidth = GetScreenEndTime() - origLeft;

   // Choose the zoom that is most different to the current zoom.
   double Zoom1 = GetZoomOfPreset( project, TracksPrefs::Zoom1Choice() );
   double Zoom2 = GetZoomOfPreset( project, TracksPrefs::Zoom2Choice() );
   double Z = viewInfo.GetZoom();// Current Zoom.
   double ChosenZoom =
      fabs(log(Zoom1 / Z)) > fabs(log( Z / Zoom2)) ? Zoom1:Zoom2;

   project.Zoom(ChosenZoom);
   trackPanel->Refresh(false);
//   const double newWidth = GetScreenEndTime() - viewInfo.h;
//   const double newh = origLeft + (origWidth - newWidth) / 2;
//   TP_ScrollWindow(newh);
}

void OnZoomFit(const CommandContext &context)
{
   DoZoomFit( context.project );
}

void OnZoomFitV(const CommandContext &context)
{
   auto &project = context.project;

   DoZoomFitV(project);

   project.GetVerticalScrollBar().SetThumbPosition(0);
   project.RedrawProject();
   project.ModifyState(true);
}

void OnAdvancedVZoom(const CommandContext &context)
{
   auto &project = context.project;
   auto commandManager = project.GetCommandManager();

   bool checked = !gPrefs->Read(wxT("/GUI/VerticalZooming"), 0L);
   gPrefs->Write(wxT("/GUI/VerticalZooming"), checked);
   gPrefs->Flush();
   commandManager->Check(wxT("AdvancedVZoom"), checked);
   MenuCreator::RebuildAllMenuBars();
}

void OnCollapseAllTracks(const CommandContext &context)
{
   auto &project = context.project;
   auto tracks = project.GetTracks();

   for (auto t : tracks->Any())
      t->SetMinimized(true);

   project.ModifyState(true);
   project.RedrawProject();
}

void OnExpandAllTracks(const CommandContext &context)
{
   auto &project = context.project;
   auto tracks = project.GetTracks();

   for (auto t : tracks->Any())
      t->SetMinimized(false);

   project.ModifyState(true);
   project.RedrawProject();
}

void OnGoSelStart(const CommandContext &context)
{
   auto &project = context.project;
   auto &viewInfo = project.GetViewInfo();
   auto &selectedRegion = viewInfo.selectedRegion;
   auto &trackPanel = *project.GetTrackPanel();

   if (selectedRegion.isPoint())
      return;

   project.TP_ScrollWindow(
      selectedRegion.t0() - ((trackPanel.GetScreenEndTime() - viewInfo.h) / 2));
}

void OnGoSelEnd(const CommandContext &context)
{
   auto &project = context.project;
   auto &viewInfo = project.GetViewInfo();
   auto &selectedRegion = viewInfo.selectedRegion;
   auto &trackPanel = *project.GetTrackPanel();

   if (selectedRegion.isPoint())
      return;

   project.TP_ScrollWindow(
      selectedRegion.t1() - ((trackPanel.GetScreenEndTime() - viewInfo.h) / 2));
}

void OnHistory(const CommandContext &context)
{
   auto &project = context.project;

   auto historyWindow = project.GetHistoryWindow(true);
   historyWindow->Show();
   historyWindow->Raise();
}

void OnKaraoke(const CommandContext &context)
{
   auto &project = context.project;

   auto lyricsWindow = project.GetLyricsWindow(true);
   lyricsWindow->Show();
   lyricsWindow->Raise();
}

void OnMixerBoard(const CommandContext &context)
{
   auto &project = context.project;

   auto mixerBoardFrame = project.GetMixerBoardFrame(true);
   mixerBoardFrame->Show();
   mixerBoardFrame->Raise();
   mixerBoardFrame->SetFocus();
}

void OnShowExtraMenus(const CommandContext &context)
{
   auto &project = context.project;
   auto commandManager = project.GetCommandManager();

   bool checked = !gPrefs->Read(wxT("/GUI/ShowExtraMenus"), 0L);
   gPrefs->Write(wxT("/GUI/ShowExtraMenus"), checked);
   gPrefs->Flush();
   commandManager->Check(wxT("ShowExtraMenus"), checked);
   MenuCreator::RebuildAllMenuBars();
}

void OnShowClipping(const CommandContext &context)
{
   auto &project = context.project;
   auto commandManager = project.GetCommandManager();
   auto trackPanel = project.GetTrackPanel();

   bool checked = !gPrefs->Read(wxT("/GUI/ShowClipping"), 0L);
   gPrefs->Write(wxT("/GUI/ShowClipping"), checked);
   gPrefs->Flush();
   commandManager->Check(wxT("ShowClipping"), checked);
   trackPanel->UpdatePrefs();
   trackPanel->Refresh(false);
}

#if defined(EXPERIMENTAL_EFFECTS_RACK)
void OnShowEffectsRack(const CommandContext &WXUNUSED(context) )
{
   EffectManager::Get().ShowRack();
}
#endif

}; // struct Handler

} // namespace

static CommandHandlerObject &findCommandHandler(AudacityProject &) {
   // Handler is not stateful.  Doesn't need a factory registered with
   // AudacityProject.
   static ViewActions::Handler instance;
   return instance;
};

// Menu definitions

#define FN(X) findCommandHandler, \
   static_cast<CommandFunctorPointer>(& ViewActions::Handler :: X)
#define XXO(X) _(X), wxString{X}.Contains("...")

MenuTable::BaseItemPtr ToolbarsMenu( AudacityProject& );

MenuTable::BaseItemPtr ViewMenu( AudacityProject& )
{
   using namespace MenuTable;
   using Options = CommandManager::Options;

   static const auto checkOff = Options{}.CheckState( false );

   return Menu( _("&View"),
      Menu( _("&Zoom"),
         Command( wxT("ZoomIn"), XXO("Zoom &In"), FN(OnZoomIn),
            ZoomInAvailableFlag, wxT("Ctrl+1") ),
         Command( wxT("ZoomNormal"), XXO("Zoom &Normal"), FN(OnZoomNormal),
            TracksExistFlag, wxT("Ctrl+2") ),
         Command( wxT("ZoomOut"), XXO("Zoom &Out"), FN(OnZoomOut),
            ZoomOutAvailableFlag, wxT("Ctrl+3") ),
         Command( wxT("ZoomSel"), XXO("&Zoom to Selection"), FN(OnZoomSel),
            TimeSelectedFlag, wxT("Ctrl+E") ),
         Command( wxT("ZoomToggle"), XXO("Zoom &Toggle"), FN(OnZoomToggle),
            TracksExistFlag, wxT("Shift+Z") ),
         Separator(),
         Command( wxT("AdvancedVZoom"), XXO("Advanced &Vertical Zooming"),
            FN(OnAdvancedVZoom), AlwaysEnabledFlag,
            Options{}.CheckState( gPrefs->Read(wxT("/GUI/VerticalZooming"), 0L) ) )
      ),

      Menu( _("T&rack Size"),
         Command( wxT("FitInWindow"), XXO("&Fit to Width"), FN(OnZoomFit),
            TracksExistFlag, wxT("Ctrl+F") ),
         Command( wxT("FitV"), XXO("Fit to &Height"), FN(OnZoomFitV),
            TracksExistFlag, wxT("Ctrl+Shift+F") ),
         Command( wxT("CollapseAllTracks"), XXO("&Collapse All Tracks"),
            FN(OnCollapseAllTracks), TracksExistFlag, wxT("Ctrl+Shift+C") ),
         Command( wxT("ExpandAllTracks"), XXO("E&xpand Collapsed Tracks"),
            FN(OnExpandAllTracks), TracksExistFlag, wxT("Ctrl+Shift+X") )
      ),

      Menu( _("Sk&ip to"),
         Command( wxT("SkipSelStart"), XXO("Selection Sta&rt"),
            FN(OnGoSelStart), TimeSelectedFlag,
            Options{ wxT("Ctrl+["), _("Skip to Selection Start") } ),
         Command( wxT("SkipSelEnd"), XXO("Selection En&d"), FN(OnGoSelEnd),
            TimeSelectedFlag,
            Options{ wxT("Ctrl+]"), _("Skip to Selection End") } )
      ),

      Separator(),

      // History window should be available either for UndoAvailableFlag
      // or RedoAvailableFlag,
      // but we can't make the AddItem flags and mask have both,
      // because they'd both have to be true for the
      // command to be enabled.
      //    If user has Undone the entire stack, RedoAvailableFlag is on
      //    but UndoAvailableFlag is off.
      //    If user has done things but not Undone anything,
      //    RedoAvailableFlag is off but UndoAvailableFlag is on.
      // So in either of those cases,
      // (AudioIONotBusyFlag | UndoAvailableFlag | RedoAvailableFlag) mask
      // would fail.
      // The only way to fix this in the current architecture
      // is to hack in special cases for RedoAvailableFlag
      // in AudacityProject::UpdateMenus() (ugly)
      // and CommandManager::HandleCommandEntry() (*really* ugly --
      // shouldn't know about particular command names and flags).
      // Here's the hack that would be necessary in
      // AudacityProject::UpdateMenus(), if somebody decides to do it:
      //    // Because EnableUsingFlags requires all the flag bits match the
      //    // corresponding mask bits,
      //    // "UndoHistory" specifies only
      //    // AudioIONotBusyFlag | UndoAvailableFlag, because that
      //    // covers the majority of cases where it should be enabled.
      //    // If history is not empty but we've Undone the whole stack,
      //    // we also want to enable,
      //    // to show the Redo's on stack.
      //    // "UndoHistory" might already be enabled,
      //    // but add this check for RedoAvailableFlag.
      //    if (flags & RedoAvailableFlag)
      //       GetCommandManager()->Enable(wxT("UndoHistory"), true);
      // So for now, enable the command regardless of stack.
      // It will just show empty sometimes.
      // FOR REDESIGN,
      // clearly there are some limitations with the flags/mask bitmaps.

      /* i18n-hint: Clicking this menu item shows the various editing steps
         that have been taken.*/
      Command( wxT("UndoHistory"), XXO("&History..."), FN(OnHistory),
         AudioIONotBusyFlag ),

      Command( wxT("Karaoke"), XXO("&Karaoke..."), FN(OnKaraoke),
         LabelTracksExistFlag ),
      Command( wxT("MixerBoard"), XXO("&Mixer Board..."), FN(OnMixerBoard),
         PlayableTracksExistFlag ),

      Separator(),

      //////////////////////////////////////////////////////////////////////////

      ToolbarsMenu,

      Separator(),

      Command( wxT("ShowExtraMenus"), XXO("&Extra Menus (on/off)"),
         FN(OnShowExtraMenus), AlwaysEnabledFlag,
         Options{}.CheckState( gPrefs->Read(wxT("/GUI/ShowExtraMenus"), 0L) ) ),
      Command( wxT("ShowClipping"), XXO("&Show Clipping (on/off)"),
         FN(OnShowClipping), AlwaysEnabledFlag,
         Options{}.CheckState( gPrefs->Read(wxT("/GUI/ShowClipping"), 0L) ) )
#if defined(EXPERIMENTAL_EFFECTS_RACK)
      ,
      Command( wxT("ShowEffectsRack"), XXO("Show Effects Rack"),
         FN(OnShowEffectsRack), AlwaysEnabledFlag, checkOff )
#endif
   );
}

#undef XXO
#undef FN
