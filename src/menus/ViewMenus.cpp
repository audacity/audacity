#include "../CommonCommandFlags.h"
#include "../Menus.h"
#include "Prefs.h"
#include "Project.h"
#include "../ProjectHistory.h"
#include "../ProjectSettings.h"
#include "../ProjectWindow.h"
#include "../Track.h"
#include "../TrackInfo.h"
#include "../TrackPanel.h"
#include "../UndoManager.h"
#include "ViewInfo.h"
#include "../commands/CommandContext.h"
#include "../commands/CommandManager.h"
#include "../prefs/GUIPrefs.h"
#include "../prefs/TracksPrefs.h"
#include "../tracks/ui/TrackView.h"

#ifdef EXPERIMENTAL_EFFECTS_RACK
#include "../effects/EffectUI.h"
#endif

#include <wx/app.h>
#include <wx/scrolbar.h>

// private helper classes and functions
namespace {

double GetZoomOfSelection( const AudacityProject &project )
{
   auto &viewInfo = ViewInfo::Get( project );
   auto &window = ProjectWindow::Get( project );

   const double lowerBound =
      std::max(viewInfo.selectedRegion.t0(),
         window.ScrollingLowerBoundTime());
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
   auto width = viewInfo.GetTracksUsableWidth();
   return (width - 1) / denom;
}

double GetZoomOfPreset( const AudacityProject &project, int preset )
{

   // Sets a limit on how far we will zoom out as a factor over zoom to fit.
   const double maxZoomOutFactor = 4.0;
   // Sets how many pixels we allow for one uint, such as seconds.
   const double pixelsPerUnit = 5.0;

   double result = 1.0;
   auto &window = ProjectWindow::Get( project );
   double zoomToFit = window.GetZoomOfToFit();
   using namespace WaveTrackViewConstants;
   switch( preset ){
      default:
      case kZoomDefault:
         result = ZoomInfo::GetDefaultZoom();
         break;
      case kZoomToFit:
         result = zoomToFit;
         break;
      case kZoomToSelection:
         result = GetZoomOfSelection( project );
         break;
      case kZoomMinutes:
         result = pixelsPerUnit * 1.0/60;
         break;
      case kZoomSeconds:
         result = pixelsPerUnit * 1.0;
         break;
      case kZoom5ths:
         result = pixelsPerUnit * 5.0;
         break;
      case kZoom10ths:
         result = pixelsPerUnit * 10.0;
         break;
      case kZoom20ths:
         result = pixelsPerUnit * 20.0;
         break;
      case kZoom50ths:
         result = pixelsPerUnit * 50.0;
         break;
      case kZoom100ths:
         result = pixelsPerUnit * 100.0;
         break;
      case kZoom500ths:
         result = pixelsPerUnit * 500.0;
         break;
      case kZoomMilliSeconds:
         result = pixelsPerUnit * 1000.0;
         break;
      case kZoomSamples:
         result = 44100.0;
         break;
      case kZoom4To1:
         result = 44100.0 * 4;
         break;
      case kMaxZoom:
         result = ZoomInfo::GetMaxZoom();
         break;
   };
   if( result < (zoomToFit/maxZoomOutFactor) )
      result = zoomToFit / maxZoomOutFactor;
   return result;
}

}

namespace {
void DoZoomFitV(AudacityProject &project)
{
   auto &viewInfo = ViewInfo::Get( project );
   auto &tracks = TrackList::Get( project );

   // Only nonminimized audio tracks will be resized
   auto range = tracks.Any<AudioTrack>()
      - [](const Track *pTrack){
         return TrackView::Get( *pTrack ).GetMinimized(); };
   auto count = range.size();
   if (count == 0)
      return;

   // Find total height to apportion
   auto height = viewInfo.GetHeight();
   height -= 28;
   
   // The height of minimized and non-audio tracks cannot be apportioned
   height -=
      tracks.Any().sum( TrackView::GetTrackHeight )
         - range.sum( TrackView::GetTrackHeight );
   
   // Give each resized track the average of the remaining height
   // Bug 2803: Cast count to int, because otherwise the result of 
   // division will be unsigned too, and will be a very large number 
   // if height was negative!
   height = height / (int)count;
   // Use max() so that we don't set a negative height when there is
   // not enough room.
   height = std::max( (int)TrackInfo::MinimumTrackHeight(), height );

   for (auto t : range)
      TrackView::Get( *t ).SetExpandedHeight(height);
}
}

namespace ViewActions {

// Menu handler functions

struct Handler final
   : CommandHandlerObject // MUST be the first base class!
   , ClientData::Base
{

void OnZoomIn(const CommandContext &context)
{
   auto &project = context.project;
   auto &trackPanel = TrackPanel::Get( project );
   auto &window = ProjectWindow::Get( project );
   window.ZoomInByFactor( 2.0 );
   trackPanel.Refresh(false);
}

void OnZoomNormal(const CommandContext &context)
{
   auto &project = context.project;
   auto &trackPanel = TrackPanel::Get( project );
   auto &window = ProjectWindow::Get( project );

   window.Zoom(ZoomInfo::GetDefaultZoom());
   trackPanel.Refresh(false);
}

void OnZoomOut(const CommandContext &context)
{
   auto &project = context.project;
   auto &window = ProjectWindow::Get( project );
   window.ZoomOutByFactor( 1 /2.0 );
}

void OnZoomSel(const CommandContext &context)
{
   auto &project = context.project;
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;
   auto &window = ProjectWindow::Get( project );

   window.Zoom( GetZoomOfSelection( project ) );
   window.TP_ScrollWindow(selectedRegion.t0());
}

void OnZoomToggle(const CommandContext &context)
{
   auto &project = context.project;
   auto &viewInfo = ViewInfo::Get( project );
   auto &trackPanel = TrackPanel::Get( project );
   auto &window = ProjectWindow::Get( project );

//   const double origLeft = viewInfo.h;
//   const double origWidth = viewInfo.GetScreenEndTime() - origLeft;

   // Choose the zoom that is most different to the current zoom.
   double Zoom1 = GetZoomOfPreset( project, TracksPrefs::Zoom1Choice() );
   double Zoom2 = GetZoomOfPreset( project, TracksPrefs::Zoom2Choice() );
   double Z = viewInfo.GetZoom();// Current Zoom.
   double ChosenZoom =
      fabs(log(Zoom1 / Z)) > fabs(log( Z / Zoom2)) ? Zoom1:Zoom2;

   window.Zoom(ChosenZoom);
   trackPanel.Refresh(false);
//   const double newWidth = GetScreenEndTime() - viewInfo.h;
//   const double newh = origLeft + (origWidth - newWidth) / 2;
//   TP_ScrollWindow(newh);
}

void OnZoomFit(const CommandContext &context)
{
   auto &project = context.project;
   auto &window = ProjectWindow::Get( project );
   window.DoZoomFit();
}

void OnZoomFitV(const CommandContext &context)
{
   auto &project = context.project;
   auto &window = ProjectWindow::Get( project );

   DoZoomFitV(project);

   window.GetVerticalScrollBar().SetThumbPosition(0);
   ProjectHistory::Get( project ).ModifyState(true);
}

void OnAdvancedVZoom(const CommandContext &context)
{
   auto &project = context.project;
   auto &commandManager = CommandManager::Get( project );

   bool checked = !gPrefs->Read(wxT("/GUI/VerticalZooming"), 0L);
   gPrefs->Write(wxT("/GUI/VerticalZooming"), checked);
   gPrefs->Flush();
   commandManager.Check(wxT("AdvancedVZoom"), checked);
   MenuCreator::RebuildAllMenuBars();
}

void OnCollapseAllTracks(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get( project );
   auto &window = ProjectWindow::Get( project );

   for (auto t : tracks.Any())
      TrackView::Get( *t ).SetMinimized(true);

   ProjectHistory::Get( project ).ModifyState(true);
}

void OnExpandAllTracks(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get( project );
   auto &window = ProjectWindow::Get( project );

   for (auto t : tracks.Any())
      TrackView::Get( *t ).SetMinimized(false);

   ProjectHistory::Get( project ).ModifyState(true);
}

void OnGoSelStart(const CommandContext &context)
{
   auto &project = context.project;
   auto &viewInfo = ViewInfo::Get( project );
   auto &selectedRegion = viewInfo.selectedRegion;
   auto &window = ProjectWindow::Get( project );

   if (selectedRegion.isPoint())
      return;

   window.TP_ScrollWindow(
      selectedRegion.t0() - ((viewInfo.GetScreenEndTime() - viewInfo.h) / 2));
}

void OnGoSelEnd(const CommandContext &context)
{
   auto &project = context.project;
   auto &viewInfo = ViewInfo::Get( project );
   auto &selectedRegion = viewInfo.selectedRegion;
   auto &window = ProjectWindow::Get( project );

   if (selectedRegion.isPoint())
      return;

   window.TP_ScrollWindow(
      selectedRegion.t1() - ((viewInfo.GetScreenEndTime() - viewInfo.h) / 2));
}

void OnShowExtraMenus(const CommandContext &context)
{
   auto &project = context.project;
   auto &commandManager = CommandManager::Get( project );

   bool checked = !gPrefs->Read(wxT("/GUI/ShowExtraMenus"), 0L);
   gPrefs->Write(wxT("/GUI/ShowExtraMenus"), checked);
   gPrefs->Flush();
   commandManager.Check(wxT("ShowExtraMenus"), checked);
   MenuCreator::RebuildAllMenuBars();
}

void OnShowClipping(const CommandContext &context)
{
   auto &project = context.project;
   auto &commandManager = CommandManager::Get( project );
   auto &trackPanel = TrackPanel::Get( project );

   bool checked = !gPrefs->Read(wxT("/GUI/ShowClipping"), 0L);
   gPrefs->Write(wxT("/GUI/ShowClipping"), checked);
   gPrefs->Flush();
   commandManager.Check(wxT("ShowClipping"), checked);

   PrefsListener::Broadcast(ShowClippingPrefsID());

   trackPanel.Refresh(false);
}

void OnShowNameOverlay(const CommandContext &context)
{
   auto &project = context.project;
   auto &commandManager = CommandManager::Get( project );
   auto &trackPanel = TrackPanel::Get( project );

   bool checked = !gPrefs->Read(wxT("/GUI/ShowTrackNameInWaveform"), 0L);
   gPrefs->Write(wxT("/GUI/ShowTrackNameInWaveform"), checked);
   gPrefs->Flush();
   commandManager.Check(wxT("ShowTrackNameInWaveform"), checked);

   PrefsListener::Broadcast(ShowTrackNameInWaveformPrefsID());

   trackPanel.Refresh(false);
}

#if defined(EXPERIMENTAL_EFFECTS_RACK)
void OnShowEffectsRack(const CommandContext &context )
{
   auto &rack = EffectRack::Get( context.project );
   rack.Show( !rack.IsShown() );
}
#endif

// Not a menu item, but a listener for events
void OnUndoPushed( wxCommandEvent &evt )
{
   evt.Skip();
   const auto &settings = ProjectSettings::Get( mProject );
   if (settings.GetTracksFitVerticallyZoomed())
      DoZoomFitV( mProject );
}

Handler( AudacityProject &project )
   : mProject{ project }
{
   mProject.Bind( EVT_UNDO_PUSHED, &Handler::OnUndoPushed, this );
}

~Handler()
{
   mProject.Unbind( EVT_UNDO_PUSHED, &Handler::OnUndoPushed, this );
}
Handler( const Handler & ) PROHIBITED;
Handler &operator=( const Handler & ) PROHIBITED;

AudacityProject &mProject;

}; // struct Handler

} // namespace

// Handler needs a back-reference to the project, so needs a factory registered
// with AudacityProject.
static const AudacityProject::AttachedObjects::RegisteredFactory key{
   []( AudacityProject &project ) {
      return std::make_unique< ViewActions::Handler >( project ); } };

static CommandHandlerObject &findCommandHandler(AudacityProject &project) {
   return project.AttachedObjects::Get< ViewActions::Handler >( key );
};

// Menu definitions

#define FN(X) (& ViewActions::Handler :: X)

// Under /MenuBar
namespace {
using namespace MenuTable;
BaseItemSharedPtr ViewMenu()
{
   using Options = CommandManager::Options;

   static BaseItemSharedPtr menu{
   ( FinderScope{ findCommandHandler },
   Menu( wxT("View"), XXO("&View"),
      Section( "Basic",
         Menu( wxT("Zoom"), XXO("&Zoom"),
            Section( "",
               Command( wxT("ZoomIn"), XXO("Zoom &In"), FN(OnZoomIn),
                  ZoomInAvailableFlag(), wxT("Ctrl+1") ),
               Command( wxT("ZoomNormal"), XXO("Zoom &Normal"), FN(OnZoomNormal),
                  TracksExistFlag(), wxT("Ctrl+2") ),
               Command( wxT("ZoomOut"), XXO("Zoom &Out"), FN(OnZoomOut),
                  ZoomOutAvailableFlag(), wxT("Ctrl+3") ),
               Command( wxT("ZoomSel"), XXO("&Zoom to Selection"), FN(OnZoomSel),
                  TimeSelectedFlag(), wxT("Ctrl+E") ),
               Command( wxT("ZoomToggle"), XXO("Zoom &Toggle"), FN(OnZoomToggle),
                  TracksExistFlag(), wxT("Shift+Z") )
            ),
            Section( "",
               Command( wxT("AdvancedVZoom"), XXO("Advanced &Vertical Zooming"),
                  FN(OnAdvancedVZoom), AlwaysEnabledFlag,
                  Options{}.CheckTest( wxT("/GUI/VerticalZooming"), false ) )
            )
         ),

         Menu( wxT("TrackSize"), XXO("T&rack Size"),
            Command( wxT("FitInWindow"), XXO("&Fit to Width"), FN(OnZoomFit),
               TracksExistFlag(), wxT("Ctrl+F") ),
            Command( wxT("FitV"), XXO("Fit to &Height"), FN(OnZoomFitV),
               TracksExistFlag(), wxT("Ctrl+Shift+F") ),
            Command( wxT("CollapseAllTracks"), XXO("&Collapse All Tracks"),
               FN(OnCollapseAllTracks), TracksExistFlag(), wxT("Ctrl+Shift+C") ),
            Command( wxT("ExpandAllTracks"), XXO("E&xpand Collapsed Tracks"),
               FN(OnExpandAllTracks), TracksExistFlag(), wxT("Ctrl+Shift+X") )
         ),

         Menu( wxT("SkipTo"), XXO("Sk&ip to"),
            Command( wxT("SkipSelStart"), XXO("Selection Sta&rt"),
               FN(OnGoSelStart), TimeSelectedFlag(),
               Options{ wxT("Ctrl+["), XO("Skip to Selection Start") } ),
            Command( wxT("SkipSelEnd"), XXO("Selection En&d"), FN(OnGoSelEnd),
               TimeSelectedFlag(),
               Options{ wxT("Ctrl+]"), XO("Skip to Selection End") } )
         )
      ),

      Section( "Windows" ),

      Section( "Other",
         Command( wxT("ShowExtraMenus"), XXO("&Extra Menus (on/off)"),
            FN(OnShowExtraMenus), AlwaysEnabledFlag,
            Options{}.CheckTest( wxT("/GUI/ShowExtraMenus"), false ) ),
         Command( wxT("ShowTrackNameInWaveform"), XXO("Track &Name (on/off)"),
            FN(OnShowNameOverlay), AlwaysEnabledFlag,
            Options{}.CheckTest( wxT("/GUI/ShowTrackNameInWaveform"), false ) ),
         Command( wxT("ShowClipping"), XXO("&Show Clipping (on/off)"),
            FN(OnShowClipping), AlwaysEnabledFlag,
            Options{}.CheckTest( wxT("/GUI/ShowClipping"), false ) )
   #if defined(EXPERIMENTAL_EFFECTS_RACK)
         ,
         Command( wxT("ShowEffectsRack"), XXO("Show Effects Rack"),
            FN(OnShowEffectsRack), AlwaysEnabledFlag,
            Options{}.CheckTest( [](AudacityProject &project){
               auto &rack = EffectRack::Get( project );
               return rack.IsShown(); } ) )
   #endif
      )
   ) ) };
   return menu;
   
}

AttachedItem sAttachment1{
   wxT(""),
   Shared( ViewMenu() )
};
}

#undef FN
