#include "AudioIO.h"
#include "../CommonCommandFlags.h"
#include "../MenuCreator.h"
#include "Prefs.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "ProjectHistory.h"
#include "Track.h"
#include "../TrackPanel.h"
#include "UndoManager.h"
#include "ViewInfo.h"
#include "Viewport.h"
#include "CommandContext.h"
#include "CommandManager.h"
#include "../prefs/GUIPrefs.h"
#include "../prefs/TracksPrefs.h"

#include <wx/app.h>
#include <wx/scrolbar.h>

#include <numeric>

#include "toolbars/ToolManager.h"

// private helper classes and functions
namespace {
double GetZoomOfSelection(const AudacityProject& project)
{
    auto& viewInfo = ViewInfo::Get(project);
    auto& viewport = Viewport::Get(project);

    const double lowerBound
        =std::max(viewInfo.selectedRegion.t0(),
                  viewport.ScrollingLowerBoundTime());
    const double denom
        =viewInfo.selectedRegion.t1() - lowerBound;
    if (denom <= 0.0) {
        return viewInfo.GetZoom();
    }

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

double GetZoomOfPreset(const AudacityProject& project, int preset)
{
    // Sets a limit on how far we will zoom out as a factor over zoom to fit.
    const double maxZoomOutFactor = 4.0;
    // Sets how many pixels we allow for one uint, such as seconds.
    const double pixelsPerUnit = 5.0;

    double result = 1.0;
    auto& viewport = Viewport::Get(project);
    double zoomToFit = viewport.GetZoomOfToFit();
    using namespace WaveChannelViewConstants;
    switch (preset) {
    default:
    case kZoomDefault:
        result = ZoomInfo::GetDefaultZoom();
        break;
    case kZoomToFit:
        result = zoomToFit;
        break;
    case kZoomToSelection:
        result = GetZoomOfSelection(project);
        break;
    case kZoomMinutes:
        result = pixelsPerUnit * 1.0 / 60;
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
    }
    if (result < (zoomToFit / maxZoomOutFactor)) {
        result = zoomToFit / maxZoomOutFactor;
    }
    return result;
}
}

namespace {
// Menu handler functions

void OnZoomIn(const CommandContext& context)
{
    auto& project = context.project;
    auto& trackPanel = TrackPanel::Get(project);
    auto& viewport = Viewport::Get(project);

    auto gAudioIO = AudioIO::Get();
    // LLL: Handling positioning differently when audio is
    // actively playing.  Don't do this if paused.
    if (gAudioIO->IsStreamActive(
            ProjectAudioIO::Get(project).GetAudioIOToken())
        && !gAudioIO->IsPaused()) {
        viewport.ZoomBy(2.0);
        viewport.ScrollIntoView(gAudioIO->GetStreamTime());
    } else {
        viewport.ZoomAboutSelection(2.0);
    }

    trackPanel.Refresh(false);
}

void OnZoomNormal(const CommandContext& context)
{
    auto& project = context.project;
    auto& trackPanel = TrackPanel::Get(project);
    auto& viewport = Viewport::Get(project);
    viewport.Zoom(ZoomInfo::GetDefaultZoom());
    trackPanel.Refresh(false);
}

void OnZoomOut(const CommandContext& context)
{
    auto& project = context.project;
    auto& viewport = Viewport::Get(project);
    viewport.ZoomAboutCenter(0.5);
}

void OnZoomSel(const CommandContext& context)
{
    auto& project = context.project;
    auto& selectedRegion = ViewInfo::Get(project).selectedRegion;
    auto& viewport = Viewport::Get(project);
    viewport.Zoom(GetZoomOfSelection(project));
    viewport.SetHorizontalThumb(selectedRegion.t0());
}

void OnZoomToggle(const CommandContext& context)
{
    auto& project = context.project;
    auto& viewInfo = ViewInfo::Get(project);
    auto& trackPanel = TrackPanel::Get(project);
    auto& viewport = Viewport::Get(project);

//   const double origLeft = viewInfo.h;
//   const double origWidth = viewInfo.GetScreenEndTime() - origLeft;

    // Choose the zoom that is most different to the current zoom.
    double Zoom1 = GetZoomOfPreset(project, TracksPrefs::Zoom1Choice());
    double Zoom2 = GetZoomOfPreset(project, TracksPrefs::Zoom2Choice());
    double Z = viewInfo.GetZoom();// Current Zoom.
    double ChosenZoom
        =fabs(log(Zoom1 / Z)) > fabs(log(Z / Zoom2)) ? Zoom1 : Zoom2;

    viewport.Zoom(ChosenZoom);
    trackPanel.Refresh(false);
//   const double newWidth = GetScreenEndTime() - viewInfo.h;
//   const double newh = origLeft + (origWidth - newWidth) / 2;
//   SetHorizontalThumb(newh);
}

void OnZoomFit(const CommandContext& context)
{
    auto& project = context.project;
    auto& viewport = Viewport::Get(project);
    viewport.ZoomFitHorizontally();
}

void OnZoomFitV(const CommandContext& context)
{
    auto& project = context.project;
    auto& viewport = Viewport::Get(project);
    viewport.ZoomFitVertically();
    ProjectHistory::Get(project).ModifyState(true);
}

void OnCollapseAllTracks(const CommandContext& context)
{
    auto& project = context.project;
    Viewport::Get(project).CollapseAllTracks();
    ProjectHistory::Get(project).ModifyState(true);
}

void OnExpandAllTracks(const CommandContext& context)
{
    auto& project = context.project;
    Viewport::Get(project).ExpandAllTracks();
    ProjectHistory::Get(project).ModifyState(true);
}

void OnGoSelStart(const CommandContext& context)
{
    auto& project = context.project;
    auto& viewInfo = ViewInfo::Get(project);
    auto& selectedRegion = viewInfo.selectedRegion;
    auto& viewport = Viewport::Get(project);

    if (selectedRegion.isPoint()) {
        return;
    }

    viewport.SetHorizontalThumb(
        selectedRegion.t0() - ((viewInfo.GetScreenEndTime() - viewInfo.hpos) / 2));
}

void OnGoSelEnd(const CommandContext& context)
{
    auto& project = context.project;
    auto& viewInfo = ViewInfo::Get(project);
    auto& selectedRegion = viewInfo.selectedRegion;
    auto& viewport = Viewport::Get(project);

    if (selectedRegion.isPoint()) {
        return;
    }

    viewport.SetHorizontalThumb(
        selectedRegion.t1() - ((viewInfo.GetScreenEndTime() - viewInfo.hpos) / 2));
}

void OnShowExtraMenus(const CommandContext& context)
{
    auto& project = context.project;
    auto& commandManager = CommandManager::Get(project);

    bool checked = !gPrefs->Read(wxT("/GUI/ShowExtraMenus"), 0L);
    gPrefs->Write(wxT("/GUI/ShowExtraMenus"), checked);
    gPrefs->Flush();
    commandManager.Check(wxT("ShowExtraMenus"), checked);
    MenuCreator::RebuildAllMenuBars();
}

void OnShowClipping(const CommandContext& context)
{
    auto& project = context.project;
    auto& commandManager = CommandManager::Get(project);
    auto& trackPanel = TrackPanel::Get(project);

    ShowClippingPref().Toggle();
    gPrefs->Flush();

    trackPanel.Refresh(false);
}

void OnShowRMS(const CommandContext& context)
{
    auto& project = context.project;
    auto& trackPanel = TrackPanel::Get(project);

    ShowRMSPref().Toggle();
    gPrefs->Flush();

    ToolManager::ModifyAllProjectToolbarMenus();

    trackPanel.Refresh(false);
}
} // namespace

// Menu definitions

// Under /MenuBar
namespace {
using namespace MenuRegistry;
auto ViewMenu()
{
    static auto menu = std::shared_ptr{
        Menu(wxT("View"), XXO("&View"),
             Section("Basic",
                     Menu(wxT("Zoom"), XXO("&Zoom"),
                          Command(wxT("ZoomIn"), XXO("Zoom &In"), OnZoomIn,
                                  ZoomInAvailableFlag(), wxT("Ctrl+1")),
                          Command(wxT("ZoomNormal"), XXO("Zoom &Normal"), OnZoomNormal,
                                  TracksExistFlag(), wxT("Ctrl+2")),
                          Command(wxT("ZoomOut"), XXO("Zoom &Out"), OnZoomOut,
                                  ZoomOutAvailableFlag(), wxT("Ctrl+3")),
                          Command(wxT("ZoomSel"), XXO("&Zoom to Selection"), OnZoomSel,
                                  TimeSelectedFlag(), wxT("Ctrl+E")),
                          Command(wxT("ZoomToggle"), XXO("Zoom &Toggle"), OnZoomToggle,
                                  TracksExistFlag(), wxT("Shift+Z"))
                          ),

                     Menu(wxT("TrackSize"), XXO("T&rack Size"),
                          Command(wxT("FitInWindow"), XXO("&Fit to Width"), OnZoomFit,
                                  TracksExistFlag(), wxT("Ctrl+F")),
                          Command(wxT("FitV"), XXO("Fit to &Height"), OnZoomFitV,
                                  TracksExistFlag(), wxT("Ctrl+Shift+F")),
                          Command(wxT("CollapseAllTracks"), XXO("&Collapse All Tracks"),
                                  OnCollapseAllTracks, TracksExistFlag(), wxT("Ctrl+Shift+C")),
                          Command(wxT("ExpandAllTracks"), XXO("E&xpand Collapsed Tracks"),
                                  OnExpandAllTracks, TracksExistFlag(), wxT("Ctrl+Shift+X"))
                          ),

                     Menu(wxT("SkipTo"), XXO("Sk&ip to"),
                          Command(wxT("SkipSelStart"), XXO("Selection Sta&rt"),
                                  OnGoSelStart, TimeSelectedFlag(),
                                  Options { wxT("Ctrl+["), XO("Skip to Selection Start") }),
                          Command(wxT("SkipSelEnd"), XXO("Selection En&d"), OnGoSelEnd,
                                  TimeSelectedFlag(),
                                  Options { wxT("Ctrl+]"), XO("Skip to Selection End") })
                          )
                     ),

             Section("Windows"),

             Section("Other",
                     Command(wxT("ShowExtraMenus"), XXO("E&xtra Menus"),
                             OnShowExtraMenus, AlwaysEnabledFlag,
                             Options {}.CheckTest(wxT("/GUI/ShowExtraMenus"), false)),
                     Command(wxT("ShowClipping"), XXO("&Show Clipping in Waveform"),
                             OnShowClipping, AlwaysEnabledFlag,
                             Options {}.CheckTest(ShowClippingPref())),
                     Command(wxT("ShowRMS"), XXO("Sho&w RMS in Waveform"),
                             OnShowRMS, AlwaysEnabledFlag,
                             Options {}.CheckTest(ShowRMSPref()))
                     )
             ) };
    return menu;
}

AttachedItem sAttachment1{ Indirect(ViewMenu()) };
}

#undef FN
