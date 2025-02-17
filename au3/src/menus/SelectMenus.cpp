#include "../AdornedRulerPanel.h"
#include "AudioIO.h"
#include "BasicUI.h"
#include "../CommonCommandFlags.h"
#include "Prefs.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "../ProjectAudioManager.h"
#include "ProjectHistory.h"
#include "ProjectRate.h"
#include "ProjectSnap.h"
#include "../ProjectWindows.h"
#include "../SelectUtilities.h"
#include "SyncLock.h"
#include "../TrackPanel.h"
#include "Viewport.h"
#include "WaveClip.h"
#include "WaveTrack.h"
#include "LabelTrack.h"
#include "CommandContext.h"
#include "MenuRegistry.h"
#include "../toolbars/ControlToolBar.h"
#include "../tracks/ui/SelectHandle.h"
#include "../tracks/labeltrack/ui/LabelTrackView.h"
#include "../tracks/playabletrack/wavetrack/ui/WaveChannelView.h"

// private helper classes and functions
namespace {
constexpr auto GetWindowSize(double projectRate)
{
    return size_t(std::max(1.0, projectRate / 100));
}

double NearestZeroCrossing(AudacityProject& project, double t0)
{
    auto rate = ProjectRate::Get(project).GetRate();
    auto& tracks = TrackList::Get(project);

    // Window is 1/100th of a second.
    auto windowSize = GetWindowSize(rate);
    Floats dist{ windowSize, true };

    int nTracks = 0;
    for (auto one : tracks.Selected<const WaveTrack>()) {
        const auto nChannels = one->NChannels();
        auto oneWindowSize = size_t(std::max(1.0, one->GetRate() / 100));
        Floats buffer1{ oneWindowSize };
        Floats buffer2{ oneWindowSize };
        float* const buffers[]{ buffer1.get(), buffer2.get() };
        auto s = one->TimeToLongSamples(t0);

        // fillTwo to ensure that missing values are treated as 2, and hence do
        // not get used as zero crossings.
        one->GetFloats(0, nChannels, buffers,
                       s - (int)oneWindowSize / 2, oneWindowSize, false, FillFormat::fillTwo);

        // Looking for actual crossings.  Update dist
        for (size_t iChannel = 0; iChannel < nChannels; ++iChannel) {
            const auto oneDist = buffers[iChannel];
            double prev = 2.0;
            for (size_t i = 0; i < oneWindowSize; ++i) {
                float fDist = fabs(oneDist[i]); // score is absolute value
                if (prev * oneDist[i] > 0) { // both same sign?  No good.
                    fDist = fDist + 0.4; // No good if same sign.
                } else if (prev > 0.0) {
                    fDist = fDist + 0.1; // medium penalty for downward crossing.
                }
                prev = oneDist[i];
                oneDist[i] = fDist;
            }

            // TODO: The mixed rate zero crossing code is broken,
            // if oneWindowSize > windowSize we'll miss out some
            // samples - so they will still be zero, so we'll use them.
            for (size_t i = 0; i < windowSize; i++) {
                size_t j;
                if (windowSize != oneWindowSize) {
                    j = i * (oneWindowSize - 1) / (windowSize - 1);
                } else {
                    j = i;
                }

                dist[i] += oneDist[j];
                // Apply a small penalty for distance from the original endpoint
                // We'll always prefer an upward
                dist[i]
                    +=0.1 * (abs(int(i) - int(windowSize / 2))) / float(windowSize / 2);
            }
        }
        nTracks++;
    }

    // Find minimum
    int argmin = 0;
    float min = 3.0;
    for (size_t i = 0; i < windowSize; ++i) {
        if (dist[i] < min) {
            argmin = i;
            min = dist[i];
        }
    }

    // If we're worse than 0.2 on average, on one track, then no good.
    if ((nTracks == 1) && (min > (0.2 * nTracks))) {
        return t0;
    }
    // If we're worse than 0.6 on average, on multi-track, then no good.
    if ((nTracks > 1) && (min > (0.6 * nTracks))) {
        return t0;
    }

    return t0 + (argmin - (int)windowSize / 2) / rate;
}

// If this returns true, then there was a key up, and nothing more to do,
// after this function has completed.
// (at most this function just does a ModifyState for the keyup)
bool OnlyHandleKeyUp(const CommandContext& context)
{
    auto& project = context.project;
    auto evt = context.pEvt;
    bool bKeyUp = (evt) && evt->GetEventType() == wxEVT_KEY_UP;

    if (ProjectAudioIO::Get(project).IsAudioActive()) {
        return bKeyUp;
    }
    if (!bKeyUp) {
        return false;
    }

    ProjectHistory::Get(project).ModifyState(false);
    return true;
}

enum CursorDirection {
    DIRECTION_LEFT = -1,
    DIRECTION_RIGHT = +1
};

enum SelectionOperation {
    SELECTION_EXTEND,
    SELECTION_CONTRACT,
    CURSOR_MOVE
};

enum TimeUnit {
    TIME_UNIT_SECONDS,
    TIME_UNIT_PIXELS
};

struct SeekInfo
{
    wxLongLong mLastSelectionAdjustment { ::wxGetUTCTimeMillis() };
    double mSeekShort{ 0.0 };
    double mSeekLong{ 0.0 };
};

void SeekWhenAudioActive(double seekStep, wxLongLong& lastSelectionAdjustment)
{
    auto gAudioIO = AudioIO::Get();

    lastSelectionAdjustment = ::wxGetUTCTimeMillis();

    gAudioIO->SeekStream(seekStep);
}

// Handles moving a selection edge with the keyboard in snap-to-time mode;
// returns the moved value.
// Will move at least minPix pixels -- set minPix positive to move forward,
// negative to move backward.
// Helper for moving by keyboard with snap-to-grid enabled
double GridMove
    (AudacityProject& project, double t, int minPix)
{
    auto& projectSnap = ProjectSnap::Get(project);
    auto& viewInfo = ViewInfo::Get(project);

    auto result = projectSnap.SingleStep(t, minPix >= 0).time;

    if (
        std::abs(viewInfo.TimeToPosition(result) - viewInfo.TimeToPosition(t))
        >= abs(minPix)) {
        return result;
    }

    // Otherwise, move minPix pixels, then snap to the time.
    result = viewInfo.OffsetTimeByPixels(t, minPix);
    return projectSnap.SnapTime(result).time;
}

double OffsetTime
    (AudacityProject& project,
    double t, double offset, TimeUnit timeUnit, SnapMode snapMode)
{
    auto& viewInfo = ViewInfo::Get(project);

    if (timeUnit == TIME_UNIT_SECONDS) {
        return t + offset; // snapping is currently ignored for non-pixel moves
    }
    if (snapMode == SnapMode::SNAP_OFF) {
        return viewInfo.OffsetTimeByPixels(t, (int)offset);
    }

    return GridMove(project, t, (int)offset);
}

// Moving a cursor, and collapsed selection.
void MoveWhenAudioInactive
    (AudacityProject& project, double seekStep, TimeUnit timeUnit)
{
    auto& viewInfo = ViewInfo::Get(project);
    auto& trackPanel = TrackPanel::Get(project);
    auto& tracks = TrackList::Get(project);
    auto& ruler = AdornedRulerPanel::Get(project);
    const auto& settings = ProjectSnap::Get(project);
    auto& viewport = Viewport::Get(project);

    // If TIME_UNIT_SECONDS, snap-to will be off.
    auto snapMode = settings.GetSnapMode();

    // Move the cursor
    // Already in cursor mode?
    if (viewInfo.selectedRegion.isPoint()) {
        double newT = OffsetTime(
            project, viewInfo.selectedRegion.t0(), seekStep, timeUnit, snapMode);
        // constrain.
        newT = std::max(0.0, newT);
        // Move
        viewInfo.selectedRegion.setT0(
            newT,
            false); // do not swap selection boundaries
        viewInfo.selectedRegion.collapseToT0();

        // Move the visual cursor, avoiding an unnecessary complete redraw
        trackPanel.DrawOverlays(false);
        ruler.DrawOverlays(false);
    } else {
        // Transition to cursor mode.
        constexpr auto maySwapBoundaries = false;
        if (seekStep < 0) {
            if (snapMode != SnapMode::SNAP_OFF) {
                viewInfo.selectedRegion.setT0(
                    settings.SnapTime(viewInfo.selectedRegion.t0()).time,
                    maySwapBoundaries);
            }
            viewInfo.selectedRegion.collapseToT0();
        } else {
            if (snapMode != SnapMode::SNAP_OFF) {
                viewInfo.selectedRegion.setT1(
                    settings.SnapTime(viewInfo.selectedRegion.t1()).time,
                    maySwapBoundaries);
            }
            viewInfo.selectedRegion.collapseToT1();
        }
        trackPanel.Refresh(false);
    }

    // Make sure NEW position is in view
    viewport.ScrollIntoView(viewInfo.selectedRegion.t1());
    return;
}

void SeekWhenAudioInactive
    (AudacityProject& project, double seekStep, TimeUnit timeUnit,
    SelectionOperation operation)
{
    auto& viewInfo = ViewInfo::Get(project);
    auto& tracks = TrackList::Get(project);
    const auto& settings = ProjectSnap::Get(project);
    auto& viewport = Viewport::Get(project);

    if (operation == CURSOR_MOVE) {
        MoveWhenAudioInactive(project, seekStep, timeUnit);
        return;
    }

    auto snapMode = settings.GetSnapMode();
    const double t0 = viewInfo.selectedRegion.t0();
    const double t1 = viewInfo.selectedRegion.t1();
    const double end = std::max(
        tracks.GetEndTime(), viewInfo.GetScreenEndTime());

    // Is it t0 or t1 moving?
    bool bMoveT0 = (operation == SELECTION_CONTRACT && seekStep > 0)
                   || (operation == SELECTION_EXTEND && seekStep < 0);
    // newT is where we want to move to
    double newT = OffsetTime(project,
                             bMoveT0 ? t0 : t1, seekStep, timeUnit, snapMode);
    // constrain to be in the track/screen limits.
    newT = std::max(0.0, newT);
    newT = std::min(newT, end);
    // optionally constrain to be a contraction, i.e. so t0/t1 do not cross over
    if (operation == SELECTION_CONTRACT) {
        newT = bMoveT0 ? std::min(t1, newT) : std::max(t0, newT);
    }

    // Actually move
    if (bMoveT0) {
        viewInfo.selectedRegion.setT0(newT);
    } else {
        viewInfo.selectedRegion.setT1(newT);
    }

    // Ensure it is visible
    viewport.ScrollIntoView(newT);
}

// Handle small cursor and play head movements
void SeekLeftOrRight
    (AudacityProject& project, double direction, SelectionOperation operation,
    SeekInfo& info)
{
    // PRL:  What I found and preserved, strange though it be:
    // During playback:  jump depends on preferences and is independent of the
    // zoom and does not vary if the key is held
    // Else: jump depends on the zoom and gets bigger if the key is held

    if (ProjectAudioIO::Get(project).IsAudioActive()) {
        if (operation == CURSOR_MOVE) {
            SeekWhenAudioActive(info.mSeekShort * direction,
                                info.mLastSelectionAdjustment);
        } else if (operation == SELECTION_EXTEND) {
            SeekWhenAudioActive(info.mSeekLong * direction,
                                info.mLastSelectionAdjustment);
        }
        // Note: no action for CURSOR_CONTRACT
        return;
    }

    // If the last adjustment was very recent, we are
    // holding the key down and should move faster.
    const wxLongLong curtime = ::wxGetUTCTimeMillis();
    enum {
        MIN_INTERVAL = 50
    };
    const bool fast
        =(curtime - info.mLastSelectionAdjustment < MIN_INTERVAL);

    info.mLastSelectionAdjustment = curtime;

    // How much faster should the cursor move if shift is down?
    enum {
        LARGER_MULTIPLIER = 4
    };
    const double seekStep = (fast ? LARGER_MULTIPLIER : 1.0) * direction;

    SeekWhenAudioInactive(project, seekStep, TIME_UNIT_PIXELS, operation);
}

// Move the cursor forward or backward, while paused or while playing.
void DoCursorMove(
    AudacityProject& project, double seekStep,
    wxLongLong& lastSelectionAdjustment)
{
    if (ProjectAudioIO::Get(project).IsAudioActive()) {
        SeekWhenAudioActive(seekStep, lastSelectionAdjustment);
    } else {
        lastSelectionAdjustment = ::wxGetUTCTimeMillis();
        MoveWhenAudioInactive(project, seekStep, TIME_UNIT_SECONDS);
    }

    ProjectHistory::Get(project).ModifyState(false);
}

void DoBoundaryMove(AudacityProject& project, int step, SeekInfo& info)
{
    auto& viewInfo = ViewInfo::Get(project);
    auto& tracks = TrackList::Get(project);
    auto& viewport = Viewport::Get(project);

    // step is negative, then is moving left.  step positive, moving right.
    // Move the left/right selection boundary, to expand the selection

    // If the last adjustment was very recent, we are
    // holding the key down and should move faster.
    wxLongLong curtime = ::wxGetUTCTimeMillis();
    int pixels = step;
    if (curtime - info.mLastSelectionAdjustment < 50) {
        pixels *= 4;
    }
    info.mLastSelectionAdjustment = curtime;

    // we used to have a parameter boundaryContract to say if expanding or
    // contracting.  it is no longer needed.
    bool bMoveT0 = (step < 0);// ^ boundaryContract ;

    if (ProjectAudioIO::Get(project).IsAudioActive()) {
        auto gAudioIO = AudioIO::Get();
        double indicator = gAudioIO->GetStreamTime();
        if (bMoveT0) {
            viewInfo.selectedRegion.setT0(indicator, false);
        } else {
            viewInfo.selectedRegion.setT1(indicator);
        }

        ProjectHistory::Get(project).ModifyState(false);
        return;
    }

    const double t0 = viewInfo.selectedRegion.t0();
    const double t1 = viewInfo.selectedRegion.t1();
    const double end = std::max(
        tracks.GetEndTime(), viewInfo.GetScreenEndTime());

    double newT = viewInfo.OffsetTimeByPixels(bMoveT0 ? t0 : t1, pixels);
    // constrain to be in the track/screen limits.
    newT = std::max(0.0, newT);
    newT = std::min(newT, end);
    // optionally constrain to be a contraction, i.e. so t0/t1 do not cross over
    //if( boundaryContract )
    //   newT = bMoveT0 ? std::min( t1, newT ) : std::max( t0, newT );

    // Actually move
    if (bMoveT0) {
        viewInfo.selectedRegion.setT0(newT);
    } else {
        viewInfo.selectedRegion.setT1(newT);
    }

    // Ensure it is visible
    viewport.ScrollIntoView(newT);

    ProjectHistory::Get(project).ModifyState(false);
}
}

namespace SelectActions {
// Menu handler functions

struct Handler : CommandHandlerObject, // MUST be the first base class!
    ClientData::Base, PrefsListener
{
    void OnSelectAll(const CommandContext& context)
    {
        auto& trackPanel = TrackPanel::Get(context.project);
        auto& tracks = TrackList::Get(context.project);

        for (auto lt : tracks.Selected<LabelTrack>()) {
            auto& view = LabelTrackView::Get(*lt);
            if (view.SelectAllText(context.project)) {
                trackPanel.Refresh(false);
                return;
            }
        }

        //Presumably, there might be not more than one track
        //that expects text input
        for (auto wt : tracks.Any<WaveTrack>()) {
            auto& view = WaveChannelView::GetFirst(*wt);
            if (view.SelectAllText(context.project)) {
                trackPanel.Refresh(false);
                return;
            }
        }

        SelectUtilities::DoSelectAll(context.project);
    }

    void OnSelectNone(const CommandContext& context)
    {
        auto& project = context.project;
        auto& selectedRegion = ViewInfo::Get(project).selectedRegion;

        selectedRegion.collapseToT0();
        SelectUtilities::SelectNone(project);
        ProjectHistory::Get(project).ModifyState(false);
    }

    void OnSelectAllTracks(const CommandContext& context)
    {
        auto& project = context.project;
        SelectUtilities::DoSelectTimeAndTracks(project, false, true);
    }

    void OnSelectSyncLockSel(const CommandContext& context)
    {
        auto& project = context.project;
        auto& tracks = TrackList::Get(project);

        bool selected = false;
        for (auto t : tracks.Any() + &Track::SupportsBasicEditing
             + &SyncLock::IsSyncLockSelectedP - &Track::IsSelected) {
            t->SetSelected(true);
            selected = true;
        }

        if (selected) {
            ProjectHistory::Get(project).ModifyState(false);
        }
    }

    void OnSetLeftSelection(const CommandContext& context)
    {
        SelectUtilities::OnSetRegion(context.project,
                                     true, true, XO("Set Left Selection Boundary"));
    }

    void OnSetRightSelection(const CommandContext& context)
    {
        SelectUtilities::OnSetRegion(context.project,
                                     false, true, XO("Set Right Selection Boundary"));
    }

    void OnSelectStartCursor(const CommandContext& context)
    {
        auto& project = context.project;
        auto& tracks = TrackList::Get(project);
        auto& selectedRegion = ViewInfo::Get(project).selectedRegion;

        double kWayOverToRight = std::numeric_limits<double>::max();

        auto range = tracks.Selected();
        if (!range) {
            return;
        }

        double minOffset = range.min(&Track::GetStartTime);

        if (minOffset
            >= (kWayOverToRight * (1 - std::numeric_limits<double>::epsilon()))) {
            return;
        }

        selectedRegion.setT0(minOffset);
        ProjectHistory::Get(project).ModifyState(false);
    }

    void OnSelectCursorEnd(const CommandContext& context)
    {
        auto& project = context.project;
        auto& tracks = TrackList::Get(project);
        auto& selectedRegion = ViewInfo::Get(project).selectedRegion;

        double kWayOverToLeft = std::numeric_limits<double>::lowest();

        auto range = tracks.Selected();
        if (!range) {
            return;
        }

        double maxEndOffset = range.max(&Track::GetEndTime);

        if (maxEndOffset
            <= (kWayOverToLeft * (1 - std::numeric_limits<double>::epsilon()))) {
            return;
        }

        selectedRegion.setT1(maxEndOffset);
        ProjectHistory::Get(project).ModifyState(false);
    }

    void OnSelectTrackStartToEnd(const CommandContext& context)
    {
        auto& project = context.project;
        auto& viewInfo = ViewInfo::Get(project);
        auto& tracks = TrackList::Get(project);

        auto range = tracks.Selected();
        double maxEndOffset = range.max(&Track::GetEndTime);
        double minOffset = range.min(&Track::GetStartTime);

        if (maxEndOffset < minOffset) {
            return;
        }

        viewInfo.selectedRegion.setTimes(minOffset, maxEndOffset);
        ProjectHistory::Get(project).ModifyState(false);
    }

// Handler state:
    SelectedRegion mRegionSave{};

    void OnSelectionSave(const CommandContext& context)
    {
        auto& project = context.project;
        auto& selectedRegion = ViewInfo::Get(project).selectedRegion;

        mRegionSave = selectedRegion;
    }

    void OnSelectionRestore(const CommandContext& context)
    {
        auto& project = context.project;
        auto& selectedRegion = ViewInfo::Get(project).selectedRegion;
        auto& viewport = Viewport::Get(project);

        if ((mRegionSave.t0() == 0.0)
            && (mRegionSave.t1() == 0.0)) {
            return;
        }

        selectedRegion = mRegionSave;
        viewport.ScrollIntoView(selectedRegion.t0());

        ProjectHistory::Get(project).ModifyState(false);
    }

// Handler state:
    bool mCursorPositionHasBeenStored{ false };
    double mCursorPositionStored{ 0.0 };

    void OnSelectCursorStoredCursor(const CommandContext& context)
    {
        auto& project = context.project;
        auto& selectedRegion = ViewInfo::Get(project).selectedRegion;
        auto isAudioActive = ProjectAudioIO::Get(project).IsAudioActive();

        if (mCursorPositionHasBeenStored) {
            auto gAudioIO = AudioIO::Get();
            double cursorPositionCurrent = isAudioActive
                                           ? gAudioIO->GetStreamTime()
                                           : selectedRegion.t0();
            selectedRegion.setTimes(
                std::min(cursorPositionCurrent, mCursorPositionStored),
                std::max(cursorPositionCurrent, mCursorPositionStored));

            ProjectHistory::Get(project).ModifyState(false);
        }
    }

    void OnCursorPositionStore(const CommandContext& context)
    {
        auto& project = context.project;
        auto& selectedRegion = ViewInfo::Get(project).selectedRegion;
        auto isAudioActive = ProjectAudioIO::Get(project).IsAudioActive();

        auto gAudioIO = AudioIO::Get();
        mCursorPositionStored
            =isAudioActive ? gAudioIO->GetStreamTime() : selectedRegion.t0();
        mCursorPositionHasBeenStored = true;
    }

    void OnZeroCrossing(const CommandContext& context)
    {
        auto& project = context.project;
        auto& selectedRegion = ViewInfo::Get(project).selectedRegion;
        const auto& tracks = TrackList::Get(project);

        // Selecting precise sample indices across tracks that may have clips with
        // various stretch ratios in itself is not possible. Even in single-track
        // mode, we cannot know what the final waveform will look like until
        // stretching is applied, making this operation futile. Hence we disallow
        // it if any stretched clip is involved.
        const auto projectRate = ProjectRate(project).GetRate();
        const auto searchWindowDuration = GetWindowSize(projectRate) / projectRate;
        const auto wouldSearchClipWithPitchOrSpeed
            =[searchWindowDuration](const WaveTrack& track, double t)
        {
            const auto clips = track.GetSortedClipsIntersecting(
                t - searchWindowDuration / 2, t + searchWindowDuration / 2);
            return any_of(
                clips.begin(), clips.end(),
                [](const auto& clip) { return clip->HasPitchOrSpeed(); });
        };
        const auto selected = tracks.Selected<const WaveTrack>();
        if (std::any_of(
                selected.begin(), selected.end(), [&](const WaveTrack* track) {
            return wouldSearchClipWithPitchOrSpeed(
                *track, selectedRegion.t0())
                   || wouldSearchClipWithPitchOrSpeed(
                *track, selectedRegion.t1());
        })) {
            using namespace BasicUI;
            ShowMessageBox(
                XO("Zero-crossing search regions intersect stretched clip(s)."),
                MessageBoxOptions {}.Caption(XO("Error")).IconStyle(Icon::Error));
            return;
        }

        const double t0 = NearestZeroCrossing(project, selectedRegion.t0());
        if (selectedRegion.isPoint()) {
            selectedRegion.setTimes(t0, t0);
        } else {
            const double t1 = NearestZeroCrossing(project, selectedRegion.t1());
            // Empty selection is generally not much use, so do not make it if empty.
            if (fabs(t1 - t0) * ProjectRate::Get(project).GetRate() > 1.5) {
                selectedRegion.setTimes(t0, t1);
            }
        }

        ProjectHistory::Get(project).ModifyState(false);
    }

    void OnSnapToOff(const CommandContext& context)
    {
        auto& project = context.project;
        ProjectSnap::Get(project).SetSnapMode(SnapMode::SNAP_OFF);
    }

    void OnSnapToNearest(const CommandContext& context)
    {
        auto& project = context.project;
        ProjectSnap::Get(project).SetSnapMode(SnapMode::SNAP_NEAREST);
    }

    void OnSnapToPrior(const CommandContext& context)
    {
        auto& project = context.project;
        ProjectSnap::Get(project).SetSnapMode(SnapMode::SNAP_PRIOR);
    }

    void OnSelToStart(const CommandContext& context)
    {
        auto& project = context.project;
        auto& viewport = Viewport::Get(project);
        viewport.ScrollToStart(true);
        ProjectHistory::Get(project).ModifyState(false);
    }

    void OnSelToEnd(const CommandContext& context)
    {
        auto& project = context.project;
        auto& viewport = Viewport::Get(project);
        viewport.ScrollToEnd(true);
        ProjectHistory::Get(project).ModifyState(false);
    }

// Handler state:
    SeekInfo mSeekInfo;

    void OnSelExtendLeft(const CommandContext& context)
    {
        if (!OnlyHandleKeyUp(context)) {
            SeekLeftOrRight(context.project, DIRECTION_LEFT, SELECTION_EXTEND,
                            mSeekInfo);
        }
    }

    void OnSelExtendRight(const CommandContext& context)
    {
        if (!OnlyHandleKeyUp(context)) {
            SeekLeftOrRight(context.project, DIRECTION_RIGHT, SELECTION_EXTEND,
                            mSeekInfo);
        }
    }

    void OnSelSetExtendLeft(const CommandContext& context)
    {
        DoBoundaryMove(context.project, DIRECTION_LEFT, mSeekInfo);
    }

    void OnSelSetExtendRight(const CommandContext& context)
    {
        DoBoundaryMove(context.project, DIRECTION_RIGHT, mSeekInfo);
    }

    void OnSelContractLeft(const CommandContext& context)
    {
        if (!OnlyHandleKeyUp(context)) {
            SeekLeftOrRight(context.project, DIRECTION_RIGHT, SELECTION_CONTRACT,
                            mSeekInfo);
        }
    }

    void OnSelContractRight(const CommandContext& context)
    {
        if (!OnlyHandleKeyUp(context)) {
            SeekLeftOrRight(context.project, DIRECTION_LEFT, SELECTION_CONTRACT,
                            mSeekInfo);
        }
    }

    void OnCursorSelStart(const CommandContext& context)
    {
        auto& project = context.project;
        auto& selectedRegion = ViewInfo::Get(project).selectedRegion;
        auto& viewport = Viewport::Get(project);

        selectedRegion.collapseToT0();
        ProjectHistory::Get(project).ModifyState(false);
        viewport.ScrollIntoView(selectedRegion.t0());
    }

    void OnCursorSelEnd(const CommandContext& context)
    {
        auto& project = context.project;
        auto& selectedRegion = ViewInfo::Get(project).selectedRegion;
        auto& viewport = Viewport::Get(project);

        selectedRegion.collapseToT1();
        ProjectHistory::Get(project).ModifyState(false);
        viewport.ScrollIntoView(selectedRegion.t1());
    }

    void OnCursorTrackStart(const CommandContext& context)
    {
        auto& project = context.project;
        auto& tracks = TrackList::Get(project);
        auto& selectedRegion = ViewInfo::Get(project).selectedRegion;
        auto& viewport = Viewport::Get(project);

        double kWayOverToRight = std::numeric_limits<double>::max();

        auto trackRange = tracks.Selected() + &Track::SupportsBasicEditing;
        if (trackRange.empty()) {
            // This should have been prevented by command manager
            return;
        }

        // Range is surely nonempty now
        auto minOffset = std::max(0.0, trackRange.min(&Track::GetStartTime));

        if (minOffset
            >= (kWayOverToRight * (1 - std::numeric_limits<double>::epsilon()))) {
            return;
        }

        selectedRegion.setTimes(minOffset, minOffset);
        ProjectHistory::Get(project).ModifyState(false);
        viewport.ScrollIntoView(selectedRegion.t0());
    }

    void OnCursorTrackEnd(const CommandContext& context)
    {
        auto& project = context.project;
        auto& tracks = TrackList::Get(project);
        auto& selectedRegion = ViewInfo::Get(project).selectedRegion;
        auto& viewport = Viewport::Get(project);

        double kWayOverToLeft = std::numeric_limits<double>::lowest();

        auto trackRange = tracks.Selected() + &Track::SupportsBasicEditing;
        if (trackRange.empty()) {
            // This should have been prevented by command manager
            return;
        }

        // Range is surely nonempty now
        auto maxEndOffset = trackRange.max(&Track::GetEndTime);

        if (maxEndOffset
            < (kWayOverToLeft * (1 - std::numeric_limits<double>::epsilon()))) {
            return;
        }

        selectedRegion.setTimes(maxEndOffset, maxEndOffset);
        ProjectHistory::Get(project).ModifyState(false);
        viewport.ScrollIntoView(selectedRegion.t1());
    }

    void OnSkipStart(const CommandContext& context)
    {
        auto& project = context.project;
        wxCommandEvent evt;

        auto& controlToolBar = ControlToolBar::Get(project);
        controlToolBar.OnRewind(evt);
        ProjectHistory::Get(project).ModifyState(false);
    }

    void OnSkipEnd(const CommandContext& context)
    {
        auto& project = context.project;
        wxCommandEvent evt;

        auto& controlToolBar = ControlToolBar::Get(project);
        controlToolBar.OnFF(evt);
        ProjectHistory::Get(project).ModifyState(false);
    }

    void OnCursorLeft(const CommandContext& context)
    {
        if (!OnlyHandleKeyUp(context)) {
            SeekLeftOrRight(context.project, DIRECTION_LEFT, CURSOR_MOVE,
                            mSeekInfo);
        }
    }

    void OnCursorRight(const CommandContext& context)
    {
        if (!OnlyHandleKeyUp(context)) {
            SeekLeftOrRight(context.project, DIRECTION_RIGHT, CURSOR_MOVE,
                            mSeekInfo);
        }
    }

    void OnCursorShortJumpLeft(const CommandContext& context)
    {
        DoCursorMove(context.project,
                     -mSeekInfo.mSeekShort, mSeekInfo.mLastSelectionAdjustment);
    }

    void OnCursorShortJumpRight(const CommandContext& context)
    {
        DoCursorMove(context.project,
                     mSeekInfo.mSeekShort, mSeekInfo.mLastSelectionAdjustment);
    }

    void OnCursorLongJumpLeft(const CommandContext& context)
    {
        DoCursorMove(context.project,
                     -mSeekInfo.mSeekLong, mSeekInfo.mLastSelectionAdjustment);
    }

    void OnCursorLongJumpRight(const CommandContext& context)
    {
        DoCursorMove(context.project,
                     mSeekInfo.mSeekLong, mSeekInfo.mLastSelectionAdjustment);
    }

    void OnSeekLeftShort(const CommandContext& context)
    {
        auto& project = context.project;
        SeekLeftOrRight(project, DIRECTION_LEFT, CURSOR_MOVE, mSeekInfo);
    }

    void OnSeekRightShort(const CommandContext& context)
    {
        auto& project = context.project;
        SeekLeftOrRight(project, DIRECTION_RIGHT, CURSOR_MOVE, mSeekInfo);
    }

    void OnSeekLeftLong(const CommandContext& context)
    {
        auto& project = context.project;
        SeekLeftOrRight(project, DIRECTION_LEFT, SELECTION_EXTEND, mSeekInfo);
    }

    void OnSeekRightLong(const CommandContext& context)
    {
        auto& project = context.project;
        SeekLeftOrRight(project, DIRECTION_RIGHT, SELECTION_EXTEND, mSeekInfo);
    }

#if 1
// Legacy functions, not used as of version 2.3.0
    void OnSelectAllTime(const CommandContext& context)
    {
        auto& project = context.project;
        SelectUtilities::DoSelectTimeAndTracks(project, true, false);
    }

#endif

    void UpdatePrefs() override
    {
        gPrefs->Read(wxT("/AudioIO/SeekShortPeriod"), &mSeekInfo.mSeekShort, 1.0);
        gPrefs->Read(wxT("/AudioIO/SeekLongPeriod"), &mSeekInfo.mSeekLong, 15.0);
    }

    Handler()
    {
        UpdatePrefs();
    }

    Handler(const Handler&) = delete;
    Handler& operator=(const Handler&) = delete;
}; // struct Handler
} // namespace

// Handler is stateful.  Needs a factory registered with
// AudacityProject.
static const AudacityProject::AttachedObjects::RegisteredFactory key{
    [](AudacityProject&) {
        return std::make_unique< SelectActions::Handler >();
    } };

static CommandHandlerObject& findCommandHandler(AudacityProject& project)
{
    return project.AttachedObjects::Get< SelectActions::Handler >(key);
}

// Menu definitions

#define FN(X) (&SelectActions::Handler :: X)

namespace {
using namespace MenuRegistry;
auto SelectMenu()
{
    static auto menu = std::shared_ptr{
        (FinderScope{ findCommandHandler },
         /* i18n-hint: (verb) It's an item on a menu. */
         Menu(wxT("Select"), XXO("&Select"),
              Section("Basic",
                      Command(wxT("SelectAll"), XXO("&All"), FN(OnSelectAll),
                              TracksExistFlag(),
                              Options { wxT("Ctrl+A"), XO("Select All") }),
                      Command(wxT("SelectNone"), XXO("&None"), FN(OnSelectNone),
                              TracksExistFlag(),
                              Options { wxT("Ctrl+Shift+A"), XO("Select None") }),

                      //////////////////////////////////////////////////////////////////////////

                      Menu(wxT("Tracks"), XXO("&Tracks"),
                           Command(wxT("SelAllTracks"), XXO("In All &Tracks"),
                                   FN(OnSelectAllTracks),
                                   TracksExistFlag(),
                                   wxT("Ctrl+Shift+K")),
                           Command(wxT("SelSyncLockTracks"), XXO("In All &Sync-Locked Tracks"),
                                   FN(OnSelectSyncLockSel),
                                   EditableTracksSelectedFlag() | IsSyncLockedFlag(),
                                   Options { wxT("Ctrl+Shift+Y"), XO("Select Sync-Locked") })
                           ),

                      //////////////////////////////////////////////////////////////////////////

                      Menu(wxT("Region"), XXO("R&egion"),
                           Section("",
                                   Command(wxT("SetLeftSelection"), XXO("&Left at Playback Position"),
                                           FN(OnSetLeftSelection), TracksExistFlag(),
                                           Options { wxT("["), XO("Set Selection Left at Play Position") }),
                                   Command(wxT("SetRightSelection"), XXO("&Right at Playback Position"),
                                           FN(OnSetRightSelection), TracksExistFlag(),
                                           Options { wxT("]"), XO("Set Selection Right at Play Position") }),
                                   Command(wxT("SelTrackStartToCursor"), XXO("Track &Start to Cursor"),
                                           FN(OnSelectStartCursor), AlwaysEnabledFlag,
                                           Options { wxT("Shift+J"), XO("Select Track Start to Cursor") }),
                                   Command(wxT("SelCursorToTrackEnd"), XXO("Cursor to Track &End"),
                                           FN(OnSelectCursorEnd), AlwaysEnabledFlag,
                                           Options { wxT("Shift+K"), XO("Select Cursor to Track End") }),
                                   Command(wxT("SelTrackStartToEnd"), XXO("Track Start to En&d"),
                                           FN(OnSelectTrackStartToEnd), AlwaysEnabledFlag,
                                           Options {}.LongName(XO("Select Track Start to End")))
                                   ),

                           Section("",
                                   // GA: Audacity had 'Store Re&gion' here previously. There is no
                                   // one-step way to restore the 'Saved Cursor Position' in Select Menu,
                                   // so arguably using the word 'Selection' to do duty for both saving
                                   // the region or the cursor is better. But it does not belong in a
                                   // 'Region' submenu.
                                   Command(wxT("SelSave"), XXO("S&tore Selection"), FN(OnSelectionSave),
                                           WaveTracksSelectedFlag()),
                                   // Audacity had 'Retrieve Regio&n' here previously.
                                   Command(wxT("SelRestore"), XXO("Retrieve Selectio&n"),
                                           FN(OnSelectionRestore), TracksExistFlag())
                                   )
                           )

                      //////////////////////////////////////////////////////////////////////////

                      ),

              Section("",
                      Command(wxT("SelCursorStoredCursor"),
                              XXO("Cursor to Stored &Cursor Position"),
                              FN(OnSelectCursorStoredCursor), TracksExistFlag(),
                              Options {}.LongName(XO("Select Cursor to Stored"))),

                      Command(wxT("StoreCursorPosition"), XXO("Store Cursor Pos&ition"),
                              FN(OnCursorPositionStore),
                              WaveTracksExistFlag())
                      // Save cursor position is used in some selections.
                      // Maybe there should be a restore for it?
                      ),

              Section("",
                      Command(wxT("ZeroCross"), XXO("At &Zero Crossings"),
                              FN(OnZeroCrossing), EditableTracksSelectedFlag(),
                              Options { wxT("Z"), XO("Select Zero Crossing") })
                      )
              )) };
    return menu;
}

AttachedItem sAttachment1{ Indirect(SelectMenu()) };

auto ExtraSelectionMenu()
{
    static auto menu = std::shared_ptr{
        (FinderScope{ findCommandHandler },
         Menu(wxT("Select"), XXO("&Selection"),
              Command(wxT("SnapToOff"), XXO("Snap-To &Off"), FN(OnSnapToOff),
                      AlwaysEnabledFlag),
              Command(wxT("SnapToNearest"), XXO("Snap-To &Nearest"),
                      FN(OnSnapToNearest), AlwaysEnabledFlag),
              Command(wxT("SnapToPrior"), XXO("Snap-To &Prior"), FN(OnSnapToPrior),
                      AlwaysEnabledFlag),
              Command(wxT("SelStart"), XXO("Selection to &Start"), FN(OnSelToStart),
                      AlwaysEnabledFlag, wxT("Shift+Home")),
              Command(wxT("SelEnd"), XXO("Selection to En&d"), FN(OnSelToEnd),
                      AlwaysEnabledFlag, wxT("Shift+End")),
              Command(wxT("SelExtLeft"), XXO("Selection Extend &Left"),
                      FN(OnSelExtendLeft),
                      TracksExistFlag() | TrackPanelHasFocus(),
                      Options { wxT("Shift+Left") }.WantKeyUp().AllowDup()),
              Command(wxT("SelExtRight"), XXO("Selection Extend &Right"),
                      FN(OnSelExtendRight),
                      TracksExistFlag() | TrackPanelHasFocus(),
                      Options { wxT("Shift+Right") }.WantKeyUp().AllowDup()),
              Command(wxT("SelSetExtLeft"), XXO("Set (or Extend) Le&ft Selection"),
                      FN(OnSelSetExtendLeft),
                      TracksExistFlag() | TrackPanelHasFocus()),
              Command(wxT("SelSetExtRight"), XXO("Set (or Extend) Rig&ht Selection"),
                      FN(OnSelSetExtendRight),
                      TracksExistFlag() | TrackPanelHasFocus()),
              Command(wxT("SelCntrLeft"), XXO("Selection Contract L&eft"),
                      FN(OnSelContractLeft),
                      TracksExistFlag() | TrackPanelHasFocus(),
                      Options { wxT("Ctrl+Shift+Right") }.WantKeyUp()),
              Command(wxT("SelCntrRight"), XXO("Selection Contract R&ight"),
                      FN(OnSelContractRight),
                      TracksExistFlag() | TrackPanelHasFocus(),
                      Options { wxT("Ctrl+Shift+Left") }.WantKeyUp())
              )) };
    return menu;
}

AttachedItem sAttachment2{ Indirect(ExtraSelectionMenu()),
                           wxT("Optional/Extra/Part1")
};
}

namespace {
auto CursorMenu()
{
    static const auto CanStopFlags = AudioIONotBusyFlag() | CanStopAudioStreamFlag();

    // JKC: ANSWER-ME: How is 'cursor to' different to 'Skip To' and how is it
    // useful?
    // GA: 'Skip to' moves the viewpoint to center of the track and preserves the
    // selection. 'Cursor to' does neither. 'Center at' might describe it better
    // than 'Skip'.
    static auto menu = std::shared_ptr{
        (FinderScope{ findCommandHandler },
         Menu(wxT("Cursor"), XXO("&Cursor to"),
              Command(wxT("CursSelStart"), XXO("Selection Star&t"),
                      FN(OnCursorSelStart),
                      TimeSelectedFlag(),
                      Options {}.LongName(XO("Cursor to Selection Start"))),
              Command(wxT("CursSelEnd"), XXO("Selection En&d"),
                      FN(OnCursorSelEnd),
                      TimeSelectedFlag(),
                      Options {}.LongName(XO("Cursor to Selection End"))),

              Command(wxT("CursTrackStart"), XXO("Track &Start"),
                      FN(OnCursorTrackStart),
                      EditableTracksSelectedFlag(),
                      Options { wxT("J"), XO("Cursor to Track Start") }),
              Command(wxT("CursTrackEnd"), XXO("Track &End"),
                      FN(OnCursorTrackEnd),
                      EditableTracksSelectedFlag(),
                      Options { wxT("K"), XO("Cursor to Track End") }),

              Command(wxT("CursProjectStart"), XXO("&Project Start"),
                      FN(OnSkipStart),
                      CanStopFlags,
                      Options { wxT("Home"), XO("Cursor to Project Start") }),
              Command(wxT("CursProjectEnd"), XXO("Project E&nd"), FN(OnSkipEnd),
                      CanStopFlags,
                      Options { wxT("End"), XO("Cursor to Project End") })
              )) };
    return menu;
}

AttachedItem sAttachment0{ Indirect(CursorMenu()),
                           wxT("Transport/Basic")
};

auto ExtraCursorMenu()
{
    static auto menu = std::shared_ptr{
        (FinderScope{ findCommandHandler },
         Menu(wxT("Cursor"), XXO("&Cursor"),
              Command(wxT("CursorLeft"), XXO("Cursor &Left"), FN(OnCursorLeft),
                      TracksExistFlag() | TrackPanelHasFocus(),
                      Options { wxT("Left") }.WantKeyUp().AllowDup()),
              Command(wxT("CursorRight"), XXO("Cursor &Right"), FN(OnCursorRight),
                      TracksExistFlag() | TrackPanelHasFocus(),
                      Options { wxT("Right") }.WantKeyUp().AllowDup()),
              Command(wxT("CursorShortJumpLeft"), XXO("Cursor Sh&ort Jump Left"),
                      FN(OnCursorShortJumpLeft),
                      TracksExistFlag() | TrackPanelHasFocus(), wxT(",")),
              Command(wxT("CursorShortJumpRight"), XXO("Cursor Shor&t Jump Right"),
                      FN(OnCursorShortJumpRight),
                      TracksExistFlag() | TrackPanelHasFocus(), wxT(".")),
              Command(wxT("CursorLongJumpLeft"), XXO("Cursor Long J&ump Left"),
                      FN(OnCursorLongJumpLeft),
                      TracksExistFlag() | TrackPanelHasFocus(), wxT("Shift+,")),
              Command(wxT("CursorLongJumpRight"), XXO("Cursor Long Ju&mp Right"),
                      FN(OnCursorLongJumpRight),
                      TracksExistFlag() | TrackPanelHasFocus(), wxT("Shift+."))
              )) };
    return menu;
}

AttachedItem sAttachment4{ Indirect(ExtraCursorMenu()),
                           wxT("Optional/Extra/Part2")
};

auto ExtraSeekMenu()
{
    static auto menu = std::shared_ptr{
        (FinderScope{ findCommandHandler },
         Menu(wxT("Seek"), XXO("See&k"),
              Command(wxT("SeekLeftShort"), XXO("Short Seek &Left During Playback"),
                      FN(OnSeekLeftShort), AudioIOBusyFlag(),
                      Options { wxT("Left") }.AllowDup()),
              Command(wxT("SeekRightShort"),
                      XXO("Short Seek &Right During Playback"), FN(OnSeekRightShort),
                      AudioIOBusyFlag(),
                      Options { wxT("Right") }.AllowDup()),
              Command(wxT("SeekLeftLong"), XXO("Long Seek Le&ft During Playback"),
                      FN(OnSeekLeftLong), AudioIOBusyFlag(),
                      Options { wxT("Shift+Left") }.AllowDup()),
              Command(wxT("SeekRightLong"), XXO("Long Seek Rig&ht During Playback"),
                      FN(OnSeekRightLong), AudioIOBusyFlag(),
                      Options { wxT("Shift+Right") }.AllowDup())
              )) };
    return menu;
}

AttachedItem sAttachment5{ Indirect(ExtraSeekMenu()),
                           wxT("Optional/Extra/Part1")
};
}

#undef FN
