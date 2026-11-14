/**********************************************************************

Audacity: A Digital Audio Editor

BrushHandle.cpp

Edward Hui

**********************************************************************/

#include "BrushHandle.h"
#include "Scrubbing.h"
#include "ChannelView.h"

#include "AColor.h"
#include "SpectrumAnalyst.h"
#include "NumberScale.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "ProjectHistory.h"
#include "../../ProjectSettings.h"

#include "../../RefreshCode.h"
#include "../../SelectUtilities.h"
#include "SelectionState.h"
#include "../../SpectralDataManager.h"
#include "../../TrackArtist.h"
#include "TrackFocus.h"
#include "../../TrackPanel.h"
#include "../../TrackPanelDrawingContext.h"
#include "../../TrackPanelMouseEvent.h"
#include "ViewInfo.h"
#include "WaveClip.h"
#include "WaveTrack.h"
#include "SpectrogramSettings.h"
#include "../../../images/Cursors.h"
#include "../playabletrack/wavetrack/ui/SpectrumView.h"

#include <cmath>
#include <wx/event.h>
#include <iostream>

enum {
    //This constant determines the size of the horizontal region (in pixels) around
    //the right and left selection bounds that can be used for horizontal selection adjusting
    //(or, vertical distance around top and bottom bounds in spectrograms,
    // for vertical selection adjusting)
    SELECTION_RESIZE_REGION = 3,

    // Seems 4 is too small to work at the top.  Why?
    FREQ_SNAP_DISTANCE = 10,
};

// #define SPECTRAL_EDITING_ESC_KEY

bool BrushHandle::IsDragging() const
{
    return mSelectionStateChanger.get() != NULL;
}

namespace {
/// Converts a frequency to screen y position.
wxInt64 FrequencyToPosition(const WaveChannel& wc,
                            double frequency,
                            wxInt64 trackTopEdge,
                            int trackHeight)
{
    const auto& settings = SpectrogramSettings::Get(wc);
    float minFreq, maxFreq;
    SpectrogramBounds::Get(wc).GetBounds(wc, minFreq, maxFreq);
    const NumberScale numberScale(settings.GetScale(minFreq, maxFreq));
    const float p = numberScale.ValueToPosition(frequency);
    return trackTopEdge + wxInt64((1.0 - p) * trackHeight);
}

/// Converts a position (mouse Y coordinate) to
/// frequency, in Hz.
double PositionToFrequency(const WaveChannel& wc,
                           bool maySnap,
                           wxInt64 mouseYCoordinate,
                           wxInt64 trackTopEdge,
                           int trackHeight)
{
    const double rate = wc.GetRate();

    // Handle snapping
    if (maySnap
        && mouseYCoordinate - trackTopEdge < FREQ_SNAP_DISTANCE) {
        return rate;
    }
    if (maySnap
        && trackTopEdge + trackHeight - mouseYCoordinate < FREQ_SNAP_DISTANCE) {
        return -1;
    }

    const auto& settings = SpectrogramSettings::Get(wc);
    const auto& cache = SpectrogramBounds::Get(wc);
    float minFreq, maxFreq;
    cache.GetBounds(wc, minFreq, maxFreq);
    const NumberScale numberScale(settings.GetScale(minFreq, maxFreq));
    const double p = double(mouseYCoordinate - trackTopEdge) / trackHeight;
    return numberScale.PositionToValue(1.0 - p);
}

long long PositionToLongSample(const WaveTrack* wt,
                               const ViewInfo& viewInfo,
                               int trackTopEdgeX,
                               int mousePosX)
{
    wxInt64 posTime = viewInfo.PositionToTime(mousePosX, trackTopEdgeX);
    sampleCount sc = wt->TimeToLongSamples(posTime);
    return sc.as_long_long();
}

template<typename T>
inline void SetIfNotNull(T* pValue, const T Value)
{
    if (pValue == NULL) {
        return;
    }
    *pValue = Value;
}

// This returns true if we're a spectral editing track.
inline bool isSpectralSelectionView(const ChannelView* pChannelView)
{
    const auto pChannel = pChannelView && pChannelView->IsSpectral()
                          ? pChannelView->FindChannel<const WaveChannel>() : nullptr;
    return pChannel
           && SpectrogramSettings::Get(*pChannel).SpectralSelectionEnabled();
}

wxCursor* CrosshairCursor()
{
    static auto crosshairCursor
        =::MakeCursor(wxCURSOR_IBEAM, CrosshairCursorXpm, 16, 16);
    return &*crosshairCursor;
}
}

BrushHandle::BrushHandle(
    std::shared_ptr<StateSaver> pStateSaver,
    const std::shared_ptr<ChannelView>& pChannelView,
    const TrackList& trackList,
    const TrackPanelMouseState& st, const ViewInfo& viewInfo,
    const std::shared_ptr<SpectralData>& pSpectralData,
    const ProjectSettings& pSettings)
    : mpStateSaver{move(pStateSaver)}
    , mpSpectralData(pSpectralData)
    , mpView{pChannelView}
{
    const wxMouseState& state = st.state;
    auto wc = pChannelView->FindChannel<WaveChannel>();
    double rate = mpSpectralData->GetSR();

    mRect = st.rect;
    mBrushRadius = pSettings.GetBrushRadius();
    const auto& settings = SpectrogramSettings::Get(*wc);
    mFreqUpperBound = settings.maxFreq - 1;
    mFreqLowerBound = settings.minFreq + 1;
    mIsSmartSelection = pSettings.IsSmartSelection();
    mIsOvertones = pSettings.IsOvertones();
    // Borrowed from TimeToLongSample
    mSampleCountLowerBound = floor(wc->GetStartTime() * rate + 0.5);
    mSampleCountUpperBound = floor(wc->GetEndTime() * rate + 0.5);
}

BrushHandle::~BrushHandle()
{
}

std::shared_ptr<const Track> BrushHandle::FindTrack() const
{
    return TrackFromChannel(const_cast<BrushHandle&>(*this).FindChannel());
}

namespace {
// Is the distance between A and B less than D?
template< class A, class B, class DIST > bool within(A a, B b, DIST d)
{
    return (a > b - d) && (a < b + d);
}

inline double findMaxRatio(double center, double rate)
{
    const double minFrequency = 1.0;
    const double maxFrequency = (rate / 2.0);
    const double frequency
        =std::min(maxFrequency,
                  std::max(minFrequency, center));
    return
        std::min(frequency / minFrequency, maxFrequency / frequency);
}
}

void BrushHandle::Enter(bool, AudacityProject* project)
{
}

bool BrushHandle::Escape(AudacityProject* project)
{
    return false;
}

// Add or remove data according to the ctrl key press
void BrushHandle::HandleHopBinData(int hopNum, int freqBinNum)
{
    // Ignore the mouse dragging outside valid area
    // We should also check for the available freq. range that is visible to user
    long long sampleCount = hopNum * mpSpectralData->GetHopSize();
    wxInt64 freq = freqBinNum * mpSpectralData->GetSR() / mpSpectralData->GetWindowSize();

    if (sampleCount < mSampleCountLowerBound || sampleCount > mSampleCountUpperBound
        || freq < mFreqLowerBound || freq > mFreqUpperBound) {
        return;
    }

    if (mbCtrlDown) {
        mpSpectralData->removeHopBinData(hopNum, freqBinNum);
    } else {
        mpSpectralData->addHopBinData(hopNum, freqBinNum);
    }
}

UIHandle::Result BrushHandle::Click
    (const TrackPanelMouseEvent& evt, AudacityProject* pProject)
{
    if (mpStateSaver) {
        // Clear all unless there is a modifier key down
        mpStateSaver->Init(*pProject, !evt.event.HasAnyModifiers());
    }

    using namespace RefreshCode;

    const auto pView = mpView.lock();
    if (!pView) {
        return Cancelled;
    }

    wxMouseEvent& event = evt.event;
    auto& trackPanel = TrackPanel::Get(*pProject);
    auto& viewInfo = ViewInfo::Get(*pProject);

    return RefreshAll;
}

UIHandle::Result BrushHandle::Drag
    (const TrackPanelMouseEvent& evt, AudacityProject* pProject)
{
    using namespace RefreshCode;

    const auto pView = mpView.lock();
    if (!pView) {
        return Cancelled;
    }

    wxMouseEvent& event = evt.event;
    const auto wc = FindChannel();
    auto& trackPanel = TrackPanel::Get(*pProject);
    auto& viewInfo = ViewInfo::Get(*pProject);

    auto posXToHopNum = [&](int x0){
        double posTime = viewInfo.PositionToTime(x0, mRect.x);
        sampleCount sc = wc->TimeToLongSamples(posTime);
        auto hopSize = mpSpectralData->GetHopSize();
        return (sc.as_long_long() + hopSize / 2) / hopSize;
    };

    auto posYToFreqBin = [&](int y0){
        int resFreq = PositionToFrequency(*wc, 0, y0, mRect.y, mRect.height);
        double resFreqBin = resFreq / (mpSpectralData->GetSR() / mpSpectralData->GetWindowSize());
        return static_cast<int>(std::round(resFreqBin));
    };

    auto drawCircle = [&](int h0, int bm){
        // For each (h0, b0), draw circle
        int h2 = mBrushRadius;
        int b2 = 0;
        int hChange = 1 - (mBrushRadius << 1);
        int bChange = 0;
        int radiusError = 0;
        while (h2 >= b2) {
            for (int i = h0 - h2; i <= h0 + h2; i++) {
                HandleHopBinData(i, bm + b2);
                HandleHopBinData(i, bm - b2);
            }
            for (int i = h0 - b2; i <= h0 + b2; i++) {
                HandleHopBinData(i, bm + h2);
                HandleHopBinData(i, bm - h2);
            }

            b2++;
            radiusError += bChange;
            bChange += 2;
            if (((radiusError << 1) + hChange) > 0) {
                h2--;
                radiusError += hChange;
                hChange += 2;
            }
        } // End of full circle drawing
    };

    // Clip the coordinates
    // TODO: Find ways to access the ClipParameters (for the mid)
    int dest_xcoord = std::clamp(event.m_x, mRect.x + 10, mRect.x + mRect.width);
    int dest_ycoord = std::clamp(event.m_y, mRect.y + 10, mRect.y + mRect.height);

    int h1 = posXToHopNum(dest_xcoord);
    int b1 = posYToFreqBin(dest_ycoord);

    mbCtrlDown = event.ControlDown() || event.AltDown();

    // Use the hop and bin number to calculate the brush stroke, instead of the mouse coordinates
    // For mouse coordinate:
    // (src_xcoord, src_ycoord) -> (dest_xcoord, dest_ycoord)

    const auto& hopSize = mpSpectralData->GetHopSize();
    if (!mpSpectralData->coordHistory.empty()) {
        int src_xcoord = mpSpectralData->coordHistory.back().first;
        int src_ycoord = mpSpectralData->coordHistory.back().second;

        int h0 = posXToHopNum(src_xcoord);
        int b0 = posYToFreqBin(src_ycoord);
        int wd = mBrushRadius * 2;

        int dh =  abs(h1 - h0), sh = h0 < h1 ? 1 : -1;
        int db = -abs(b1 - b0), sb = b0 < b1 ? 1 : -1;
        int err = dh + db, err2;

        // Line drawing (draw points until the start coordinate reaches the end)
        while (true) {
            if (h0 == h1 && b0 == b1) {
                break;
            }
            err2 = err * 2;
            if (err2 * 2 >= db) {
                err += db;
                h0 += sh;
            }
            if (err2 * 2 <= dh) {
                err += dh;
                b0 += sb;
            }

            int bm = b0;
            if (mIsSmartSelection) {
                // Correct the y coord (snap to highest energy freq. bin)
                if (auto* sView = dynamic_cast<SpectrumView*>(pView.get())) {
                    int resFreqBin
                        =SpectralDataManager::FindFrequencySnappingBin(*wc,
                                                                       h0 * hopSize, hopSize, mFreqSnappingRatio, bm);
                    if (resFreqBin != -1) {
                        bm = resFreqBin;
                    }
                }
            }

            if (mIsOvertones) {
                // take bm and calculate the highest energy
                std::vector<int> binsToWork = SpectralDataManager::FindHighestFrequencyBins(*wc,
                                                                                            h0 * hopSize, hopSize, mOvertonesThreshold, bm);
                for (auto& bins: binsToWork) {
                    drawCircle(h0, bins);
                }
            }

            drawCircle(h0, bm);
        } // End of line connecting
    }
    mpSpectralData->coordHistory.push_back(std::make_pair(dest_xcoord, dest_ycoord));
    return RefreshAll;
}

HitTestPreview BrushHandle::Preview
    (const TrackPanelMouseState& st, AudacityProject* pProject)
{
    TranslatableString tip;
    wxCursor* pCursor = CrosshairCursor();
    return { tip, pCursor };
}

UIHandle::Result BrushHandle::Release
    (const TrackPanelMouseEvent& evt, AudacityProject* pProject,
    wxWindow*)
{
    using namespace RefreshCode;
    mpSpectralData->saveAndClearBuffer();
    if (mpStateSaver) {
        mpStateSaver->Commit();
        mpStateSaver.reset();
    }
    if (mbCtrlDown) {
        ProjectHistory::Get(*pProject).PushState(
            XO("Erased selected area"),
            XO("Erased selected area"));
        ProjectHistory::Get(*pProject).ModifyState(true);
    } else {
        ProjectHistory::Get(*pProject).PushState(
            XO("Selected area using Brush Tool"),
            XO("Brush tool selection"));
        ProjectHistory::Get(*pProject).ModifyState(true);
    }

    return RefreshNone;
}

UIHandle::Result BrushHandle::Cancel(AudacityProject* pProject)
{
    mpStateSaver.reset();
    return RefreshCode::RefreshAll;
}

void BrushHandle::Draw(
    TrackPanelDrawingContext& context,
    const wxRect& rect, unsigned iPass)
{
    if (iPass == TrackArtist::PassTracks) {
        auto& dc = context.dc;
        wxPoint coord;
        coord.x = mMostRecentX;
        coord.y = mMostRecentY;
        dc.SetBrush(*wxTRANSPARENT_BRUSH);
        dc.SetPen(*wxYELLOW_PEN);
        dc.DrawCircle(coord, mBrushRadius);
    }
}

std::shared_ptr<WaveChannel> BrushHandle::FindChannel()
{
    auto pView = mpView.lock();
    if (!pView) {
        return {}
    } else {
        return pView->FindChannel<WaveChannel>();
    }
}

BrushHandle::StateSaver::~StateSaver() = default;
