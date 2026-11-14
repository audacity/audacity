/**********************************************************************

Audacity: A Digital Audio Editor

SpectrumVZoomHandle.cpp

Paul Licameli split from WaveChannelVZoomHandle.cpp

**********************************************************************/

#include "SpectrumVZoomHandle.h"

#include "WaveChannelVZoomHandle.h"

#include "../../../../HitTestResult.h"
#include "NumberScale.h"
#include "Prefs.h"
#include "ProjectHistory.h"
#include "../../../../RefreshCode.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "WaveTrack.h"
#include "SpectrogramSettings.h"

SpectrumVZoomHandle::SpectrumVZoomHandle(
    const std::shared_ptr<WaveChannel>& pChannel, const wxRect& rect, int y)
    : mpChannel{pChannel}, mZoomStart(y), mZoomEnd(y), mRect(rect)
{
}

SpectrumVZoomHandle::~SpectrumVZoomHandle() = default;

std::shared_ptr<const Track> SpectrumVZoomHandle::FindTrack() const
{
    return TrackFromChannel(mpChannel.lock());
}

std::shared_ptr<WaveChannel> SpectrumVZoomHandle::FindWaveChannel()
{
    return mpChannel.lock();
}

void SpectrumVZoomHandle::Enter(bool, AudacityProject*)
{
}

bool SpectrumVZoomHandle::HandlesRightClick()
{
    return true;
}

UIHandle::Result SpectrumVZoomHandle::Click
    (const TrackPanelMouseEvent&, AudacityProject*)
{
    return RefreshCode::RefreshNone;
}

UIHandle::Result SpectrumVZoomHandle::Drag(
    const TrackPanelMouseEvent& evt, AudacityProject* pProject)
{
    using namespace RefreshCode;
    if (!FindTrack()) {
        return Cancelled;
    }
    return WaveChannelVZoomHandle::DoDrag(evt, pProject, mZoomStart, mZoomEnd, true);
}

HitTestPreview SpectrumVZoomHandle::Preview
    (const TrackPanelMouseState& st, AudacityProject*)
{
    return WaveChannelVZoomHandle::HitPreview(true);
}

UIHandle::Result SpectrumVZoomHandle::Release(
    const TrackPanelMouseEvent& evt, AudacityProject* pProject,
    wxWindow* pParent)
{
    const auto pChannel = FindWaveChannel();
    if (!pChannel) {
        return RefreshCode::Cancelled;
    }
    return WaveChannelVZoomHandle::DoRelease(
        evt, pProject, pParent, *pChannel, mRect,
        DoZoom, SpectrumVRulerMenuTable::Instance(),
        mZoomStart, mZoomEnd);
}

UIHandle::Result SpectrumVZoomHandle::Cancel(AudacityProject*)
{
    // Cancel is implemented!  And there is no initial state to restore,
    // so just return a code.
    return RefreshCode::RefreshAll;
}

void SpectrumVZoomHandle::Draw(
    TrackPanelDrawingContext& context,
    const wxRect& rect, unsigned iPass)
{
    const auto pChannel = FindTrack();
    if (!pChannel) {
        return;
    }
    return WaveChannelVZoomHandle::DoDraw(
        context, rect, iPass, mZoomStart, mZoomEnd, true);
}

wxRect SpectrumVZoomHandle::DrawingArea(
    TrackPanelDrawingContext&,
    const wxRect& rect, const wxRect& panelRect, unsigned iPass)
{
    return WaveChannelVZoomHandle::DoDrawingArea(rect, panelRect, iPass);
}

// ZoomKind says how to zoom.
// If ZoomStart and ZoomEnd are not equal, this may override
// the zoomKind and cause a drag-zoom-in.
void SpectrumVZoomHandle::DoZoom(
    AudacityProject* pProject,
    WaveChannel& wc,
    WaveChannelViewConstants::ZoomActions ZoomKind,
    const wxRect& rect, int zoomStart, int zoomEnd,
    bool fixedMousePoint)
{
    using namespace WaveChannelViewConstants;
    static const float ZOOMLIMIT = 0.001f;

    int height = rect.height;
    int ypos = rect.y;

    // Ensure start and end are in order (swap if not).
    if (zoomEnd < zoomStart) {
        std::swap(zoomStart, zoomEnd);
    }

    float min, max, minBand = 0;
    const double rate = wc.GetRate();
    const float halfrate = rate / 2;
    float maxFreq = 8000.0;
    const auto& specSettings = SpectrogramSettings::Get(wc);
    NumberScale scale;
    const bool spectrumLinear
        =(specSettings.scaleType == SpectrogramSettings::stLinear);
    auto& bounds = SpectrogramBounds::Get(wc);

    bool bDragZoom = WaveChannelVZoomHandle::IsDragZooming(zoomStart, zoomEnd, true);
    // Add 100 if spectral to separate the kinds of zoom.
    const int kSpectral = 100;

    // Possibly override the zoom kind.
    if (bDragZoom) {
        ZoomKind = kZoomInByDrag;
    }

    float top=2.0;
    float half=0.5;

    {
        bounds.GetBounds(wc, min, max);
        scale = (specSettings.GetScale(min, max));
        const auto fftLength = specSettings.GetFFTLength();
        const float binSize = rate / fftLength;
        maxFreq = SpectrumMaxFreq.Read();
        // JKC:  Following discussions of Bug 1208 I'm allowing zooming in
        // down to one bin.
        //      const int minBins =
        //         std::min(10, fftLength / 2); //minimum 10 freq bins, unless there are less
        const int minBins = 1;
        minBand = minBins * binSize;
    }

    // Compute min and max.
    switch (ZoomKind) {
    default:
        // If we have covered all the cases, this won't happen.
        // In release builds Audacity will ignore the zoom.
        wxFAIL_MSG("Zooming Case not implemented by Audacity");
        break;

    // VZooming on spectral we don't implement the other zoom presets.
    // They are also not in the menu.
    case kZoomReset:
    {
        // Zoom out to normal level.
        min = spectrumLinear ? 0.0f : 1.0f;
        max = maxFreq;
    }
    break;
    case kZoom1to1:
    case kZoomHalfWave:
    {
        // Zoom out full
        min = spectrumLinear ? 0.0f : 1.0f;
        max = halfrate;
    }
    break;
    case kZoomInByDrag:
    {
        double xmin = 1 - (zoomEnd - ypos) / (float)height;
        double xmax = 1 - (zoomStart - ypos) / (float)height;
        const float middle = (xmin + xmax) / 2;
        const float middleValue = scale.PositionToValue(middle);

        min = std::max(spectrumLinear ? 0.0f : 1.0f,
                       std::min(middleValue - minBand / 2,
                                scale.PositionToValue(xmin)
                                ));
        max = std::min(halfrate,
                       std::max(middleValue + minBand / 2,
                                scale.PositionToValue(xmax)
                                ));
    }
    break;
    case kZoomIn:
    {
        // Center the zoom-in at the click
        const float p1 = (zoomStart - ypos) / (float)height;
        const float middle = 1.0f - p1;
        const float middleValue = scale.PositionToValue(middle);

        if (fixedMousePoint) {
            min = std::max(spectrumLinear ? 0.0f : 1.0f,
                           std::min(middleValue - minBand * middle,
                                    scale.PositionToValue(0.5f * middle)
                                    ));
            max = std::min(halfrate,
                           std::max(middleValue + minBand * p1,
                                    scale.PositionToValue(middle + 0.5f * p1)
                                    ));
        } else {
            min = std::max(spectrumLinear ? 0.0f : 1.0f,
                           std::min(middleValue - minBand / 2,
                                    scale.PositionToValue(middle - 0.25f)
                                    ));
            max = std::min(halfrate,
                           std::max(middleValue + minBand / 2,
                                    scale.PositionToValue(middle + 0.25f)
                                    ));
        }
    }
    break;
    case kZoomOut:
    {
        // Zoom out
        const float p1 = (zoomStart - ypos) / (float)height;
        // (Used to zoom out centered at midline, ignoring the click, if linear view.
        //  I think it is better to be consistent.  PRL)
        // Center zoom-out at the midline
        const float middle    // spectrumLinear ? 0.5f :
            =1.0f - p1;

        if (fixedMousePoint) {
            min = std::max(spectrumLinear ? 0.0f : 1.0f, scale.PositionToValue(-middle));
            max = std::min(halfrate, scale.PositionToValue(1.0f + p1));
        } else {
            min = std::max(spectrumLinear ? 0.0f : 1.0f, scale.PositionToValue(middle - 1.0f));
            max = std::min(halfrate, scale.PositionToValue(middle + 1.0f));
        }
    }
    break;
    }

    // Now actually apply the zoom.
    bounds.SetBounds(min, max);

    zoomEnd = zoomStart = 0;
    if (pProject) {
        ProjectHistory::Get(*pProject).ModifyState(true);
    }
}

///////////////////////////////////////////////////////////////////////////////
// Table class

PopupMenuTable& SpectrumVRulerMenuTable::Instance()
{
    static SpectrumVRulerMenuTable instance;
    return instance;
}

BEGIN_POPUP_MENU(SpectrumVRulerMenuTable)
//Generate scales (Mel, Logarithmic, etc)
const auto& names = SpectrogramSettings::GetScaleNames();
for (int ii = 0, nn = names.size(); ii < nn; ++ii) {
    AppendRadioItem(names[ii].Internal(),
                    OnFirstSpectrumScaleID + ii, names[ii].Msgid(),
                    POPUP_MENU_FN(OnSpectrumScaleType),
                    []( PopupMenuHandler& handler, wxMenu& menu, int id ){
        auto& wc
            =static_cast<SpectrumVRulerMenuTable&>(handler)
              .mpData->wc;
        if (id
            == OnFirstSpectrumScaleID
            + static_cast<int>(SpectrogramSettings::Get(wc).scaleType)) {
            menu.Check(id, true);
        }
    }
                    );
}

BeginSection("Zoom");
AppendItem("In", OnZoomInVerticalID, XXO("Zoom In"), POPUP_MENU_FN(OnZoomInVertical));
AppendItem("Out", OnZoomOutVerticalID, XXO("Zoom Out"), POPUP_MENU_FN(OnZoomOutVertical));
AppendItem("Fit", OnZoomFitVerticalID, XXO("Zoom to Fit"), POPUP_MENU_FN(OnZoomFitVertical));
AppendItem("Reset", OnZoomResetID, XXO("Reset Zoom"), POPUP_MENU_FN(OnZoomReset));
EndSection();

END_POPUP_MENU()

void SpectrumVRulerMenuTable::OnSpectrumScaleType(wxCommandEvent& evt)
{
    auto& wc = mpData->wc;
    const SpectrogramSettings::ScaleType newScaleType
        =SpectrogramSettings::ScaleType(
              std::max(0,
                       std::min((int)(SpectrogramSettings::stNumScaleTypes)-1,
                                evt.GetId() - OnFirstSpectrumScaleID
                                )));
    if (SpectrogramSettings::Get(wc).scaleType != newScaleType) {
        SpectrogramSettings::Own(wc).scaleType = newScaleType;

        ProjectHistory::Get(mpData->project).ModifyState(true);

        using namespace RefreshCode;
        mpData->result = UpdateVRuler | RefreshAll;
    }
}
