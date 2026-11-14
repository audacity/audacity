/**********************************************************************

  Audacity: A Digital Audio Editor

  EnvelopeEditor.cpp

  Paul Licameli split this from Envelope.cpp

**********************************************************************/

#include "EnvelopeEditor.h"

#include <wx/dc.h>
#include <wx/event.h>

#include "AColor.h"
#include "Envelope.h"
#include "TrackArt.h"
#include "TrackArtist.h"
#include "TrackPanelDrawingContext.h"
#include "ViewInfo.h"
#include "tracks/ui/EnvelopeHandle.h"

namespace {
void DrawPoint(wxDC& dc, const wxRect& r, int x, int y, bool top)
{
    if (y >= 0 && y <= r.height) {
        wxRect circle(r.x + x, r.y + (top ? y - 1 : y - 2), 4, 4);
        dc.DrawEllipse(circle);
    }
}
}

void EnvelopeEditor::DrawPoints(const Envelope& env,
                                TrackPanelDrawingContext& context, const wxRect& r,
                                bool dB, double dBRange,
                                float zoomMin, float zoomMax, bool mirrored, int origin)
{
    auto& dc = context.dc;
    const auto artist = TrackArtist::Get(context);
    const auto& zoomInfo = *artist->pZoomInfo;

    bool highlight = false;

    wxPen& pen = highlight ? AColor::uglyPen : AColor::envelopePen;
    dc.SetPen(pen);
    dc.SetBrush(*wxWHITE_BRUSH);

    for (int i = 0; i < (int)env.GetNumberOfPoints(); i++) {
        const double time = env[i].GetT() + env.GetOffset();
        const wxInt64 position = zoomInfo.TimeToPosition(time, origin);
        if (position >= 0 && position < r.width) {
            // Change colour if this is the draggable point...
            if (i == env.GetDragPoint()) {
                dc.SetPen(pen);
                dc.SetBrush(AColor::envelopeBrush);
            }

            double v = env[i].GetVal();
            int x = (int)(position);
            int y, y2;

            y = GetWaveYPos(v, zoomMin, zoomMax, r.height, dB,
                            true, dBRange, false);
            if (!mirrored) {
                DrawPoint(dc, r, x, y, true);
            } else {
                y2 = GetWaveYPos(-v - .000000001, zoomMin, zoomMax, r.height, dB,
                                 true, dBRange, false);

                // This follows the same logic as the envelop drawing in
                // TrackArt::DrawEnvelope().
                // TODO: make this calculation into a reusable function.
                if (y2 - y < 9) {
                    int value = (int)((zoomMax / (zoomMax - zoomMin)) * r.height);
                    y = value - 4;
                    y2 = value + 4;
                }

                DrawPoint(dc, r, x, y, true);
                DrawPoint(dc, r, x, y2, false);

                // Contour
                y = GetWaveYPos(v, zoomMin, zoomMax, r.height, dB,
                                false, dBRange, false);
                y2 = GetWaveYPos(-v - .000000001, zoomMin, zoomMax, r.height, dB,
                                 false, dBRange, false);
                if (y <= y2) {
                    DrawPoint(dc, r, x, y, true);
                    DrawPoint(dc, r, x, y2, false);
                }
            }

            // Change colour back again if was the draggable point.
            if (i == env.GetDragPoint()) {
                dc.SetPen(pen);
                dc.SetBrush(*wxWHITE_BRUSH);
            }
        }
    }
}

EnvelopeEditor::EnvelopeEditor(Envelope& envelope, bool mirrored)
    : mEnvelope(envelope)
    , mMirrored(mirrored)
    , mContourOffset(-1)
    // , mInitialVal(-1.0)
    // , mInitialY(-1)
    , mUpper(false)
    , mButton(wxMOUSE_BTN_NONE)
    , mDirty(false)
{
}

EnvelopeEditor::~EnvelopeEditor()
{
}

namespace {
inline int SQR(int x) { return x * x; }
}

/// ValueOfPixel() converts a y position on screen to an envelope value.
/// @param y - y position, usually of the mouse.relative to the clip.
/// @param height - height of the rectangle we are in.
/// @upper - true if we are on the upper line, false if on lower.
/// @dB - display mode either linear or log.
/// @zoomMin - vertical scale, typically -1.0
/// @zoomMax - vertical scale, typically +1.0
float EnvelopeEditor::ValueOfPixel(int y, int height, bool upper,
                                   bool dB, double dBRange,
                                   float zoomMin, float zoomMax)
{
    float v = ::ValueOfPixel(y, height, 0 != mContourOffset, dB, dBRange, zoomMin, zoomMax);

    // MB: this is mostly equivalent to what the old code did, I'm not sure
    // if anything special is needed for asymmetric ranges
    if (upper) {
        return mEnvelope.ClampValue(v);
    } else {
        return mEnvelope.ClampValue(-v);
    }
}

/// HandleMouseButtonDown either finds an existing control point or adds a NEW one
/// which is then recorded as the point to drag.
/// This is slightly complicated by there possibly being four control points for
/// a given time value:
/// We have an upper and lower envelope line.
/// Also we may be showing an inner envelope (at 0.5 the range).
bool EnvelopeEditor::HandleMouseButtonDown(const wxMouseEvent& event, wxRect& r,
                                           const ZoomInfo& zoomInfo,
                                           bool dB, double dBRange,
                                           float zoomMin, float zoomMax)
{
    int ctr = (int)(r.height * zoomMax / (zoomMax - zoomMin));
    bool upper = !mMirrored || (zoomMin >= 0.0) || (event.m_y - r.y < ctr);

    int clip_y = event.m_y - r.y;
    if (clip_y < 0) {
        clip_y = 0;           //keeps point in rect r, even if mouse isn't
    }
    if (clip_y > r.GetBottom()) {
        clip_y = r.GetBottom();
    }

    int bestNum = -1;
    int bestDistSqr = 100; // Must be within 10 pixel radius.

    // Member variables hold state that will be needed in dragging.
    mButton        = event.GetButton();
    mContourOffset = false;

    //   wxLogDebug(wxT("Y:%i Height:%i Offset:%i"), y, height, mContourOffset );
    int len = mEnvelope.GetNumberOfPoints();

    // TODO: extract this into a function FindNearestControlPoint()
    // TODO: also fix it so that we can drag the last point on an envelope.
    for (int i = 0; i < len; i++) { //search for control point nearest click
        const double time = mEnvelope[i].GetT() + mEnvelope.GetOffset();
        const wxInt64 position = zoomInfo.TimeToPosition(time);
        if (position >= 0 && position < r.width) {
            int x = (int)(position);
            int y[4];
            int numControlPoints;

            // Outer control points
            double value = mEnvelope[i].GetVal();
            y[0] = GetWaveYPos(value, zoomMin, zoomMax, r.height,
                               dB, true, dBRange, false);
            y[1] = GetWaveYPos(-value, zoomMin, zoomMax, r.height,
                               dB, true, dBRange, false);

            // Inner control points(contour)
            y[2] = GetWaveYPos(value, zoomMin, zoomMax, r.height,
                               dB, false, dBRange, false);
            y[3] = GetWaveYPos(-value - .00000001, zoomMin, zoomMax,
                               r.height, dB, false, dBRange, false);

            numControlPoints = 4;

            if (y[2] > y[3]) {
                numControlPoints = 2;
            }

            if (!mMirrored) {
                numControlPoints = 1;
            }

            const int deltaXSquared = SQR(x - (event.m_x - r.x));
            for (int j=0; j < numControlPoints; j++) {
                const int dSqr = deltaXSquared + SQR(y[j] - (event.m_y - r.y));
                if (dSqr < bestDistSqr) {
                    bestNum = i;
                    bestDistSqr = dSqr;
                    mContourOffset = (bool)(j > 1);
                }
            }
        }
    }

    if (bestNum >= 0) {
        mEnvelope.SetDragPoint(bestNum);
    } else {
        // TODO: Extract this into a function CreateNewPoint
        const double when = zoomInfo.PositionToTime(event.m_x, r.x);

        //      if (when <= 0 || when >= mTrackLen)
        //         return false;

        const double v = mEnvelope.GetValue(when);

        int ct = GetWaveYPos(v, zoomMin, zoomMax, r.height, dB,
                             false, dBRange, false);
        int cb = GetWaveYPos(-v - .000000001, zoomMin, zoomMax, r.height, dB,
                             false, dBRange, false);
        if (ct <= cb || !mMirrored) {
            int t = GetWaveYPos(v, zoomMin, zoomMax, r.height, dB,
                                true, dBRange, false);
            int b = GetWaveYPos(-v, zoomMin, zoomMax, r.height, dB,
                                true, dBRange, false);

            ct = (t + ct) / 2;
            cb = (b + cb) / 2;

            if (mMirrored
                && (event.m_y - r.y) > ct
                && ((event.m_y - r.y) < cb)) {
                mContourOffset = true;
            } else {
                mContourOffset = false;
            }
        }

        double newVal = ValueOfPixel(clip_y, r.height, upper, dB, dBRange,
                                     zoomMin, zoomMax);

        mEnvelope.SetDragPoint(mEnvelope.InsertOrReplace(when, newVal));
        mDirty = true;
    }

    mUpper = upper;

    // const int dragPoint = mEnvelope.GetDragPoint();
    // mInitialVal = mEnvelope[dragPoint].GetVal();
    // mInitialY = event.m_y+mContourOffset;

    return true;
}

void EnvelopeEditor::MoveDragPoint(const wxMouseEvent& event, wxRect& r,
                                   const ZoomInfo& zoomInfo, bool dB, double dBRange,
                                   float zoomMin, float zoomMax)
{
    int clip_y = event.m_y - r.y;
    if (clip_y < 0) {
        clip_y = 0;
    }
    if (clip_y > r.height) {
        clip_y = r.height;
    }
    double newVal = ValueOfPixel(clip_y, r.height, mUpper, dB, dBRange,
                                 zoomMin, zoomMax);

    // We no longer tolerate multiple envelope points at the same t.
    // epsilon is less than the time offset of a single sample
    // TODO: However because mTrackEpsilon assumes 200KHz this use
    // of epsilon is a tad bogus.  What we need to do instead is DELETE
    // a duplicated point on a mouse up.
    double newWhen = zoomInfo.PositionToTime(event.m_x, r.x) - mEnvelope.GetOffset();
    mEnvelope.MoveDragPoint(newWhen, newVal);
}

bool EnvelopeEditor::HandleDragging(const wxMouseEvent& event, wxRect& r,
                                    const ZoomInfo& zoomInfo, bool dB, double dBRange,
                                    float zoomMin, float zoomMax,
                                    float WXUNUSED(eMin), float WXUNUSED(eMax))
{
    mDirty = true;

    wxRect larger = r;
    larger.Inflate(10, 10);

    if (larger.Contains(event.m_x, event.m_y)) {
        // IF we're in the rect THEN we're not deleting this point (anymore).
        // ...we're dragging it.
        MoveDragPoint(event, r, zoomInfo, dB, dBRange, zoomMin, zoomMax);
        return true;
    }

    if (!mEnvelope.GetDragPointValid()) {
        // IF we already know we're deleting THEN no envelope point to update.
        return false;
    }

    // Invalidate the point
    mEnvelope.SetDragPointValid(false);
    return true;
}

// Exit dragging mode and delete dragged point if necessary.
bool EnvelopeEditor::HandleMouseButtonUp()
{
    mEnvelope.ClearDragPoint();
    mButton = wxMOUSE_BTN_NONE;
    return true;
}

// Returns true if parent needs to be redrawn
bool EnvelopeEditor::MouseEvent(const wxMouseEvent& event, wxRect& r,
                                const ZoomInfo& zoomInfo, bool dB, double dBRange,
                                float zoomMin, float zoomMax)
{
    if (event.ButtonDown() && mButton == wxMOUSE_BTN_NONE) {
        return HandleMouseButtonDown(event, r, zoomInfo, dB, dBRange,
                                     zoomMin, zoomMax);
    }
    if (event.Dragging() && mEnvelope.GetDragPoint() >= 0) {
        return HandleDragging(event, r, zoomInfo, dB, dBRange,
                              zoomMin, zoomMax);
    }
    if (event.ButtonUp() && event.GetButton() == mButton) {
        return HandleMouseButtonUp();
    }
    return false;
}
