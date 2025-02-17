/**********************************************************************

  Audacity: A Digital Audio Editor

  AutoDuck.cpp

  Markus Meyer

*******************************************************************/
#include "AutoDuck.h"
#include "EffectEditor.h"
#include "LoadEffects.h"

#include <wx/dcclient.h>
#include <wx/dcmemory.h>

#include "AColor.h"
#include "AllThemeResources.h"
#include "Prefs.h"
#include "ShuttleGui.h"
#include "Theme.h"
#include "../widgets/valnum.h"

/*
 * Effect implementation
 */

namespace {
BuiltinEffectsModule::Registration< EffectAutoDuck > reg;
}

BEGIN_EVENT_TABLE(EffectAutoDuck, wxEvtHandler)
EVT_TEXT(wxID_ANY, EffectAutoDuck::OnValueChanged)
END_EVENT_TABLE()

std::unique_ptr<EffectEditor> EffectAutoDuck::PopulateOrExchange(
    ShuttleGui& S, EffectInstance&, EffectSettingsAccess&,
    const EffectOutputs*)
{
    mUIParent = S.GetParent();
    S.SetBorder(5);
    S.StartVerticalLay(true);
    {
        S.AddSpace(0, 5);

        mPanel = safenew EffectAutoDuck::Panel(S.GetParent(), wxID_ANY, this);
        S.AddWindow(mPanel);

        S.AddSpace(0, 5);

        S.StartMultiColumn(6, wxCENTER);
        {
            mDuckAmountDbBox = S.Validator<FloatingPointValidator<double> >(
                1, &mDuckAmountDb, NumValidatorStyle::NO_TRAILING_ZEROES,
                DuckAmountDb.min, DuckAmountDb.max)
                               .NameSuffix(XO("db"))
                               .AddTextBox(XXO("Duck &amount:"), wxT(""), 10);
            S.AddUnits(XO("dB"));

            mMaximumPauseBox = S.Validator<FloatingPointValidator<double> >(
                2, &mMaximumPause, NumValidatorStyle::NO_TRAILING_ZEROES,
                MaximumPause.min, MaximumPause.max)
                               .NameSuffix(XO("seconds"))
                               .AddTextBox(XXO("Ma&ximum pause:"), wxT(""), 10);
            S.AddUnits(XO("seconds"));

            mOuterFadeDownLenBox = S.Validator<FloatingPointValidator<double> >(
                2, &mOuterFadeDownLen, NumValidatorStyle::NO_TRAILING_ZEROES,
                OuterFadeDownLen.min, OuterFadeDownLen.max)
                                   .NameSuffix(XO("seconds"))
                                   .AddTextBox(XXO("Outer fade &down length:"), wxT(""), 10);
            S.AddUnits(XO("seconds"));

            mOuterFadeUpLenBox = S.Validator<FloatingPointValidator<double> >(
                2, &mOuterFadeUpLen, NumValidatorStyle::NO_TRAILING_ZEROES,
                OuterFadeUpLen.min, OuterFadeUpLen.max)
                                 .NameSuffix(XO("seconds"))
                                 .AddTextBox(XXO("Outer fade &up length:"), wxT(""), 10);
            S.AddUnits(XO("seconds"));

            mInnerFadeDownLenBox = S.Validator<FloatingPointValidator<double> >(
                2, &mInnerFadeDownLen, NumValidatorStyle::NO_TRAILING_ZEROES,
                InnerFadeDownLen.min, InnerFadeDownLen.max)
                                   .NameSuffix(XO("seconds"))
                                   .AddTextBox(XXO("Inner fade d&own length:"), wxT(""), 10);
            S.AddUnits(XO("seconds"));

            mInnerFadeUpLenBox = S.Validator<FloatingPointValidator<double> >(
                2, &mInnerFadeUpLen, NumValidatorStyle::NO_TRAILING_ZEROES,
                InnerFadeUpLen.min, InnerFadeUpLen.max)
                                 .NameSuffix(XO("seconds"))
                                 .AddTextBox(XXO("Inner &fade up length:"), wxT(""), 10);
            S.AddUnits(XO("seconds"));
        }
        S.EndMultiColumn();

        S.StartMultiColumn(3, wxCENTER);
        {
            mThresholdDbBox = S.Validator<FloatingPointValidator<double> >(
                2, &mThresholdDb, NumValidatorStyle::NO_TRAILING_ZEROES,
                ThresholdDb.min, ThresholdDb.max)
                              .NameSuffix(XO("db"))
                              .AddTextBox(XXO("&Threshold:"), wxT(""), 10);
            S.AddUnits(XO("dB"));
        }
        S.EndMultiColumn();
    }
    S.EndVerticalLay();

    return nullptr;
}

bool EffectAutoDuck::TransferDataToWindow(const EffectSettings&)
{
    return DoTransferDataToWindow();
}

bool EffectAutoDuck::DoTransferDataToWindow()
{
    // Issue 2324: don't remove these two lines
    if (!mUIParent->TransferDataToWindow()) {
        return false;
    }

    mPanel->Refresh(false);

    return true;
}

bool EffectAutoDuck::TransferDataFromWindow(EffectSettings&)
{
    if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow()) {
        return false;
    }

    return true;
}

void EffectAutoDuck::OnValueChanged(wxCommandEvent& WXUNUSED(evt))
{
    mPanel->Refresh(false);
}

/*
 * EffectAutoDuck::Panel implementation
 */

#define CONTROL_POINT_REGION 10 // pixel distance to click on a control point
#define CONTROL_POINT_MIN_MOVE 5 // min mouse move until value is changed

#define TEXT_DISTANCE 15 // pixel distance text <-> center of control point

#define FADE_DOWN_START 150 // x coordinate
#define FADE_UP_START 450 // x coordinate
#define DUCK_AMOUNT_START 50 // y coordinate

#define FADE_SCALE 40 // scale factor for second -> pixel conversion
#define DUCK_AMOUNT_SCALE 8 // scale factor for db -> pixel conversion

static int GetDistance(const wxPoint& first, const wxPoint& second)
{
    int distanceX = abs(first.x - second.x);
    int distanceY = abs(first.y - second.y);
    if (distanceX > distanceY) {
        return distanceX;
    } else {
        return distanceY;
    }
}

BEGIN_EVENT_TABLE(EffectAutoDuck::Panel, wxPanelWrapper)
EVT_PAINT(EffectAutoDuck::Panel::OnPaint)
EVT_MOUSE_CAPTURE_CHANGED(EffectAutoDuck::Panel::OnMouseCaptureChanged)
EVT_MOUSE_CAPTURE_LOST(EffectAutoDuck::Panel::OnMouseCaptureLost)
EVT_LEFT_DOWN(EffectAutoDuck::Panel::OnLeftDown)
EVT_LEFT_UP(EffectAutoDuck::Panel::OnLeftUp)
EVT_MOTION(EffectAutoDuck::Panel::OnMotion)
END_EVENT_TABLE()

EffectAutoDuck::Panel::Panel(
    wxWindow* parent, wxWindowID winid, EffectAutoDuck* effect)
    :  wxPanelWrapper(parent, winid, wxDefaultPosition, wxSize(600, 300))
{
    mParent = parent;
    mEffect = effect;
    mCurrentControlPoint = none;
    mBackgroundBitmap = NULL;

    ResetControlPoints();
}

EffectAutoDuck::Panel::~Panel()
{
    if (HasCapture()) {
        ReleaseMouse();
    }
}

void EffectAutoDuck::Panel::ResetControlPoints()
{
    mControlPoints[innerFadeDown] = wxPoint(-100, -100);
    mControlPoints[innerFadeUp] = wxPoint(-100, -100);
    mControlPoints[outerFadeDown] = wxPoint(-100, -100);
    mControlPoints[outerFadeUp] = wxPoint(-100, -100);
    mControlPoints[duckAmount] = wxPoint(-100, -100);
}

void EffectAutoDuck::Panel::OnPaint(wxPaintEvent& WXUNUSED(evt))
{
    int clientWidth, clientHeight;
    GetSize(&clientWidth, &clientHeight);

    if (!mBackgroundBitmap || mBackgroundBitmap->GetWidth() != clientWidth
        || mBackgroundBitmap->GetHeight() != clientHeight) {
        mBackgroundBitmap = std::make_unique<wxBitmap>(clientWidth, clientHeight, 24);
    }

    wxMemoryDC dc;
    dc.SelectObject(*mBackgroundBitmap);

    dc.SetBrush(*wxWHITE_BRUSH);
    dc.SetPen(*wxBLACK_PEN);
    dc.DrawRectangle(0, 0, clientWidth, clientHeight);

    dc.SetFont(wxFont(10, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL,
                      wxFONTWEIGHT_NORMAL));
    dc.SetTextForeground(*wxBLACK);
    dc.SetTextBackground(*wxWHITE);

    double duckAmountDb = 0;
    double innerFadeDownLen = 0;
    double innerFadeUpLen = 0;
    double outerFadeDownLen = 0;
    double outerFadeUpLen = 0;
    mEffect->mDuckAmountDbBox->GetValue().ToDouble(&duckAmountDb);
    mEffect->mInnerFadeDownLenBox->GetValue().ToDouble(&innerFadeDownLen);
    mEffect->mInnerFadeUpLenBox->GetValue().ToDouble(&innerFadeUpLen);
    mEffect->mOuterFadeDownLenBox->GetValue().ToDouble(&outerFadeDownLen);
    mEffect->mOuterFadeUpLenBox->GetValue().ToDouble(&outerFadeUpLen);

    if (innerFadeDownLen < InnerFadeDownLen.min || innerFadeDownLen > InnerFadeDownLen.max
        || innerFadeUpLen < InnerFadeUpLen.min || innerFadeUpLen > InnerFadeUpLen.max
        || outerFadeDownLen < OuterFadeDownLen.min || outerFadeDownLen > OuterFadeDownLen.max
        || outerFadeUpLen < OuterFadeUpLen.min || outerFadeUpLen > OuterFadeUpLen.max
        || duckAmountDb < DuckAmountDb.min || duckAmountDb > DuckAmountDb.max) {
        // values are out of range, no preview available
        wxString message = _("Preview not available");
        int textWidth = 0, textHeight = 0;
        dc.GetTextExtent(message, &textWidth, &textHeight);
        dc.DrawText(message, (clientWidth - textWidth) / 2,
                    (clientHeight - textHeight) / 2);

        ResetControlPoints();
    } else {
        // draw preview
        dc.SetBrush(*wxTRANSPARENT_BRUSH);
        dc.SetPen(wxPen(theTheme.Colour(clrGraphLines), 3, wxPENSTYLE_SOLID));

        wxPoint points[6];

        points[0].x = 10;
        points[0].y = DUCK_AMOUNT_START;

        points[1].x = FADE_DOWN_START - (int)(outerFadeDownLen * FADE_SCALE);
        points[1].y = DUCK_AMOUNT_START;

        points[2].x = FADE_DOWN_START + (int)(innerFadeDownLen * FADE_SCALE);
        points[2].y = DUCK_AMOUNT_START
                      - (int)(duckAmountDb * DUCK_AMOUNT_SCALE);

        points[3].x = FADE_UP_START - (int)(innerFadeUpLen * FADE_SCALE);
        points[3].y = DUCK_AMOUNT_START
                      - (int)(duckAmountDb * DUCK_AMOUNT_SCALE);

        points[4].x = FADE_UP_START + (int)(outerFadeUpLen * FADE_SCALE);
        points[4].y = DUCK_AMOUNT_START;

        points[5].x = clientWidth - 10;
        points[5].y = DUCK_AMOUNT_START;

        AColor::Lines(dc, 6, points);

        dc.SetPen(wxPen(*wxBLACK, 1, wxPENSTYLE_DOT));

        AColor::Line(dc, FADE_DOWN_START, 10, FADE_DOWN_START, clientHeight - 10);
        AColor::Line(dc, FADE_UP_START, 10, FADE_UP_START, clientHeight - 10);

        dc.SetPen(AColor::envelopePen);
        dc.SetBrush(*wxWHITE_BRUSH);

        mControlPoints[outerFadeDown] = points[1];
        mControlPoints[innerFadeDown] = points[2];
        mControlPoints[innerFadeUp] = points[3];
        mControlPoints[outerFadeUp] = points[4];
        mControlPoints[duckAmount] = wxPoint(
            (points[2].x + points[3].x) / 2, points[2].y);

        for (int i = 0; i < AUTO_DUCK_PANEL_NUM_CONTROL_POINTS; i++) {
            EControlPoint cp = (EControlPoint)i;
            int digits;
            float value;

            if (cp == innerFadeDown) {
                value = innerFadeDownLen;
                digits = 2;
            } else if (cp == innerFadeUp) {
                value = innerFadeUpLen;
                digits = 2;
            } else if (cp == outerFadeDown) {
                value = outerFadeDownLen;
                digits = 2;
            } else if (cp == outerFadeUp) {
                value = outerFadeUpLen;
                digits = 2;
            } else {
                value = duckAmountDb;
                digits = 1;
            }

            wxString valueStr = Internat::ToDisplayString(value, digits);
            valueStr += wxT(" ");

            if (cp == duckAmount) {
                /* i18n-hint: short form of 'decibels'.*/
                valueStr += _("dB");
            } else {
                /* i18n-hint: short form of 'seconds'.*/
                valueStr += _("s");
            }

            int textWidth = 0, textHeight = 0;
            GetTextExtent(valueStr, &textWidth, &textHeight);

            int textPosX = mControlPoints[i].x - textWidth / 2;
            int textPosY = mControlPoints[i].y;

            if (cp == duckAmount || cp == outerFadeDown || cp == outerFadeUp) {
                textPosY -= TEXT_DISTANCE + textHeight;
            } else {
                textPosY += TEXT_DISTANCE;
            }

            dc.DrawText(valueStr, textPosX, textPosY);

            dc.DrawEllipse(mControlPoints[i].x - 3,
                           mControlPoints[i].y - 3, 6, 6);
        }
    }

    // copy background buffer to paint dc
    wxPaintDC paintDC(this);
    paintDC.Blit(0, 0, clientWidth, clientHeight, &dc, 0, 0);

    // clean up: necessary to free resources on Windows
    dc.SetPen(wxNullPen);
    dc.SetBrush(wxNullBrush);
    dc.SetFont(wxNullFont);
    dc.SelectObject(wxNullBitmap);
}

void EffectAutoDuck::Panel::OnMouseCaptureChanged(
    wxMouseCaptureChangedEvent& WXUNUSED(evt))
{
    SetCursor(wxNullCursor);
    mCurrentControlPoint = none;
}

void EffectAutoDuck::Panel::OnMouseCaptureLost(
    wxMouseCaptureLostEvent& WXUNUSED(evt))
{
    mCurrentControlPoint = none;

    if (HasCapture()) {
        ReleaseMouse();
    }
}

EffectAutoDuck::Panel::EControlPoint
EffectAutoDuck::Panel::GetNearestControlPoint(const wxPoint& pt)
{
    int dist[AUTO_DUCK_PANEL_NUM_CONTROL_POINTS];
    int i;

    for (i = 0; i < AUTO_DUCK_PANEL_NUM_CONTROL_POINTS; i++) {
        dist[i] = GetDistance(pt, mControlPoints[i]);
    }

    int curMinimum = 0;
    for (i = 0; i < AUTO_DUCK_PANEL_NUM_CONTROL_POINTS; i++) {
        if (dist[i] < dist[curMinimum]) {
            curMinimum = i;
        }
    }

    if (dist[curMinimum] <= CONTROL_POINT_REGION) {
        return (EControlPoint)curMinimum;
    } else {
        return none;
    }
}

void EffectAutoDuck::Panel::OnLeftDown(wxMouseEvent& evt)
{
    EControlPoint nearest = GetNearestControlPoint(evt.GetPosition());

    if (nearest != none) {
        // this control point has been clicked
        mMouseDownPoint = evt.GetPosition();

        mCurrentControlPoint = nearest;
        mControlPointMoveActivated = false;

        for (int i = 0; i < AUTO_DUCK_PANEL_NUM_CONTROL_POINTS; i++) {
            mMoveStartControlPoints[i] = mControlPoints[i];
        }

        if (!HasCapture()) {
            CaptureMouse();
        }
    }
}

void EffectAutoDuck::Panel::OnLeftUp(wxMouseEvent& WXUNUSED(evt))
{
    if (mCurrentControlPoint != none) {
        mCurrentControlPoint = none;
        ReleaseMouse();
    }
}

void EffectAutoDuck::Panel::OnMotion(wxMouseEvent& evt)
{
    switch (GetNearestControlPoint(evt.GetPosition())) {
    case none:
        SetCursor(wxNullCursor);
        break;
    case innerFadeDown:
    case innerFadeUp:
    case outerFadeDown:
    case outerFadeUp:
        SetCursor(wxCursor(wxCURSOR_SIZEWE));
        break;
    case duckAmount:
        SetCursor(wxCursor(wxCURSOR_SIZENS));
        break;
    }

    if (mCurrentControlPoint != none) {
        if (!mControlPointMoveActivated) {
            int dist;

            if (mCurrentControlPoint == duckAmount) {
                dist = abs(evt.GetY() - mMouseDownPoint.y);
            } else {
                dist = abs(evt.GetX() - mMouseDownPoint.x);
            }

            if (dist >= CONTROL_POINT_MIN_MOVE) {
                mControlPointMoveActivated = true;
            }
        }

        if (mControlPointMoveActivated) {
            float newValue;

            switch (mCurrentControlPoint) {
            case outerFadeDown:
                newValue = ((double)(FADE_DOWN_START - evt.GetX())) / FADE_SCALE;
                mEffect->mOuterFadeDownLen = std::clamp<double>(newValue, OuterFadeDownLen.min, OuterFadeDownLen.max);
                break;
            case outerFadeUp:
                newValue = ((double)(evt.GetX() - FADE_UP_START)) / FADE_SCALE;
                mEffect->mOuterFadeUpLen = std::clamp<double>(newValue, OuterFadeUpLen.min, OuterFadeUpLen.max);
                break;
            case innerFadeDown:
                newValue = ((double)(evt.GetX() - FADE_DOWN_START)) / FADE_SCALE;
                mEffect->mInnerFadeDownLen = std::clamp<double>(newValue, InnerFadeDownLen.min, InnerFadeDownLen.max);
                break;
            case innerFadeUp:
                newValue = ((double)(FADE_UP_START - evt.GetX())) / FADE_SCALE;
                mEffect->mInnerFadeUpLen = std::clamp<double>(newValue, InnerFadeUpLen.min, InnerFadeUpLen.max);
                break;
            case duckAmount:
                newValue = ((double)(DUCK_AMOUNT_START - evt.GetY())) / DUCK_AMOUNT_SCALE;
                mEffect->mDuckAmountDb = std::clamp<double>(newValue, DuckAmountDb.min, DuckAmountDb.max);
                break;
            case none:
                wxASSERT(false); // should not happen
            }
            mEffect->DoTransferDataToWindow();
            Refresh(false);
        }
    }
}
