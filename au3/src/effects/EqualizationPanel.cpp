/**********************************************************************

   Audacity: A Digital Audio Editor

   EqualizationPanel.cpp

   Mitch Golden
   Vaughan Johnson (Preview)
   Martyn Shaw (FIR filters, response curve, graphic EQ)

   Paul Licameli split from Equalization.cpp

*//****************************************************************//**

   \class EqualizationPanel
   \brief EqualizationPanel is used with EqualizationDialog and controls
   a graph for EffectEqualization.  We should look at amalgamating the
   various graphing code, such as provided by FrequencyPlotDialog and FilterPanel.

*//*******************************************************************/
#include "EqualizationPanel.h"
#include "EqualizationCurvesList.h"
#include "EqualizationFilter.h"

#include <wx/dcclient.h>
#include <wx/dcmemory.h>
#include <wx/settings.h>
#include "AColor.h"
#include "Envelope.h"
#include "../EnvelopeEditor.h"
#include "FFT.h"
#include "Theme.h"
#include "../TrackArtist.h"
#include "ViewInfo.h"
#include "../widgets/RulerPanel.h"
#include "AllThemeResources.h"

//----------------------------------------------------------------------------
// EqualizationPanel
//----------------------------------------------------------------------------

BEGIN_EVENT_TABLE(EqualizationPanel, wxPanelWrapper)
EVT_PAINT(EqualizationPanel::OnPaint)
EVT_MOUSE_EVENTS(EqualizationPanel::OnMouseEvent)
EVT_MOUSE_CAPTURE_LOST(EqualizationPanel::OnCaptureLost)
EVT_SIZE(EqualizationPanel::OnSize)
EVT_IDLE(EqualizationPanel::OnIdle)
END_EVENT_TABLE()

EqualizationPanel::EqualizationPanel(
    wxWindow* parent, wxWindowID winid, EqualizationCurvesList& curvesList,
    RulerPanel& freqRuler, RulerPanel& dbRuler)
    : wxPanelWrapper(parent, winid)
    , mCurvesList{curvesList}
    , mFreqRuler{freqRuler}
    , mdBRuler{dbRuler}
{
    auto& parameters = mCurvesList.mParameters;
    mParent = parent;

    mBitmap = NULL;
    mWidth = 0;
    mHeight = 0;

    mLinEditor = std::make_unique<EnvelopeEditor>(
        parameters.mLinEnvelope, false);
    mLogEditor = std::make_unique<EnvelopeEditor>(
        parameters.mLogEnvelope, false);

    // Initial Recalc() needed to populate mOutr before the first paint event
    Recalc();
}

EqualizationPanel::~EqualizationPanel()
{
    if (HasCapture()) {
        ReleaseMouse();
    }
}

void EqualizationPanel::Recalc()
{
    auto& parameters = mCurvesList.mParameters;
    const auto& windowSize = parameters.mWindowSize;

    mOutr = Floats{ windowSize };

    parameters.CalcFilter();  //to calculate the actual response
    InverseRealFFT(windowSize,
                   parameters.mFilterFuncR.get(),
                   parameters.mFilterFuncI.get(), mOutr.get());
}

void EqualizationPanel::OnSize(wxSizeEvent& WXUNUSED(event))
{
    Refresh(false);
}

#include "../TrackPanelDrawingContext.h"
void EqualizationPanel::OnPaint(wxPaintEvent& WXUNUSED(event))
{
    const auto& parameters = mCurvesList.mParameters;
    const auto& dBMax = parameters.mdBMax;
    const auto& dBMin = parameters.mdBMin;
    const auto& M = parameters.mM;
    const auto& drawMode = parameters.mDrawMode;
    const auto& drawGrid = parameters.mDrawGrid;
    const auto& loFreq = parameters.mLoFreq;
    const auto& hiFreq = parameters.mHiFreq;
    const auto& windowSize = parameters.mWindowSize;
    const auto& filterFuncR = parameters.mFilterFuncR;
    const auto& filterFuncI = parameters.mFilterFuncI;

    wxPaintDC dc(this);
    int width, height;
    GetSize(&width, &height);

    if (!mBitmap || mWidth != width || mHeight != height) {
        mWidth = width;
        mHeight = height;
        mBitmap = std::make_unique<wxBitmap>(mWidth, mHeight, 24);
    }

    wxBrush bkgndBrush(wxSystemSettings::GetColour(wxSYS_COLOUR_3DFACE));

    wxMemoryDC memDC;
    memDC.SelectObject(*mBitmap);

    wxRect bkgndRect;
    bkgndRect.x = 0;
    bkgndRect.y = 0;
    bkgndRect.width = mWidth;
    bkgndRect.height = mHeight;
    memDC.SetBrush(bkgndBrush);
    memDC.SetPen(*wxTRANSPARENT_PEN);
    memDC.DrawRectangle(bkgndRect);

    bkgndRect.y = mHeight;
    memDC.DrawRectangle(bkgndRect);

    wxRect border;
    border.x = 0;
    border.y = 0;
    border.width = mWidth;
    border.height = mHeight;

    memDC.SetBrush(*wxWHITE_BRUSH);
    memDC.SetPen(*wxBLACK_PEN);
    memDC.DrawRectangle(border);

    mEnvRect = border;
    mEnvRect.Deflate(PANELBORDER, PANELBORDER);

    // Pure blue x-axis line
    memDC.SetPen(wxPen(theTheme.Colour(clrGraphLines), 1, wxPENSTYLE_SOLID));
    int center = (int)(mEnvRect.height * dBMax / (dBMax - dBMin) + .5);
    AColor::Line(memDC,
                 mEnvRect.GetLeft(), mEnvRect.y + center,
                 mEnvRect.GetRight(), mEnvRect.y + center);

    // Draw the grid, if asked for.  Do it now so it's underneath the main plots.
    if (drawGrid) {
        mFreqRuler.ruler.DrawGrid(memDC, mEnvRect.height, true, true, PANELBORDER, PANELBORDER);
        mdBRuler.ruler.DrawGrid(memDC, mEnvRect.width, true, true, PANELBORDER, PANELBORDER);
    }

    // Med-blue envelope line
    memDC.SetPen(wxPen(theTheme.Colour(clrGraphLines), 3, wxPENSTYLE_SOLID));

    // Draw envelope
    int x, y, xlast = 0, ylast = 0;
    {
        Doubles values{ size_t(mEnvRect.width) };
        parameters.ChooseEnvelopeToPaint()
        .GetValues(values.get(), mEnvRect.width, 0.0, 1.0 / mEnvRect.width);
        bool off = false, off1 = false;
        for (int i = 0; i < mEnvRect.width; i++) {
            x = mEnvRect.x + i;
            y = lrint(mEnvRect.height * ((dBMax - values[i]) / (dBMax - dBMin)) + .25); //needs more optimising, along with'what you get'?
            if (y >= mEnvRect.height) {
                y = mEnvRect.height - 1;
                off = true;
            } else {
                off = false;
                off1 = false;
            }
            if ((i != 0) & (!off1)) {
                AColor::Line(memDC, xlast, ylast,
                             x, mEnvRect.y + y);
            }
            off1 = off;
            xlast = x;
            ylast = mEnvRect.y + y;
        }
    }

    //Now draw the actual response that you will get.
    //mFilterFunc has a linear scale, window has a log one so we have to fiddle about
    memDC.SetPen(wxPen(theTheme.Colour(clrResponseLines), 1, wxPENSTYLE_SOLID));
    double scale = (double)mEnvRect.height / (dBMax - dBMin); //pixels per dB
    double yF;  //gain at this freq
    double delta = hiFreq / (((double)windowSize / 2.));  //size of each freq bin

    bool lin = parameters.IsLinear();  // log or lin scale?

    double loLog = log10(loFreq);
    double step = lin ? hiFreq : (log10(hiFreq) - loLog);
    step /= ((double)mEnvRect.width - 1.);
    double freq;  //actual freq corresponding to x position
    int halfM = (M - 1) / 2;
    int n;  //index to mFreqFunc
    for (int i=0; i < mEnvRect.width; i++) {
        x = mEnvRect.x + i;
        freq = lin ? step * i : pow(10., loLog + i * step); //Hz
        if ((lin ? step : (pow(10., loLog + (i + 1) * step) - freq)) < delta) { //not enough resolution in FFT
            // set up for calculating cos using recurrence - faster than calculating it directly each time
            double theta = M_PI * freq / hiFreq; //radians, normalized
            double wtemp = sin(0.5 * theta);
            double wpr = -2.0 * wtemp * wtemp;
            double wpi = -1.0 * sin(theta);
            double wr = cos(theta * halfM);
            double wi = sin(theta * halfM);

            yF = 0.;
            for (int j=0; j < halfM; j++) {
                yF += 2. * mOutr[j] * wr; // This works for me, compared to the previous version.  Compare wr to cos(theta*(halfM-j)).  Works for me.  Keep everything as doubles though.
                // do recurrence
                wr = (wtemp = wr) * wpr - wi * wpi + wr;
                wi = wi * wpr + wtemp * wpi + wi;
            }
            yF += mOutr[halfM];
            yF = fabs(yF);
            if (yF != 0.) {
                yF = LINEAR_TO_DB(yF);
            } else {
                yF = dBMin;
            }
        } else { //use FFT, it has enough resolution
            n = (int)(freq / delta + .5);
            if (pow(filterFuncR[n], 2) + pow(filterFuncI[n], 2) != 0.) {
                yF = 10.0 * log10(pow(filterFuncR[n], 2) + pow(filterFuncI[n], 2)); //10 here, a power
            } else {
                yF = dBMin;
            }
        }
        if (yF < dBMin) {
            yF = dBMin;
        }
        yF = center - scale * yF;
        if (yF > mEnvRect.height) {
            yF = mEnvRect.height - 1;
        }
        if (yF < 0.) {
            yF=0.;
        }
        y = (int)(yF + .5);

        if (i != 0) {
            AColor::Line(memDC, xlast, ylast, x, mEnvRect.y + y);
        }
        xlast = x;
        ylast = mEnvRect.y + y;
    }

    memDC.SetPen(*wxBLACK_PEN);
    if (drawMode) {
        ZoomInfo zoomInfo(0.0, mEnvRect.width - 1);

        // Back pointer to TrackPanel won't be needed in the one drawing
        // function we use here
        TrackArtist artist(nullptr);

        artist.pZoomInfo = &zoomInfo;
        TrackPanelDrawingContext context{ memDC, {}, {}, &artist };
        EnvelopeEditor::DrawPoints(parameters.ChooseEnvelopeToPaint(),
                                   context, mEnvRect, false, 0.0,
                                   dBMin, dBMax, false);
    }

    dc.Blit(0, 0, mWidth, mHeight, &memDC, 0, 0, wxCOPY, FALSE);
}

void EqualizationPanel::OnMouseEvent(wxMouseEvent& event)
{
    const auto& parameters = mCurvesList.mParameters;
    const auto& dBMax = parameters.mdBMax;
    const auto& dBMin = parameters.mdBMin;
    const auto& drawMode = parameters.mDrawMode;
    const auto& lin = parameters.mLin;

    if (!drawMode) {
        return;
    }

    if (event.ButtonDown() && !HasCapture()) {
        CaptureMouse();
    }

    auto& pEditor = (lin ? mLinEditor : mLogEditor);
    if (pEditor->MouseEvent(event, mEnvRect, ZoomInfo(0.0, mEnvRect.width),
                            false, 0.0,
                            dBMin, dBMax)
        ) {
        mCurvesList.ForceRecalc();
    }

    if (event.ButtonUp() && HasCapture()) {
        mCurvesList.EnvelopeUpdated();
        ReleaseMouse();
    }
}

void EqualizationPanel::OnCaptureLost(wxMouseCaptureLostEvent& WXUNUSED(event))
{
    if (HasCapture()) {
        ReleaseMouse();
    }
}

void EqualizationPanel::OnIdle(wxIdleEvent& event)
{
    if (mCurvesList.mRecalcRequired) {
        Recalc();
        Refresh(false);
        mCurvesList.mRecalcRequired = false;
    }
}
