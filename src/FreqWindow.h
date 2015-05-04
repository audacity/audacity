/**********************************************************************

  Audacity: A Digital Audio Editor

  FreqWindow.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_FREQ_WINDOW__
#define __AUDACITY_FREQ_WINDOW__

#include <memory>
#include <vector>
#include <wx/brush.h>
#include <wx/dcmemory.h>
#include <wx/frame.h>
#include <wx/panel.h>
#include <wx/checkbox.h>
#include <wx/dialog.h>
#include <wx/gdicmn.h>
#include <wx/pen.h>
#include <wx/font.h>
#include <wx/scrolbar.h>
#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/stattext.h>
#include <wx/statusbr.h>
#include <wx/textctrl.h>
#include <wx/utils.h>

#include "widgets/Ruler.h"

class wxStatusBar;
class wxButton;
class wxChoice;

class FreqWindow;
class FreqGauge;

class TrackList;

DECLARE_EXPORTED_EVENT_TYPE(AUDACITY_DLL_API, EVT_FREQWINDOW_RECALC, -1);

class SpectrumAnalyst
{
public:

   enum Algorithm {
      Spectrum,
      Autocorrelation,
      CubeRootAutocorrelation,
      EnhancedAutocorrelation,
      Cepstrum,

      NumAlgorithms
   };

   SpectrumAnalyst();
   ~SpectrumAnalyst();

   // Return true iff successful
   bool Calculate(Algorithm alg,
      int windowFunc, // see FFT.h for values
      int windowSize, double rate,
      const float *data, int dataLen,
      float *pYMin = NULL, float *pYMax = NULL, // outputs
      FreqGauge *progress = NULL);

   const float *GetProcessed() const;
   int GetProcessedSize() const;

   float GetProcessedValue(float freq0, float freq1) const;
   float FindPeak(float xPos, float *pY) const;

private:
   float CubicInterpolate(float y0, float y1, float y2, float y3, float x) const;
   float CubicMaximize(float y0, float y1, float y2, float y3, float * max) const;

private:
   Algorithm mAlg;
   double mRate;
   int mWindowSize;
   std::vector<float> mProcessed;
};

class FreqGauge : public wxStatusBar
{
public:
   FreqGauge(wxWindow * parent);

   void SetRange(int range, int bar = 12, int gap = 3);
   void SetValue(int value);
   void Reset();

private:
   wxRect mRect;
   int mRange;
   int mCur;
   int mLast;
   int mInterval;
   int mBar;
   int mGap;
   int mMargin;
};

class FreqPlot : public wxWindow
{
public:
   FreqPlot(wxWindow *parent);

   // We don't need or want to accept focus.
   bool AcceptsFocus() const;

private:
   void OnPaint(wxPaintEvent & event);
   void OnErase(wxEraseEvent & event);
   void OnMouseEvent(wxMouseEvent & event);

private:
    FreqWindow *freqWindow;

    DECLARE_EVENT_TABLE();
};

class FreqWindow : public wxDialog
{
public:
   FreqWindow(wxWindow *parent, wxWindowID id,
              const wxString & title, const wxPoint & pos);
   virtual ~ FreqWindow();

   virtual bool Show( bool show = true );

private:
   void GetAudio();

   void PlotMouseEvent(wxMouseEvent & event);
   void PlotPaint(wxPaintEvent & event);

   void OnCloseWindow(wxCloseEvent & event);
   void OnCloseButton(wxCommandEvent & event);
   void OnSize(wxSizeEvent & event);
   void OnPanScroller(wxScrollEvent & event);
   void OnZoomSlider(wxCommandEvent & event);
   void OnAlgChoice(wxCommandEvent & event);
   void OnSizeChoice(wxCommandEvent & event);
   void OnFuncChoice(wxCommandEvent & event);
   void OnAxisChoice(wxCommandEvent & event);
   void OnExport(wxCommandEvent & event);
   void OnReplot(wxCommandEvent & event);
   void OnGridOnOff(wxCommandEvent & event);
   void OnRecalc(wxCommandEvent & event);

   void SendRecalcEvent();
   void Recalc();
   void DrawPlot();
   void DrawBackground(wxMemoryDC & dc);

 private:
   bool mDrawGrid;
   int mSize;
   SpectrumAnalyst::Algorithm mAlg;
   int mFunc;
   int mAxis;
   int dBRange;
   AudacityProject *p;

#ifdef __WXMSW__
   static const int fontSize = 8;
#else
   static const int fontSize = 10;
#endif

   RulerPanel *vRuler;
   RulerPanel *hRuler;
   FreqPlot *mFreqPlot;
   FreqGauge *mProgress;

   wxRect mPlotRect;

   wxFont mFreqFont;

   wxCursor *mArrowCursor;
   wxCursor *mCrossCursor;

   wxButton *mCloseButton;
   wxButton *mExportButton;
   wxButton *mReplotButton;
   wxCheckBox *mGridOnOff;
   wxChoice *mAlgChoice;
   wxChoice *mSizeChoice;
   wxChoice *mFuncChoice;
   wxChoice *mAxisChoice;
   wxScrollBar *mPanScroller;
   wxSlider *mZoomSlider;
   wxTextCtrl *mCursorText;
   wxTextCtrl *mPeakText;


   double mRate;
   int mDataLen;
   float *mData;
   int mWindowSize;

   bool mLogAxis;
   float mYMin;
   float mYMax;
   float mYStep;

   wxBitmap *mBitmap;

   int mMouseX;
   int mMouseY;

   std::auto_ptr<SpectrumAnalyst> mAnalyst;

   DECLARE_EVENT_TABLE();

   friend class FreqPlot;
};

#endif
