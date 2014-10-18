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
#include <wx/frame.h>
#include <wx/panel.h>
#include <wx/checkbox.h>
#include <wx/dialog.h>
#include <wx/gdicmn.h>
#include <wx/pen.h>
#include <wx/font.h>
#include <wx/sizer.h>
#include <wx/stattext.h>

#include "widgets/Ruler.h"

class wxStatusBar;
class wxButton;
class wxChoice;

class FreqWindow;

class TrackList;

class ProgressDialog;

class FreqPlot:public wxWindow {
 public:
   FreqPlot(wxWindow * parent, wxWindowID id,
            const wxPoint & pos, const wxSize & size);

   void OnMouseEvent(wxMouseEvent & event);
   void OnPaint(wxPaintEvent & event);
   void OnErase(wxEraseEvent & event);

 private:

    FreqWindow * freqWindow;

    DECLARE_EVENT_TABLE()
};

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
      float *pYMin = 0, float *pYMax = 0, // outputs
      ProgressDialog *progress = 0);

   const float *GetProcessed() const { return &mProcessed[0]; }
   int GetProcessedSize() const { return mProcessed.size() / 2; }

   float GetProcessedValue(float freq0, float freq1) const;
   float FindPeak(float xPos, float *pY) const;

private:

   Algorithm mAlg;
   double mRate;
   int mWindowSize;
   std::vector<float> mProcessed;
};

class FreqWindow:public wxDialog {
 public:
   FreqWindow(wxWindow * parent, wxWindowID id,
              const wxString & title, const wxPoint & pos);

   virtual ~ FreqWindow();
   void GetAudio();

   void Plot();

   void PlotMouseEvent(wxMouseEvent & event);
   void PlotPaint(wxPaintEvent & event);

   void OnCloseWindow(wxCloseEvent & event);
   void OnCloseButton(wxCommandEvent & event);
   void OnSize(wxSizeEvent & event);
   void OnAlgChoice(wxCommandEvent & event);
   void OnSizeChoice(wxCommandEvent & event);
   void OnFuncChoice(wxCommandEvent & event);
   void OnAxisChoice(wxCommandEvent & event);
   void OnExport(wxCommandEvent & event);
   void OnReplot(wxCommandEvent & event);
   void OnGridOnOff(wxCommandEvent & event);

   void Recalc();
   void DrawPlot();

 private:
   float *mBuffer;
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

   wxStatusBar *mInfo;

   FreqPlot *mFreqPlot;

   wxFont mFreqFont;

   wxCursor *mArrowCursor;
   wxCursor *mCrossCursor;

   wxButton *mCloseButton;
   wxButton *mExportButton;
   wxButton *mReplotButton;
   wxChoice *mAlgChoice;
   wxChoice *mSizeChoice;
   wxChoice *mFuncChoice;
   wxChoice *mAxisChoice;
   wxCheckBox *mGridOnOff;

   wxRect mPlotRect;
   wxRect mInfoRect;
   wxRect mUpdateRect;
   wxFlexGridSizer *szr;
   RulerPanel *vRuler;
   RulerPanel *hRuler;
   wxStaticText *mInfoText;
   int mLeftMargin;
   int mBottomMargin;
   int mInfoHeight;

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

   DECLARE_EVENT_TABLE()
};

#endif
