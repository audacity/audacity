/**********************************************************************

  Audacity: A Digital Audio Editor

  FreqWindow.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_FREQ_WINDOW__
#define __AUDACITY_FREQ_WINDOW__

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

class FreqWindow;

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
   int mAlg;
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
   float *mProcessed;
   int mProcessedSize;

   bool mLogAxis;
   float mYMin;
   float mYMax;
   float mYStep;

   wxBitmap *mBitmap;

   int mMouseX;
   int mMouseY;

   float GetProcessedValue(float freq0, float freq1);

   DECLARE_EVENT_TABLE()
};

#endif
