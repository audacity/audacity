/**********************************************************************

Audacity: A Digital Audio Editor

EffectScienFilter.h

Norm C
Mitch Golden
Vaughan Johnson (Preview)

***********************************************************************/

#ifndef __AUDACITY_EFFECT_SCIENFILTER__
#define __AUDACITY_EFFECT_SCIENFILTER__

#define MAX_FILTER_ORDER 10

#include <wx/button.h>
#include <wx/panel.h>
#include <wx/dialog.h>
#include <wx/dynarray.h>
#include <wx/intl.h>
#include <wx/stattext.h>
#include <wx/slider.h>
#include <wx/sizer.h>
#include <wx/string.h>
#include <wx/bitmap.h>
#include <wx/choice.h>
#include <wx/checkbox.h>

#if wxUSE_ACCESSIBILITY
#include <wx/access.h>
#endif

#include "Effect.h"
#include "../WaveTrack.h"
#include "../widgets/Ruler.h"
#include "Biquad.h"

class ScienFilterDialog;


class EffectScienFilter: public Effect {

public:

   EffectScienFilter();
   virtual ~EffectScienFilter();

   virtual wxString GetEffectName() {
      return wxString(_("Classic Filter..."));
   }

   virtual std::set<wxString> GetEffectCategories() {
      std::set<wxString> result;
      result.insert(wxT("http://lv2plug.in/ns/lv2core#EQPlugin"));
      return result;
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("Classic Filter"));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Performing Classic Filtering"));
   }

   virtual bool Init();
   virtual bool PromptUser();
   virtual bool DontPromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );
   bool CalcFilterCoeffs (void);

   virtual bool Process();

   // Lowest frequency to display in response graph
   enum {loFreqI=20};

private:
   bool ProcessOne(int count, WaveTrack * t,
      sampleCount start, sampleCount len);

   void Filter(sampleCount len,
      float *buffer);
   void ReadPrefs();

   //int mM;

   float mCutoff;
   int mOrder;
   float mRipple;
   float mStopbandRipple;
   int mFilterType;		// Butterworth etc.
   int mFilterSubtype;	// lowpass, highpass
   BiquadStruct* mpBiquad[5];	// MAX_ORDER/2

   double mdBMax;
   double mdBMin;
   bool mPrompting;
   bool mEditingBatchParams;

public:

   friend class ScienFilterDialog;
   friend class ScienFilterPanel;
};


class ScienFilterPanel: public wxPanel
{
public:
   ScienFilterPanel( double loFreq, double hiFreq,
      ScienFilterDialog *parent,
      wxWindowID id,
      const wxPoint& pos = wxDefaultPosition,
      const wxSize& size = wxDefaultSize);
   ~ScienFilterPanel();

#if 0
   Needed only if user can draw in the graph
      void OnMouseEvent(wxMouseEvent & event);
   void OnCaptureLost(wxMouseCaptureLostEvent & event);
#endif
   void OnPaint(wxPaintEvent & event);
   void OnSize (wxSizeEvent & event);

   // We don't need or want to accept focus.
   bool AcceptsFocus() const { return false; }

   float dBMax;
   float dBMin;

private:

   wxBitmap *mBitmap;
   wxRect mEnvRect;
   ScienFilterDialog *mParent;
   int mWidth;
   int mHeight;

   double mLoFreq;
   double mHiFreq;

   DECLARE_EVENT_TABLE()
};


// WDR: class declarations

//----------------------------------------------------------------------------
// ScienFilterDialog
//----------------------------------------------------------------------------

class ScienFilterDialog: public wxDialog //, public XMLTagHandler
{
public:
   // constructors and destructors
   ScienFilterDialog(EffectScienFilter * effect,
      double loFreq, double hiFreq,
      wxWindow *parent, wxWindowID id,
      const wxString &title,
      const wxPoint& pos = wxDefaultPosition,
      const wxSize& size = wxDefaultSize,
      long style = wxDEFAULT_DIALOG_STYLE );
   ~ScienFilterDialog();

   // WDR: method declarations for ScienFilterDialog
   virtual bool Validate();
   virtual bool TransferDataToWindow();
   virtual bool TransferGraphLimitsFromWindow();
   virtual bool CalcFilter(EffectScienFilter* effect);
   float FilterMagnAtFreq (float Freq);

   wxChoice* mFilterTypeCtl;
   wxChoice* mFilterSubTypeCtl;
   wxChoice* mFilterOrderCtl;

   float Cutoff;
   int Order;
   float Ripple;
   float StopbandRipple;
   int FilterType;		// Butterworth etc.
   int FilterSubtype;	// lowpass, highpass

   float dBMin;
   float dBMax;
   int interp;
   RulerPanel *dBRuler;
   RulerPanel *freqRuler;

private:
   void MakeScienFilterDialog();
   void Finish(bool ok);

private:
   // WDR: member variable declarations for ScienFilterDialog

   enum
   {
      ID_FILTERPANEL = 10000,
      ID_DBMAX,
      ID_DBMIN,
      ID_FILTER_TYPE,
      ID_FILTER_SUBTYPE,
      ID_FILTER_ORDER,
      ID_RIPPLE,
      ID_CUTOFF,
      ID_STOPBAND_RIPPLE
   };

private:
   // WDR: handler declarations for ScienFilterDialog
   void OnPaint( wxPaintEvent &event );
   void OnSize( wxSizeEvent &event );
   void OnErase( wxEraseEvent &event );
   void OnSlider( wxCommandEvent &event );

   void OnOrder( wxCommandEvent &event );
   void OnCutoff( wxCommandEvent &event );
   void OnRipple( wxCommandEvent &event );
   void OnStopbandRipple( wxCommandEvent &event );
   void OnFilterType( wxCommandEvent &event );
   void OnFilterSubtype( wxCommandEvent &event );

   void OnSliderDBMAX( wxCommandEvent &event );
   void OnSliderDBMIN( wxCommandEvent &event );
   void OnPreview(wxCommandEvent &event);
   void OnOk( wxCommandEvent &event );
   void OnCancel( wxCommandEvent &event );
   void EnableDisableRippleCtl (int FilterType);
private:
   EffectScienFilter * m_pEffect;

   double mLoFreq;
   double mNyquist;

   ScienFilterPanel *mPanel;
   wxSlider *dBMinSlider;
   wxSlider *dBMaxSlider;
   wxBoxSizer *szrV;
   wxFlexGridSizer *szr3;
   wxBoxSizer *szr4;
   wxBoxSizer *szr2;
   wxFlexGridSizer *szr1;
   wxSize size;
   wxTextCtrl* mRippleCtl;
   wxTextCtrl* mStopbandRippleCtl;
   wxTextCtrl* mCutoffCtl;

   // sizers for pass and stop-band attenuations
   wxBoxSizer *szrPass;
   wxBoxSizer *szrStop;

private:
   DECLARE_EVENT_TABLE()

};

#if wxUSE_ACCESSIBILITY

// ScienceFilter and Equalisation effects both need SliderAx class.
// For now it is declared and defined in Equalisation effect.
// TODO: Move it to its own file.

#endif // wxUSE_ACCESSIBILITY

#endif
