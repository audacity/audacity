/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectScienFilter.h

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
      return wxString(_("Scientific Filter..."));
   }

   virtual std::set<wxString> GetEffectCategories() {
      std::set<wxString> result;
      result.insert(wxT("http://lv2plug.in/ns/lv2core#EQPlugin"));
      return result;
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("Scientific Filter"));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Performing ScienFilter"));
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
               //long windowSize, wxString CurveName, bool disallowCustom,
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

private:
   DECLARE_EVENT_TABLE()

};

#if wxUSE_ACCESSIBILITY

class SliderAx: public wxWindowAccessible
{
public:
   SliderAx(wxWindow * window, wxString fmt);

   virtual ~ SliderAx();

   // Retrieves the address of an IDispatch interface for the specified child.
   // All objects must support this property.
   virtual wxAccStatus GetChild( int childId, wxAccessible** child );

   // Gets the number of children.
   virtual wxAccStatus GetChildCount(int* childCount);

   // Gets the default action for this object (0) or > 0 (the action for a child).
   // Return wxACC_OK even if there is no action. actionName is the action, or the empty
   // string if there is no action.
   // The retrieved string describes the action that is performed on an object,
   // not what the object does as a result. For example, a toolbar button that prints
   // a document has a default action of "Press" rather than "Prints the current document."
   virtual wxAccStatus GetDefaultAction( int childId, wxString *actionName );

   // Returns the description for this object or a child.
   virtual wxAccStatus GetDescription( int childId, wxString *description );

   // Gets the window with the keyboard focus.
   // If childId is 0 and child is NULL, no object in
   // this subhierarchy has the focus.
   // If this object has the focus, child should be 'this'.
   virtual wxAccStatus GetFocus( int *childId, wxAccessible **child );

   // Returns help text for this object or a child, similar to tooltip text.
   virtual wxAccStatus GetHelpText( int childId, wxString *helpText );

   // Returns the keyboard shortcut for this object or child.
   // Return e.g. ALT+K
   virtual wxAccStatus GetKeyboardShortcut( int childId, wxString *shortcut );

   // Returns the rectangle for this object (id = 0) or a child element (id > 0).
   // rect is in screen coordinates.
   virtual wxAccStatus GetLocation( wxRect& rect, int elementId );

   // Gets the name of the specified object.
   virtual wxAccStatus GetName( int childId, wxString *name );

   // Returns a role constant.
   virtual wxAccStatus GetRole( int childId, wxAccRole *role );

   // Gets a variant representing the selected children
   // of this object.
   // Acceptable values:
   // - a null variant (IsNull() returns TRUE)
   // - a list variant (GetType() == wxT("list"))
   // - an integer representing the selected child element,
   //   or 0 if this object is selected (GetType() == wxT("long"))
   // - a "void*" pointer to a wxAccessible child object
   virtual wxAccStatus GetSelections( wxVariant *selections );

   // Returns a state constant.
   virtual wxAccStatus GetState(int childId, long* state);

   // Returns a localized string representing the value for the object
   // or child.
   virtual wxAccStatus GetValue(int childId, wxString* strValue);

private:
   wxWindow *mParent;
   wxString mFmt;
};

#endif // wxUSE_ACCESSIBILITY

#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 309f263d-748c-4dc0-9e68-9e86732890bb

