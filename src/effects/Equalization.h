/**********************************************************************

  Audacity: A Digital Audio Editor

  Equalization.h

  Mitch Golden
  Vaughan Johnson (Preview)

***********************************************************************/

#ifndef __AUDACITY_EFFECT_EQUALIZATION__
#define __AUDACITY_EFFECT_EQUALIZATION__
#define NUMBER_OF_BANDS 31
#define NUM_PTS 180
#define PANELBORDER 1   // only increase from '1' for testing purposes - MJS

#include <wx/button.h>
#include <wx/panel.h>
#include <wx/dialog.h>
#include <wx/dynarray.h>
#include <wx/intl.h>
#include <wx/listctrl.h>
#include <wx/stattext.h>
#include <wx/slider.h>
#include <wx/sizer.h>
#include <wx/string.h>
#include <wx/bitmap.h>
#include <wx/choice.h>
#include <wx/radiobut.h>
#include <wx/checkbox.h>

#if wxUSE_ACCESSIBILITY
#include <wx/access.h>
#endif

#include "Effect.h"
#include "../Envelope.h"
#include "../WaveTrack.h"
#include "../xml/XMLTagHandler.h"
#include "../widgets/Grid.h"
#include "../widgets/Ruler.h"
#include "../RealFFTf.h"

class EqualizationDialog;

//
// One point in a curve
//
class EQPoint
{
public:
   EQPoint( const double f, const double d ) { Freq = f; dB = d; }
   double Freq;
   double dB;
};
WX_DECLARE_OBJARRAY( EQPoint, EQPointArray);

//
// One curve in a list
//
// LLL:  This "really" isn't needed as the EQPointArray could be
//       attached as wxClientData to the wxChoice entries.  I
//       didn't realize this until after the fact and am too
//       lazy to change it.  (But, hollar if you want me to.)
//
class EQCurve
{
public:
   EQCurve( const wxString & name ) { Name = name; }
   EQCurve( const wxChar * name ) { Name = name; }
   wxString Name;
   EQPointArray points;
};
WX_DECLARE_OBJARRAY( EQCurve, EQCurveArray );

#ifdef EXPERIMENTAL_EQ_SSE_THREADED
class EffectEqualization48x;
#endif

class EffectEqualization: public Effect {

public:

   EffectEqualization();
   virtual ~EffectEqualization();

   virtual wxString GetEffectName() {
      return wxString(_("Equalization..."));
   }

   virtual std::set<wxString> GetEffectCategories() {
      std::set<wxString> result;
      result.insert(wxT("http://lv2plug.in/ns/lv2core#EQPlugin"));
      return result;
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("Equalization"));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Performing Equalization"));
   }

   virtual bool Init();
   virtual bool PromptUser();
   virtual bool DontPromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );

   virtual bool Process();

   // Number of samples in an FFT window
   enum {windowSize=16384};   //MJS - work out the optimum for this at run time?  Have a dialog box for it?

   // Low frequency of the FFT.  20Hz is the
   // low range of human hearing
   enum {loFreqI=20};



private:
   bool ProcessOne(int count, WaveTrack * t,
                   sampleCount start, sampleCount len);

   void Filter(sampleCount len,
               float *buffer);

   void ReadPrefs();

   HFFT hFFT;
   float *mFFTBuffer;
   float *mFilterFuncR;
   float *mFilterFuncI;
   int mM;
   wxString mCurveName;
   bool mLin;
   double mdBMax;
   double mdBMin;
   bool mDrawMode;
   int mInterp;
   bool mPrompting;
   bool mDrawGrid;
   bool mEditingBatchParams;
#ifdef EXPERIMENTAL_EQ_SSE_THREADED
   bool mBench;
   EffectEqualization48x *mEffectEqualization48x;
friend class EffectEqualization48x;
#endif

public:

friend class EqualizationDialog;
friend class EqualizationPanel;
};


class EqualizationPanel: public wxPanel
{
public:
   EqualizationPanel( double loFreq, double hiFreq,
               Envelope *env,
               EqualizationDialog *parent,
               float *filterFuncR, float *filterFuncI, long windowSize,
               wxWindowID id,
               const wxPoint& pos = wxDefaultPosition,
               const wxSize& size = wxDefaultSize);
   ~EqualizationPanel();

   void OnMouseEvent(wxMouseEvent & event);
   void OnCaptureLost(wxMouseCaptureLostEvent & event);
   void OnPaint(wxPaintEvent & event);
   void OnSize (wxSizeEvent & event);

   // We don't need or want to accept focus.
   bool AcceptsFocus() const { return false; }

   void Recalc();

   int M;
   float dBMax;
   float dBMin;
   bool RecalcRequired;

   Envelope *mEnvelope;

private:

   wxBitmap *mBitmap;
   wxRect mEnvRect;
   EqualizationDialog *mParent;
   int mWidth;
   int mHeight;
   long mWindowSize;
   float *mFilterFuncR;
   float *mFilterFuncI;
   float *mOutr;
   float *mOuti;

   double mLoFreq;
   double mHiFreq;

   DECLARE_EVENT_TABLE()
};


// WDR: class declarations

//----------------------------------------------------------------------------
// EqualizationDialog
//----------------------------------------------------------------------------

class EqualizationDialog: public wxDialog, public XMLTagHandler
{
public:
   // constructors and destructors
   EqualizationDialog(EffectEqualization * effect,
               double loFreq, double hiFreq,
               float *filterFuncR, float *filterFuncI, long windowSize, wxString CurveName, bool disallowCustom,
               wxWindow *parent, wxWindowID id,
               const wxString &title,
               const wxPoint& pos = wxDefaultPosition,
               const wxSize& size = wxDefaultSize,
               long style = wxDEFAULT_DIALOG_STYLE );
   ~EqualizationDialog();

   // WDR: method declarations for EqualizationDialog
   virtual bool Validate();
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();
   virtual bool CalcFilter();

   void EnvelopeUpdated();
   void EnvelopeUpdated(Envelope *env, bool lin);
   static const double thirdOct[];
   wxRadioButton *mFaderOrDraw[2];
#ifdef EXPERIMENTAL_EQ_SSE_THREADED
   wxRadioButton *mMathProcessingType[5]; // default, sse, sse threaded, AVX, AVX threaded (note AVX is not implemented yet
#endif
   wxChoice *mInterpChoice;
   wxCheckBox *mLinFreq;
   int M;
   wxString curveName;
   bool linCheck;
   float dBMin;
   float dBMax;
   double whens[NUM_PTS];
   double whenSliders[NUMBER_OF_BANDS+1];
   int bandsInUse;
   bool drawMode;
   int interp;
   bool drawGrid;
   RulerPanel *dBRuler;
   RulerPanel *freqRuler;
   friend class EditCurvesDialog;

private:
   void MakeEqualizationDialog();
   void CreateChoice();
   void LoadCurves(wxString fileName = wxT(""), bool append = false);
   void SaveCurves(wxString fileName = wxT(""));
   void Select(int sel);
   void setCurve(int currentCurve);
   void setCurve(wxString curveName);
   void setCurve(void);
   void GraphicEQ(Envelope *env);
   void spline(double x[], double y[], int n, double y2[]);
   double splint(double x[], double y[], int n, double y2[], double xr);
   void LayoutEQSliders();
   void RevertCustom();
   void Finish(bool ok);

   // XMLTagHandler callback methods for loading and saving
   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs);
   XMLTagHandler *HandleXMLChild(const wxChar *tag);
   void WriteXML(XMLWriter &xmlFile);

private:
   // WDR: member variable declarations for EqualizationDialog

   enum
   {
      ID_FILTERPANEL = 10000,
      ID_LENGTH,
      ID_DBMAX,
      ID_DBMIN,
      ID_CURVE,
      ID_MANAGE,
      ID_DELETE,
      ID_CLEAR,
      ID_INVERT,
      drawRadioID,
      sliderRadioID,
#ifdef EXPERIMENTAL_EQ_SSE_THREADED
      defaultMathRadioID, 
      sSERadioID, 
      sSEThreadedRadioID, 
      aVXRadioID, 
      aVXThreadedRadioID, 
      ID_BENCH,
#endif
      ID_INTERP,
      ID_LIN_FREQ,
      GridOnOffID,
      ID_SLIDER   // needs to come last
   };

private:
   // WDR: handler declarations for EqualizationDialog
   void OnPaint( wxPaintEvent &event );
   void OnSize( wxSizeEvent &event );
   void OnErase( wxEraseEvent &event );
   void OnSlider( wxCommandEvent &event );
   void OnInterp( wxCommandEvent &event );
   void OnSliderM( wxCommandEvent &event );
   void OnSliderDBMAX( wxCommandEvent &event );
   void OnSliderDBMIN( wxCommandEvent &event );
   void OnDrawRadio(wxCommandEvent &event );
   void OnSliderRadio(wxCommandEvent &event );
#ifdef EXPERIMENTAL_EQ_SSE_THREADED
   void OnProcessingRadio(wxCommandEvent &event );
   void OnBench( wxCommandEvent & event);
#endif
   void OnLinFreq(wxCommandEvent &event );
   void UpdateGraphic(void);
   void EnvLogToLin(void);
   void EnvLinToLog(void);
   void ErrMin(void);
   void OnCurve( wxCommandEvent &event );
   void OnManage( wxCommandEvent &event );
   void OnClear( wxCommandEvent &event );
   void OnInvert( wxCommandEvent &event );
   void OnPreview(wxCommandEvent &event);
   void OnOk( wxCommandEvent &event );
   void OnCancel( wxCommandEvent &event );
   void OnGridOnOff( wxCommandEvent &event );
private:
   EffectEqualization * m_pEffect;

   bool mDisallowCustom;
   double mLoFreq;
   double mHiFreq;
   float *mFilterFuncR;
   float *mFilterFuncI;
   long mWindowSize;
   bool mDirty;
   wxSlider * m_sliders[NUMBER_OF_BANDS];
   int m_sliders_old[NUMBER_OF_BANDS];
   double m_EQVals[NUMBER_OF_BANDS+1];

   EqualizationPanel *mPanel;
   Envelope *mLogEnvelope;
   Envelope *mLinEnvelope;
   wxBoxSizer *mCurveSizer;
   wxChoice *mCurve;
   wxButton *mDelete;
   wxButton *mManage;
   wxStaticText *mMText;
   wxStaticText *octText;
   wxSlider *MSlider;
   wxSlider *dBMinSlider;
   wxSlider *dBMaxSlider;
   wxBoxSizer *szrC;
   wxBoxSizer *szrG;
   wxBoxSizer *szrV;
   wxBoxSizer *szrH;
   wxBoxSizer *szrI;
   wxBoxSizer *szrL;
#ifdef EXPERIMENTAL_EQ_SSE_THREADED
   wxBoxSizer *szrM;
#endif
   wxFlexGridSizer *szr1;
   wxBoxSizer *szr2;
   wxBoxSizer *szr3;
   wxBoxSizer *szr4;
   wxBoxSizer *szr5;
   wxSize size;
   wxCheckBox *mGridOnOff;
   EQCurveArray mCurves;
   EQCurve mCustomBackup;

private:
   DECLARE_EVENT_TABLE()

};

// EditCurvesDialog.  Note that the 'modified' curve used to be called 'custom' but is now called 'unnamed'
// Some things that deal with 'unnamed' curves still use, for example, 'mCustomBackup' as variable names.
class EditCurvesDialog:public wxDialog
{
public:
   EditCurvesDialog(EqualizationDialog * parent, int position);
   ~EditCurvesDialog();

private:

   enum EQCurvesDialogControls
   {
      CurvesListID = 11000,
      UpButtonID,
      DownButtonID,
      RenameButtonID,
      DeleteButtonID,
      ImportButtonID,
      ExportButtonID,
      LibraryButtonID,
      DefaultsButtonID
   };

   wxListCtrl *mList;   // List of curves.
   EQCurveArray mEditCurves;   // Copy of curves to muck about with
   EqualizationDialog *mParent;   // the parent EQ Dialog
   int mPosition; // position of current curve in list
   void Populate();
   void PopulateOrExchange(ShuttleGui &S);
   void PopulateList(int position);
   void OnUp(wxCommandEvent &event);
   void OnDown(wxCommandEvent &event);
   long GetPreviousItem(long item);
   void OnRename( wxCommandEvent &event );
   void OnDelete( wxCommandEvent &event );
   void OnImport( wxCommandEvent &event );
   void OnExport( wxCommandEvent &event );
   void OnLibrary( wxCommandEvent &event );
   void OnDefaults( wxCommandEvent &event );
   void OnOK(wxCommandEvent &event);
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
