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

#include "../Experimental.h"

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
#include "../xml/XMLTagHandler.h"
#include "../widgets/Grid.h"
#include "../widgets/Ruler.h"
#include "../RealFFTf.h"

#define EQUALIZATION_PLUGIN_SYMBOL XO("Equalization")


class Envelope;
class EqualizationPanel;

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
   EQCurve( const wxString & name = wxEmptyString ) { Name = name; }
   EQCurve( const wxChar * name ) { Name = name; }
   wxString Name;
   EQPointArray points;
};
WX_DECLARE_OBJARRAY( EQCurve, EQCurveArray );

#ifdef EXPERIMENTAL_EQ_SSE_THREADED
class EffectEqualization48x;
#endif

class EffectEqualization final : public Effect,
                           public XMLTagHandler
{
public:
   EffectEqualization();
   virtual ~EffectEqualization();

   // IdentInterface implementation

   wxString GetSymbol() override;
   wxString GetDescription() override;

   // EffectIdentInterface implementation

   EffectType GetType() override;

   // EffectClientInterface implementation

   bool GetAutomationParameters(EffectAutomationParameters & parms) override;
   bool SetAutomationParameters(EffectAutomationParameters & parms) override;
   bool LoadFactoryDefaults() override;

   // EffectUIClientInterface implementation

   bool ValidateUI() override;

   // Effect implementation

   bool Startup() override;
   bool Init() override;
   bool Process() override;

   bool PopulateUI(wxWindow *parent) override;
   bool CloseUI() override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

private:
   // EffectEqualization implementation

   // Number of samples in an FFT window
   enum : size_t {windowSize=16384};   //MJS - work out the optimum for this at run time?  Have a dialog box for it?

   // Low frequency of the FFT.  20Hz is the
   // low range of human hearing
   enum {loFreqI=20};

   bool ProcessOne(int count, WaveTrack * t,
                   sampleCount start, sampleCount len);
   bool CalcFilter();
   void Filter(size_t len, float *buffer);
   
   void Flatten();
   void ForceRecalc();
   void EnvelopeUpdated();
   void EnvelopeUpdated(Envelope *env, bool lin);
   bool IsLinear();

   void LoadCurves(const wxString &fileName = wxEmptyString, bool append = false);
   void SaveCurves(const wxString &fileName = wxEmptyString);
   // Merge NEW curves only or update all factory presets.
   void UpdateDefaultCurves( bool updateAll = false);
   void Select(int sel);
   void setCurve(int currentCurve);
   void setCurve(const wxString &curveName);
   void setCurve(void);
   bool GetDefaultFileName(wxFileName &fileName);
   
   // XMLTagHandler callback methods for loading and saving
   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs);
   XMLTagHandler *HandleXMLChild(const wxChar *tag);
   void WriteXML(XMLWriter &xmlFile);

   void UpdateCurves();
   void UpdateDraw();

   void LayoutEQSliders();
   void UpdateGraphic(void);
   void EnvLogToLin(void);
   void EnvLinToLog(void);
   void ErrMin(void);
   void GraphicEQ(Envelope *env);
   void spline(double x[], double y[], int n, double y2[]);
   double splint(double x[], double y[], int n, double y2[], double xr);

   void OnSize( wxSizeEvent & event );
   void OnErase( wxEraseEvent & event );
   void OnSlider( wxCommandEvent & event );
   void OnInterp( wxCommandEvent & event );
   void OnSliderM( wxCommandEvent & event );
   void OnSliderDBMAX( wxCommandEvent & event );
   void OnSliderDBMIN( wxCommandEvent & event );
   void OnDrawMode( wxCommandEvent &event );
   void OnGraphicMode( wxCommandEvent &event );
   void OnCurve( wxCommandEvent & event );
   void OnManage( wxCommandEvent & event );
   void OnClear( wxCommandEvent & event );
   void OnInvert( wxCommandEvent & event );
   void OnGridOnOff( wxCommandEvent & event );
   void OnLinFreq( wxCommandEvent & event );
#ifdef EXPERIMENTAL_EQ_SSE_THREADED
   void OnProcessingRadio( wxCommandEvent & event );
   void OnBench( wxCommandEvent & event );
#endif

private:
   HFFT hFFT;
   float *mFFTBuffer;
   float *mFilterFuncR;
   float *mFilterFuncI;
   size_t mM;
   wxString mCurveName;
   bool mLin;
   float mdBMax;
   float mdBMin;
   bool mDrawMode;
   int mInterp;
   bool mDrawGrid;

   double mWhens[NUM_PTS];
   double mWhenSliders[NUMBER_OF_BANDS+1];
   int mBandsInUse;
   RulerPanel *mdBRuler;
   RulerPanel *mFreqRuler;

   wxArrayString mInterpolations;
   bool mDisallowCustom;
   double mLoFreq;
   double mHiFreq;
   size_t mWindowSize;
   bool mDirty;
   int mSlidersOld[NUMBER_OF_BANDS];
   double mEQVals[NUMBER_OF_BANDS+1];

   EQCurveArray mCurves;

   std::unique_ptr<Envelope> mLogEnvelope, mLinEnvelope;
   Envelope *mEnvelope;

#ifdef EXPERIMENTAL_EQ_SSE_THREADED
   bool mBench;
   std::unique_ptr<EffectEqualization48x> mEffectEqualization48x;
   friend class EffectEqualization48x;
#endif

   wxSizer *szrC;
   wxSizer *szrG;
   wxSizer *szrV;
   wxSizer *szrH;
   wxSizer *szrI;
   wxSizer *szrL;
   wxSizer *szr1;
   wxSizer *szr2;
   wxSizer *szr3;
   wxSizer *szr4;
   wxSizer *szr5;

   wxSizerItem *mLeftSpacer;

   EqualizationPanel *mPanel;
   wxPanel *mGraphicPanel;
   wxRadioButton *mDraw;
   wxRadioButton *mGraphic;
   wxCheckBox *mLinFreq;
   wxCheckBox *mGridOnOff;
   wxChoice *mInterpChoice;
   wxChoice *mCurve;
   wxButton *mManage;
   wxStaticText *mMText;
   wxSlider *mMSlider;
   wxSlider *mdBMinSlider;
   wxSlider *mdBMaxSlider;
   wxSlider *mSliders[NUMBER_OF_BANDS];

   static int wxCMPFUNC_CONV SortCurvesByName (EQCurve **first, EQCurve **second)
   {
      return (*first)->Name.CmpNoCase((*second)->Name);
   }

   static int wxCMPFUNC_CONV SortCurvePoints (EQPoint **p0, EQPoint **p1)
   {
      return (*p0)->Freq > (*p1)->Freq;
   }

#ifdef EXPERIMENTAL_EQ_SSE_THREADED
   wxRadioButton *mMathProcessingType[5]; // default, sse, sse threaded, AVX, AVX threaded (note AVX is not implemented yet
   wxBoxSizer *szrM;
#endif

   DECLARE_EVENT_TABLE()

   friend class EqualizationPanel;
   friend class EditCurvesDialog;
};

class EqualizationPanel final : public wxPanelWrapper
{
public:
   EqualizationPanel(EffectEqualization *effect, wxWindow *parent);
   ~EqualizationPanel();

   // We don't need or want to accept focus.
   bool AcceptsFocus() const { return false; }
   // So that wxPanel is not included in Tab traversal - see wxWidgets bug 15581
   bool AcceptsFocusFromKeyboard() const { return false; }

   void ForceRecalc();

private:
   void Recalc();

   void OnMouseEvent(wxMouseEvent & event);
   void OnCaptureLost(wxMouseCaptureLostEvent & event);
   void OnPaint(wxPaintEvent & event);
   void OnSize (wxSizeEvent & event);

public:
//   int & mM;
//   float & mdBMax;
//   float & mdBMin;
//   Envelope & mEnvelope;

private:
   wxWindow *mParent;
   EffectEqualization *mEffect;

   bool mRecalcRequired;

   std::unique_ptr<wxBitmap> mBitmap;
   wxRect mEnvRect;
   int mWidth;
   int mHeight;
//   size_t mWindowSize;
//   float *mFilterFuncR;
//   float *mFilterFuncI;
   float *mOutr;
   float *mOuti;

//   double mLoFreq;
//   double mHiFreq;

   DECLARE_EVENT_TABLE()
};

// EditCurvesDialog.  Note that the 'modified' curve used to be called 'custom' but is now called 'unnamed'
// Some things that deal with 'unnamed' curves still use, for example, 'mCustomBackup' as variable names.
class EditCurvesDialog final : public wxDialogWrapper
{
public:
   EditCurvesDialog(wxWindow * parent, EffectEqualization * effect, int position);
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
   wxWindow *mParent; // the parent EQ Dialog
   EffectEqualization *mEffect;   // the parent EQ effect
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

   void OnListSelectionChange( wxListEvent &event );
   DECLARE_EVENT_TABLE()
};

#if wxUSE_ACCESSIBILITY

class SliderAx final : public wxWindowAccessible
{
public:
   SliderAx(wxWindow * window, const wxString &fmt);

   virtual ~ SliderAx();

   // Retrieves the address of an IDispatch interface for the specified child.
   // All objects must support this property.
   wxAccStatus GetChild(int childId, wxAccessible** child) override;

   // Gets the number of children.
   wxAccStatus GetChildCount(int* childCount) override;

   // Gets the default action for this object (0) or > 0 (the action for a child).
   // Return wxACC_OK even if there is no action. actionName is the action, or the empty
   // string if there is no action.
   // The retrieved string describes the action that is performed on an object,
   // not what the object does as a result. For example, a toolbar button that prints
   // a document has a default action of "Press" rather than "Prints the current document."
   wxAccStatus GetDefaultAction(int childId, wxString *actionName) override;

   // Returns the description for this object or a child.
   wxAccStatus GetDescription(int childId, wxString *description) override;

   // Gets the window with the keyboard focus.
   // If childId is 0 and child is NULL, no object in
   // this subhierarchy has the focus.
   // If this object has the focus, child should be 'this'.
   wxAccStatus GetFocus(int *childId, wxAccessible **child) override;

   // Returns help text for this object or a child, similar to tooltip text.
   wxAccStatus GetHelpText(int childId, wxString *helpText) override;

   // Returns the keyboard shortcut for this object or child.
   // Return e.g. ALT+K
   wxAccStatus GetKeyboardShortcut(int childId, wxString *shortcut) override;

   // Returns the rectangle for this object (id = 0) or a child element (id > 0).
   // rect is in screen coordinates.
   wxAccStatus GetLocation(wxRect& rect, int elementId) override;

   // Gets the name of the specified object.
   wxAccStatus GetName(int childId, wxString *name) override;

   // Returns a role constant.
   wxAccStatus GetRole(int childId, wxAccRole *role) override;

   // Gets a variant representing the selected children
   // of this object.
   // Acceptable values:
   // - a null variant (IsNull() returns TRUE)
   // - a list variant (GetType() == wxT("list"))
   // - an integer representing the selected child element,
   //   or 0 if this object is selected (GetType() == wxT("long"))
   // - a "void*" pointer to a wxAccessible child object
   wxAccStatus GetSelections(wxVariant *selections) override;

   // Returns a state constant.
   wxAccStatus GetState(int childId, long* state) override;

   // Returns a localized string representing the value for the object
   // or child.
   wxAccStatus GetValue(int childId, wxString* strValue) override;

private:
   wxWindow *mParent;
   wxString mFmt;
};

#endif // wxUSE_ACCESSIBILITY

#endif
