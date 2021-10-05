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

#include <wx/setup.h> // for wxUSE_* macros

#include "Effect.h"
#include "RealFFTf.h"

// Flags to specialise the UI
const int kEqOptionGraphic =1;
const int kEqOptionCurve   =1<<1;
// The legacy version offers both Graphic and curve on the same UI.
const int kEqLegacy = kEqOptionGraphic + kEqOptionCurve;

class wxBitmap;
class wxBoxSizer;
class wxButton;
class wxCheckBox;
class wxChoice;
class wxListCtrl;
class wxListEvent;
class wxRadioButton;
class wxSizer;
class wxSizerItem;
class wxSlider;
class wxStaticText;
class Envelope;
class EnvelopeEditor;
class EqualizationPanel;
class RulerPanel;

//
// One point in a curve
//
class EQPoint
{
public:
   EQPoint( const double f, const double d ) { Freq = f; dB = d; }

   bool operator < (const EQPoint &p1) const
   {
      return Freq < p1.Freq;
   }

   double Freq;
   double dB;
};

//
// One curve in a list
//
// LLL:  This "really" isn't needed as the array of points could be
//       attached as wxClientData to the wxChoice entries.  I
//       didn't realize this until after the fact and am too
//       lazy to change it.  (But, hollar if you want me to.)
//
class EQCurve
{
public:
   EQCurve( const wxString & name = {} ) { Name = name; }
   EQCurve( const wxChar * name ) { Name = name; }

   bool operator < (const EQCurve &that) const
   {
      return Name.CmpNoCase(that.Name) < 0;
   }

   wxString Name;
   std::vector<EQPoint> points;
};

using EQCurveArray = std::vector<EQCurve>;

#ifdef EXPERIMENTAL_EQ_SSE_THREADED
class EffectEqualization48x;
#endif

class EffectEqualization : public Effect,
                           public XMLTagHandler
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectEqualization(int Options = kEqLegacy);
   
   virtual ~EffectEqualization();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;
   ManualPageID ManualPage() override;

   // EffectDefinitionInterface implementation

   EffectType GetType() override;

   // EffectClientInterface implementation

   bool DefineParams( ShuttleParams & S ) override;
   bool GetAutomationParameters(CommandParameters & parms) override;
   bool SetAutomationParameters(CommandParameters & parms) override;
   bool LoadFactoryDefaults() override;

   RegistryPaths GetFactoryPresets() override;
   bool LoadFactoryPreset(int id) override;

   // EffectUIClientInterface implementation

   bool ValidateUI() override;

   // Effect implementation

   bool Startup() override;
   bool Init() override;
   bool Process() override;

   bool CloseUI() override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

private:
   // EffectEqualization implementation
   wxString GetPrefsPrefix();

   // Number of samples in an FFT window
   static const size_t windowSize = 16384u; //MJS - work out the optimum for this at run time?  Have a dialog box for it?

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

   void LoadCurves(const wxString &fileName = {}, bool append = false);
   void SaveCurves(const wxString &fileName = {});
   // Merge NEW curves only or update all factory presets.
   void UpdateDefaultCurves( bool updateAll = false);
   void Select(int sel);
   void setCurve(int currentCurve);
   void setCurve(const wxString &curveName);
   void setCurve(void);
   bool GetDefaultFileName(wxFileName &fileName);
   
   // XMLTagHandler callback methods for loading and saving
   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs) override;
   XMLTagHandler *HandleXMLChild(const wxChar *tag) override;
   void WriteXML(XMLWriter &xmlFile) const;

   void UpdateCurves();
   void UpdateDraw();

   //void LayoutEQSliders();
   void UpdateGraphic(void);
   void EnvLogToLin(void);
   void EnvLinToLog(void);
   void ErrMin(void);
   void GraphicEQ(Envelope *env);
   void spline(double x[], double y[], size_t n, double y2[]);
   double splint(double x[], double y[], size_t n, double y2[], double xr);

   void OnErase( wxEvent &event );
   void OnSize( wxSizeEvent & event );
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
   int mOptions;
   HFFT hFFT;
   Floats mFFTBuffer, mFilterFuncR, mFilterFuncI;
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
   size_t mBandsInUse;
   RulerPanel *mdBRuler;
   RulerPanel *mFreqRuler;

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
   //wxPanel *mGraphicPanel;
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

#ifdef EXPERIMENTAL_EQ_SSE_THREADED
   wxRadioButton *mMathProcessingType[5]; // default, sse, sse threaded, AVX, AVX threaded (note AVX is not implemented yet
   wxBoxSizer *szrM;
#endif

   DECLARE_EVENT_TABLE()

   friend class EqualizationPanel;
   friend class EditCurvesDialog;
};

class EffectEqualizationCurve final : public EffectEqualization
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectEqualizationCurve() : EffectEqualization( kEqOptionCurve ) {}
};

class EffectEqualizationGraphic final : public EffectEqualization
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectEqualizationGraphic() : EffectEqualization( kEqOptionGraphic ) {}
};

class EqualizationPanel final : public wxPanelWrapper
{
public:
   EqualizationPanel(
      wxWindow *parent, wxWindowID winid, EffectEqualization *effect);
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
   std::unique_ptr<EnvelopeEditor> mLinEditor, mLogEditor;

   bool mRecalcRequired;

   std::unique_ptr<wxBitmap> mBitmap;
   wxRect mEnvRect;
   int mWidth;
   int mHeight;
//   size_t mWindowSize;
//   float *mFilterFuncR;
//   float *mFilterFuncI;
   Floats mOutr, mOuti;

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

#endif
