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
#include "../ShuttleAutomation.h"
#include "../widgets/wxPanelWrapper.h"
#include "XMLTagHandler.h"

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

using Floats = ArrayOf<float>;

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

class EffectEqualization : public StatefulEffect,
                           public XMLTagHandler
{
public:
   static inline EffectEqualization *
   FetchParameters(EffectEqualization &e, EffectSettings &) { return &e; }
   static const ComponentInterfaceSymbol Symbol;

   EffectEqualization(int Options = kEqLegacy);
   
   virtual ~EffectEqualization();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;
   bool VisitSettings(SettingsVisitor &visitor, EffectSettings &settings)
      override;
   bool VisitSettings(
      ConstSettingsVisitor &visitor, const EffectSettings &settings)
      const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;
   OptionalMessage LoadFactoryDefaults(EffectSettings &settings)
      const override;
   OptionalMessage DoLoadFactoryDefaults(EffectSettings &settings);

   RegistryPaths GetFactoryPresets() const override;
   OptionalMessage LoadFactoryPreset(int id, EffectSettings &settings)
      const override;

   // EffectUIClientInterface implementation

   bool ValidateUI(EffectSettings &) override;

   // Effect implementation

   bool Init() override;
   bool Process(EffectInstance &instance, EffectSettings &settings) override;

   bool CloseUI() override;
   std::unique_ptr<EffectUIValidator> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs) override;
   bool TransferDataToWindow(const EffectSettings &settings) override;

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
   bool HandleXMLTag(const std::string_view& tag, const AttributesList &attrs) override;
   XMLTagHandler *HandleXMLChild(const std::string_view& tag) override;
   void WriteXML(XMLWriter &xmlFile) const;

   void UpdateCurves();
   void UpdateRuler();
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

   const EffectParameterMethods& Parameters() const override;

   DECLARE_EVENT_TABLE()

   friend class EqualizationPanel;
   friend class EditCurvesDialog;

   enum kInterpolations
   {
      kBspline,
      kCosine,
      kCubic,
      nInterpolations
   };
   static const EnumValueSymbol kInterpStrings[nInterpolations];

// Not all of these are visited now
static constexpr EffectParameter FilterLength{ &EffectEqualization::mM,
   L"FilterLength",        8191,    21,      8191,    0      };
static constexpr EffectParameter CurveName{ &EffectEqualization::mCurveName,
   L"CurveName",           L"unnamed", L"", L"", L""};
static constexpr EffectParameter InterpLin{ &EffectEqualization::mLin,
   L"InterpolateLin",      false,   false,   true,    false  };
static constexpr EnumParameter InterpMeth{ &EffectEqualization::mInterp,
   L"InterpolationMethod", 0,       0,       0,       0, kInterpStrings, nInterpolations      };
static constexpr EffectParameter DrawMode{ &EffectEqualization::mDrawMode,
   L"",                   true,    false,   true,    false  };
static constexpr EffectParameter DrawGrid{ &EffectEqualization::mDrawGrid,
   L"",                   true,    false,   true,    false  };
static constexpr EffectParameter dBMin{ &EffectEqualization::mdBMin,
   L"",                   -30.0f,   -120.0,  -10.0,   0      };
static constexpr EffectParameter dBMax{ &EffectEqualization::mdBMax,
   L"",                   30.0f,    0.0,     60.0,    0      };
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
