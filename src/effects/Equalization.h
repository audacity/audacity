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

#include <wx/setup.h> // for wxUSE_* macros

#include "Effect.h"
#include "EqualizationCurvesList.h"
#include "EqualizationFilter.h"

class wxBitmap;
class wxBoxSizer;
class wxButton;
class wxCheckBox;
class wxChoice;
class wxRadioButton;
class wxSizer;
class wxSizerItem;
class wxSlider;
class wxStaticText;
class EqualizationPanel;
class RulerPanel;

class EffectEqualization : public StatefulEffect
{
public:
   static inline EqualizationParameters *
   FetchParameters(EffectEqualization &e, EffectSettings &)
   { return &e.mParameters; }
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
   // Convenience function template for binding event handler functions
   template<typename EventTag, typename Class, typename Event>
   void BindTo(
      wxEvtHandler &src, const EventTag& eventType, void (Class::*pmf)(Event &))
   {
      src.Bind(eventType, pmf, static_cast<Class *>(this));
   }

   // EffectEqualization implementation

   bool ProcessOne(int count, WaveTrack * t,
                   sampleCount start, sampleCount len);
   
   void Flatten();
   void ForceRecalc();

   void setCurve(int currentCurve);
   void setCurve(const wxString &curveName);
   void setCurve(void);

   void UpdateCurves();
   void UpdateRuler();
   void UpdateDraw();

   //void LayoutEQSliders();
   void UpdateGraphic(void);
   void EnvLogToLin(void);
   void EnvLinToLog(void);
   void ErrMin(void);
   void GraphicEQ(Envelope &env);
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
   void OnIdle( wxIdleEvent &event );

   int mOptions;

   EqualizationFilter mParameters;
   EqualizationCurvesList mCurvesList{ mParameters };

   double mWhens[NUM_PTS];
   double mWhenSliders[NUMBER_OF_BANDS+1];
   size_t mBandsInUse;
   RulerPanel *mdBRuler;
   RulerPanel *mFreqRuler;

   bool mDisallowCustom;
   int mSlidersOld[NUMBER_OF_BANDS];
   double mEQVals[NUMBER_OF_BANDS+1];

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

#endif
