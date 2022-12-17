/**********************************************************************

  Audacity: A Digital Audio Editor

  Equalization.h

  Mitch Golden
  Vaughan Johnson (Preview)

***********************************************************************/

#ifndef __AUDACITY_EFFECT_EQUALIZATION__
#define __AUDACITY_EFFECT_EQUALIZATION__

#include <wx/setup.h> // for wxUSE_* macros

#include "Effect.h"
#include "EqualizationBandSliders.h"

class wxBitmap;
class wxBoxSizer;
class wxButton;
class wxCheckBox;
class wxChoice;
class wxRadioButton;
class wxSizer;
class wxSizerItem;
class wxStaticText;
class EqualizationPanel;
class RulerPanel;

class EqualizationUI : public wxEvtHandler {
public:
   EqualizationUI(EffectSettingsManager &manager, wxWindow *& uiParent,
      const TranslatableString &name, EqualizationCurvesList &curvesList,
      int options
   )  : mManager{ manager }
      , mUIParent{ uiParent }
      , mName{ name }
      , mCurvesList{ curvesList }
      , mOptions{ options }
   {}

   bool ValidateUI(EffectSettings &settings);
   void Init() { mBands.Init(); }
   void setCurve(int currentCurve);
   void setCurve(const wxString &curveName);
   std::unique_ptr<EffectUIValidator> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs);
   bool TransferDataToWindow(const EffectSettings &settings);

private:
   // Convenience function template for binding event handler functions
   template<typename EventTag, typename Class, typename Event>
   void BindTo(
      wxEvtHandler &src, const EventTag& eventType, void (Class::*pmf)(Event &))
   {
      src.Bind(eventType, pmf, static_cast<Class *>(this));
   }

   void UpdateCurves();
   void UpdateRuler();
   void UpdateDraw();
   void UpdateGraphic();

   void OnSize( wxSizeEvent & event );
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

   void setCurve();

   EffectSettingsManager &mManager;
   wxWindow *& mUIParent;
   EqualizationCurvesList &mCurvesList;
   TranslatableString mName;
   const int mOptions;

   RulerPanel *mdBRuler;
   RulerPanel *mFreqRuler;

   bool mDisallowCustom{ false };

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

   wxWeakRef<EqualizationPanel> mPanel{};
   //wxPanel *mGraphicPanel;
   wxRadioButton *mDraw{};
   wxRadioButton *mGraphic{};
   wxCheckBox *mLinFreq;
   wxCheckBox *mGridOnOff;
   wxChoice *mInterpChoice;
   wxWeakRef<wxChoice> mCurve{};
   wxButton *mManage;
   wxStaticText *mMText;
   wxSlider *mMSlider{};
   wxSlider *mdBMinSlider;
   wxSlider *mdBMaxSlider;
   EqualizationBandSliders mBands{ mCurvesList };

   DECLARE_EVENT_TABLE()
};

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
   // EffectEqualization implementation

   bool ProcessOne(int count, WaveTrack * t,
                   sampleCount start, sampleCount len);
   
   EqualizationFilter mParameters;
   EqualizationCurvesList mCurvesList{ mParameters };
   const int mOptions;
   EqualizationUI mUI{ *this, mUIParent, GetName(), mCurvesList, mOptions };

   const EffectParameterMethods& Parameters() const override;
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
