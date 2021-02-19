/**********************************************************************

Audacity: A Digital Audio Editor

EffectScienFilter.h

Norm C
Mitch Golden
Vaughan Johnson (Preview)

***********************************************************************/

#ifndef __AUDACITY_EFFECT_SCIENFILTER__
#define __AUDACITY_EFFECT_SCIENFILTER__

#include <wx/setup.h> // for wxUSE_* macros

#include "Biquad.h"

#include "StatefulPerTrackEffect.h"
#include "ShuttleAutomation.h"
#include "wxPanelWrapper.h"
#include <float.h> // for FLT_MAX

class wxBitmap;
class wxChoice;
class wxSlider;
class wxStaticText;
class wxTextCtrl;
class RulerPanel;
class ShuttleGui;

class EffectScienFilterPanel;

class EffectScienFilter final : public StatefulPerTrackEffect
{
public:
   static inline EffectScienFilter *
   FetchParameters(EffectScienFilter &e, EffectSettings &) { return &e; }
   static const ComponentInterfaceSymbol Symbol;

   EffectScienFilter();
   virtual ~EffectScienFilter();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;

   unsigned GetAudioInCount() const override;
   unsigned GetAudioOutCount() const override;
   bool ProcessInitialize(EffectSettings &settings, double sampleRate,
      ChannelNames chanMap) override;
   size_t ProcessBlock(EffectSettings &settings,
      const float *const *inBlock, float *const *outBlock, size_t blockLen)
      override;

   // Effect implementation

   bool Init() override;
   std::unique_ptr<EffectUIValidator> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs) override;
   bool TransferDataToWindow(const EffectSettings &settings) override;
   bool TransferDataFromWindow(EffectSettings &settings) override;

private:
   // EffectScienFilter implementation

   bool TransferGraphLimitsFromWindow();
   void CalcFilter();
   float FilterMagnAtFreq(float Freq);
   void EnableDisableRippleCtl (int FilterType);

   void OnSize( wxSizeEvent & evt );
   void OnSlider( wxCommandEvent & evt );

   void OnOrder( wxCommandEvent & evt );
   void OnCutoff( wxCommandEvent & evt );
   void OnRipple( wxCommandEvent & evt );
   void OnStopbandRipple( wxCommandEvent & evt );
   void OnFilterType( wxCommandEvent & evt );
   void OnFilterSubtype( wxCommandEvent & evt );

   void OnSliderDBMAX( wxCommandEvent & evt );
   void OnSliderDBMIN( wxCommandEvent & evt );

   wxWeakRef<wxWindow> mUIParent{};

   float mCutoff;
   float mRipple;
   float mStopbandRipple;
   int mFilterType;		// Butterworth etc.
   int mFilterSubtype;	// lowpass, highpass
   int mOrder;
   int mOrderIndex;
   ArrayOf<Biquad> mpBiquad;

   double mdBMax;
   double mdBMin;
   bool mEditingBatchParams;

   double mLoFreq;
   double mNyquist;

   EffectScienFilterPanel *mPanel;
   wxSlider *mdBMinSlider;
   wxSlider *mdBMaxSlider;

   wxStaticText *mRippleCtlP;
   wxTextCtrl *mRippleCtl;
   wxStaticText *mRippleCtlU;

   wxTextCtrl *mCutoffCtl;

   wxStaticText *mStopbandRippleCtlP;
   wxTextCtrl *mStopbandRippleCtl;
   wxStaticText *mStopbandRippleCtlU;

   wxChoice *mFilterTypeCtl;
   wxChoice *mFilterSubTypeCtl;
   wxChoice *mFilterOrderCtl;

   RulerPanel *mdBRuler;
   RulerPanel *mfreqRuler;

   const EffectParameterMethods& Parameters() const override;
   DECLARE_EVENT_TABLE()

   friend class EffectScienFilterPanel;

   enum kSubTypes
   {
      kLowPass  = Biquad::kLowPass,
      kHighPass = Biquad::kHighPass,
      nSubTypes = Biquad::nSubTypes
   };
   static const EnumValueSymbol kSubTypeStrings[nSubTypes];

   enum kTypes
   {
      kButterworth,
      kChebyshevTypeI,
      kChebyshevTypeII,
      nTypes
   };
   static const EnumValueSymbol kTypeStrings[nTypes];

   static_assert(nSubTypes == WXSIZEOF(kSubTypeStrings), "size mismatch");

static constexpr EnumParameter Type{ &EffectScienFilter::mFilterType,
   L"FilterType",       kButterworth,  0,    nTypes - 1,    1, kTypeStrings, nTypes  };
static constexpr EnumParameter Subtype{ &EffectScienFilter::mFilterSubtype,
   L"FilterSubtype",    kLowPass,      0,    nSubTypes - 1, 1, kSubTypeStrings, nSubTypes  };
static constexpr EffectParameter Order{ &EffectScienFilter::mOrder,
   L"Order",            1,             1,    10,               1  };
static constexpr EffectParameter Cutoff{ &EffectScienFilter::mCutoff,
   L"Cutoff",           1000.0f,        1.0,  FLT_MAX,          1  };
static constexpr EffectParameter Passband{ &EffectScienFilter::mRipple,
   L"PassbandRipple",   1.0f,           0.0,  100.0,            1  };
static constexpr EffectParameter Stopband{ &EffectScienFilter::mStopbandRipple,
   L"StopbandRipple",   30.0f,          0.0,  100.0,            1  };
};

class EffectScienFilterPanel final : public wxPanelWrapper
{
public:
   EffectScienFilterPanel(
      wxWindow *parent, wxWindowID winid,
      EffectScienFilter *effect, double lo, double hi);
   virtual ~EffectScienFilterPanel();

   // We don't need or want to accept focus.
   bool AcceptsFocus() const;
   // So that wxPanel is not included in Tab traversal - see wxWidgets bug 15581
   bool AcceptsFocusFromKeyboard() const;

   void SetFreqRange(double lo, double hi);
   void SetDbRange(double min, double max);

private:
   void OnPaint(wxPaintEvent & evt);
   void OnSize(wxSizeEvent & evt);

private:
   EffectScienFilter *mEffect;
   wxWindow *mParent;

   double mLoFreq;
   double mHiFreq;

   double mDbMin;
   double mDbMax;

   std::unique_ptr<wxBitmap> mBitmap;
   wxRect mEnvRect;
   int mWidth;
   int mHeight;

   friend class EffectScienFilter;

   DECLARE_EVENT_TABLE()
};

#endif
