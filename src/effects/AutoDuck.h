/**********************************************************************

  Audacity: A Digital Audio Editor

  AutoDuck.h

  Markus Meyer

**********************************************************************/

#ifndef __AUDACITY_EFFECT_AUTODUCK__
#define __AUDACITY_EFFECT_AUTODUCK__

#include "StatefulEffect.h"
#include "../ShuttleAutomation.h"
#include <float.h> // for DBL_MAX
#include "../widgets/wxPanelWrapper.h"

class wxBitmap;
class wxTextCtrl;
class ShuttleGui;

#define AUTO_DUCK_PANEL_NUM_CONTROL_POINTS 5

class EffectAutoDuck final : public StatefulEffect
{
public:
   static inline EffectAutoDuck *
   FetchParameters(EffectAutoDuck &e, EffectSettings &) { return &e; }
   static const ComponentInterfaceSymbol Symbol;

   EffectAutoDuck();
   virtual ~EffectAutoDuck();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;

   // Effect implementation

   bool Init() override;
   bool Process(EffectContext &context,
      EffectInstance &instance, EffectSettings &settings) override;
   std::unique_ptr<EffectEditor> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs) override;
   bool TransferDataToWindow(const EffectSettings &settings) override;
   bool DoTransferDataToWindow();
   bool TransferDataFromWindow(EffectSettings &settings) override;

private:
   // EffectAutoDuck implementation

   bool ApplyDuckFade(EffectContext &context,
      int trackNum, WaveTrack *t, double t0, double t1);

   void OnValueChanged(wxCommandEvent & evt);

private:
   wxWeakRef<wxWindow> mUIParent{};

   double mDuckAmountDb;
   double mInnerFadeDownLen;
   double mInnerFadeUpLen;
   double mOuterFadeDownLen;
   double mOuterFadeUpLen;
   double mThresholdDb;
   double mMaximumPause;

   const WaveTrack *mControlTrack;

   wxTextCtrl *mDuckAmountDbBox;
   wxTextCtrl *mInnerFadeDownLenBox;
   wxTextCtrl *mInnerFadeUpLenBox;
   wxTextCtrl *mOuterFadeDownLenBox;
   wxTextCtrl *mOuterFadeUpLenBox;
   wxTextCtrl *mThresholdDbBox;
   wxTextCtrl *mMaximumPauseBox;

   class Panel;
   Panel *mPanel;

   const EffectParameterMethods& Parameters() const override;
   DECLARE_EVENT_TABLE()

static constexpr EffectParameter DuckAmountDb{ &EffectAutoDuck::mDuckAmountDb,
   L"DuckAmountDb",     -12.0,   -24.0,   0.0,     1  };
static constexpr EffectParameter InnerFadeDownLen{ &EffectAutoDuck::mInnerFadeDownLen,
   L"InnerFadeDownLen", 0.0,     0.0,     3.0,     1  };
static constexpr EffectParameter InnerFadeUpLen{ &EffectAutoDuck::mInnerFadeUpLen,
   L"InnerFadeUpLen",   0.0,     0.0,     3.0,     1  };
static constexpr EffectParameter OuterFadeDownLen{ &EffectAutoDuck::mOuterFadeDownLen,
   L"OuterFadeDownLen", 0.5,     0.0,     3.0,     1  };
static constexpr EffectParameter OuterFadeUpLen{ &EffectAutoDuck::mOuterFadeUpLen,
   L"OuterFadeUpLen",   0.5,     0.0,     3.0,     1  };
static constexpr EffectParameter ThresholdDb{ &EffectAutoDuck::mThresholdDb,
   L"ThresholdDb",      -30.0,   -100.0,  0.0,     1  };
static constexpr EffectParameter MaximumPause{ &EffectAutoDuck::mMaximumPause,
   L"MaximumPause",     1.0,     0.0,     DBL_MAX, 1  };
};

class EffectAutoDuck::Panel final : public wxPanelWrapper
{
public:
   Panel(
      wxWindow *parent, wxWindowID winid, EffectAutoDuck *effect);
   virtual ~Panel();

private:
   enum EControlPoint
   {
      innerFadeDown = 0,
      outerFadeDown,
      innerFadeUp,
      outerFadeUp,
      duckAmount,
      none = 99,
   };

   bool AcceptsFocus() const override { return false; }
   // So that wxPanel is not included in Tab traversal - see wxWidgets bug 15581
   bool AcceptsFocusFromKeyboard() const override { return false; }


   void OnPaint(wxPaintEvent & evt);
   void OnMouseCaptureChanged(wxMouseCaptureChangedEvent & evt);
   void OnMouseCaptureLost(wxMouseCaptureLostEvent & evt);
   void OnLeftDown(wxMouseEvent & evt);
   void OnLeftUp(wxMouseEvent & evt);
   void OnMotion(wxMouseEvent & evt);

   void ResetControlPoints();
   EControlPoint GetNearestControlPoint(const wxPoint & pt);

private:
   wxWindow *mParent;
   EffectAutoDuck *mEffect;
   std::unique_ptr<wxBitmap> mBackgroundBitmap;
   EControlPoint mCurrentControlPoint;
   wxPoint mControlPoints[AUTO_DUCK_PANEL_NUM_CONTROL_POINTS];
   wxPoint mMoveStartControlPoints[AUTO_DUCK_PANEL_NUM_CONTROL_POINTS];
   wxPoint mMouseDownPoint;
   bool mControlPointMoveActivated;

   DECLARE_EVENT_TABLE()
};

#endif
