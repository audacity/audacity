/**********************************************************************

  Audacity: A Digital Audio Editor

  AutoDuck.h

  Markus Meyer

**********************************************************************/

#ifndef __AUDACITY_EFFECT_AUTODUCK__
#define __AUDACITY_EFFECT_AUTODUCK__

#include <wx/bitmap.h>
#include <wx/event.h>
#include <wx/gdicmn.h>
#include <wx/panel.h>
#include <wx/string.h>
#include <wx/textctrl.h>
#include <wx/window.h>

#include "../ShuttleGui.h"
#include "../WaveTrack.h"

#include "Effect.h"

class EffectAutoDuckPanel;

#define AUTO_DUCK_PANEL_NUM_CONTROL_POINTS 5

#define AUTODUCK_PLUGIN_SYMBOL XO("Auto Duck")

class EffectAutoDuck : public Effect
{
public:
   EffectAutoDuck();
   virtual ~EffectAutoDuck();

   // IdentInterface implementation

   virtual wxString GetSymbol();
   virtual wxString GetDescription();

   // EffectIdentInterface implementation

   virtual EffectType GetType();

   // EffectClientInterface implementation

   virtual bool GetAutomationParameters(EffectAutomationParameters & parms);
   virtual bool SetAutomationParameters(EffectAutomationParameters & parms);

   // Effect implementation

   virtual bool Startup();
   virtual bool Init();
   virtual void End();
   virtual bool Process();
   virtual void PopulateOrExchange(ShuttleGui & S);
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

private:
   // EffectAutoDuck implementation

   bool ApplyDuckFade(int trackNumber, WaveTrack *t, double t0, double t1);

   void OnValueChanged(wxCommandEvent & evt);

private:
   double mDuckAmountDb;
   double mInnerFadeDownLen;
   double mInnerFadeUpLen;
   double mOuterFadeDownLen;
   double mOuterFadeUpLen;
   double mThresholdDb;
   double mMaximumPause;

   WaveTrack *mControlTrack;

   wxTextCtrl *mDuckAmountDbBox;
   wxTextCtrl *mInnerFadeDownLenBox;
   wxTextCtrl *mInnerFadeUpLenBox;
   wxTextCtrl *mOuterFadeDownLenBox;
   wxTextCtrl *mOuterFadeUpLenBox;
   wxTextCtrl *mThresholdDbBox;
   wxTextCtrl *mMaximumPauseBox;
   EffectAutoDuckPanel *mPanel;

   DECLARE_EVENT_TABLE();

   friend class EffectAutoDuckPanel;
};

class EffectAutoDuckPanel : public wxPanel
{
public:
   EffectAutoDuckPanel(wxWindow *parent, EffectAutoDuck *effect);
   virtual ~EffectAutoDuckPanel();

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

   virtual bool AcceptsFocus() const {return false;}

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
   wxBitmap *mBackgroundBitmap;
   EControlPoint mCurrentControlPoint;
   wxPoint mControlPoints[AUTO_DUCK_PANEL_NUM_CONTROL_POINTS];
   wxPoint mMoveStartControlPoints[AUTO_DUCK_PANEL_NUM_CONTROL_POINTS];
   wxPoint mMouseDownPoint;
   bool mControlPointMoveActivated;

   DECLARE_EVENT_TABLE();
};

#endif
