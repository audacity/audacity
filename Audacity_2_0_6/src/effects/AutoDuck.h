/**********************************************************************

  Audacity: A Digital Audio Editor

  AutoDuck.h

  Markus Meyer

**********************************************************************/

#ifndef __AUDACITY_EFFECT_AUTODUCK__
#define __AUDACITY_EFFECT_AUTODUCK__

#include <wx/dialog.h>
#include <wx/panel.h>
#include <wx/textctrl.h>

#include "Effect.h"

class EffectAutoDuck;
class EffectAutoDuckPanel;

class EffectAutoDuckDialog: public wxDialog
{
public:
   EffectAutoDuckDialog(EffectAutoDuck* effect, wxWindow* parent);

private:
   friend class EffectAutoDuckPanel;

   void OnOk(wxCommandEvent& evt);
   void OnCancel(wxCommandEvent& evt);
   void OnValueChanged(wxCommandEvent& evt);

   EffectAutoDuck* mEffect;

   wxTextCtrl* mDuckAmountDbBox;
   wxTextCtrl* mInnerFadeDownLenBox;
   wxTextCtrl* mInnerFadeUpLenBox;
   wxTextCtrl* mOuterFadeDownLenBox;
   wxTextCtrl* mOuterFadeUpLenBox;
   wxTextCtrl* mThresholdDbBox;
   wxTextCtrl* mMaximumPauseBox;
   EffectAutoDuckPanel* mPanel;

   DECLARE_EVENT_TABLE()
};

#define AUTO_DUCK_PANEL_NUM_CONTROL_POINTS 5

class EffectAutoDuckPanel: public wxPanel
{
public:
   EffectAutoDuckPanel(EffectAutoDuckDialog* parent, wxWindowID id);
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

#if defined(__WXMAC__) && wxUSE_ACCESSIBILITY
   virtual bool AcceptsFocus() const {return false;}
#endif

   void OnPaint(wxPaintEvent& evt);
   void OnMouseCaptureChanged(wxMouseCaptureChangedEvent &evt);
   void OnMouseCaptureLost(wxMouseCaptureLostEvent &evt);
   void OnLeftDown(wxMouseEvent &evt);
   void OnLeftUp(wxMouseEvent &evt);
   void OnMotion(wxMouseEvent &evt);

   void ResetControlPoints();
   EControlPoint GetNearestControlPoint(const wxPoint& pt);

   EffectAutoDuckDialog* mParent;
   wxBitmap* mBackgroundBitmap;
   EControlPoint mCurrentControlPoint;
   wxPoint mControlPoints[AUTO_DUCK_PANEL_NUM_CONTROL_POINTS];
   wxPoint mMoveStartControlPoints[AUTO_DUCK_PANEL_NUM_CONTROL_POINTS];
   wxPoint mMouseDownPoint;
   bool mControlPointMoveActivated;

   DECLARE_EVENT_TABLE()
};

class EffectAutoDuck: public Effect
{
   friend class EffectAutoDuckDialog;

public:
   EffectAutoDuck();

   virtual wxString GetEffectName()
   {
      return wxString(_("Auto Duck..."));
   }

   virtual std::set<wxString> GetEffectCategories()
   {
     std::set<wxString> result;
     result.insert(wxT("http://lv2plug.in/ns/lv2core#DynamicsPlugin"));
     return result;
   }

   virtual wxString GetEffectIdentifier()
   {
      return wxString(wxT("AutoDuck"));
   }

   virtual wxString GetEffectAction()
   {
      return wxString(_("Processing Auto Duck..."));
   }

   virtual bool PromptUser();
   virtual bool TransferParameters(Shuttle & shuttle);

   virtual bool Init();
   virtual void End();
   virtual bool CheckWhetherSkipEffect();
   virtual bool Process();

private:
   bool ApplyDuckFade(int trackNumber, WaveTrack* t, double t0, double t1);

   double mDuckAmountDb;
   double mInnerFadeDownLen;
   double mInnerFadeUpLen;
   double mOuterFadeDownLen;
   double mOuterFadeUpLen;
   double mThresholdDb;
   double mMaximumPause;

   WaveTrack* mControlTrack;
};

#endif
