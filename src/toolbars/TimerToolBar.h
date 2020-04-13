/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeToolBar.h

  Jonatã Bolzan Loss

**********************************************************************/

#ifndef __AUDACITY_TIME_TOOLBAR__
#define __AUDACITY_TIME_TOOLBAR__

#include <wx/defs.h>

#include "ToolBar.h"
#include "../widgets/NumericTextCtrl.h"

class NumericTextCtrl;
class TimerToolBarListener;

class TimerToolBar final : public ToolBar
{
public:
   TimerToolBar(AudacityProject &project);
   virtual ~TimerToolBar();
   
   static TimerToolBar &Get(AudacityProject &project);
   static const TimerToolBar &Get(const AudacityProject &project);
   
   void Populate() override;
   void Repaint(wxDC * WXUNUSED(dc)) override {};
   void EnableDisableButtons() override {};
   void UpdatePrefs() override;
   void RegenerateTooltips() override {};
   int GetInitialWidth() override {return 250;} 
   int GetMinToolbarWidth() override {return 50;}
   void SetToDefaultSize() override;
   wxSize GetDockedSize() override;
   void SetDocked(ToolDock *dock, bool pushed) override;
   void SetListener(TimerToolBarListener *l);
   void SetAudioTimeFormat(const NumericFormatSymbol & format);
   void ResizingDone() override;

private:
   void SetResizingLimits();
   wxSize ComputeSizing(int digitH);

   void OnUpdate(wxCommandEvent &evt);
   void OnSize(wxSizeEvent &evt);
   void OnIdle(wxIdleEvent &evt);

   TimerToolBarListener *mListener;
   NumericTextCtrl *mAudioTime;
   float mDigitRatio;
   bool mSettingInitialSize;

   static const int minDigitH = 17;
   static const int maxDigitH = 100;

public:
   
   DECLARE_CLASS(TimerToolBar)
   DECLARE_EVENT_TABLE()
};

inline wxSize TimerToolBar::ComputeSizing(int digitH)
{
   return mAudioTime->ComputeSizing(false, digitH * mDigitRatio, digitH);
}

#endif
