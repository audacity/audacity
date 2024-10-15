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
#include "Observer.h"

class NumericTextCtrl;
struct ProjectNumericFormatsEvent;

class TimeToolBar final : public ToolBar
{
public:
   static Identifier ID();

   TimeToolBar(AudacityProject &project);
   virtual ~TimeToolBar();
   
   DockID DefaultDockID() const override;

   static TimeToolBar &Get(AudacityProject &project);
   static const TimeToolBar &Get(const AudacityProject &project);
   
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
   void SetAudioTimeFormat(const NumericFormatID & format);
   void ResizingDone() override;

private:
   void SetResizingLimits();
   wxSize ComputeSizing(int digitH);
   void OnFormatsChanged(ProjectNumericFormatsEvent);
   void OnUpdate(wxCommandEvent &evt);
   void OnSize(wxSizeEvent &evt);
   void OnIdle(wxIdleEvent &evt);

   NumericTextCtrl *mAudioTime;
   float mDigitRatio;
   bool mSettingInitialSize = true;

   static const int minDigitH = 17;
   static const int maxDigitH = 100;

   Observer::Subscription mFormatChangedToFitValueSubscription;
   Observer::Subscription mFormatsSubscription;

public:
   
   DECLARE_CLASS(TimeToolBar)
   DECLARE_EVENT_TABLE()
};

#endif
