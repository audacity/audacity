/**********************************************************************

  Audacity: A Digital Audio Editor

  TimerToolBar.h

  Jonatã Bolzan Loss

**********************************************************************/

#ifndef __AUDACITY_BIG_COUNTER__
#define __AUDACITY_BIG_COUNTER__

#include <wx/defs.h>

#include "ToolBar.h"
#include "ToolManager.h"

class SelectionBarListener;
class NumericTextCtrl;

class TimerToolBar final : public ToolBar {
   
public:
   TimerToolBar(AudacityProject &project);
   virtual ~TimerToolBar();
   
   static TimerToolBar &Get(AudacityProject &project);
   static const TimerToolBar &Get(const AudacityProject &project);
   
   void Create(wxWindow *parent) override;
   
   void Populate() override;
   void Repaint(wxDC * WXUNUSED(dc)) override {};
   void EnableDisableButtons() override {};
   void UpdatePrefs() override;
   
   void SetTimes(double audio);
   void RegenerateTooltips() override {};

   int GetInitialWidth() override {return 250;} 
   int GetMinToolbarWidth()  override { return 150; }
   void SetToDefaultSize() override;
   wxSize GetDockedSize() override {
      return GetSmartDockedSize();
   };

   
private:
   NumericTextCtrl * AddTime( const TranslatableString &Name, int id);
   
   void OnFocus(wxFocusEvent &event);
   void OnCaptureKey(wxCommandEvent &event);
   void OnSize(wxSizeEvent &evt);
   void OnIdle( wxIdleEvent &evt );
   void OnSnapTo(wxCommandEvent & event);
   
   SelectionBarListener * mListener;
   double mRate;
   double mAudio;
   
   NumericTextCtrl   *mAudioTime;
   wxChoice          *mSnapTo;
   
public:
   
   DECLARE_CLASS(TimerToolBar)
   DECLARE_EVENT_TABLE()
};

#endif
