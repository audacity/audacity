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

 private:
   NumericTextCtrl * AddTime( const wxString Name, int id);

   void OnFocus(wxFocusEvent &event);
   void OnCaptureKey(wxCommandEvent &event);
   void OnSize(wxSizeEvent &evt);
   void OnSnapTo(wxCommandEvent & event);
   virtual void SetDocked(ToolDock *dock, bool pushed)override;

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
