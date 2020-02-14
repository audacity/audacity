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
class wxSize;

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
   void OnUpdate(wxCommandEvent &evt);
   void SetTimes(double audio);

   void SetListener(TimerToolBarListener *l);
   void SetAudioTimeFormat(const NumericFormatSymbol & format);
   void RegenerateTooltips() override {};

   int GetInitialWidth() override {return 250;} 
   int GetMinToolbarWidth()  override { return mMinWidth; }
   void SetToDefaultSize() override;
   wxSize GetDockedSize() override {
      return GetSmartDockedSize();
   };
   void SetDocked(ToolDock *dock, bool pushed) override;
   void ResizeTime( const wxSize & sz );
   void SetResizingLimits();
   void ResizingDone() override;

private:
   NumericTextCtrl * AddTime( const TranslatableString &Name, int id, 
      wxSizer * pSizer);
   
   void OnFocus(wxFocusEvent &event);
   void OnCaptureKey(wxCommandEvent &event);
   void OnSize(wxSizeEvent &evt);
   void OnIdle( wxIdleEvent &evt );
   
   TimerToolBarListener * mListener;
   double mRate;
   double mAudio;
   int mMinWidth;
   int mDigitHeight;
   bool mbReady;
   bool mbIsCreating;
   bool mbPreserveWidth;
   bool mbPreserveHeight;
   
   NumericTextCtrl   *mAudioTime;
   wxChoice          *mSnapTo;
   
public:
   
   DECLARE_CLASS(TimerToolBar)
   DECLARE_EVENT_TABLE()
};

#endif
