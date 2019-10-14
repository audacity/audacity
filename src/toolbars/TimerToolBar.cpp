/**********************************************************************

  Audacity: A Digital Audio Editor

  TimerToolBar.cpp

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

*//*******************************************************************/


#include "../Audacity.h"
#include "SelectionBar.h"

#include "SelectionBarListener.h"
#include "ToolManager.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#include <wx/setup.h> // for wxUSE_* macros

#ifndef WX_PRECOMP
#include <wx/intl.h>
#include <wx/settings.h>
#include <wx/sizer.h>
#include <wx/valtext.h>
#include <wx/stattext.h>
#endif
#include <wx/statline.h>

#include "SelectionBarListener.h"
#include "SelectionBar.h"
#include "TimerToolBar.h"

//#include "../widgets/AButton.h"
#include "../AudioIO.h"
#include "../AColor.h"
#include "../KeyboardCapture.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Snap.h"
#include "../widgets/NumericTextCtrl.h"
#include "../AllThemeResources.h"

#if wxUSE_ACCESSIBILITY
#include "../widgets/WindowAccessible.h"
#endif

IMPLEMENT_CLASS(TimerToolBar, ToolBar);

enum {
   TimerToolBarFirstID = 2700,
   AudioTimeID,
};

BEGIN_EVENT_TABLE(TimerToolBar, ToolBar)
   EVT_SIZE(TimerToolBar::OnSize)
   EVT_COMMAND(wxID_ANY, EVT_CAPTURE_KEY, TimerToolBar::OnCaptureKey)
END_EVENT_TABLE()

TimerToolBar::TimerToolBar( AudacityProject &project )
: ToolBar(project, TimerBarID, _("TimerToolBar"), wxT("TimerToolBar"),true),
  mListener(NULL), mAudioTime(NULL)
{
   mRate = (double) gPrefs->Read(wxT("/SamplingRate/DefaultProjectSampleRate"),
      AudioIO::GetOptimalSupportedSampleRate());
}

TimerToolBar::~TimerToolBar()
{
}

TimerToolBar &TimerToolBar::Get( AudacityProject &project )
{
   auto &toolManager = ToolManager::Get( project );
   return *static_cast<TimerToolBar*>( toolManager.GetToolBar(TimerBarID) );
}

const TimerToolBar &TimerToolBar::Get( const AudacityProject &project )
{
   return Get( const_cast<AudacityProject&>( project )) ;
}

void TimerToolBar::Create(wxWindow * parent)
{
   ToolBar::Create(parent);
   UpdatePrefs();
}

NumericTextCtrl * TimerToolBar::AddTime( const wxString Name, int id){
   auto formatName = mListener ? mListener->AS_GetSelectionFormat()
      : NumericFormatSymbol{};
   auto pCtrl = safenew NumericTextCtrl(
      this, id, NumericConverter::TIME, formatName, 0.0, mRate);
   pCtrl->SetName(Name);
   return pCtrl;
}

void TimerToolBar::Populate()
{
   mAudioTime = AddTime(_("Audio Position"), AudioTimeID);

   Add(mAudioTime, 0, wxALIGN_CENTER);

   Layout();
   SetMinSize(GetSizer()->GetMinSize());
}

void TimerToolBar::UpdatePrefs()
{
   SetLabel(_("TimerToolBar"));
   ToolBar::UpdatePrefs();
}

void TimerToolBar::OnSize(wxSizeEvent & event)
{
   event.Skip();

   int sh = GetSize().GetHeight() - 10;

   if (mAudioTime)
   {
     mAudioTime->SetDigitSize( sh*.63, sh );
     wxSize ms = mAudioTime->GetSize();
     //int mw = ms.GetWidth();
     //mAudioTime->SetMinSize(GetSizer()->GetMinSize());
     //printf("(size) %i %i\n", GetSizer()->GetSize());
   }
   //SetMinSize( GetSizer()->GetMinSize() );
   //Layout();
   //Fit();

   //Refresh(true);
   //evt.Skip();
}

void TimerToolBar::SetTimes(double audio)
{
   mAudioTime->SetValue(audio);
}

void TimerToolBar::OnFocus(wxFocusEvent &event)
{
	KeyboardCapture::OnFocus(*this, event);
}

void TimerToolBar::OnCaptureKey(wxCommandEvent &event)
{
   wxKeyEvent *kevent = (wxKeyEvent *)event.GetEventObject();
   wxWindow *w = FindFocus();
   int keyCode = kevent->GetKeyCode();

   // Convert numeric keypad entries.
   if ((keyCode >= WXK_NUMPAD0) && (keyCode <= WXK_NUMPAD9)) {
      keyCode -= WXK_NUMPAD0 - '0';
   }

   if (keyCode >= '0' && keyCode <= '9') {
      return;
   }

   event.Skip();
}

void TimerToolBar::SetDocked(ToolDock *dock, bool pushed) {
   ToolBar::SetDocked(dock, pushed);
   Fit();
}


void TimerToolBar::OnSnapTo(wxCommandEvent & WXUNUSED(event))
{
   mListener->AS_SetSnapTo(mSnapTo->GetSelection());
}

static RegisteredToolbarFactory factory{ TimerBarID,
   []( AudacityProject &project ){
      return ToolBar::Holder{ safenew TimerToolBar{ project } }; }
};
