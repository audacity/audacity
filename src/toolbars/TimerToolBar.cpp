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
#include <wx/choice.h>
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
#include "../ProjectAudioIO.h"
#include "../ProjectSettings.h"
#include "../Snap.h"
#include "../ViewInfo.h"
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
  EVT_IDLE( TimerToolBar::OnIdle )
  EVT_COMMAND(wxID_ANY, EVT_CAPTURE_KEY, TimerToolBar::OnCaptureKey)
END_EVENT_TABLE()

TimerToolBar::TimerToolBar( AudacityProject &project )
: ToolBar(project, TimeBarID, XO("TimeToolBar"), wxT("TimeToolBar"),true),
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
   return *static_cast<TimerToolBar*>( toolManager.GetToolBar(TimeBarID) );
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

NumericTextCtrl * TimerToolBar::AddTime(
   const TranslatableString &Name, int id)
{
   //auto formatName = mListener ? mListener->AS_GetSelectionFormat()
   //: NumericFormatSymbol{};
   auto formatName = NumericConverter::HoursMinsSecondsFormat();
   auto pCtrl = safenew NumericTextCtrl(
      this, id, NumericConverter::TIME, formatName, 0.0, mRate);
   pCtrl->SetName(Name);
   pCtrl->SetReadOnly(true);
   return pCtrl;
}

void TimerToolBar::Populate()
{
   mAudioTime = AddTime(XO("Audio Position"), AudioTimeID);
   
   Add(mAudioTime, 0, wxALIGN_CENTER);
   
   Layout();
   SetMinSize(GetSizer()->GetMinSize());
}

void TimerToolBar::UpdatePrefs()
{
   SetLabel(XO("Time"));
   ToolBar::UpdatePrefs();
}

void TimerToolBar::SetToDefaultSize(){
   wxSize sz;
   sz.SetHeight( 80 );
   sz.SetWidth( GetInitialWidth());
   SetSize( sz );
}


void TimerToolBar::OnSize(wxSizeEvent & ev)
{
   ev.Skip();

   if (!mAudioTime)
      return;
   
   // This 'OnSize' function is also called during moving the
   // toolbar.  

   // 10 and 50 are magic numbers to allow some space around
   // the numeric control.  The horizontal space reserved
   // is deliberately not quite enough.  Size of font is set
   // primarily by the height of the toolbar.  The width 
   // calculations just stop the font width being 
   // ridiculously too large to fit.
   // In practice a user will drag the size to get a good font
   // size and adjust the width so that part of the control 
   // does not disappear.
   float f = mAudioTime->GetAspectRatio();
   int sh = ev.GetSize().GetHeight() - 10;
   int sw = (ev.GetSize().GetWidth() - 50)/f;
   sw = wxMin( sh, sw );
   // The 22 and 77 are magic numbers setting min and max 
   // font sizes.
   sw = wxMin( sw, 77 );
   sw = wxMax( 20, sw );
   sh = sw * 0.63;
   mAudioTime->SetDigitSize( sh, sw );

   // Refresh and update immediately, so that we don't get 
   // to see grot from partly redrawn toolbar during 
   // the resizing.
   Refresh(true);
   Update();
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

void TimerToolBar::OnIdle( wxIdleEvent &evt )
{
   evt.Skip();
   auto &project = mProject;

   double audioTime;

   auto &projectAudioIO = ProjectAudioIO::Get( project );
   if ( projectAudioIO.IsAudioActive() ){
      auto gAudioIO = AudioIOBase::Get();
      audioTime = gAudioIO->GetStreamTime();
   }
   else {
      const auto &playRegion = ViewInfo::Get( project ).playRegion;
      audioTime = playRegion.GetStart();
   }

   SetTimes( audioTime);
}


void TimerToolBar::OnSnapTo(wxCommandEvent & WXUNUSED(event))
{
   mListener->AS_SetSnapTo(mSnapTo->GetSelection());
}

static RegisteredToolbarFactory factory{ TimeBarID,
   []( AudacityProject &project ){
      return ToolBar::Holder{ safenew TimerToolBar{ project } }; }
};
