/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 TimerToolBar.cpp
 
 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.
 
 *//*******************************************************************/


#include "../Audacity.h"
#include "../Experimental.h"

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
   EVT_COMMAND(wxID_ANY, EVT_TIMETEXTCTRL_UPDATED, TimerToolBar::OnUpdate)
   EVT_COMMAND(wxID_ANY, EVT_CAPTURE_KEY, TimerToolBar::OnCaptureKey)
END_EVENT_TABLE()

TimerToolBar::TimerToolBar( AudacityProject &project )
: ToolBar(project, TimeBarID, XO("Time"), wxT("Time"),true),
mListener(NULL), mAudioTime(NULL)
{
   mMinWidth = 50;
   mDigitHeight = 48;
   mbReady = false;
   mbPreserveHeight = false;
   mbPreserveWidth = false;
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
   mbIsCreating = true;
   ToolBar::Create(parent);
   UpdatePrefs();
   mbIsCreating = false;
}

NumericTextCtrl * TimerToolBar::AddTime(
   const TranslatableString &Name, int id, wxSizer * pSizer)
{
   auto formatName = mListener ? mListener->TT_GetAudioTimeFormat()
     : NumericConverter::HoursMinsSecondsFormat();
   auto pCtrl = safenew NumericTextCtrl(
      this, id, NumericConverter::TIME, formatName, 0.0, mRate);
   pCtrl->SetName(Name);
   pCtrl->SetReadOnly(true);
   pCtrl->SetDigitSize( mDigitHeight * 0.66,mDigitHeight );
   pSizer->Add(pCtrl, 0, wxALIGN_CENTER , 0);
   return pCtrl;
}

void TimerToolBar::Populate()
{

   wxSizer *mainSizer = GetSizer();

   mAudioTime = AddTime(XO("Audio Position"), AudioTimeID, mainSizer);
   
   mainSizer->Layout();
   RegenerateTooltips();
   SetMinSize(GetSizer()->GetMinSize());
}

void TimerToolBar::SetListener(TimerToolBarListener *l)
{
   mbPreserveWidth = true;
   mListener = l;
   SetAudioTimeFormat(mListener->TT_GetAudioTimeFormat());
   mbPreserveWidth = false;
};


void TimerToolBar::UpdatePrefs()
{
   mbPreserveWidth = true;
   wxCommandEvent e;
   e.SetInt(mAudioTime->GetFormatIndex());
   OnUpdate(e);
   mbPreserveWidth = false;

   SetLabel(XO("Time"));
   ToolBar::UpdatePrefs();
}

void TimerToolBar::SetAudioTimeFormat(const NumericFormatSymbol & format)
{
   bool changed =
      mAudioTime->SetFormatString(mAudioTime->GetBuiltinFormat(format));

   // Test first whether changed, to avoid infinite recursion from OnUpdate
   if ( changed || !mbReady ) {
      if (!mbReady) {
         //wxLogDebug("Ready!");
         mbReady = true;
      }
      wxCommandEvent e;
      e.SetInt(mAudioTime->GetFormatIndex());
      OnUpdate(e);
   }
}

// Called when the format drop downs is changed.
// This causes recreation of the toolbar contents.
void TimerToolBar::OnUpdate(wxCommandEvent &evt)
{
   //wxLogDebug("OnUpdate");
   int index = evt.GetInt();
   wxWindow *w = FindFocus();

   bool bHasFocus = (w == mAudioTime);
   mbPreserveHeight = true;

   // If docked, font height is determined by toolbar height.
   // If undocked, determined by last resize.
   if (IsDocked())
   {
      mDigitHeight = GetSize().GetHeight() - 6;
      mDigitHeight = wxMin(mDigitHeight, 25);
   }

   evt.Skip(false);
   // Save format name before recreating the controls so they resize properly
   {
      auto format = mAudioTime->GetBuiltinName(index);
      if (mListener)
         mListener->TT_SetAudioTimeFormat(format);
   }
   wxSize sz = GetSize();
   RegenerateTooltips();
   // ReCreateButtons() will get rid of our sizers and controls
   // so reset pointer first.
   mAudioTime = NULL;
   ReCreateButtons();

   auto x = GetSizer()->GetMinSize().GetX();
   SetMinSize(wxSize(x, sz.GetHeight()));

   SetResizingLimits();

   if( bHasFocus )
      mAudioTime->SetFocus();
   Updated();
   mbPreserveHeight = false;
}

void TimerToolBar::SetDocked(ToolDock *dock, bool pushed)
{
   //wxLogDebug("SetDocked");
   ToolBar::SetDocked(dock, pushed);
   if (!mbReady)
      return;

   // Do not use IsDocked() here.  It's not valid!
   if ( dock == nullptr) {
      // The min height when undocked is always 22
      wxSize sz = GetMinSize();
      sz.y = 22;
      SetMinSize(sz);
   }
   else {
      //SetResizingLimits();
      ResizingDone();
   }

}

void TimerToolBar::SetToDefaultSize(){
   wxSize sz;
   sz.SetHeight( 48 );
   sz.SetWidth( GetInitialWidth());
   SetSize( sz );
}

void TimerToolBar::SetResizingLimits() {

   //wxLogDebug("Set Resizing Limits");
   if (!IsDocked()) {
      wxWindow * pWnd = GetParent();
      ToolFrame * pFrame = dynamic_cast<ToolFrame*>(pWnd);
      Layout();
      if (pFrame) {
         pFrame->Layout();
         // Set smallest conceivable min size
         pFrame->SetMinSize(wxSize(80, 24));
         // Fit frame around the toolbar
         pFrame->Fit();

         // The resize handle takes 39 pixels.
         // And was not accounted for in the Fit().
         wxSize sz = pFrame->GetSize();
         sz.x += 39;
         sz.y += 2;
         pFrame->SetSize(sz);
         // Now compute and lock in the min frame size
         // using aspect ratio, so that dragging won't go 
         // smaller than this.
         pFrame->LockInMinSize(this);
      }
   }
   else
   {
      Fit(); // Toolbar size to fit around minimum sizer.
      Layout();
      //GetSizer()->RecalcSizes();
      wxSize sz1 = GetSizer()->GetMinSize();
      int minHeight = 21;
      if (sz1.y > minHeight) {
         sz1.x = (sz1.x * minHeight) / sz1.y;
         GetSizer()->SetMinSize(sz1);
         SetMinSize(sz1);
      }
   }
}

// We are given the size of the toolbar.
// Compute digit Height and layout the Numeric Control.
void TimerToolBar::ResizeTime( const wxSize & sz) {

   int mx = sz.GetWidth() - 25;
   int my = sz.GetHeight();
   //wxLogDebug("ResizeTime %i,%i",mx,my);

   // Digit height is 2 less than y
   // OR fits to width, if width is a bit low.
   int h = my-2;
   h = wxMax(17, h);
   h = wxMin(h, 77);

   //wxLogDebug("Try h=%i dimensions(%i,%i)", h,mx,my);
   h++;
   do {
      h--;
      mAudioTime->SetDigitSize(h*0.66, h);
   } while ((h > 17) && mAudioTime->IsTooBig(mx, my));
   mAudioTime->Layout();
   //wxLogDebug(" accept height:%i", h);

   mDigitHeight = h;
}

// This 'OnSize' function is also called during moving the
// toolbar.  
void TimerToolBar::OnSize(wxSizeEvent & ev)
{
   //wxLogDebug("OnSize");
   if (!mbReady)
      return;

   ev.Skip();

   if (!mAudioTime)
      return;
   
   // If we are changing the time format, then we
   // preserve the height.  Otherwise we size the font to fit 
   // the space given.
   if (mbPreserveWidth || !mbPreserveHeight) {
      ResizeTime( ev.GetSize() );
   }

   // Refresh and update immediately, so that we don't get 
   // to see grot from partly redrawn toolbar during 
   // the resizing.
   Refresh(true);
   Update();
}

// The intention of this is to get the resize handle in the 
// correct position, after we've let go in dragging.
void TimerToolBar::ResizingDone() {
   if (!mbReady)
      return;
   wxSize sz = GetSize();
   //wxLogDebug("ReSizingDone %i,%i",sz.x,sz.y);

   Fit();
   sz.x = GetSize().GetX();
   //wxLogDebug("Fitted %i,%i",sz.x,sz.y);
   SetSize(sz);
   Updated();
}


void TimerToolBar::SetTimes(double audio)
{
   mAudioTime->SetValue(wxMax( 0.0, audio));
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

static RegisteredToolbarFactory factory{ TimeBarID,
   []( AudacityProject &project ){
      return ToolBar::Holder{ safenew TimerToolBar{ project } }; }
};

#ifdef EXPERIMENTAL_TIMER_TOOLBAR
namespace {
AttachedToolBarMenuItem sAttachment{
   /* i18n-hint: Clicking this menu item shows the toolbar
      for viewing actual time of the cursor */
   TimeBarID, wxT("ShowTimeTB"), XXO("&Time Toolbar"),
   { Registry::OrderingHint::After, "ShowSelectionTB" }
};
}
#endif

