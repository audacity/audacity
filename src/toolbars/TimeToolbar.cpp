/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeToolbar.cpp

  Copyright 2005 Dominic Mazzoni

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

*******************************************************************//**

\class TimeToolbar
\brief (not quite a Toolbar) at foot of screen for showing the position
of the play cursor.

*//****************************************************************//**

\class TimeToolbarListener
\brief A parent class of TimeToolbar, used to forward events to do
with changes in the TimeToolbar.

*//*******************************************************************/


#include "../Audacity.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifndef WX_PRECOMP
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/combobox.h>
#include <wx/intl.h>
#include <wx/radiobut.h>
#include <wx/settings.h>
#include <wx/sizer.h>
#include <wx/valtext.h>
#include <wx/stattext.h>
#endif
#include <wx/statline.h>


#include "TimeToolbarListener.h"
#include "TimeToolbar.h"

#include "../widgets/AButton.h"
#include "../AudioIO.h"
#include "../AColor.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Snap.h"
#include "../widgets/NumericTextCtrl.h"
#include "../AllThemeResources.h"

IMPLEMENT_CLASS(TimeToolbar, ToolBar);

const static wxChar *numbers[] =
{
   wxT("0"), wxT("1"), wxT("2"), wxT("3"), wxT("4"),
   wxT("5"), wxT("6"), wxT("7"), wxT("8"), wxT("9")
};

enum {
   TimeToolbarFirstID = 2700,
   TimeTBFirstButton,
   TimeTBMenuID = TimeTBFirstButton,
   AudioTimeID,
};

BEGIN_EVENT_TABLE(TimeToolbar, ToolBar)
   EVT_SIZE(TimeToolbar::OnSize)
// EVT_COMMAND( OnMenuId, wxEVT_COMMAND_BUTTON_CLICKED, TimeToolbar::OnButton )
   EVT_COMMAND(wxID_ANY, EVT_TIMETEXTCTRL_UPDATED, TimeToolbar::OnUpdate)
   EVT_COMMAND(wxID_ANY, EVT_CAPTURE_KEY, TimeToolbar::OnCaptureKey)
END_EVENT_TABLE()

TimeToolbar::TimeToolbar()
: ToolBar(TimeBarID, _("Timer"), wxT("Timer")),
  mListener(NULL), 
  mAudioTime(NULL)
{
   // Make sure we have a valid rate as the NumericTextCtrl()s
   // created in Populate()
   // depend on it.  Otherwise, division-by-zero floating point exceptions
   // will occur.
   // Refer to bug #462 for a scenario where the division-by-zero causes
   // Audacity to fail.
   mRate = (double) gPrefs->Read(wxT("/SamplingRate/DefaultProjectSampleRate"),
      AudioIO::GetOptimalSupportedSampleRate());
}

TimeToolbar::~TimeToolbar()
{
}

void TimeToolbar::Create(wxWindow * parent)
{
   ToolBar::Create(parent);
}



NumericTextCtrl * TimeToolbar::AddTime( const wxString Name, int id, wxSizer * pSizer ){
   wxString formatName = mListener ? mListener->ATTB_GetSelectionFormat() 
      : wxString(wxEmptyString);
   NumericTextCtrl * pCtrl = safenew NumericTextCtrl(
      NumericConverter::TIME, this, id, formatName, 0.0, mRate);
   pCtrl->SetName(Name);
   pCtrl->SetForegroundColour( theTheme.Colour( clrTrackPanelText ) );
   pCtrl->EnableMenu();
   pSizer->Add(pCtrl, 0, wxALIGN_TOP | wxRIGHT, 5);
   return pCtrl;
}

void TimeToolbar::Populate()
{
   SetBackgroundColour( theTheme.Colour( clrMedium  ) );
   mAudioTime = nullptr;


   // This will be inherited by all children:
   SetFont(wxFont(
#ifdef __WXMAC__
                  12
#else
                  9
#endif
                  ,
                  wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL));
   wxFlexGridSizer *mainSizer;

   /* we don't actually need a control yet, but we want to use its methods
    * to do some look-ups, so we'll have to create one. We can't make the
    * look-ups static because they depend on translations which are done at
    * runtime */

   // Outer sizer has space top and left.
   // Inner sizers have space on right only.
   // This choice makes for a nice border and internal spacing and places clear responsibility
   // on each sizer as to what spacings it creates.
   Add((mainSizer = safenew wxFlexGridSizer(1, 1, 1)), 0, wxALIGN_TOP | wxLEFT | wxTOP, 5);

   //
   // Top row (mostly labels)
   //

   wxColour clrText =  theTheme.Colour( clrTrackPanelText );
   wxColour clrText2 = *wxBLUE;


   //
   // Botton row, (mostly time controls)
   //

   mAudioTime = AddTime(_("Audio Position"), AudioTimeID, mainSizer );
   mAudioTime->SetScaleFactor( 2.4 );

   mainSizer->Layout();
   RegenerateTooltips();
   Layout();

   SetMinSize( GetSizer()->GetMinSize() );
}

void TimeToolbar::UpdatePrefs()
{
   mRate = (double) gPrefs->Read(wxT("/SamplingRate/DefaultProjectSampleRate"), AudioIO::GetOptimalSupportedSampleRate());

   wxCommandEvent e;
   e.SetInt(mAudioTime->GetFormatIndex());
   OnUpdate(e);

   // Set label to pull in language change
   SetLabel(_("Selection"));

   RegenerateTooltips();

   // Give base class a chance
   ToolBar::UpdatePrefs();
}

void TimeToolbar::SetListener(TimeToolbarListener *l)
{
   mListener = l;
   SetRate(mListener->ATTB_GetRate());
   SetSelectionFormat(mListener->ATTB_GetSelectionFormat());
};

void TimeToolbar::RegenerateTooltips()
{
}

void TimeToolbar::OnSize(wxSizeEvent &evt)
{
   Refresh( true );

   evt.Skip();
}

// Called when one of the format drop downs is changed.
void TimeToolbar::OnUpdate(wxCommandEvent &evt)
{
   int index = evt.GetInt();
   wxWindow *w = FindFocus();
   NumericTextCtrl ** Ctrls[1] = { &mAudioTime };
   int i;
   int iFocus = -1;
   for(i=0;i<1;i++)
      if( w == *Ctrls[i] )
         iFocus = i;

   evt.Skip(false);

   wxString format;

   // Save format name before recreating the controls so they resize properly
   format = mAudioTime->GetBuiltinName(index);
   mListener->ATTB_SetSelectionFormat(format);

   RegenerateTooltips();

   // ToolBar::ReCreateButtons() will get rid of our sizers and controls
   // so reset pointers first.
   for( i=0;i<1;i++)
      *Ctrls[i]=NULL;

   ToolBar::ReCreateButtons();

   ValuesToControls();

   format = mAudioTime->GetBuiltinFormat(index);
   for( i=0;i<1;i++)
      (*Ctrls[i])->SetFormatString( format );

   if( iFocus >=0 )
      (*Ctrls[iFocus])->SetFocus();
   Updated();
}

void TimeToolbar::ValuesToControls()
{
   NumericTextCtrl ** Ctrls[1] = { &mAudioTime };
   double Values[5] = { mAudio };
   int i;
   for(i=0;i<1;i++)
      if( *Ctrls[i] )
         (*Ctrls[i])->SetValue( Values[i] );
}

void TimeToolbar::SetTimes( double audio )
{
   mAudio = audio;

   ValuesToControls();
}

void TimeToolbar::SetField(const wxChar *msg, int fieldNum)
{
   if (fieldNum < 0 || fieldNum >= 10)
      return;

   if (mField[fieldNum] != msg) {
      mField[fieldNum] = msg;
      Refresh(false);
   }
}

void TimeToolbar::SetSelectionFormat(const wxString & format)
{
   mAudioTime->SetFormatString(mAudioTime->GetBuiltinFormat(format));

   wxCommandEvent e;
   e.SetInt(mAudioTime->GetFormatIndex());
   OnUpdate(e);
}

void TimeToolbar::SetRate(double rate)
{
   if (rate != mRate) {
      // if the rate is actually being changed
      mRate = rate;   // update the stored rate
      // update the TimeTextCtrls if they exist
      NumericTextCtrl ** Ctrls[1] = { &mAudioTime };
      int i;
      for(i=0;i<1;i++)
         if( *Ctrls[i] )
            (*Ctrls[i])->SetSampleRate( rate );
   }
}

void TimeToolbar::OnFocus(wxFocusEvent &event)
{
   if (event.GetEventType() == wxEVT_KILL_FOCUS) {
      AudacityProject::ReleaseKeyboard(this);
   }
   else {
      AudacityProject::CaptureKeyboard(this);
   }

   Refresh(false);
   event.Skip();
}

void TimeToolbar::OnCaptureKey(wxCommandEvent &event)
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

