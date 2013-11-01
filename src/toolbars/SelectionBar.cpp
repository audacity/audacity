/**********************************************************************

  Audacity: A Digital Audio Editor

  SelectionBar.cpp

  Copyright 2005 Dominic Mazzoni
  
  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

*******************************************************************//**

\class SelectionBar
\brief (not quite a Toolbar) at foot of screen for setting and viewing the 
selection range.

*//****************************************************************//**

\class SelectionBarListener
\brief A parent class of SelectionBar, used to forward events to do 
with changes in the SelectionBar.

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
#endif
#include <wx/statline.h>

#include "SelectionBar.h"

#include "../AudacityApp.h"
#include "../AudioIO.h"
#include "../AColor.h"
#include "../Prefs.h"
#include "../Snap.h"
#include "../widgets/TimeTextCtrl.h"

IMPLEMENT_CLASS(SelectionBar, ToolBar);

const static wxChar *numbers[] =
{
   wxT("0"), wxT("1"), wxT("2"), wxT("3"), wxT("4"),
   wxT("5"), wxT("6"), wxT("7"), wxT("8"), wxT("9")
};

enum {
   SelectionBarFirstID = 2700,
   OnRateID,
   OnSnapToID,
   OnLengthRadioID,
   OnEndRadioID,
   OnLeftTimeID,
   OnRightTimeID
};

BEGIN_EVENT_TABLE(SelectionBar, ToolBar)
   EVT_SIZE(SelectionBar::OnSize)
   EVT_TEXT(OnLeftTimeID, SelectionBar::OnLeftTime)
   EVT_TEXT(OnRightTimeID, SelectionBar::OnRightTime)
   EVT_RADIOBUTTON(OnLengthRadioID, SelectionBar::OnLengthRadio)
   EVT_RADIOBUTTON(OnEndRadioID, SelectionBar::OnEndRadio)
   EVT_CHOICE(OnSnapToID, SelectionBar::OnSnapTo)
   EVT_COMBOBOX(OnRateID, SelectionBar::OnRate)
   EVT_TEXT(OnRateID, SelectionBar::OnRate)
   EVT_COMMAND(wxID_ANY, EVT_TIMETEXTCTRL_UPDATED, SelectionBar::OnUpdate)
   EVT_COMMAND(wxID_ANY, EVT_CAPTURE_KEY, SelectionBar::OnCaptureKey)
END_EVENT_TABLE()

SelectionBar::SelectionBar()
: ToolBar(SelectionBarID, _("Selection"), wxT("Selection")),
  mListener(NULL), mRate(0.0), mStart(0.0), mEnd(0.0), mAudio(0.0),
  mLeftTime(NULL), mRightTime(NULL), mAudioTime(NULL)
{
   // Make sure we have a valid rate as the TimeTextCtrl()s created in Populate()
   // depend on it.  Otherwise, division-by-zero floating point exceptions will occur.
   // Refer to bug #462 for a scenario where the division-by-zero causes Audacity to fail.
   mRate = (double) gPrefs->Read(wxT("/SamplingRate/DefaultProjectSampleRate"), AudioIO::GetOptimalSupportedSampleRate());
}

SelectionBar::~SelectionBar()
{
}

void SelectionBar::Create(wxWindow * parent)
{
   ToolBar::Create(parent);
}

void SelectionBar::Populate()
{
   // This will be inherited by all children:
   SetFont(wxFont(9, wxSWISS, wxNORMAL, wxNORMAL));

   wxFlexGridSizer *mainSizer;
   wxBoxSizer *hSizer;

   /* we don't actually need a control yet, but we want to use it's methods
    * to do some look-ups, so we'll have to create one. We can't make the 
    * look-ups static because they depend on translations which are done at
    * runtime */
   wxString formatName = mListener ? mListener->AS_GetSelectionFormat() : wxString(wxEmptyString);

   mainSizer = new wxFlexGridSizer(7, 1, 1);
   Add(mainSizer, 0, wxALIGN_CENTER_VERTICAL);

   //
   // Top row (mostly labels)
   //

   mainSizer->Add(new wxStaticText(this, -1, _("Project Rate (Hz):"),
   // LLL:  On my Ubuntu 7.04 install, the label wraps to two lines
   //       and I could not figure out why.  Thus...hackage.
#if defined(__WXGTK__)
                  wxDefaultPosition, wxSize(110, -1)),
#else
                  wxDefaultPosition, wxDefaultSize),
#endif
               0, wxALIGN_CENTER_VERTICAL | wxRIGHT, 5);

   mainSizer->Add(5, 1);

   mainSizer->Add(new wxStaticText(this, -1, _("Snap To:")),
               0, wxALIGN_CENTER_VERTICAL | wxRIGHT, 5);

   mainSizer->Add(new wxStaticText(this, -1, _("Selection Start:")),
               0, wxALIGN_CENTER_VERTICAL | wxRIGHT, 5);

   bool showSelectionLength = false;
   gPrefs->Read(wxT("/ShowSelectionLength"), &showSelectionLength);
   
   hSizer = new wxBoxSizer(wxHORIZONTAL);
   mRightEndButton = new wxRadioButton(this, OnEndRadioID, _("End"),
                                       wxDefaultPosition, wxDefaultSize,
                                       wxRB_GROUP);
   mRightEndButton->SetName(_("End"));
   mRightEndButton->SetValue(!showSelectionLength);
   hSizer->Add(mRightEndButton,
               0, wxRIGHT | wxALIGN_CENTER_VERTICAL, 5);
   mRightLengthButton = new wxRadioButton(this, OnLengthRadioID, _("Length"));
   mRightLengthButton->SetName(_("Length"));
   mRightLengthButton->SetValue(showSelectionLength);
   hSizer->Add(mRightLengthButton,
               0, wxALIGN_CENTER_VERTICAL | wxRIGHT, 5);
#if defined(__WXMSW__)
      // Refer to Microsoft KB article 261192 for an explanation as
      // to why this is needed.  We've only experienced it under Win2k
      // so it's probably been fixed.  But, it doesn't hurt to have this
      // in for all versions.
      wxRadioButton* dummyButton = 
         new wxRadioButton(this, wxID_ANY, _("hidden"),
                           wxDefaultPosition, wxDefaultSize,
                           wxRB_GROUP);
      dummyButton->Disable();
      dummyButton->Hide();
#endif
   mainSizer->Add(hSizer, 0,  wxALIGN_CENTER_VERTICAL | wxRIGHT, 0);

   mainSizer->Add(5, 1);

   mainSizer->Add(new wxStaticText(this, -1, _("Audio Position:")),
                  0, wxALIGN_CENTER_VERTICAL | wxRIGHT, 0);

   //
   // Middle row (mostly time controls)
   //

   mRateBox = new wxComboBox(this, OnRateID,
                             wxT(""),
                             wxDefaultPosition, wxSize(80, -1));
   mRateBox->SetName(_("Project Rate (Hz):"));
   wxTextValidator vld(wxFILTER_INCLUDE_CHAR_LIST);
   vld.SetIncludes(wxArrayString(10, numbers));
   mRateBox->SetValidator(vld);
   mRateBox->SetValue(wxString::Format(wxT("%d"), (int)mRate));
   UpdateRates(); // Must be done _after_ setting value on mRateBox!

   // We need to capture the SetFocus and KillFocus events to set up
   // for keyboard capture.  On Windows and GTK it's easy since the
   // combobox is presented as one control to hook into.
   mRateText = mRateBox;

#if defined(__WXMAC__)
   // The Mac uses a standard wxTextCtrl for the edit portion and that's
   // the control that gets the focus events.  So we have to find the
   // textctrl.
   wxWindowList kids = mRateBox->GetChildren();
   for (unsigned int i = 0; i < kids.GetCount(); i++) {
      wxClassInfo *ci = kids[i]->GetClassInfo();
      if (ci->IsKindOf(CLASSINFO(wxTextCtrl))) {
         mRateText = kids[i];
         break;
      }
   }
#endif

   mRateText->Connect(wxEVT_SET_FOCUS,
                      wxFocusEventHandler(SelectionBar::OnFocus),
                      NULL,
                      this);
   mRateText->Connect(wxEVT_KILL_FOCUS,
                      wxFocusEventHandler(SelectionBar::OnFocus),
                      NULL,
                      this);

   mainSizer->Add(mRateBox, 0, wxALIGN_CENTER_VERTICAL | wxRIGHT, 5);

   mainSizer->Add(new wxStaticLine(this, -1, wxDefaultPosition,
                                   wxSize(1, toolbarSingle),
                                   wxLI_VERTICAL),
                  0,  wxRIGHT, 5);

   mSnapTo = new wxChoice(this, OnSnapToID,
                          wxDefaultPosition, wxDefaultSize,
                          SnapManager::GetSnapLabels());
   mainSizer->Add(mSnapTo,
                  0, wxALIGN_CENTER_VERTICAL | wxRIGHT, 5);
   mSnapTo->SetName(_("Snap To"));
   mSnapTo->SetSelection(mListener ? mListener->AS_GetSnapTo() : SNAP_OFF);
   #if wxUSE_TOOLTIPS
      mSnapTo->SetToolTip(wxString::Format(_("Snap Clicks/Selections to %s"), formatName.c_str()));
   #endif

   mSnapTo->Connect(wxEVT_SET_FOCUS,
                    wxFocusEventHandler(SelectionBar::OnFocus),
                    NULL,
                    this);
   mSnapTo->Connect(wxEVT_KILL_FOCUS,
                    wxFocusEventHandler(SelectionBar::OnFocus),
                    NULL,
                    this);
   
   mLeftTime = new TimeTextCtrl(this, OnLeftTimeID, formatName, 0.0, mRate);
   mLeftTime->SetName(_("Selection Start:"));
   mLeftTime->EnableMenu();
   mainSizer->Add(mLeftTime, 0, wxALIGN_CENTER_VERTICAL | wxRIGHT, 5);

   mRightTime = new TimeTextCtrl(this, OnRightTimeID, formatName, 0.0, mRate);
   mRightTime->SetName(wxString(_("Selection ")) + (showSelectionLength ?
                                                   _("Length") :
                                                   _("End")));
   mRightTime->EnableMenu();
   mainSizer->Add(mRightTime, 0, wxALIGN_CENTER_VERTICAL | wxRIGHT, 5);

   mainSizer->Add(new wxStaticLine(this, -1, wxDefaultPosition,
                                   wxSize(1, toolbarSingle),
                                   wxLI_VERTICAL),
                  0, wxRIGHT, 5);

   mAudioTime = new TimeTextCtrl(this, wxID_ANY, formatName, 0.0, mRate);
   mAudioTime->SetName(_("Audio Position:"));
   mAudioTime->EnableMenu();
   mainSizer->Add(mAudioTime, 0, wxALIGN_CENTER_VERTICAL | wxRIGHT, 0);

   mainSizer->Layout();

   Layout();

   SetMinSize( GetSizer()->GetMinSize() );
}

void SelectionBar::UpdatePrefs()
{
   mRate = (double) gPrefs->Read(wxT("/SamplingRate/DefaultProjectSampleRate"), AudioIO::GetOptimalSupportedSampleRate());

   wxCommandEvent e;
   e.SetInt(mLeftTime->GetFormatIndex());
   OnUpdate(e);

   // Set label to pull in language change
   SetLabel(_("Selection"));

   // Give base class a chance
   ToolBar::UpdatePrefs();
}

void SelectionBar::SetListener(SelectionBarListener *l)
{
   mListener = l;
   SetRate(mListener->AS_GetRate());
   SetSnapTo(mListener->AS_GetSnapTo());
   SetSelectionFormat(mListener->AS_GetSelectionFormat());
};

void SelectionBar::OnSize(wxSizeEvent &evt)
{
   Refresh( true );

   evt.Skip();
}

void SelectionBar::ModifySelection(bool done)
{
   mStart = mLeftTime->GetTimeValue();
   double right = mRightTime->GetTimeValue();

   if (mRightEndButton->GetValue()) {
      if(mStart > right)
         mEnd = mStart;
      else
         mEnd = right;
   }
   else
      mEnd = mStart + right;

   mListener->AS_ModifySelection(mStart, mEnd, done);
}

void SelectionBar::OnLeftTime(wxCommandEvent & event)
{
   ModifySelection(event.GetInt() != 0);
}

void SelectionBar::OnRightTime(wxCommandEvent & event)
{
   ModifySelection(event.GetInt() != 0);
}

void SelectionBar::OnLengthRadio(wxCommandEvent & WXUNUSED(event))
{
   gPrefs->Write(wxT("/ShowSelectionLength"), true);
   gPrefs->Flush();
   mRightTime->SetName(wxString(_("Selection Length")));

   ValuesToControls();
}

void SelectionBar::OnEndRadio(wxCommandEvent & WXUNUSED(event))
{
   gPrefs->Write(wxT("/ShowSelectionLength"), false);
   mRightTime->SetName(wxString(_("Selection End")));

   ValuesToControls();
}

void SelectionBar::OnUpdate(wxCommandEvent &evt)
{
   int index = evt.GetInt();
   wxWindow *w = FindFocus();
   bool leftFocus = (w == mLeftTime);
   bool rightFocus = (w == mRightTime);
   bool audioFocus = (w == mAudioTime);
   
   evt.Skip(false);
   
   wxString format;

   // Save format name before recreating the controls so they resize properly
   format = mLeftTime->GetBuiltinName(index);
   mListener->AS_SetSelectionFormat(format);

#if wxUSE_TOOLTIPS
   mSnapTo->SetToolTip(wxString::Format(_("Snap Clicks/Selections to %s"), format.c_str()));
#endif
   
   // ToolBar::ReCreateButtons() will get rid of our sizers and controls
   // so reset pointers first.
   mLeftTime =
   mRightTime =
   mAudioTime = NULL;

   mRightEndButton =
   mRightLengthButton = NULL;

   mRateBox = NULL;
   mRateText = NULL;

   ToolBar::ReCreateButtons();

   ValuesToControls();

   format = mLeftTime->GetBuiltinFormat(index);
   mLeftTime->SetFormatString(format);
   mRightTime->SetFormatString(format);
   mAudioTime->SetFormatString(format);

   if (leftFocus) {
      mLeftTime->SetFocus();
   }
   else if (rightFocus) {
      mRightTime->SetFocus();
   }
   else if (audioFocus) {
      mAudioTime->SetFocus();
   }

   Updated();
}

void SelectionBar::ValuesToControls()
{
   mLeftTime->SetTimeValue(mStart);

   if (mRightEndButton->GetValue())
      mRightTime->SetTimeValue(mEnd);
   else
   {  // mRightTime is the length.
      // Be sure to take into account the sub-sample offset.
      // See TimeToLongSamples and LongSamplesToTime but here at the project rate.
      double t = (sampleCount)floor(mEnd * mRate + 0.5);
      t -= (sampleCount)floor(mStart * mRate + 0.5);
      t /= mRate;
      mRightTime->SetTimeValue(t);
   }

   mAudioTime->SetTimeValue(mAudio);
}

void SelectionBar::SetTimes(double start, double end, double audio)
{
   mStart = start;
   mEnd = end;
   mAudio = audio;

   ValuesToControls();
}

double SelectionBar::GetLeftTime()
{
   return mLeftTime->GetTimeValue();
}

double SelectionBar::GetRightTime()
{
   if (mRightEndButton->GetValue())
      return mRightTime->GetTimeValue();
   else {
      // What would be shown if we were showing the end time
      TimeTextCtrl ttc(this, wxID_ANY, wxT(""), 0.0, mRate);
      ttc.SetFormatString(mRightTime->GetFormatString());
      ttc.SetSampleRate(mRate);
      ttc.SetTimeValue(mEnd);
      return ttc.GetTimeValue();
   }
}

void SelectionBar::SetField(const wxChar *msg, int fieldNum)
{
   if (fieldNum < 0 || fieldNum >= 10)
      return;

   if (mField[fieldNum] != msg) {
      mField[fieldNum] = msg;
      Refresh(false);
   }
}

void SelectionBar::SetSnapTo(int snap)
{
   mSnapTo->SetSelection(snap);
}

void SelectionBar::SetSelectionFormat(const wxString & format)
{
   mLeftTime->SetFormatString(mLeftTime->GetBuiltinFormat(format));

   wxCommandEvent e;
   e.SetInt(mLeftTime->GetFormatIndex());
   OnUpdate(e);   
}

void SelectionBar::SetRate(double rate)
{
   if (rate != mRate) {
      // if the rate is actually being changed
      mRate = rate;   // update the stored rate
      mRateBox->SetValue(wxString::Format(wxT("%d"), (int)rate));
      // update the TimeTextCtrls if they exist
      if (mLeftTime) mLeftTime->SetSampleRate(rate);
      if (mRightTime) mRightTime->SetSampleRate(rate);
      if (mAudioTime) mAudioTime->SetSampleRate(rate);
   }
}

void SelectionBar::OnRate(wxCommandEvent & WXUNUSED(event))
{
   if (mRateBox->GetValue().ToDouble(&mRate) && // is a numeric value
         (mRate != 0.0))
   {
      if (mLeftTime) mLeftTime->SetSampleRate(mRate);
      if (mRightTime) mRightTime->SetSampleRate(mRate);
      if (mAudioTime) mAudioTime->SetSampleRate(mRate);
      if (mListener) mListener->AS_SetRate(mRate);
   }
}

void SelectionBar::UpdateRates()
{
   wxString oldValue = mRateBox->GetValue();
   mRateBox->Clear();
   for (int i = 0; i < AudioIO::NumStandardRates; i++) {
      mRateBox->Append(wxString::Format(wxT("%d"), AudioIO::StandardRates[i]));
   }
   mRateBox->SetValue(oldValue);
}

void SelectionBar::OnFocus(wxFocusEvent &event)
{
   wxCommandEvent e(EVT_CAPTURE_KEYBOARD);

   if (event.GetEventType() == wxEVT_KILL_FOCUS) {
      e.SetEventType(EVT_RELEASE_KEYBOARD);
   }
   e.SetEventObject(this);
   GetParent()->GetEventHandler()->ProcessEvent(e);

   Refresh(false);

   event.Skip();
}

void SelectionBar::OnCaptureKey(wxCommandEvent &event)
{
   wxKeyEvent *kevent = (wxKeyEvent *)event.GetEventObject();
   wxWindow *w = FindFocus();
   int keyCode = kevent->GetKeyCode();

   // Pass the SPACE through for SnapTo
   if (w == mSnapTo && keyCode == WXK_SPACE) {
      return;
   }

   // Convert numeric keypad entries.
   if ((keyCode >= WXK_NUMPAD0) && (keyCode <= WXK_NUMPAD9)) {
      keyCode -= WXK_NUMPAD0 - '0';
   }

   if (keyCode >= '0' && keyCode <= '9') {
      return;
   }

   // UP/DOWN/LEFT/RIGHT for mRateText
   if (w == mRateText) {
      switch (keyCode)
      {
         case WXK_LEFT:
         case WXK_RIGHT:
         case WXK_UP:
         case WXK_DOWN:
         case WXK_DELETE:
         case WXK_BACK:
            return;
      }
   }
   
   event.Skip();

   return;
}

void SelectionBar::OnSnapTo(wxCommandEvent & WXUNUSED(event))
{
   mListener->AS_SetSnapTo(mSnapTo->GetSelection());

   return;
}
