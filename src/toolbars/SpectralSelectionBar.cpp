/**********************************************************************

Audacity: A Digital Audio Editor

SpectralSelectionBar.cpp

Copyright 2014 Dominic Mazzoni

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

*******************************************************************//**

\class SpectralSelectionBar
\brief (not quite a Toolbar) at foot of screen for setting and viewing the
frequency selection range.

*//****************************************************************//**

\class SpectralSelectionBarListener
\brief A class used to forward events to do
with changes in the SpectralSelectionBar.

*//*******************************************************************/


#include "../Audacity.h"

#include <algorithm>

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

#include "SpectralSelectionBarListener.h"
#include "SpectralSelectionBar.h"

#include "../AudacityApp.h"
#include "../SelectedRegion.h"
#include "../widgets/NumericTextCtrl.h"

#ifdef EXPERIMENTAL_SPECTRAL_EDITING

IMPLEMENT_CLASS(SpectralSelectionBar, ToolBar);

enum {
   SpectralSelectionBarFirstID = 2750,
   OnCenterID,
   OnWidthID
};

BEGIN_EVENT_TABLE(SpectralSelectionBar, ToolBar)
EVT_SIZE(SpectralSelectionBar::OnSize)
EVT_TEXT(OnCenterID, SpectralSelectionBar::OnCenter)
EVT_TEXT(OnWidthID, SpectralSelectionBar::OnWidth)
EVT_COMMAND(wxID_ANY, EVT_FREQUENCYTEXTCTRL_UPDATED, SpectralSelectionBar::OnUpdate)
EVT_COMMAND(wxID_ANY, EVT_LOGFREQUENCYTEXTCTRL_UPDATED, SpectralSelectionBar::OnUpdate)
END_EVENT_TABLE()

SpectralSelectionBar::SpectralSelectionBar()
: ToolBar(SpectralSelectionBarID, _("SpectralSelection"), wxT("SpectralSelection")),
mListener(NULL), mCenter(0.0), mWidth(0.0),
mCenterCtrl(NULL), mWidthCtrl(NULL)
{
}

SpectralSelectionBar::~SpectralSelectionBar()
{
}

void SpectralSelectionBar::Create(wxWindow * parent)
{
   ToolBar::Create(parent);
}

void SpectralSelectionBar::Populate()
{
   // This will be inherited by all children:
   SetFont(wxFont(9, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL));

   wxFlexGridSizer *mainSizer;
   wxBoxSizer *hSizer;

   /* we don't actually need a control yet, but we want to use its methods
   * to do some look-ups, so we'll have to create one. We can't make the
   * look-ups static because they depend on translations which are done at
   * runtime */

   wxString frequencyFormatName = mListener
      ? mListener->SSBL_GetFrequencySelectionFormatName()
      : wxString(wxEmptyString);
   wxString logFrequencyFormatName = mListener
      ? mListener->SSBL_GetLogFrequencySelectionFormatName()
      : wxString(wxEmptyString);

   mainSizer = new wxFlexGridSizer(2, 1, 1);
   Add(mainSizer, 0, wxALIGN_CENTER_VERTICAL);

   //
   // Top row (labels)
   //

   mainSizer->Add(new wxStaticText(this, -1, _("Center:"),
                                   wxDefaultPosition, wxDefaultSize),
      0, wxALIGN_CENTER_VERTICAL | wxRIGHT, 5);

   mainSizer->Add(new wxStaticText(this, -1, _("Bandwidth:")),
      0, wxALIGN_CENTER_VERTICAL | wxRIGHT, 0);

   //
   // Bottom row (controls)
   //

   mCenterCtrl = new NumericTextCtrl(
      NumericConverter::FREQUENCY, this, OnCenterID, frequencyFormatName, 0.0);
   mCenterCtrl->SetName(_("Center Frequency:"));
   mCenterCtrl->EnableMenu();
   mainSizer->Add(mCenterCtrl, 0, wxALIGN_CENTER_VERTICAL | wxRIGHT, 5);

   mWidthCtrl = new NumericTextCtrl(
      NumericConverter::LOG_FREQUENCY, this, OnWidthID, logFrequencyFormatName, 0.0);
   mWidthCtrl->SetName(wxString(_("Bandwidth:")));
   mWidthCtrl->EnableMenu();
   mainSizer->Add(mWidthCtrl, 0, wxALIGN_CENTER_VERTICAL | wxRIGHT, 0);

   mainSizer->Layout();

   Layout();

   SetMinSize(GetSizer()->GetMinSize());
}

void SpectralSelectionBar::UpdatePrefs()
{
   {
      wxCommandEvent e(EVT_FREQUENCYTEXTCTRL_UPDATED);
      e.SetInt(mCenterCtrl->GetFormatIndex());
      OnUpdate(e);
   }

   {
      wxCommandEvent e(EVT_LOGFREQUENCYTEXTCTRL_UPDATED);
      e.SetInt(mWidthCtrl->GetFormatIndex());
      OnUpdate(e);
   }

   // Set label to pull in language change
   SetLabel(_("SpectralSelection"));

   // Give base class a chance
   ToolBar::UpdatePrefs();
}

void SpectralSelectionBar::SetListener(SpectralSelectionBarListener *l)
{
   mListener = l;
   SetFrequencySelectionFormatName(mListener->SSBL_GetFrequencySelectionFormatName());
   SetLogFrequencySelectionFormatName(mListener->SSBL_GetLogFrequencySelectionFormatName());
};

void SpectralSelectionBar::OnSize(wxSizeEvent &evt)
{
   Refresh(true);

   evt.Skip();
}

void SpectralSelectionBar::ModifySpectralSelection(bool done)
{
   const double nyq = mListener->SSBL_GetRate() / 2.0;

   mCenter = mCenterCtrl->GetValue();
   mWidth = mWidthCtrl->GetValue();
   double bottom, top;
   if (mCenter < 0 && mWidth < 0)
      bottom = top = SelectedRegion::UndefinedFrequency;
   else {
      if (mCenter < 0) {
         mWidth = log(std::min(nyq, exp(mWidth)));
         // Choose arbitrary center for the width
         mCenter = sqrt(nyq);
      }
      else if (mWidth < 0) {
         mCenter = std::max(1.0, std::min(nyq, mCenter));
         // Choose arbitrary width for the center
         const double ratio = std::min(mCenter, nyq / mCenter);
         mWidth = log(ratio * ratio);
      }
      else {
         mCenter = std::max(1.0, std::min(nyq, mCenter));
         double ratio = std::min(mCenter, nyq / mCenter);
         mWidth = std::min(2 * log(ratio), mWidth);
      }

      const double ratio = exp(mWidth / 2);
      bottom = mCenter / ratio, top = mCenter * ratio;
   }

   // Notify project and track panel, which may change
   // the values again, and call back to us in SetFrequencies()
   mListener->SSBL_ModifySpectralSelection(bottom, top, done);
}

void SpectralSelectionBar::OnCenter(wxCommandEvent & event)
{
   ModifySpectralSelection(event.GetInt() != 0);
}

void SpectralSelectionBar::OnWidth(wxCommandEvent & event)
{
   ModifySpectralSelection(event.GetInt() != 0);
}

void SpectralSelectionBar::OnUpdate(wxCommandEvent &evt)
{
   int index = evt.GetInt();
   wxWindow *w = FindFocus();
   bool centerFocus = (w == mCenterCtrl);
   bool widthFocus = (w == mWidthCtrl);

   evt.Skip(false);

   // Save formats before recreating the controls so they resize properly
   wxEventType type = evt.GetEventType();
   int frequencyFormatIndex = mCenterCtrl->GetFormatIndex();
   int widthFormatIndex = mWidthCtrl->GetFormatIndex();
   if (type == EVT_FREQUENCYTEXTCTRL_UPDATED) {
      frequencyFormatIndex = index;
      mListener->SSBL_SetFrequencySelectionFormatName
         (mCenterCtrl->GetBuiltinName(index));
   }
   else if (type == EVT_LOGFREQUENCYTEXTCTRL_UPDATED) {
      widthFormatIndex = index;
      mListener->SSBL_SetLogFrequencySelectionFormatName
         (mWidthCtrl->GetBuiltinName(index));
   }

   // ToolBar::ReCreateButtons() will get rid of our sizers and controls
   // so reset pointers first.
   mCenterCtrl = mWidthCtrl = NULL;

   ToolBar::ReCreateButtons();

   ValuesToControls();

   mCenterCtrl->SetFormatName
      (mCenterCtrl->GetBuiltinName(frequencyFormatIndex));
   mWidthCtrl->SetFormatName
      (mWidthCtrl->GetBuiltinName(widthFormatIndex));

   if (centerFocus) {
      mCenterCtrl->SetFocus();
   }
   else if (widthFocus) {
      mWidthCtrl->SetFocus();
   }

   Updated();
}

void SpectralSelectionBar::ValuesToControls()
{
   mCenterCtrl->SetValue(mCenter);
   mWidthCtrl->SetValue(mWidth);
}

void SpectralSelectionBar::SetFrequencies(double bottom, double top)
{
   if (bottom > 0 && top >= bottom)
      mWidth = log(top / bottom), mCenter = sqrt(top * bottom);
   else
      mWidth = mCenter = -1.0;

   ValuesToControls();
}

void SpectralSelectionBar::SetFrequencySelectionFormatName(const wxString & formatName)
{
   mCenterCtrl->SetFormatName(formatName);

   wxCommandEvent e(EVT_FREQUENCYTEXTCTRL_UPDATED);
   e.SetInt(mCenterCtrl->GetFormatIndex());
   OnUpdate(e);
}

void SpectralSelectionBar::SetLogFrequencySelectionFormatName(const wxString & formatName)
{
   mWidthCtrl->SetFormatName(formatName);

   wxCommandEvent e(EVT_LOGFREQUENCYTEXTCTRL_UPDATED);
   e.SetInt(mWidthCtrl->GetFormatIndex());
   OnUpdate(e);
}

#endif // #ifdef EXPERIMENTAL_SPECTRAL_EDITING
