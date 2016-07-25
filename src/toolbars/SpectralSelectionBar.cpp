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
#include "../MemoryX.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifndef WX_PRECOMP
#include <wx/defs.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/combobox.h>
#include <wx/intl.h>
#include <wx/radiobut.h>
#include <wx/settings.h>
#include <wx/sizer.h>
#include <wx/valtext.h>
#include <wx/window.h>
#endif
#include <wx/statline.h>

#include "SpectralSelectionBarListener.h"
#include "SpectralSelectionBar.h"

#include "../AudacityApp.h"
#include "../Prefs.h"
#include "../SelectedRegion.h"
#include "../widgets/NumericTextCtrl.h"

#include "../Experimental.h"

#ifdef EXPERIMENTAL_SPECTRAL_EDITING

IMPLEMENT_CLASS(SpectralSelectionBar, ToolBar);

enum {
   SpectralSelectionBarFirstID = 2750,
   OnCenterID,
   OnWidthID,
   OnLowID,
   OnHighID,
   OnChoiceID,
};

BEGIN_EVENT_TABLE(SpectralSelectionBar, ToolBar)
   EVT_SIZE(SpectralSelectionBar::OnSize)
   EVT_TEXT(OnCenterID, SpectralSelectionBar::OnCtrl)
   EVT_TEXT(OnWidthID, SpectralSelectionBar::OnCtrl)
   EVT_TEXT(OnLowID, SpectralSelectionBar::OnCtrl)
   EVT_TEXT(OnHighID, SpectralSelectionBar::OnCtrl)
   EVT_CHOICE(OnChoiceID, SpectralSelectionBar::OnChoice)
   EVT_COMMAND(wxID_ANY, EVT_FREQUENCYTEXTCTRL_UPDATED, SpectralSelectionBar::OnUpdate)
   EVT_COMMAND(wxID_ANY, EVT_BANDWIDTHTEXTCTRL_UPDATED, SpectralSelectionBar::OnUpdate)
END_EVENT_TABLE()

static const wxString preferencePath
(wxT("/GUI/Toolbars/SpectralSelection/CenterAndWidthChoice"));

SpectralSelectionBar::SpectralSelectionBar()
: ToolBar(SpectralSelectionBarID, _("Spectral Selection"), wxT("SpectralSelection"))
, mListener(NULL), mbCenterAndWidth(true)
, mCenter(0.0), mWidth(0.0), mLow(0.0), mHigh(0.0)
, mCenterCtrl(NULL), mWidthCtrl(NULL), mLowCtrl(NULL), mHighCtrl(NULL)
, mChoice(NULL)
{
}

SpectralSelectionBar::~SpectralSelectionBar()
{
   // Do nothing, sizer deletes the controls
}

void SpectralSelectionBar::Create(wxWindow * parent)
{
   ToolBar::Create(parent);
   mHeight = wxWindowBase::GetSizer()->GetSize().GetHeight();
}

void SpectralSelectionBar::Populate()
{
   gPrefs->Read(preferencePath, &mbCenterAndWidth, true);

   // This will be inherited by all children:
   SetFont(wxFont(
#ifdef __WXMAC__
                  12
#else
                  9
#endif
                  ,
                  wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL));

   /* we don't actually need a control yet, but we want to use its methods
   * to do some look-ups, so we'll have to create one. We can't make the
   * look-ups static because they depend on translations which are done at
   * runtime */

   wxString frequencyFormatName = mListener
      ? mListener->SSBL_GetFrequencySelectionFormatName()
      : wxString(wxEmptyString);
   wxString bandwidthFormatName = mListener
      ? mListener->SSBL_GetBandwidthSelectionFormatName()
      : wxString(wxEmptyString);

   wxFlexGridSizer *mainSizer;
   Add((mainSizer = safenew wxFlexGridSizer(1, 1, 1)), 0, wxALIGN_CENTER_VERTICAL);

   //
   // Top row, choice box
   //

   const wxString choices[2] = {
      _("Center frequency and Width"),
      _("Low and High Frequencies"),
   };
   mChoice = safenew wxChoice
      (this, OnChoiceID, wxDefaultPosition, wxDefaultSize, 2, choices,
       0, wxDefaultValidator, _("Spectral Selection"));
   mChoice->SetSelection(mbCenterAndWidth ? 0 : 1);
   mainSizer->Add(mChoice, 0, wxALIGN_CENTER_VERTICAL | wxEXPAND, 5);

   //
   // Bottom row, split into two columns, each with one control
   //

   {
      auto subSizer = std::make_unique<wxBoxSizer>(wxHORIZONTAL);

      mCenterCtrl = safenew NumericTextCtrl(
         NumericConverter::FREQUENCY, this, OnCenterID, frequencyFormatName, 0.0);
      mCenterCtrl->SetInvalidValue(SelectedRegion::UndefinedFrequency);
      mCenterCtrl->SetName(_("Center Frequency:"));
      mCenterCtrl->EnableMenu();
      subSizer->Add(mCenterCtrl, 0, wxALIGN_CENTER_VERTICAL | wxRIGHT, 5);

      mWidthCtrl = safenew NumericTextCtrl(
         NumericConverter::BANDWIDTH, this, OnWidthID, bandwidthFormatName, 0.0);
      mWidthCtrl->SetInvalidValue(-1.0);
      mWidthCtrl->SetName(wxString(_("Bandwidth:")));
      mWidthCtrl->EnableMenu();
      subSizer->Add(mWidthCtrl, 0, wxALIGN_CENTER_VERTICAL | wxRIGHT, 0);

      mLowCtrl = safenew NumericTextCtrl(
         NumericConverter::FREQUENCY, this, OnLowID, frequencyFormatName, 0.0);
      mLowCtrl->SetInvalidValue(SelectedRegion::UndefinedFrequency);
      mLowCtrl->SetName(_("Low Frequency:"));
      mLowCtrl->EnableMenu();
      subSizer->Add(mLowCtrl, 0, wxALIGN_CENTER_VERTICAL | wxRIGHT, 5);

      mHighCtrl = safenew NumericTextCtrl(
         NumericConverter::FREQUENCY, this, OnHighID, frequencyFormatName, 0.0);
      mHighCtrl->SetInvalidValue(SelectedRegion::UndefinedFrequency);
      mHighCtrl->SetName(wxString(_("High Frequency:")));
      mHighCtrl->EnableMenu();
      subSizer->Add(mHighCtrl, 0, wxALIGN_CENTER_VERTICAL | wxRIGHT, 0);

      mCenterCtrl->Show(mbCenterAndWidth);
      mWidthCtrl->Show(mbCenterAndWidth);
      mLowCtrl->Show(!mbCenterAndWidth);
      mHighCtrl->Show(!mbCenterAndWidth);

      mainSizer->Add(subSizer.release(), 0, wxALIGN_CENTER_VERTICAL | wxRIGHT, 0);
   }

   mainSizer->Layout();

   Layout();

   SetMinSize(GetSizer()->GetMinSize());
}

void SpectralSelectionBar::UpdatePrefs()
{
   {
      wxCommandEvent e(EVT_FREQUENCYTEXTCTRL_UPDATED);
      e.SetInt((mbCenterAndWidth? mCenterCtrl : mLowCtrl)->GetFormatIndex());
      OnUpdate(e);
   }

   if (mbCenterAndWidth)
   {
      wxCommandEvent e(EVT_BANDWIDTHTEXTCTRL_UPDATED);
      e.SetInt(mWidthCtrl->GetFormatIndex());
      OnUpdate(e);
   }

   // Set label to pull in language change
   SetLabel(_("Spectral Selection"));

   RegenerateTooltips();

   // Give base class a chance
   ToolBar::UpdatePrefs();
}

void SpectralSelectionBar::SetListener(SpectralSelectionBarListener *l)
{
   mListener = l;
   SetFrequencySelectionFormatName(mListener->SSBL_GetFrequencySelectionFormatName());
   SetBandwidthSelectionFormatName(mListener->SSBL_GetBandwidthSelectionFormatName());
};

void SpectralSelectionBar::OnSize(wxSizeEvent &evt)
{
   Refresh(true);

   evt.Skip();
}

void SpectralSelectionBar::ModifySpectralSelection(bool done)
{
   const double nyq = mListener->SSBL_GetRate() / 2.0;

   double bottom, top;
   if (mbCenterAndWidth) {
      mCenter = mCenterCtrl->GetValue();
      mWidth = mWidthCtrl->GetValue();
      if ((mCenter < 0 || mWidth < 0) &&
          (mLow >= 0 || mHigh >= 0))
         // Transition from defined spectral selection to undefined
         bottom = top = SelectedRegion::UndefinedFrequency;
      else if (mCenter < 0 && mWidth < 0)
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
   }
   else {
      bottom = mLowCtrl->GetValue();
      top = mHighCtrl->GetValue();

      if (bottom >= 0)
         bottom = std::min(nyq, bottom);
      else
         bottom = SelectedRegion::UndefinedFrequency;

      if (top >= 0)
         top = std::min(nyq, top);
      else
         top = SelectedRegion::UndefinedFrequency;
   }

   mLow = bottom;
   mHigh = top;
   SetBounds();

   // Notify project and track panel, which may change
   // the values again, and call back to us in SetFrequencies()
   mListener->SSBL_ModifySpectralSelection(bottom, top, done);
}

void SpectralSelectionBar::OnCtrl(wxCommandEvent & event)
{
   ModifySpectralSelection(event.GetInt() != 0);
}

void SpectralSelectionBar::OnChoice(wxCommandEvent &)
{
   mbCenterAndWidth = (0 == mChoice->GetSelection());
   gPrefs->Write(preferencePath, mbCenterAndWidth);
   gPrefs->Flush();

   mCenterCtrl->Show(mbCenterAndWidth);
   mWidthCtrl->Show(mbCenterAndWidth);
   mLowCtrl->Show(!mbCenterAndWidth);
   mHighCtrl->Show(!mbCenterAndWidth);

   ValuesToControls();
   GetSizer()->Layout();   // Required so that the layout does not mess up on Windows when changing the format.
   wxWindowBase::GetSizer()->SetMinSize(wxSize(0, mHeight));   // so that height of toolbar does not change
   wxWindowBase::GetSizer()->SetSizeHints(this);
   Updated();
}

void SpectralSelectionBar::OnUpdate(wxCommandEvent &evt)
{
   int index = evt.GetInt();
   wxWindow *w = FindFocus();
   bool centerFocus = (w && w == mCenterCtrl);
   bool widthFocus = (w && w == mWidthCtrl);
   bool lowFocus = (w && w == mLowCtrl);
   bool highFocus = (w && w == mHighCtrl);

   evt.Skip(false);

   // Save formats before recreating the controls so they resize properly
   wxEventType type = evt.GetEventType();
   if (type == EVT_FREQUENCYTEXTCTRL_UPDATED) {
      NumericTextCtrl *frequencyCtrl = (mbCenterAndWidth ? mCenterCtrl : mLowCtrl);
      wxString frequencyFormatName = frequencyCtrl->GetBuiltinName(index);
      mListener->SSBL_SetFrequencySelectionFormatName(frequencyFormatName);
   }
   else if (mbCenterAndWidth &&
            type == EVT_BANDWIDTHTEXTCTRL_UPDATED) {
      wxString bandwidthFormatName = mWidthCtrl->GetBuiltinName(index);
      mListener->SSBL_SetBandwidthSelectionFormatName(bandwidthFormatName);
   }

   // ToolBar::ReCreateButtons() will get rid of our sizers and controls
   // so reset pointers first.
   mCenterCtrl = mWidthCtrl = NULL;
   mLowCtrl = mHighCtrl = NULL;

   ToolBar::ReCreateButtons();
   ValuesToControls();


   if (centerFocus) {
      mCenterCtrl->SetFocus();
   }
   else if (widthFocus) {
      mWidthCtrl->SetFocus();
   }
   else if (lowFocus) {
      mLowCtrl->SetFocus();
   }
   else if (highFocus) {
      mHighCtrl->SetFocus();
   }

   Updated();
}

void SpectralSelectionBar::ValuesToControls()
{
   if (mbCenterAndWidth) {
      mCenterCtrl->SetValue(mCenter);
      mWidthCtrl->SetValue(mWidth);
   }
   else {
      SetBounds();
      mLowCtrl->SetValue(mLow);
      mHighCtrl->SetValue(mHigh);
   }
}

void SpectralSelectionBar::SetBounds()
{
   if (mHigh >= 0)
      mLowCtrl->SetMaxValue(mHigh);
   else
      mLowCtrl->ResetMaxValue();

   if (mLow >= 0)
      mHighCtrl->SetMinValue(mLow);
   else
      mHighCtrl->ResetMinValue();
}

void SpectralSelectionBar::SetFrequencies(double bottom, double top)
{
   mLow = bottom;
   mHigh = top;

   if (bottom > 0 && top >= bottom)
      mWidth = log(top / bottom), mCenter = sqrt(top * bottom);
   else
      mWidth = mCenter = -1.0;

   ValuesToControls();
}

void SpectralSelectionBar::SetFrequencySelectionFormatName(const wxString & formatName)
{
   NumericTextCtrl *frequencyCtrl = (mbCenterAndWidth ? mCenterCtrl : mLowCtrl);
   frequencyCtrl->SetFormatName(formatName);

   wxCommandEvent e(EVT_FREQUENCYTEXTCTRL_UPDATED);
   e.SetInt(frequencyCtrl->GetFormatIndex());
   OnUpdate(e);
}

void SpectralSelectionBar::SetBandwidthSelectionFormatName(const wxString & formatName)
{
   if (mbCenterAndWidth) {
      mWidthCtrl->SetFormatName(formatName);

      wxCommandEvent e(EVT_BANDWIDTHTEXTCTRL_UPDATED);
      e.SetInt(mWidthCtrl->GetFormatIndex());
      OnUpdate(e);
   }
}

#endif // #ifdef EXPERIMENTAL_SPECTRAL_EDITING
