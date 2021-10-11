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



#include "SpectralSelectionBar.h"
#include "SpectralSelectionBarListener.h"

#include "ToolManager.h"

#include <algorithm>

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#include <wx/setup.h> // for wxUSE_* macros

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

#include "Prefs.h"
#include "Project.h"
#include "../AllThemeResources.h"
#include "SelectedRegion.h"
#include "ViewInfo.h"

#if wxUSE_ACCESSIBILITY
#include "../widgets/WindowAccessible.h"
#endif

#include "../widgets/NumericTextCtrl.h"

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
   EVT_IDLE( SpectralSelectionBar::OnIdle )
END_EVENT_TABLE()

static const wxString preferencePath
(wxT("/GUI/Toolbars/SpectralSelection/CenterAndWidthChoice"));

SpectralSelectionBar::SpectralSelectionBar( AudacityProject &project )
: ToolBar( project,
   SpectralSelectionBarID, XO("Spectral Selection"), wxT("SpectralSelection") )
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

SpectralSelectionBar &SpectralSelectionBar::Get( AudacityProject &project )
{
   auto &toolManager = ToolManager::Get( project );
   return *static_cast<SpectralSelectionBar*>(
      toolManager.GetToolBar(SpectralSelectionBarID) );
}

const SpectralSelectionBar &SpectralSelectionBar::Get( const AudacityProject &project )
{
   return Get( const_cast<AudacityProject&>( project )) ;
}

void SpectralSelectionBar::Create(wxWindow * parent)
{
   ToolBar::Create(parent);
   UpdatePrefs();
   mHeight = wxWindowBase::GetSizer()->GetSize().GetHeight();
}

void SpectralSelectionBar::Populate()
{
   SetBackgroundColour( theTheme.Colour( clrMedium  ) );
   gPrefs->Read(preferencePath, &mbCenterAndWidth, true);

   auto frequencyFormatName = mListener
      ? mListener->SSBL_GetFrequencySelectionFormatName()
      : NumericFormatSymbol{};
   auto bandwidthFormatName = mListener
      ? mListener->SSBL_GetBandwidthSelectionFormatName()
      : NumericFormatSymbol{};

   wxFlexGridSizer *mainSizer = safenew wxFlexGridSizer(1, 1, 1);
   Add(mainSizer, 0, wxALIGN_CENTER_VERTICAL | wxLEFT, 5);

   //
   // Top row, choice box
   //

   const wxString choices[2] = {
      _("Center frequency and Width"),
      _("Low and High Frequencies"),
   };
   mChoice = safenew wxChoice
      (this, OnChoiceID, wxDefaultPosition, wxDefaultSize, 2, choices,
       0, wxDefaultValidator, _("Show"));
   mChoice->SetSelection(mbCenterAndWidth ? 0 : 1);
#if wxUSE_ACCESSIBILITY
   // so that name can be set on a standard control
   mChoice->SetAccessible(safenew WindowAccessible(mChoice));
#endif
   mChoice->SetMinSize(wxSize(mChoice->GetBestSize().x, toolbarSingle));

   mainSizer->Add(mChoice, 0, wxEXPAND | wxALIGN_TOP | wxRIGHT, 6);

   //
   // Bottom row, split into two columns, each with one control
   //

   {
      auto subSizer = std::make_unique<wxBoxSizer>(wxHORIZONTAL);

      mCenterCtrl = safenew NumericTextCtrl(
         this, OnCenterID,
         NumericConverter::FREQUENCY, frequencyFormatName, 0.0, 44100.0,
         NumericTextCtrl::Options{}
            .InvalidValue( true, SelectedRegion::UndefinedFrequency )
      );
      mCenterCtrl->SetName( XO("Center Frequency") );
      subSizer->Add(mCenterCtrl, 0, wxALIGN_CENTER_VERTICAL | wxRIGHT, 5);

      mWidthCtrl = safenew NumericTextCtrl(
         this, OnWidthID,
         NumericConverter::BANDWIDTH, bandwidthFormatName, 0.0, 44100.0,
         NumericTextCtrl::Options{}
            .InvalidValue( true, -1.0 )
      );
      mWidthCtrl->SetName( XO("Bandwidth") );
      subSizer->Add(mWidthCtrl, 0, wxALIGN_CENTER_VERTICAL | wxRIGHT, 5);

      mLowCtrl = safenew NumericTextCtrl(
         this, OnLowID,
         NumericConverter::FREQUENCY, frequencyFormatName, 0.0, 44100.0,
         NumericTextCtrl::Options{}
            .InvalidValue( true, SelectedRegion::UndefinedFrequency )
      );
      mLowCtrl->SetName( XO("Low Frequency") );
      subSizer->Add(mLowCtrl, 0, wxALIGN_CENTER_VERTICAL | wxRIGHT, 5);

      mHighCtrl = safenew NumericTextCtrl(
         this, OnHighID,
         NumericConverter::FREQUENCY, frequencyFormatName, 0.0, 44100.0,
         NumericTextCtrl::Options{}
            .InvalidValue( true, SelectedRegion::UndefinedFrequency )
      );
      mHighCtrl->SetName( XO("High Frequency") );
      subSizer->Add(mHighCtrl, 0, wxALIGN_CENTER_VERTICAL | wxRIGHT, 5);

      mCenterCtrl->Show(mbCenterAndWidth);
      mWidthCtrl->Show(mbCenterAndWidth);
      mLowCtrl->Show(!mbCenterAndWidth);
      mHighCtrl->Show(!mbCenterAndWidth);

      mainSizer->Add(subSizer.release(), 0, wxALIGN_TOP, 0);
   }

   mainSizer->Layout();

   Layout();
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
   SetLabel(XO("Spectral Selection"));

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
      // These have to be in the right order.
      if( bottom > top ){
         // Oops.  We must fix the order.
         if( mLowCtrl->HasFocus() )
            top = bottom;
         else
            bottom = top;
      }
   }


   mLow = bottom;
   mHigh = top;
   //SetBounds();

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

   wxSize sz = GetMinSize();
   sz.SetWidth(wxDefaultCoord);
   SetMinSize(sz);
   Layout();
   Fit();
   Updated();
}

void SpectralSelectionBar::OnIdle( wxIdleEvent &evt )
{
   evt.Skip();
   auto &project = mProject;
   const auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;
   SetFrequencies( selectedRegion.f0(), selectedRegion.f1() );
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
      auto frequencyFormatName = frequencyCtrl->GetBuiltinName(index);
      if (mListener)
         mListener->SSBL_SetFrequencySelectionFormatName(frequencyFormatName);
   }
   else if (mbCenterAndWidth &&
            type == EVT_BANDWIDTHTEXTCTRL_UPDATED) {
      auto bandwidthFormatName = mWidthCtrl->GetBuiltinName(index);
      if (mListener)
         mListener->SSBL_SetBandwidthSelectionFormatName(bandwidthFormatName);
   }

   // ReCreateButtons() will get rid of our sizers and controls
   // so reset pointers first.
   mCenterCtrl = mWidthCtrl = NULL;
   mLowCtrl = mHighCtrl = NULL;

   ReCreateButtons();
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
      //Bug 1633
      //The controls may not be able to show mHigh, e.g.
      //if set to Hz, and in that case we should either show invalid
      //or 'do the best we can' and truncate.  
      //If we set bounds we instead clip to the mHigh to mLow, 
      //So no SetBounds, for now.
      //SetBounds();
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
   if ( mLow != bottom || mHigh != top ) {
      mLow = bottom;
      mHigh = top;

      if (bottom > 0 && top >= bottom)
         mWidth = log(top / bottom), mCenter = sqrt(top * bottom);
      else
         mWidth = mCenter = -1.0;

      ValuesToControls();
   }
}

void SpectralSelectionBar::SetFrequencySelectionFormatName(const NumericFormatSymbol & formatName)
{
   NumericTextCtrl *frequencyCtrl = (mbCenterAndWidth ? mCenterCtrl : mLowCtrl);
   bool changed =
      frequencyCtrl->SetFormatName(formatName);
   // Test first whether changed, to avoid infinite recursion from OnUpdate
   if (changed) {
      wxCommandEvent e(EVT_FREQUENCYTEXTCTRL_UPDATED);
      e.SetInt(frequencyCtrl->GetFormatIndex());
      OnUpdate(e);
   }
}

void SpectralSelectionBar::SetBandwidthSelectionFormatName(const NumericFormatSymbol & formatName)
{
   if (mbCenterAndWidth) {
      bool changed =
         mWidthCtrl->SetFormatName(formatName);
      // Test first whether changed, to avoid infinite recursion from OnUpdate
      if (changed) {
         wxCommandEvent e(EVT_BANDWIDTHTEXTCTRL_UPDATED);
         e.SetInt(mWidthCtrl->GetFormatIndex());
         OnUpdate(e);
      }
   }
}

static RegisteredToolbarFactory factory{ SpectralSelectionBarID,
   []( AudacityProject &project ){
      return ToolBar::Holder{ safenew SpectralSelectionBar{ project } }; }
};

namespace {
AttachedToolBarMenuItem sAttachment{
   SpectralSelectionBarID,
      /* i18n-hint: Clicking this menu item shows the toolbar
      for selecting a frequency range of audio */
   wxT("ShowSpectralSelectionTB"), XXO("Spe&ctral Selection Toolbar")
};
}

#endif // #ifdef EXPERIMENTAL_SPECTRAL_EDITING
