/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangeLength.cpp

  Lynn Allan (from DM's Normalize)
  Note: Only works on complete mono track for now

*******************************************************************//**

\class EffectChangeLength
\brief An Effect.

*//****************************************************************//**

\class ChangeLengthDialog
\brief Dialog used with EffectChangeLength

*//*******************************************************************/


#include <wx/wx.h>
#include <math.h>

#include "../Prefs.h"
#include "../Project.h"
#include "ChangeLength.h"

EffectChangeLength::EffectChangeLength()
{
   Init();
}

bool EffectChangeLength::Init()
{
   mFromLength = mT1 - mT0;;	
   //bool flag = gPrefs->Read(wxT("/CsPresets/ChangeLengthTo"), &mToLength, mFromLength);
   //TODO: Presets ONLY if enabled in preferences. (and remove Cs prefix)
   gPrefs->Read(wxT("/CsPresets/ChangeLengthTo"), &mToLength, mFromLength);

   return true;
}

bool EffectChangeLength::CheckWhetherSkipEffect()
{
   bool rc = true;
   return rc;
}

void EffectChangeLength::End()
{
}

bool EffectChangeLength::PromptUser()
{
   ChangeLengthDialog dlog(mParent, -1, wxT("Change Length"));
   dlog.mToLength = mToLength;
   dlog.mFromLength = mFromLength;
   dlog.TransferDataToWindow();

   dlog.CentreOnParent();
   dlog.ShowModal();

   if (!dlog.GetReturnCode()) {
      return false;
   }

   mToLength = dlog.mToLength;
   gPrefs->Write(wxT("/CsPresets/ChangeLengthTo"), mToLength);
   
   return true;
}
bool EffectChangeLength::TransferParameters( Shuttle & shuttle )
{  
// shuttle.TransferInt(wxT(""),,0);
   return true;
}

bool EffectChangeLength::Process()
{
   TrackListOfKindIterator iter(Track::Wave, mTracks);
   mTrack = (WaveTrack *) iter.First();

   return true;
}

//----------------------------------------------------------------------------
// ChangeLengthDialog
//----------------------------------------------------------------------------

#define ID_CHANGE_LENGTH_FROM_TEXT   7000
#define ID_CHANGE_LENGTH_TO_TEXT 7001
#define ID_PERCENT_CHANGE_TEXT 7002
#define ID_TO_RANGE_TEXT 7003
#define ID_BUTTON_RECALCULATE 7004

BEGIN_EVENT_TABLE(ChangeLengthDialog,wxDialog)
   EVT_BUTTON( wxID_OK, ChangeLengthDialog::OnOk )
   EVT_BUTTON( wxID_CANCEL, ChangeLengthDialog::OnCancel )
   EVT_TEXT(ID_CHANGE_LENGTH_TO_TEXT, ChangeLengthDialog::OnText_ToLength)
	EVT_BUTTON(ID_BUTTON_RECALCULATE, ChangeLengthDialog::OnRecalculate)
END_EVENT_TABLE()

ChangeLengthDialog::ChangeLengthDialog(wxWindow *parent, wxWindowID id, const wxString &title) :
   wxDialog( parent, id, title )
{
//   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer * pBoxSizer_Dialog = new wxBoxSizer(wxVERTICAL);
   wxStaticText *statText = new wxStaticText(this, -1,
                            wxT("Change Length by Lynn Allan\n"
                             "Make shorter or longer by up to +/- 10%\n"
                             "to fit certain number of minutes\n"));
   pBoxSizer_Dialog->Add(statText, 0, wxALIGN_CENTRE | wxALL, 5);
   pBoxSizer_Dialog->Add(0, 4, 0); // spacer

   wxBoxSizer * pBoxSizer_ToLength = new wxBoxSizer(wxHORIZONTAL);
   statText = new wxStaticText(this, -1, wxT("Desired Length (minutes): "),
									       wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_ToLength->Add(statText, 0, 
									wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);
   m_pTextCtrl_ToLength =
       new wxTextCtrl(this, ID_CHANGE_LENGTH_TO_TEXT, wxT("0.0"), 
								wxDefaultPosition, wxSize(48, -1), 0,
								wxTextValidator(wxFILTER_NUMERIC));
   pBoxSizer_ToLength->Add(m_pTextCtrl_ToLength, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 4);
   pBoxSizer_Dialog->Add(pBoxSizer_ToLength, 0, wxALIGN_CENTER | wxALL, 4);

   wxStaticBoxSizer *infoGroup = new wxStaticBoxSizer(new wxStaticBox(this, -1, wxT("Information")), wxVERTICAL);

   wxBoxSizer * pBoxSizer_FromLength = new wxBoxSizer(wxHORIZONTAL);
   statText = new wxStaticText(this, -1, wxT("Current Length (minutes): "),
									       wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_FromLength->Add(statText, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);
   m_pTextCtrl_FromLength = 
		new wxTextCtrl(this, ID_CHANGE_LENGTH_FROM_TEXT, wxT("0.0"), 
							wxDefaultPosition, wxSize(48, -1), 
							wxTE_READONLY); // Read only because it's from the selection.
							// No validator because it's read only.
   pBoxSizer_FromLength->Add(m_pTextCtrl_FromLength, 0, 
									wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 4);

   infoGroup->Add(pBoxSizer_FromLength, 0, wxALIGN_CENTER | wxALL, 4);
   infoGroup->Add(0, 4, 0); // spacer

   wxBoxSizer * pBoxSizer_Range = new wxBoxSizer(wxHORIZONTAL);
   statText = new wxStaticText(this, -1, wxT("Allowed Range: "),
									       wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Range->Add(statText, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);
   m_pTextCtrl_ToRange = 
		new wxTextCtrl(this, ID_TO_RANGE_TEXT, wxT("90% to 110%"), 
							wxDefaultPosition, wxSize(90, -1), 
							wxTE_READONLY); // Read only because it's from the selection.
							// No validator because it's read only.
   pBoxSizer_Range->Add(m_pTextCtrl_ToRange, 0, 
									wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 4);
   infoGroup->Add(pBoxSizer_Range, 0, wxALIGN_CENTER | wxALL, 4);
   infoGroup->Add(0, 4, 0); // spacer

	// Group percent controls with spacers, 
	// rather than static box, so they don't look isolated.
   pBoxSizer_Dialog->Add(0, 4, 0); // spacer

   wxBoxSizer * pBoxSizer_PercentChange = new wxBoxSizer(wxHORIZONTAL);

   statText = new wxStaticText(this, -1, wxT("Percent Change:"),
												wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_PercentChange->Add(statText, 0, 
											wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);
   m_pTextCtrl_PercentChange = 
		new wxTextCtrl(this, ID_PERCENT_CHANGE_TEXT, wxT("0.0"), 
							wxDefaultPosition, wxSize(40, -1), wxTE_READONLY);
   pBoxSizer_PercentChange->Add(m_pTextCtrl_PercentChange, 0, 
											wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 4);
   infoGroup->Add(pBoxSizer_PercentChange, 0, wxALIGN_CENTER | wxALL, 4);
   pBoxSizer_Dialog->Add(infoGroup, 0, wxALIGN_CENTER | wxALL, 4);

   wxBoxSizer * pBoxSizer_Controls = new wxBoxSizer(wxHORIZONTAL);
   wxButton *recalculate = new wxButton(this, ID_BUTTON_RECALCULATE, wxT("Recalculate"));
   pBoxSizer_Controls->Add(recalculate, 0, wxALIGN_CENTRE|wxALL, 5);

   wxButton *cancel = new wxButton(this, wxID_CANCEL, _("&Cancel"));
   pBoxSizer_Controls->Add(cancel, 0, wxALIGN_CENTRE|wxALL, 5);

   wxButton *ok = new wxButton(this, wxID_OK, _("&OK"));
   ok->SetDefault();
   pBoxSizer_Controls->Add(ok, 0, wxALIGN_CENTRE|wxALL, 5);

   pBoxSizer_Dialog->Add(pBoxSizer_Controls, 0, wxALIGN_CENTRE|wxALIGN_CENTER_VERTICAL|wxALL, 5);

   SetAutoLayout(true);
   SetSizer(pBoxSizer_Dialog);
   pBoxSizer_Dialog->Fit(this);
   pBoxSizer_Dialog->SetSizeHints(this);
}

bool ChangeLengthDialog::TransferDataToWindow()
{
   m_pTextCtrl_FromLength->SetValue(wxString::Format(wxT("%.1f"), (mFromLength / 60.0)));

   double percentChange = ((mToLength - mFromLength) / mFromLength) * 100.0;
   if (percentChange > 10.0) {
      mToLength = mFromLength * 1.10;
      percentChange = 10.0;
   }
   if (percentChange < -10.0) {
      mToLength = mFromLength * 0.90;
      percentChange = -10.0;
   }
   m_pTextCtrl_ToRange->SetValue(wxString::Format(wxT("%.1f to %.1f"), 
          ((mFromLength * 0.90) / 60.0), ((mFromLength * 1.10) / 60.0)));
   m_pTextCtrl_PercentChange->SetValue(wxString::Format(wxT("%.1f"), percentChange));
   m_pTextCtrl_ToLength->SetValue(wxString::Format(wxT("%.1f"), (mToLength / 60.0)));
   return true;
}

bool ChangeLengthDialog::TransferDataFromWindow()
{
   double minutes;
   m_pTextCtrl_ToLength->GetValue().ToDouble(&minutes);
   mToLength = minutes * 60.0;
   return true;
}

void ChangeLengthDialog::OnText_ToLength(wxCommandEvent & event)
{
   return;
   if (m_pTextCtrl_ToLength) {
      wxString str = m_pTextCtrl_ToLength->GetValue();
      double newValue = 0.0;
      str.ToDouble(&newValue);
		mToLength = newValue;

		mPercentChange = ((mToLength - mFromLength) / mFromLength) * 100.0;
   }
}

void ChangeLengthDialog::OnRecalculate(wxCommandEvent &event)
{
   TransferDataFromWindow();
   TransferDataToWindow();
}

void ChangeLengthDialog::OnOk(wxCommandEvent &event)
{
   TransferDataFromWindow();

   EndModal(true);
}

void ChangeLengthDialog::OnCancel(wxCommandEvent &event)
{
   EndModal(false);
}
