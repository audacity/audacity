/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangeTempo.cpp

  Vaughan Johnson, 
  Dominic Mazzoni

*******************************************************************//**

\class EffectChangeTempo
\brief An EffectSoundTouch provides speeding up or 
  slowing down tempo without changing pitch.

*//****************************************************************//**

\class ChangeTempoDialog
\brief Dialog used with EffectChangeTempo

*//*******************************************************************/

#include "../Audacity.h" // for USE_SOUNDTOUCH

#if USE_SOUNDTOUCH

#include "ChangeTempo.h"

#include "../ShuttleGui.h"
#include "TimeWarper.h"

#include <math.h>

#include <wx/button.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/valtext.h>


//
// EffectChangeTempo
//

EffectChangeTempo::EffectChangeTempo()
{
   m_PercentChange = 0.0;
   m_FromBPM = 0; // indicates not yet set
   m_ToBPM = 0; // indicates not yet set
   m_FromLength = 0.0;
   m_ToLength = 0.0;
}

double EffectChangeTempo::CalcPreviewInputLength(double previewLength)
{
   return previewLength * (100.0 + m_PercentChange) / 100.0;
}

wxString EffectChangeTempo::GetEffectDescription() { 
   // Note: This is useful only after change amount has been set.
   return wxString::Format(_("Applied effect: %s %.1f%%"),
                           this->GetEffectName().c_str(),
                           m_PercentChange);
} 

bool EffectChangeTempo::Init()
{
   // The selection might have changed since the last time EffectChangeTempo
   // was invoked, so recalculate the Length parameters.
   m_FromLength = mT1 - mT0;
   m_ToLength = (m_FromLength * 100.0) / (100.0 + m_PercentChange);

   mSoundTouch = NULL;

   return true;
}

bool EffectChangeTempo::PromptUser()
{
   ChangeTempoDialog dlog(this, mParent);
   dlog.m_PercentChange = m_PercentChange;
   dlog.m_FromBPM = m_FromBPM;
   dlog.m_ToBPM = m_ToBPM;
   dlog.m_FromLength = m_FromLength;
   dlog.m_ToLength = m_ToLength;
   // Don't need to call TransferDataToWindow, although other
   //	Audacity dialogs (from which I derived this one) do it, because
   //	ShowModal calls stuff that eventually calls wxWindowBase::OnInitDialog,
   //	which calls dlog.TransferDataToWindow();
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (dlog.GetReturnCode() == wxID_CANCEL)
      return false;

   m_PercentChange = dlog.m_PercentChange;
   m_FromBPM = dlog.m_FromBPM;
   m_ToBPM = dlog.m_ToBPM;
   m_ToLength = dlog.m_ToLength;
   return true;
}

bool EffectChangeTempo::TransferParameters( Shuttle & shuttle )
{  
   shuttle.TransferDouble(wxT("Percentage"),m_PercentChange,0.0);
   return true;
}

bool EffectChangeTempo::Process()
{
   mSoundTouch = new SoundTouch();
   mSoundTouch->setTempoChange(m_PercentChange);
   double mT1Dashed = mT0 + (mT1 - mT0)/(m_PercentChange/100.0 + 1.0);
   SetTimeWarper(new RegionTimeWarper(mT0, mT1,
            new LinearTimeWarper(mT0, mT0, mT1, mT1Dashed )));
   bool success = this->EffectSoundTouch::Process();
   if( success )
      mT1 = mT0 + (mT1 - mT0)/(m_PercentChange/100 + 1.);
   return success;
}

//----------------------------------------------------------------------------
// ChangeTempoDialog
//----------------------------------------------------------------------------

#define PERCENTCHANGE_MIN -99
#define PERCENTCHANGE_MAX 100 // warped above zero to actually go up to 400%
#define PERCENTCHANGE_SLIDER_WARP 1.30105 // warp power takes max from 100 to 400.

#define ID_TEXT_PERCENTCHANGE 10001
#define ID_SLIDER_PERCENTCHANGE 10002
#define ID_TEXT_FROMBPM 10003
#define ID_TEXT_TOBPM 10004
#define ID_TEXT_FROMLENGTH 10005
#define ID_TEXT_TOLENGTH 10006

// event table for ChangeTempoDialog

BEGIN_EVENT_TABLE(ChangeTempoDialog, EffectDialog)
    EVT_TEXT(ID_TEXT_PERCENTCHANGE, ChangeTempoDialog::OnText_PercentChange)
    EVT_SLIDER(ID_SLIDER_PERCENTCHANGE, ChangeTempoDialog::OnSlider_PercentChange)
    EVT_TEXT(ID_TEXT_FROMBPM, ChangeTempoDialog::OnText_FromBPM)
    EVT_TEXT(ID_TEXT_TOBPM, ChangeTempoDialog::OnText_ToBPM)
    EVT_TEXT(ID_TEXT_TOLENGTH, ChangeTempoDialog::OnText_ToLength)

    EVT_BUTTON(ID_EFFECT_PREVIEW, ChangeTempoDialog::OnPreview)
END_EVENT_TABLE()

ChangeTempoDialog::ChangeTempoDialog(EffectChangeTempo *effect, wxWindow *parent)
:  EffectDialog(parent, _("Change Tempo"), PROCESS_EFFECT),
   mEffect(effect)
{
   m_bLoopDetect = false;

   // NULL out these control members because there are some cases where the
   // event table handlers get called during this method, and those handlers that
   // can cause trouble check for NULL.
   m_pTextCtrl_PercentChange = NULL;
   m_pSlider_PercentChange = NULL;
   m_pTextCtrl_FromBPM = NULL;
   m_pTextCtrl_ToBPM = NULL;
   m_pTextCtrl_FromLength = NULL;
   m_pTextCtrl_ToLength = NULL;

   // effect parameters
   m_PercentChange = 0.0;
   m_FromBPM = 0; // indicates not yet set
   m_ToBPM = 0; // indicates not yet set
   m_FromLength = 0.0;
   m_ToLength = 0.0;

   Init();
}

void ChangeTempoDialog::PopulateOrExchange(ShuttleGui & S)
{
   wxTextValidator nullvld(wxFILTER_INCLUDE_CHAR_LIST);
   wxTextValidator numvld(wxFILTER_NUMERIC);

   S.SetBorder(10);
   S.StartHorizontalLay(wxCENTER, false);
   {
      S.AddTitle(_("Change Tempo without Changing Pitch") +
                 wxString(wxT("\n\n")) +
                 _("by Vaughan Johnson && Dominic Mazzoni") +
                 wxString(wxT("\n")) + 
                 _("using SoundTouch, by Olli Parviainen"));
   }
   S.EndHorizontalLay();
   S.SetBorder(5);

   //
   S.StartMultiColumn(2, wxCENTER);
   {
      m_pTextCtrl_PercentChange = S.Id(ID_TEXT_PERCENTCHANGE)
         .AddTextBox(_("Percent Change:"), wxT(""), 12);
      m_pTextCtrl_PercentChange->SetValidator(numvld);
   }
   S.EndMultiColumn();

   //
   S.StartHorizontalLay(wxEXPAND);
   {
      S.SetStyle(wxSL_HORIZONTAL);
      m_pSlider_PercentChange = S.Id(ID_SLIDER_PERCENTCHANGE)
         .AddSlider(wxT(""), 0, (int)PERCENTCHANGE_MAX, (int)PERCENTCHANGE_MIN);
      m_pSlider_PercentChange->SetName(_("Percent Change"));
   }
   S.EndHorizontalLay();

   // 
   S.StartMultiColumn(5, wxCENTER);
   {
      //
      S.AddUnits(_("Beats per minute:"));

      m_pTextCtrl_FromBPM = S.Id(ID_TEXT_FROMBPM)
         .AddTextBox(_("from"), wxT(""), 12);
      m_pTextCtrl_FromBPM->SetName(_("From beats per minute"));
      m_pTextCtrl_FromBPM->SetValidator(numvld);

      m_pTextCtrl_ToBPM = S.Id(ID_TEXT_TOBPM)
         .AddTextBox(_("to"), wxT(""), 12);
      m_pTextCtrl_ToBPM->SetName(_("To beats per minute"));
      m_pTextCtrl_ToBPM->SetValidator(numvld);

      //
      S.AddUnits(_("Length (seconds):"));

      m_pTextCtrl_FromLength = S.Id(ID_TEXT_FROMLENGTH)
         .AddTextBox(_("from"), wxT(""), 12);
      m_pTextCtrl_FromLength->SetName(_("From length in seconds"));
      m_pTextCtrl_FromLength->SetValidator(nullvld);

      m_pTextCtrl_ToLength = S.Id(ID_TEXT_TOLENGTH)
         .AddTextBox(_("to"), wxT(""), 12);
      m_pTextCtrl_ToLength->SetName(_("To length in seconds"));
      m_pTextCtrl_ToLength->SetValidator(numvld);
   }
   S.EndMultiColumn();

   return;
}

bool ChangeTempoDialog::TransferDataToWindow()
{
   m_bLoopDetect = true;

   // percent change controls
   this->Update_Text_PercentChange();
   this->Update_Slider_PercentChange();

   // from/to BPM controls
   wxString str;
   if (m_pTextCtrl_FromBPM) {
      if (m_FromBPM != 0)
         str.Printf(wxT("%d"), m_FromBPM);
      else
         str = wxT("");
      m_pTextCtrl_FromBPM->SetValue(str);
   }
   if (m_pTextCtrl_ToBPM) {
      if (m_ToBPM != 0)
         str.Printf(wxT("%d"), m_ToBPM);
      else
         str = wxT("");
      m_pTextCtrl_ToBPM->SetValue(str);
   }

   // from/to Length controls
   if (m_pTextCtrl_FromLength) {
      str.Printf(wxT("%.2f"), m_FromLength);
      m_pTextCtrl_FromLength->SetValue(str);
      m_pTextCtrl_FromLength->Enable(false); // Disable because the value comes from the user selection.
   }
   if (m_pTextCtrl_ToLength) {
      str.Printf(wxT("%.2f"), m_ToLength);
      m_pTextCtrl_ToLength->SetValue(str);
   }

   m_bLoopDetect = false;

   return true;
}

bool ChangeTempoDialog::TransferDataFromWindow()
{
   wxString str;

   // percent change controls
   if (m_pTextCtrl_PercentChange) {
      str = m_pTextCtrl_PercentChange->GetValue();
      double newValue = 0;
      str.ToDouble(&newValue);
      m_PercentChange = newValue;
   }

   // Ignore Slider_PercentChange because TextCtrl_PercentChange
   // always tracks it & is more precise (decimal points).

   // from/to BPM controls
   long newLong;
   if (m_pTextCtrl_FromBPM) {
      str = m_pTextCtrl_FromBPM->GetValue();
      str.ToLong(&newLong);
      m_FromBPM = (unsigned int)(newLong);
   }
   if (m_pTextCtrl_ToBPM) {
      str = m_pTextCtrl_ToBPM->GetValue();
      str.ToLong(&newLong);
      m_ToBPM = (unsigned int)(newLong);
   }

   // from/to Length controls
   // Don't do m_pTextCtrl_ToLength. It's disabled.
   if (m_pTextCtrl_ToLength) {
      str = m_pTextCtrl_ToLength->GetValue();
      str.ToLong(&newLong);
      m_ToLength = (int)(newLong);
   }

   return true;
}

// handler implementations for ChangeTempoDialog

void ChangeTempoDialog::OnText_PercentChange(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   if (m_pTextCtrl_PercentChange) {
      wxString str = m_pTextCtrl_PercentChange->GetValue();
      double newValue = 0;
      str.ToDouble(&newValue);
      m_PercentChange = newValue;

      m_bLoopDetect = true;
      this->Update_Slider_PercentChange();
      this->Update_Text_ToBPM();
      this->Update_Text_ToLength();
      m_bLoopDetect = false;

      FindWindow(wxID_OK)->Enable(m_PercentChange > -100.0);
   }
}

void ChangeTempoDialog::OnSlider_PercentChange(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   if (m_pSlider_PercentChange) {
      m_PercentChange = (double)(m_pSlider_PercentChange->GetValue());
      // Warp positive values to actually go up faster & further than negatives.
      if (m_PercentChange > 0.0)
         m_PercentChange = pow(m_PercentChange, PERCENTCHANGE_SLIDER_WARP);

      m_bLoopDetect = true;
      this->Update_Text_PercentChange();
      this->Update_Text_ToBPM();
      this->Update_Text_ToLength();
      m_bLoopDetect = false;
   }
}

void ChangeTempoDialog::OnText_FromBPM(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   if (m_pTextCtrl_FromBPM) {
      wxString str = m_pTextCtrl_FromBPM->GetValue();
      long newValue;
      str.ToLong(&newValue);
      m_FromBPM = (unsigned int)(newValue);

      m_bLoopDetect = true;

      this->Update_Text_ToBPM();

      m_bLoopDetect = false;
   }
}

void ChangeTempoDialog::OnText_ToBPM(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   if (m_pTextCtrl_ToBPM) {
      wxString str = m_pTextCtrl_ToBPM->GetValue();
      long newValue;
      str.ToLong(&newValue);
      m_ToBPM = (unsigned int)(newValue);

      m_bLoopDetect = true;

      // If FromBPM has already been set, then there's a new percent change.
      if (m_FromBPM != 0) {
         m_PercentChange = (((double)(m_ToBPM) * 100.0) / (double)(m_FromBPM)) - 100.0;

         this->Update_Text_PercentChange();
         this->Update_Slider_PercentChange();

         this->Update_Text_ToLength();
      }
      
      m_bLoopDetect = false;
   }
}

void ChangeTempoDialog::OnText_ToLength(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   if (m_pTextCtrl_ToLength) {
      wxString str = m_pTextCtrl_ToLength->GetValue();
      double newValue = 0;
      str.ToDouble(&newValue);
      m_ToLength = newValue;

      m_PercentChange = ((m_FromLength * 100.0) / m_ToLength) - 100.0;

      m_bLoopDetect = true;

      this->Update_Text_PercentChange();
      this->Update_Slider_PercentChange();

      this->Update_Text_ToBPM();
      
      m_bLoopDetect = false;
   }
}

void ChangeTempoDialog::OnPreview(wxCommandEvent &event)
{
   TransferDataFromWindow();

   // Save & restore parameters around Preview, because we didn't do OK.
   double oldPercentChange = mEffect->m_PercentChange;
   if( m_PercentChange < -99.0)
   {
      m_PercentChange = -99.0;
      this->Update_Text_PercentChange();
   }
   mEffect->m_PercentChange = m_PercentChange;
   mEffect->Preview();
   mEffect->m_PercentChange = oldPercentChange;
}

// helper fns

void ChangeTempoDialog::Update_Text_PercentChange()
{
   if (m_pTextCtrl_PercentChange) {
      wxString str;
      str.Printf(wxT("%.3f"), m_PercentChange);
      m_pTextCtrl_PercentChange->SetValue(str);
      FindWindow(wxID_OK)->Enable(m_PercentChange > -100.0);
   }
}

void ChangeTempoDialog::Update_Slider_PercentChange()
{
   if (m_pSlider_PercentChange) {
      double unwarped = m_PercentChange;
      if (unwarped > 0.0)
         // Un-warp values above zero to actually go up to PERCENTCHANGE_MAX.
         unwarped = pow(m_PercentChange, (1.0 / PERCENTCHANGE_SLIDER_WARP));

      // Add 0.5 to unwarped so trunc -> round.
      m_pSlider_PercentChange->SetValue((int)(unwarped + 0.5));
   }
}

void ChangeTempoDialog::Update_Text_ToBPM() 
// Use m_FromBPM & m_PercentChange to set new m_ToBPM & control.
{
   // Update ToBPM iff FromBPM has been set.
   if (m_FromBPM == 0)
      return;

   m_ToBPM = (unsigned int)((((double)(m_FromBPM) *
                              (100.0 + m_PercentChange)) / 100.0) +
                            0.5); // Add 0.5 so trunc -> round.
   if (m_pTextCtrl_ToBPM) {
      wxString str;
      str.Printf(wxT("%d"), m_ToBPM);
      m_pTextCtrl_ToBPM->SetValue(str);
   }
}

void ChangeTempoDialog::Update_Text_ToLength() 
// Use m_FromLength & m_PercentChange to set new m_ToLength & control.
{
   m_ToLength = (m_FromLength * 100.0) / (100.0 + m_PercentChange);
   if (m_pTextCtrl_ToLength) {
      wxString str;
      str.Printf(wxT("%.2f"), m_ToLength);
      m_pTextCtrl_ToLength->SetValue(str);
   }
}

#endif // USE_SOUNDTOUCH

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 1a049a55-47b1-44d3-ba79-bb925f8e7b93

