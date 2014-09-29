/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeScale.cpp

  Clayton Otey

*******************************************************************//**

\class EffectTimeScale
\brief An EffectTimeScale does high quality sliding time scaling/pitch shifting

*//****************************************************************//**

\class TimeScaleDialog
\brief Dialog used with EffectTimeScale

*//*******************************************************************/

#include "../Audacity.h" // for USE_SBSMS

#if USE_SBSMS

#include "TimeScale.h"
#include "sbsms.h"

#include "../ShuttleGui.h"

#include <math.h>

#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/checkbox.h>
#include <wx/valtext.h>

//
// EffectTimeScale
//

EffectTimeScale::EffectTimeScale()
{
   m_RatePercentChangeStart = 0;
   m_RatePercentChangeEnd = 0;
   m_PitchHalfStepsStart = 0;
   m_PitchHalfStepsEnd = 0;
   m_PitchPercentChangeStart = 0;
   m_PitchPercentChangeEnd = 0;
   m_PreAnalyze = false;
}

wxString EffectTimeScale::GetEffectDescription() {
   // Note: This is useful only after change amount has been set.
   return wxString::Format(_("Applied effect: %s"), this->GetEffectName().c_str());
}

bool EffectTimeScale::Init()
{
   return true;
}

bool EffectTimeScale::PromptUser()
{
   TimeScaleDialog dlog(this, mParent);
   dlog.m_RatePercentChangeStart = m_RatePercentChangeStart;
   dlog.m_RatePercentChangeEnd = m_RatePercentChangeEnd;
   dlog.m_PitchHalfStepsStart = m_PitchHalfStepsStart;
   dlog.m_PitchHalfStepsEnd = m_PitchHalfStepsEnd;
   dlog.m_PitchPercentChangeStart = m_PitchPercentChangeStart;
   dlog.m_PitchPercentChangeEnd = m_PitchPercentChangeEnd;
   dlog.m_PreAnalyze = m_PreAnalyze;

   // Don't need to call TransferDataToWindow, although other
   // Audacity dialogs (from which I derived this one) do it, because
   // ShowModal calls stuff that eventually calls wxWindowBase::OnInitDialog,
   // which calls dlog.TransferDataToWindow();
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (dlog.GetReturnCode() == wxID_CANCEL)
      return false;

   m_RatePercentChangeStart = dlog.m_RatePercentChangeStart;
   m_RatePercentChangeEnd = dlog.m_RatePercentChangeEnd;
   m_PitchHalfStepsStart = dlog.m_PitchHalfStepsStart;
   m_PitchHalfStepsEnd = dlog.m_PitchHalfStepsEnd;
   m_PitchPercentChangeStart = dlog.m_PitchPercentChangeStart;
   m_PitchPercentChangeEnd = dlog.m_PitchPercentChangeEnd;
   m_PreAnalyze = dlog.m_PreAnalyze;

   return true;
}

bool EffectTimeScale::TransferParameters( Shuttle & shuttle )
{
   shuttle.TransferDouble(wxT("RatePercentChangeStart"),m_RatePercentChangeStart,0.0);
   shuttle.TransferDouble(wxT("RatePercentChangeEnd"),m_RatePercentChangeEnd,0.0);
   shuttle.TransferDouble(wxT("PitchHalfStepsStart"),m_PitchHalfStepsStart,0.0);
   shuttle.TransferDouble(wxT("PitchHalfStepsEnd"),m_PitchHalfStepsEnd,0.0);
   shuttle.TransferDouble(wxT("PitchPercentChangeStart"),m_PitchPercentChangeStart,0.0);
   shuttle.TransferDouble(wxT("PitchPercentChangeEnd"),m_PitchPercentChangeEnd,0.0);
   shuttle.TransferBool(wxT("PreAnalyze"),m_PreAnalyze,false);
   return true;
}

inline double PercentChangeToRatio(double percentChange)
{
   return 1.0 + percentChange / 100.0;
}

inline double HalfStepsToPercentChange(double halfSteps)
{
   return 100.0 * (pow(2.0,halfSteps/12.0) - 1.0);
}

inline double PercentChangeToHalfSteps(double percentChange)
{
   return 17.312340490667560888319096172023 * log(PercentChangeToRatio(percentChange));
}

bool EffectTimeScale::Process()
{
   double pitchStart = PercentChangeToRatio(m_PitchPercentChangeStart);
   double pitchEnd = PercentChangeToRatio(m_PitchPercentChangeEnd);
   double rateStart = PercentChangeToRatio(m_RatePercentChangeStart);
   double rateEnd = PercentChangeToRatio(m_RatePercentChangeEnd);
   this->EffectSBSMS::setParameters(rateStart,rateEnd,pitchStart,pitchEnd,SlideLinearOutputRate,SlideLinearOutputRate,false,false,false);
   return this->EffectSBSMS::Process();
}

//----------------------------------------------------------------------------
// TimeScaleDialog
//----------------------------------------------------------------------------

#define RATE_PERCENTCHANGE_MAX_SLIDER 150
#define RATE_PERCENTCHANGE_MIN_SLIDER -75
#define RATE_PERCENTCHANGE_MAX_TEXT 500
#define RATE_PERCENTCHANGE_MIN_TEXT -90
#define RATE_PERCENTCHANGE_DEFAULT 0
#define PITCH_HALFSTEPS_MIN_TEXT -12
#define PITCH_HALFSTEPS_MAX_TEXT 12
#define PITCH_PERCENTCHANGE_MIN_TEXT -50
#define PITCH_PERCENTCHANGE_MAX_TEXT 100


#define ID_TEXT_RATE_PERCENTCHANGE_START 10001
#define ID_TEXT_RATE_PERCENTCHANGE_END 10002
#define ID_TEXT_PITCH_HALFSTEPS_START 10003
#define ID_TEXT_PITCH_HALFSTEPS_END 10004
#define ID_TEXT_PITCH_PERCENTCHANGE_START 10005
#define ID_TEXT_PITCH_PERCENTCHANGE_END 10006
#define ID_SLIDER_RATE_PERCENTCHANGE_START 10007
#define ID_SLIDER_RATE_PERCENTCHANGE_END 10008
#define ID_CHECKBOX_PREANALYZE 10009

// event table for TimeScaleDialog

BEGIN_EVENT_TABLE(TimeScaleDialog, EffectDialog)
   EVT_TEXT(ID_TEXT_RATE_PERCENTCHANGE_START, TimeScaleDialog::OnText_RatePercentChangeStart)
   EVT_TEXT(ID_TEXT_RATE_PERCENTCHANGE_END, TimeScaleDialog::OnText_RatePercentChangeEnd)
   EVT_TEXT(ID_TEXT_PITCH_HALFSTEPS_START, TimeScaleDialog::OnText_PitchHalfStepsStart)
   EVT_TEXT(ID_TEXT_PITCH_HALFSTEPS_END, TimeScaleDialog::OnText_PitchHalfStepsEnd)
   EVT_TEXT(ID_TEXT_PITCH_PERCENTCHANGE_START, TimeScaleDialog::OnText_PitchPercentChangeStart)
   EVT_TEXT(ID_TEXT_PITCH_PERCENTCHANGE_END, TimeScaleDialog::OnText_PitchPercentChangeEnd)
   EVT_SLIDER(ID_SLIDER_RATE_PERCENTCHANGE_START, TimeScaleDialog::OnSlider_RatePercentChangeStart)
   EVT_SLIDER(ID_SLIDER_RATE_PERCENTCHANGE_END, TimeScaleDialog::OnSlider_RatePercentChangeEnd)
   EVT_CHECKBOX(ID_CHECKBOX_PREANALYZE, TimeScaleDialog::OnCheckBox_PreAnalyze)
END_EVENT_TABLE()

TimeScaleDialog::TimeScaleDialog(EffectTimeScale *effect, wxWindow *parent)
   :  EffectDialog(parent, _("Sliding Time Scale/Pitch Shift"), INSERT_EFFECT),
      mEffect(effect)
{
   m_bLoopDetect = false;

   // NULL out these control members because there are some cases where the
   // event table handlers get called during this method, and those handlers that
   // can cause trouble check for NULL.

   m_pTextCtrl_RatePercentChangeStart = NULL;
   m_pTextCtrl_RatePercentChangeEnd = NULL;
   m_pSlider_RatePercentChangeStart = NULL;
   m_pSlider_RatePercentChangeEnd = NULL;
   m_pTextCtrl_PitchPercentChangeStart = NULL;
   m_pTextCtrl_PitchPercentChangeEnd = NULL;
   m_pTextCtrl_PitchHalfStepsStart = NULL;
   m_pTextCtrl_PitchHalfStepsEnd = NULL;
   m_pCheckBox_PreAnalyze = NULL;

   // effect parameters
   m_RatePercentChangeStart = 0;
   m_RatePercentChangeEnd = 0;
   m_PitchPercentChangeStart = 0;
   m_PitchPercentChangeEnd = 0;
   m_PitchHalfStepsStart = 0;
   m_PitchHalfStepsEnd = 0;
   m_PreAnalyze = false;

   Init();
}

void TimeScaleDialog::PopulateOrExchange(ShuttleGui & S)
{

   wxTextValidator nullvld(wxFILTER_INCLUDE_CHAR_LIST);
   wxTextValidator numvld(wxFILTER_NUMERIC);

   S.SetBorder(5);
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, 0);
   // Rate Start
   S.StartStatic(_("Initial Tempo Change (%)"));
   {
      S.StartMultiColumn(1, wxCENTER);
      {
         m_pTextCtrl_RatePercentChangeStart = S.Id(ID_TEXT_RATE_PERCENTCHANGE_START)
            .AddTextBox(wxT(""), wxT(""), 12);
         m_pTextCtrl_RatePercentChangeStart->SetValidator(numvld);
      }
      S.EndMultiColumn();
      S.StartHorizontalLay(wxEXPAND,0);
      {
         S.SetStyle(wxSL_HORIZONTAL);
         m_pSlider_RatePercentChangeStart = S.Id(ID_SLIDER_RATE_PERCENTCHANGE_START)
            .AddSlider(wxT(""), (int)RATE_PERCENTCHANGE_DEFAULT, (int)RATE_PERCENTCHANGE_MAX_SLIDER, (int)RATE_PERCENTCHANGE_MIN_SLIDER);
      }
      S.EndHorizontalLay();
   }
   S.EndStatic();

   S.StartStatic(_("Final Tempo Change (%)"));
   {
      S.StartMultiColumn(1, wxCENTER);
      {
         m_pTextCtrl_RatePercentChangeEnd = S.Id(ID_TEXT_RATE_PERCENTCHANGE_END)
            .AddTextBox(wxT(""), wxT(""), 12);
         m_pTextCtrl_RatePercentChangeEnd->SetValidator(numvld);
      }
      S.EndMultiColumn();
      S.StartHorizontalLay(wxEXPAND,0);
      {
         S.SetStyle(wxSL_HORIZONTAL);
         m_pSlider_RatePercentChangeEnd = S.Id(ID_SLIDER_RATE_PERCENTCHANGE_END)
            .AddSlider(wxT(""), (int)RATE_PERCENTCHANGE_DEFAULT, (int)RATE_PERCENTCHANGE_MAX_SLIDER, (int)RATE_PERCENTCHANGE_MIN_SLIDER);
      }
      S.EndHorizontalLay();
   }
   S.EndStatic();

   // Pitch Start
   S.StartStatic(_("Initial Pitch Shift"));
   {
      S.StartMultiColumn(2, wxCENTER);
      {
         m_pTextCtrl_PitchHalfStepsStart = S.Id(ID_TEXT_PITCH_HALFSTEPS_START)
            .AddTextBox(_("(semitones) [-12 to 12]:"), wxT(""), 12);
         m_pTextCtrl_PitchHalfStepsStart->SetValidator(numvld);

         m_pTextCtrl_PitchPercentChangeStart = S.Id(ID_TEXT_PITCH_PERCENTCHANGE_START)
            .AddTextBox(_("(%) [-50 to 100]:"), wxT(""), 12);
         m_pTextCtrl_PitchPercentChangeStart->SetValidator(numvld);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   // Pitch End
   S.StartStatic(_("Final Pitch Shift"));
   {
      S.StartMultiColumn(2, wxCENTER);
      {
         m_pTextCtrl_PitchHalfStepsEnd = S.Id(ID_TEXT_PITCH_HALFSTEPS_END)
            .AddTextBox(_("(semitones) [-12 to 12]:"), wxT(""), 12);
         m_pTextCtrl_PitchHalfStepsEnd->SetValidator(numvld);

         m_pTextCtrl_PitchPercentChangeEnd = S.Id(ID_TEXT_PITCH_PERCENTCHANGE_END)
            .AddTextBox(_("(%) [-50 to 100]:"), wxT(""), 12);
         m_pTextCtrl_PitchPercentChangeEnd->SetValidator(numvld);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();
   S.EndMultiColumn();

   return;
}

bool TimeScaleDialog::TransferDataToWindow()
{
   m_bLoopDetect = true;

   this->Update_Text_RatePercentChangeStart();
   this->Update_Text_RatePercentChangeEnd();
   this->Update_Slider_RatePercentChangeStart();
   this->Update_Slider_RatePercentChangeEnd();
   this->Update_Text_PitchHalfStepsStart();
   this->Update_Text_PitchHalfStepsEnd();
   this->Update_Text_PitchPercentChangeStart();
   this->Update_Text_PitchPercentChangeEnd();
   this->Update_CheckBox_PreAnalyze();

   m_bLoopDetect = false;

   return true;
}

bool TimeScaleDialog::TransferDataFromWindow()
{
   wxString str;

   if (m_pTextCtrl_RatePercentChangeStart) {
      str = m_pTextCtrl_RatePercentChangeStart->GetValue();
      double newValue = 0;
      str.ToDouble(&newValue);
      m_RatePercentChangeStart = newValue;
   }

   if (m_pTextCtrl_RatePercentChangeEnd) {
      str = m_pTextCtrl_RatePercentChangeEnd->GetValue();
      double newValue = 0;
      str.ToDouble(&newValue);
      m_RatePercentChangeEnd = newValue;
   }

   if (m_pTextCtrl_PitchHalfStepsStart) {
      str = m_pTextCtrl_PitchHalfStepsStart->GetValue();
      double newValue = 0;
      str.ToDouble(&newValue);
      m_PitchHalfStepsStart = newValue;
   }

   if (m_pTextCtrl_PitchHalfStepsEnd) {
      str = m_pTextCtrl_PitchHalfStepsEnd->GetValue();
      double newValue = 0;
      str.ToDouble(&newValue);
      m_PitchHalfStepsEnd = newValue;
   }

   if (m_pTextCtrl_PitchPercentChangeStart) {
      str = m_pTextCtrl_PitchPercentChangeStart->GetValue();
      double newValue = 0;
      str.ToDouble(&newValue);
      m_PitchPercentChangeStart = newValue;
   }

   if (m_pTextCtrl_PitchPercentChangeEnd) {
      str = m_pTextCtrl_PitchPercentChangeEnd->GetValue();
      double newValue = 0;
      str.ToDouble(&newValue);
      m_PitchPercentChangeEnd = newValue;
   }

   if(m_pCheckBox_PreAnalyze) {
      m_PreAnalyze = m_pCheckBox_PreAnalyze->GetValue();
   }

   return true;
}

bool TimeScaleDialog::CheckParameters()
{
   return
      (m_RatePercentChangeStart >= RATE_PERCENTCHANGE_MIN_TEXT &&
       m_RatePercentChangeStart <= RATE_PERCENTCHANGE_MAX_TEXT)
      &&
      (m_RatePercentChangeEnd >= RATE_PERCENTCHANGE_MIN_TEXT &&
       m_RatePercentChangeEnd <= RATE_PERCENTCHANGE_MAX_TEXT)
      &&
      (m_PitchHalfStepsStart >= PITCH_HALFSTEPS_MIN_TEXT &&
       m_PitchHalfStepsStart <= PITCH_HALFSTEPS_MAX_TEXT)
      &&
      (m_PitchHalfStepsEnd >= PITCH_HALFSTEPS_MIN_TEXT &&
       m_PitchHalfStepsEnd <= PITCH_HALFSTEPS_MAX_TEXT)
      &&
      (m_PitchPercentChangeStart >= PITCH_PERCENTCHANGE_MIN_TEXT &&
       m_PitchPercentChangeStart <= PITCH_PERCENTCHANGE_MAX_TEXT)
      &&
      (m_PitchPercentChangeEnd >= PITCH_PERCENTCHANGE_MIN_TEXT &&
       m_PitchPercentChangeEnd <= PITCH_PERCENTCHANGE_MAX_TEXT);
}

// handler implementations for TimeScaleDialog

void TimeScaleDialog::OnText_RatePercentChangeStart(wxCommandEvent & WXUNUSED(event))
{
   if (m_bLoopDetect)
      return;

   if (m_pTextCtrl_RatePercentChangeStart) {
      wxString str = m_pTextCtrl_RatePercentChangeStart->GetValue();
      double newValue = 0;
      str.ToDouble(&newValue);
      m_RatePercentChangeStart = newValue;

      m_bLoopDetect = true;
      this->Update_Slider_RatePercentChangeStart();
      m_bLoopDetect = false;

      FindWindow(wxID_OK)->Enable(CheckParameters());
   }
}

void TimeScaleDialog::OnText_RatePercentChangeEnd(wxCommandEvent & WXUNUSED(event))
{
   if (m_bLoopDetect)
      return;

   if (m_pTextCtrl_RatePercentChangeEnd) {
      wxString str = m_pTextCtrl_RatePercentChangeEnd->GetValue();
      double newValue = 0;
      str.ToDouble(&newValue);
      m_RatePercentChangeEnd = newValue;

      m_bLoopDetect = true;
      this->Update_Slider_RatePercentChangeEnd();
      m_bLoopDetect = false;

      FindWindow(wxID_OK)->Enable(CheckParameters());
   }
}

void TimeScaleDialog::OnSlider_RatePercentChangeStart(wxCommandEvent & WXUNUSED(event))
{
   if (m_bLoopDetect)
      return;

   if (m_pSlider_RatePercentChangeStart) {
      m_RatePercentChangeStart = (double)(m_pSlider_RatePercentChangeStart->GetValue());

      m_bLoopDetect = true;
      this->Update_Text_RatePercentChangeStart();
      m_bLoopDetect = false;
   }
}

void TimeScaleDialog::OnSlider_RatePercentChangeEnd(wxCommandEvent & WXUNUSED(event))
{
   if (m_bLoopDetect)
      return;

   if (m_pSlider_RatePercentChangeEnd) {
      m_RatePercentChangeEnd = (double)(m_pSlider_RatePercentChangeEnd->GetValue());

      m_bLoopDetect = true;
      this->Update_Text_RatePercentChangeEnd();
      m_bLoopDetect = false;
   }
}

void TimeScaleDialog::OnText_PitchHalfStepsStart(wxCommandEvent & WXUNUSED(event))
{
   if (m_bLoopDetect)
      return;

   if (m_pTextCtrl_PitchHalfStepsStart) {
      wxString str = m_pTextCtrl_PitchHalfStepsStart->GetValue();
      double newValue = 0;
      str.ToDouble(&newValue);
      m_PitchHalfStepsStart = newValue;
      m_PitchPercentChangeStart = HalfStepsToPercentChange(newValue);

      m_bLoopDetect = true;
      this->Update_Text_PitchPercentChangeStart();
      m_bLoopDetect = false;

      FindWindow(wxID_OK)->Enable(CheckParameters());
   }
}

void TimeScaleDialog::OnText_PitchHalfStepsEnd(wxCommandEvent & WXUNUSED(event))
{
   if (m_bLoopDetect)
      return;

   if (m_pTextCtrl_PitchHalfStepsEnd) {
      wxString str = m_pTextCtrl_PitchHalfStepsEnd->GetValue();
      double newValue = 0;
      str.ToDouble(&newValue);
      m_PitchHalfStepsEnd = newValue;
      m_PitchPercentChangeEnd = HalfStepsToPercentChange(newValue);

      m_bLoopDetect = true;
      this->Update_Text_PitchPercentChangeEnd();
      m_bLoopDetect = false;

      FindWindow(wxID_OK)->Enable(CheckParameters());
   }
}

void TimeScaleDialog::OnText_PitchPercentChangeStart(wxCommandEvent & WXUNUSED(event))
{
   if (m_bLoopDetect)
      return;

   if (m_pTextCtrl_PitchPercentChangeStart) {
      wxString str = m_pTextCtrl_PitchPercentChangeStart->GetValue();
      double newValue = 0;
      str.ToDouble(&newValue);
      m_PitchPercentChangeStart = newValue;
      m_PitchHalfStepsStart = PercentChangeToHalfSteps(newValue);

      m_bLoopDetect = true;
      this->Update_Text_PitchHalfStepsStart();
      m_bLoopDetect = false;

      FindWindow(wxID_OK)->Enable(CheckParameters());
   }
}

void TimeScaleDialog::OnText_PitchPercentChangeEnd(wxCommandEvent & WXUNUSED(event))
{
   if (m_bLoopDetect)
      return;

   if (m_pTextCtrl_PitchPercentChangeEnd) {
      wxString str = m_pTextCtrl_PitchPercentChangeEnd->GetValue();
      double newValue = 0;
      str.ToDouble(&newValue);
      m_PitchPercentChangeEnd = newValue;
      m_PitchHalfStepsEnd = PercentChangeToHalfSteps(newValue);

      m_bLoopDetect = true;
      this->Update_Text_PitchHalfStepsEnd();
      m_bLoopDetect = false;

      FindWindow(wxID_OK)->Enable(CheckParameters());
   }
}

void TimeScaleDialog::OnCheckBox_PreAnalyze(wxCommandEvent & WXUNUSED(event))
{
   if (m_pCheckBox_PreAnalyze) {
      m_PreAnalyze = m_pCheckBox_PreAnalyze->GetValue();
   }
}

void TimeScaleDialog::Update_Text_RatePercentChangeStart()
{
   if (m_pTextCtrl_RatePercentChangeStart) {
      wxString str;
      str.Printf(wxT("%.3f"), m_RatePercentChangeStart);
      m_pTextCtrl_RatePercentChangeStart->SetValue(str);
   }
}

void TimeScaleDialog::Update_Text_RatePercentChangeEnd()
{
   if (m_pTextCtrl_RatePercentChangeEnd) {
      wxString str;
      str.Printf(wxT("%.3f"), m_RatePercentChangeEnd);
      m_pTextCtrl_RatePercentChangeEnd->SetValue(str);
   }
}

void TimeScaleDialog::Update_Slider_RatePercentChangeStart()
{
   if (m_pSlider_RatePercentChangeStart) {
      m_pSlider_RatePercentChangeStart->SetValue((int)(m_RatePercentChangeStart + 0.5));
   }
}

void TimeScaleDialog::Update_Slider_RatePercentChangeEnd()
{
   if (m_pSlider_RatePercentChangeEnd) {
      m_pSlider_RatePercentChangeEnd->SetValue((int)(m_RatePercentChangeEnd + 0.5));
   }
}

void TimeScaleDialog::Update_Text_PitchHalfStepsStart()
{
   if (m_pTextCtrl_PitchHalfStepsStart) {
      wxString str;
      str.Printf(wxT("%.3f"), m_PitchHalfStepsStart);
      m_pTextCtrl_PitchHalfStepsStart->SetValue(str);
   }
}

void TimeScaleDialog::Update_Text_PitchHalfStepsEnd()
{
   if (m_pTextCtrl_PitchHalfStepsEnd) {
      wxString str;
      str.Printf(wxT("%.3f"), m_PitchHalfStepsEnd);
      m_pTextCtrl_PitchHalfStepsEnd->SetValue(str);
   }
}

void TimeScaleDialog::Update_Text_PitchPercentChangeStart()
{
   if (m_pTextCtrl_PitchPercentChangeStart) {
      wxString str;
      str.Printf(wxT("%.3f"), m_PitchPercentChangeStart);
      m_pTextCtrl_PitchPercentChangeStart->SetValue(str);
   }
}

void TimeScaleDialog::Update_Text_PitchPercentChangeEnd()
{
   if (m_pTextCtrl_PitchPercentChangeEnd) {
      wxString str;
      str.Printf(wxT("%.3f"), m_PitchPercentChangeEnd);
      m_pTextCtrl_PitchPercentChangeEnd->SetValue(str);
   }
}

void TimeScaleDialog::Update_CheckBox_PreAnalyze()
{
   if (m_pCheckBox_PreAnalyze) {
      m_pCheckBox_PreAnalyze->SetValue(m_PreAnalyze);
   }
}

#endif
