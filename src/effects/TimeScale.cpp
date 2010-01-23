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
   m_RateStart = 0;
   m_RateEnd = 0;
   m_HalfStepsStart = 0;
   m_HalfStepsEnd = 0;
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
   dlog.m_RateStart = m_RateStart;
   dlog.m_RateEnd = m_RateEnd;
   dlog.m_HalfStepsStart = m_HalfStepsStart;
   dlog.m_HalfStepsEnd = m_HalfStepsEnd;
   dlog.m_PreAnalyze = m_PreAnalyze;

   // Don't need to call TransferDataToWindow, although other 
   // Audacity dialogs (from which I derived this one) do it, because 
   // ShowModal calls stuff that eventually calls wxWindowBase::OnInitDialog, 
   // which calls dlog.TransferDataToWindow();
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (dlog.GetReturnCode() == wxID_CANCEL)
      return false;
   
   m_RateStart = dlog.m_RateStart;
   m_RateEnd = dlog.m_RateEnd;
   m_HalfStepsStart = dlog.m_HalfStepsStart;
   m_HalfStepsEnd = dlog.m_HalfStepsEnd;
   m_PreAnalyze = dlog.m_PreAnalyze;
   
   return true;
}

bool EffectTimeScale::TransferParameters( Shuttle & shuttle )
{
   shuttle.TransferDouble(wxT("RateStart"),m_RateStart,0.0);
   shuttle.TransferDouble(wxT("RateEnd"),m_RateEnd,0.0);
   shuttle.TransferDouble(wxT("HalfStepsStart"),m_HalfStepsStart,0.0);
   shuttle.TransferDouble(wxT("HalfStepsEnd"),m_HalfStepsEnd,0.0);
   shuttle.TransferBool(wxT("PreAnalyze"),m_PreAnalyze,false);
   return true;
}

bool EffectTimeScale::Process()
{
   double pitchStart = pow(2.0,-(m_HalfStepsStart)/12.0);
   double pitchEnd = pow(2.0,-(m_HalfStepsEnd)/12.0);
   double rateStart = (100.0+m_RateStart)/100.0;
   double rateEnd = (100.0+m_RateEnd)/100.0;
   int quality = 1;
   this->EffectSBSMS::setParameters(rateStart,rateEnd,pitchStart,pitchEnd,quality,m_PreAnalyze);
   return this->EffectSBSMS::Process();
}

//----------------------------------------------------------------------------
// TimeScaleDialog
//----------------------------------------------------------------------------

#define RATE_MAX 150
#define RATE_DEFAULT 0
#define RATE_MIN -75
#define HALFSTEPS_MIN -12
#define HALFSTEPS_MAX 12

#define ID_TEXT_RATE_START 10001
#define ID_TEXT_RATE_END 10002
#define ID_TEXT_HALFSTEPS_START 10003
#define ID_TEXT_HALFSTEPS_END 10004
#define ID_SLIDER_RATE_START 10007
#define ID_SLIDER_RATE_END 10008
#define ID_CHECKBOX_PREANALYZE 10009

// event table for TimeScaleDialog

BEGIN_EVENT_TABLE(TimeScaleDialog, EffectDialog)
   EVT_TEXT(ID_TEXT_RATE_START, TimeScaleDialog::OnText_RateStart)
   EVT_TEXT(ID_TEXT_RATE_END, TimeScaleDialog::OnText_RateEnd)
   EVT_TEXT(ID_TEXT_HALFSTEPS_START, TimeScaleDialog::OnText_HalfStepsStart)
   EVT_TEXT(ID_TEXT_HALFSTEPS_END, TimeScaleDialog::OnText_HalfStepsEnd)
   EVT_SLIDER(ID_SLIDER_RATE_START, TimeScaleDialog::OnSlider_RateStart)
   EVT_SLIDER(ID_SLIDER_RATE_END, TimeScaleDialog::OnSlider_RateEnd)
   EVT_CHECKBOX(ID_CHECKBOX_PREANALYZE, TimeScaleDialog::OnCheckBox_PreAnalyze)
END_EVENT_TABLE()

TimeScaleDialog::TimeScaleDialog(EffectTimeScale *effect, wxWindow *parent)
   :  EffectDialog(parent, _("Time Scale"), INSERT_EFFECT),
      mEffect(effect)
{
   m_bLoopDetect = false;
   
   // NULL out these control members because there are some cases where the 
   // event table handlers get called during this method, and those handlers that 
   // can cause trouble check for NULL.
   
   m_pTextCtrl_RateStart = NULL;
   m_pTextCtrl_RateEnd = NULL;
   m_pSlider_RateStart = NULL;
   m_pSlider_RateEnd = NULL; 
   m_pTextCtrl_HalfStepsStart = NULL;
   m_pTextCtrl_HalfStepsEnd = NULL;
   m_pCheckBox_PreAnalyze = NULL;

   // effect parameters
   m_RateStart = 0;
   m_RateEnd = 0;
   m_HalfStepsStart = 0;
   m_HalfStepsEnd = 0;
   m_PreAnalyze = false;

   Init();
}

void TimeScaleDialog::PopulateOrExchange(ShuttleGui & S)
{

   wxTextValidator nullvld(wxFILTER_INCLUDE_CHAR_LIST);
   wxTextValidator numvld(wxFILTER_NUMERIC);

   S.SetBorder(10);
   S.StartHorizontalLay(wxCENTER, false);
   {
      S.AddTitle(_("Sliding Time Scale/Pitch Shift") +
                 wxString(wxT("\n")) + 
                 _("using SBSMS, by Clayton Otey"));
   }
   S.EndHorizontalLay();
   S.SetBorder(5);
   
   // Rate Text
   S.StartMultiColumn(2, wxCENTER);
   {
      m_pTextCtrl_RateStart = S.Id(ID_TEXT_RATE_START)
         .AddTextBox(_("Initial Tempo Change (%):"), wxT(""), 12);
      m_pTextCtrl_RateStart->SetValidator(numvld);
      
      m_pTextCtrl_RateEnd = S.Id(ID_TEXT_RATE_END)
         .AddTextBox(_("Final Tempo Change (%):"), wxT(""), 12);
      m_pTextCtrl_RateEnd->SetValidator(numvld);
   }
   S.EndMultiColumn();
   
   // Rate Slider
   S.StartHorizontalLay(wxEXPAND);
   {
      S.SetStyle(wxSL_HORIZONTAL);
      m_pSlider_RateStart = S.Id(ID_SLIDER_RATE_START)
         .AddSlider(wxT(""), (int)RATE_DEFAULT, (int)RATE_MAX, (int)RATE_MIN);
      m_pSlider_RateStart->SetName(_("Initial Tempo Change (%)"));
   }
   S.EndHorizontalLay();
   
   S.StartHorizontalLay(wxEXPAND);
   {
      S.SetStyle(wxSL_HORIZONTAL);
      m_pSlider_RateEnd = S.Id(ID_SLIDER_RATE_END)
         .AddSlider(wxT(""), (int)RATE_DEFAULT, (int)RATE_MAX, (int)RATE_MIN);
      m_pSlider_RateEnd->SetName(_("Final Tempo Change (%)"));
   }
   S.EndHorizontalLay();
   
   // HalfStep Text
   S.StartMultiColumn(2, wxCENTER);
   {
      m_pTextCtrl_HalfStepsStart = S.Id(ID_TEXT_HALFSTEPS_START)
         .AddTextBox(_("Initial Pitch Shift (semitones) [-12 to 12]:"), wxT(""), 12);
      m_pTextCtrl_HalfStepsStart->SetValidator(numvld);

      m_pTextCtrl_HalfStepsEnd = S.Id(ID_TEXT_HALFSTEPS_END)
         .AddTextBox(_("Final Pitch Shift (semitones) [-12 to 12]:"), wxT(""), 12);
      m_pTextCtrl_HalfStepsEnd->SetValidator(numvld);
   }
   S.EndMultiColumn();
      
   S.StartHorizontalLay(wxEXPAND);
   {
      S.SetStyle(wxSL_HORIZONTAL);
      m_pCheckBox_PreAnalyze = S.Id(ID_CHECKBOX_PREANALYZE)
         .AddCheckBox(wxT("Dynamic Transient Sharpening"), wxT("Dynamic Transient Sharpening"));
   }
   S.EndHorizontalLay();
   
   return;
}

bool TimeScaleDialog::TransferDataToWindow()
{
   m_bLoopDetect = true;

   this->Update_Text_RateStart();
   this->Update_Slider_RateStart();
   this->Update_Text_RateEnd();
   this->Update_Slider_RateEnd();
   this->Update_Text_HalfStepsStart();
   this->Update_Text_HalfStepsEnd();
   this->Update_CheckBox_PreAnalyze();

   m_bLoopDetect = false;
   
   return true;
}

bool TimeScaleDialog::TransferDataFromWindow()
{
   wxString str;

   if (m_pTextCtrl_RateStart) {
      str = m_pTextCtrl_RateStart->GetValue();
      double newValue = 0;
      str.ToDouble(&newValue);
      m_RateStart = newValue;
   }

   if (m_pTextCtrl_RateEnd) {
      str = m_pTextCtrl_RateEnd->GetValue();
      double newValue = 0;
      str.ToDouble(&newValue);
      m_RateEnd = newValue;
   }

   if (m_pTextCtrl_HalfStepsStart) {
      str = m_pTextCtrl_HalfStepsStart->GetValue();
      double newValue = 0;
      str.ToDouble(&newValue);
      m_HalfStepsStart = newValue;
   }

   if (m_pTextCtrl_HalfStepsEnd) {
      str = m_pTextCtrl_HalfStepsEnd->GetValue();
      double newValue = 0;
      str.ToDouble(&newValue);
      m_HalfStepsEnd = newValue;
   }

   if(m_pCheckBox_PreAnalyze) {
      m_PreAnalyze = m_pCheckBox_PreAnalyze->GetValue();
   }

   return true;
}

bool TimeScaleDialog::CheckParameters()
{
   return 
      (m_RateStart >= -90.0 && m_RateStart <= 500.0) 
      &&
      (m_RateEnd >= -90.0 && m_RateEnd <= 500.0) 
      &&
      (m_HalfStepsStart >= -12 && m_HalfStepsStart <=12)
      &&
      (m_HalfStepsEnd >= -12 && m_HalfStepsEnd <=12);
}

// handler implementations for TimeScaleDialog

void TimeScaleDialog::OnText_RateStart(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   if (m_pTextCtrl_RateStart) {
      wxString str = m_pTextCtrl_RateStart->GetValue();
      double newValue = 0;
      str.ToDouble(&newValue);
      m_RateStart = newValue;

      m_bLoopDetect = true;
      this->Update_Slider_RateStart();
      m_bLoopDetect = false;

      FindWindow(wxID_OK)->Enable(CheckParameters());
   }
}

void TimeScaleDialog::OnText_RateEnd(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   if (m_pTextCtrl_RateEnd) {
      wxString str = m_pTextCtrl_RateEnd->GetValue();
      double newValue = 0;
      str.ToDouble(&newValue);
      m_RateEnd = newValue;

      m_bLoopDetect = true;
      this->Update_Slider_RateEnd();
      m_bLoopDetect = false;
      
      FindWindow(wxID_OK)->Enable(CheckParameters());
   }
}

void TimeScaleDialog::OnSlider_RateStart(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   if (m_pSlider_RateStart) {
      m_RateStart = (double)(m_pSlider_RateStart->GetValue()); 
      
      m_bLoopDetect = true;
      this->Update_Text_RateStart();
      m_bLoopDetect = false;
   }
}

void TimeScaleDialog::OnSlider_RateEnd(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   if (m_pSlider_RateEnd) {
      m_RateEnd = (double)(m_pSlider_RateEnd->GetValue()); 
      
      m_bLoopDetect = true;
      this->Update_Text_RateEnd();
      m_bLoopDetect = false;
   }
}

void TimeScaleDialog::OnText_HalfStepsStart(wxCommandEvent & event)
{
  if (m_pTextCtrl_HalfStepsStart) {
    wxString str = m_pTextCtrl_HalfStepsStart->GetValue();
    double newValue = 0;
    str.ToDouble(&newValue);
    m_HalfStepsStart = newValue;

    FindWindow(wxID_OK)->Enable(CheckParameters());
  }
}

void TimeScaleDialog::OnText_HalfStepsEnd(wxCommandEvent & event)
{
  if (m_pTextCtrl_HalfStepsEnd) {
    wxString str = m_pTextCtrl_HalfStepsEnd->GetValue();
    double newValue = 0;
    str.ToDouble(&newValue);
    m_HalfStepsEnd = newValue;
    
    FindWindow(wxID_OK)->Enable(CheckParameters());
  }
}

void TimeScaleDialog::OnCheckBox_PreAnalyze(wxCommandEvent & event)
{
   if (m_pCheckBox_PreAnalyze) {
      m_PreAnalyze = m_pCheckBox_PreAnalyze->GetValue();
   }
}

void TimeScaleDialog::Update_Text_RateStart()
{
   if (m_pTextCtrl_RateStart) {
      wxString str;
      str.Printf(wxT("%.3f"), m_RateStart);
      m_pTextCtrl_RateStart->SetValue(str);
   }
}

void TimeScaleDialog::Update_Text_RateEnd()
{
   if (m_pTextCtrl_RateEnd) {
      wxString str;
      str.Printf(wxT("%.3f"), m_RateEnd);
      m_pTextCtrl_RateEnd->SetValue(str);
   }
}

void TimeScaleDialog::Update_Slider_RateStart()
{
   if (m_pSlider_RateStart) {
      m_pSlider_RateStart->SetValue((int)(m_RateStart + 0.5)); 
   }
}

void TimeScaleDialog::Update_Slider_RateEnd()
{
   if (m_pSlider_RateEnd) {
      m_pSlider_RateEnd->SetValue((int)(m_RateEnd + 0.5)); 
   }
}

void TimeScaleDialog::Update_Text_HalfStepsStart()
{
   if (m_pTextCtrl_HalfStepsStart) {
      wxString str;
      str.Printf(wxT("%.3f"), m_HalfStepsStart);
      m_pTextCtrl_HalfStepsStart->SetValue(str);
   }
}

void TimeScaleDialog::Update_Text_HalfStepsEnd()
{
   if (m_pTextCtrl_HalfStepsEnd) {
      wxString str;
      str.Printf(wxT("%.3f"), m_HalfStepsEnd);
      m_pTextCtrl_HalfStepsEnd->SetValue(str);
   }
}

void TimeScaleDialog::Update_CheckBox_PreAnalyze()
{
   if (m_pCheckBox_PreAnalyze) {
      m_pCheckBox_PreAnalyze->SetValue(m_PreAnalyze);
   }
}

#endif
