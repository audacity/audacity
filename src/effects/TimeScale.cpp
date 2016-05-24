/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeScale.cpp

  Clayton Otey

*******************************************************************//**

\class EffectTimeScale
\brief An EffectTimeScale does high quality sliding time scaling/pitch shifting

*//*******************************************************************/

#include "../Audacity.h" // for USE_SBSMS

#if USE_SBSMS

#include "TimeScale.h"

#include <math.h>

#include <wx/intl.h>

#include "../ShuttleGui.h"
#include "../widgets/valnum.h"

#include "sbsms.h"

enum
{
   ID_RatePercentChangeStart = 10000,
   ID_RatePercentChangeEnd,
   ID_PitchHalfStepsStart,
   ID_PitchHalfStepsEnd,
   ID_PitchPercentChangeStart,
   ID_PitchPercentChangeEnd
};

// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name                Type    Key                            Def   Min      Max    Scale
Param( RatePercentStart,   double, XO("RatePercentChangeStart"),  0.0,  -90.0,   500,   1  );
Param( RatePercentEnd,     double, XO("RatePercentChangeEnd"),    0.0,  -90.0,   500,   1  );
Param( HalfStepsStart,     double, XO("PitchHalfStepsStart"),     0.0,  -12.0,   12.0,  1  );
Param( HalfStepsEnd,       double, XO("PitchHalfStepsEnd"),       0.0,  -12.0,   12.0,  1  );
Param( PitchPercentStart,  double, XO("PitchPercentChangeStart"), 0.0,  -50.0,   100.0, 1  );
Param( PitchPercentEnd,    double, XO("PitchPercentChangeEnd"),   0.0,  -50.0,   100.0, 1  );

//
// EffectTimeScale
//

BEGIN_EVENT_TABLE(EffectTimeScale, wxEvtHandler)
   EVT_TEXT(ID_RatePercentChangeStart, EffectTimeScale::OnText_RatePercentChangeStart)
   EVT_TEXT(ID_RatePercentChangeEnd, EffectTimeScale::OnText_RatePercentChangeEnd)
   EVT_TEXT(ID_PitchHalfStepsStart, EffectTimeScale::OnText_PitchHalfStepsStart)
   EVT_TEXT(ID_PitchHalfStepsEnd, EffectTimeScale::OnText_PitchHalfStepsEnd)
   EVT_TEXT(ID_PitchPercentChangeStart, EffectTimeScale::OnText_PitchPercentChangeStart)
   EVT_TEXT(ID_PitchPercentChangeEnd, EffectTimeScale::OnText_PitchPercentChangeEnd)
   EVT_SLIDER(ID_RatePercentChangeStart, EffectTimeScale::OnSlider_RatePercentChangeStart)
   EVT_SLIDER(ID_RatePercentChangeEnd, EffectTimeScale::OnSlider_RatePercentChangeEnd)
END_EVENT_TABLE()

EffectTimeScale::EffectTimeScale()
{
   m_RatePercentChangeStart = DEF_RatePercentStart;
   m_RatePercentChangeEnd = DEF_RatePercentEnd;
   m_PitchHalfStepsStart = DEF_HalfStepsStart;
   m_PitchHalfStepsEnd = DEF_HalfStepsEnd;
   m_PitchPercentChangeStart = DEF_PitchPercentStart;
   m_PitchPercentChangeEnd = DEF_PitchPercentEnd;

   slideTypeRate = SlideLinearOutputRate;
   slideTypePitch = SlideLinearOutputRate;
   bPreview = false;
   previewSelectedDuration = 0.0;
   
   SetLinearEffectFlag(true);
}

EffectTimeScale::~EffectTimeScale()
{
}

// IdentInterface implementation

wxString EffectTimeScale::GetSymbol()
{
   return TIMESCALE_PLUGIN_SYMBOL;
}

wxString EffectTimeScale::GetName()
{
   return XO("Sliding Time Scale/Pitch Shift");
}

wxString EffectTimeScale::GetDescription()
{
   return XO("Allows continuous changes to the tempo and/or pitch");
}

// EffectIdentInterface implementation

EffectType EffectTimeScale::GetType()
{
   return EffectTypeProcess;
}

// EffectClientInterface implementation

bool EffectTimeScale::GetAutomationParameters(EffectAutomationParameters & parms)
{
   parms.Write(KEY_RatePercentStart, m_RatePercentChangeStart);
   parms.Write(KEY_RatePercentEnd, m_RatePercentChangeEnd);
   parms.Write(KEY_HalfStepsStart, m_PitchHalfStepsStart);
   parms.Write(KEY_HalfStepsEnd, m_PitchHalfStepsEnd);
   parms.Write(KEY_PitchPercentStart, m_PitchPercentChangeStart);
   parms.Write(KEY_PitchPercentEnd, m_PitchPercentChangeEnd);

   return true;
}

bool EffectTimeScale::SetAutomationParameters(EffectAutomationParameters & parms)
{
   ReadAndVerifyDouble(RatePercentStart);
   ReadAndVerifyDouble(RatePercentEnd);
   ReadAndVerifyDouble(HalfStepsStart);
   ReadAndVerifyDouble(HalfStepsEnd);
   ReadAndVerifyDouble(PitchPercentStart);
   ReadAndVerifyDouble(PitchPercentEnd);

   m_RatePercentChangeStart = RatePercentStart;
   m_RatePercentChangeEnd = RatePercentEnd;
   m_PitchHalfStepsStart = HalfStepsStart;
   m_PitchHalfStepsEnd = HalfStepsEnd;
   m_PitchPercentChangeStart = PitchPercentStart;
   m_PitchPercentChangeEnd = PitchPercentEnd;
   
   return true;
}

// Effect implementation

bool EffectTimeScale::Init()
{
   return true;
}

double EffectTimeScale::CalcPreviewInputLength(double previewLength)
{
   double inputLength = Effect::GetDuration();
   if(inputLength == 0.0) {
      return 0.0;
   } else {
      double rateStart = PercentChangeToRatio(m_RatePercentChangeStart);
      double rateEnd = PercentChangeToRatio(m_RatePercentChangeEnd);
      double tOut = previewLength/inputLength;
      double t = EffectSBSMS::getInvertedStretchedTime(rateStart,rateEnd,slideTypeRate,tOut);
      return t * inputLength;
   }
}

void EffectTimeScale::Preview(bool dryOnly)
{
   previewSelectedDuration = Effect::GetDuration();
   bPreview = true;
   Effect::Preview(dryOnly);
   bPreview = false;
}

bool EffectTimeScale::Process()
{
   double pitchStart = PercentChangeToRatio(m_PitchPercentChangeStart);
   double pitchEnd = PercentChangeToRatio(m_PitchPercentChangeEnd);
   double rateStart = PercentChangeToRatio(m_RatePercentChangeStart);
   double rateEnd = PercentChangeToRatio(m_RatePercentChangeEnd);
  
   if(bPreview) {
      double t = (mT1-mT0) / previewSelectedDuration;
      rateEnd = EffectSBSMS::getRate(rateStart,rateEnd,slideTypeRate,t);
      pitchEnd = EffectSBSMS::getRate(pitchStart,pitchEnd,slideTypePitch,t);
   }
   
   EffectSBSMS::setParameters(rateStart,rateEnd,pitchStart,pitchEnd,slideTypeRate,slideTypePitch,false,false,false);
   return EffectSBSMS::Process();
}

void EffectTimeScale::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(5);
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      // Rate Start
      S.StartStatic(_("Initial Tempo Change (%)"));
      {
         S.StartMultiColumn(1, wxCENTER);
         {
            FloatingPointValidator<double>
               vldRatePercentChangeStart(3, &m_RatePercentChangeStart, NUM_VAL_NO_TRAILING_ZEROES);
            vldRatePercentChangeStart.SetRange(MIN_RatePercentStart, MAX_RatePercentStart);
         
            m_pTextCtrl_RatePercentChangeStart = S.Id(ID_RatePercentChangeStart)
               .AddTextBox(wxT(""), wxT(""), 12);
            m_pTextCtrl_RatePercentChangeStart->SetValidator(vldRatePercentChangeStart);
         }
         S.EndMultiColumn();
         S.StartHorizontalLay(wxEXPAND, 0);
         {
            S.SetStyle(wxSL_HORIZONTAL);
            m_pSlider_RatePercentChangeStart = S.Id(ID_RatePercentChangeStart)
               .AddSlider(wxT(""), DEF_RatePercentStart, MAX_RatePercentStart, MIN_RatePercentStart);
         }
         S.EndHorizontalLay();
      }
      S.EndStatic();

      S.StartStatic(_("Final Tempo Change (%)"));
      {
         S.StartMultiColumn(1, wxCENTER);
         {
            FloatingPointValidator<double>
               vldRatePercentChangeEnd(3, &m_RatePercentChangeEnd, NUM_VAL_NO_TRAILING_ZEROES);
            vldRatePercentChangeEnd.SetRange(MIN_RatePercentEnd, MAX_RatePercentEnd);
         
            m_pTextCtrl_RatePercentChangeEnd = S.Id(ID_RatePercentChangeEnd)
               .AddTextBox(wxT(""), wxT(""), 12);
            m_pTextCtrl_RatePercentChangeEnd->SetValidator(vldRatePercentChangeEnd);
         }
         S.EndMultiColumn();
         S.StartHorizontalLay(wxEXPAND, 0);
         {
            S.SetStyle(wxSL_HORIZONTAL);
            m_pSlider_RatePercentChangeEnd = S.Id(ID_RatePercentChangeEnd)
               .AddSlider(wxT(""), DEF_RatePercentEnd, MAX_RatePercentEnd, MIN_RatePercentEnd);
         }
         S.EndHorizontalLay();
      }
      S.EndStatic();

      // Pitch Start
      S.StartStatic(_("Initial Pitch Shift"));
      {
         S.StartMultiColumn(2, wxCENTER);
         {
            FloatingPointValidator<double>
               vldPitchHalfStepsStart(3, &m_PitchHalfStepsStart, NUM_VAL_NO_TRAILING_ZEROES);
            vldPitchHalfStepsStart.SetRange(MIN_HalfStepsStart, MAX_HalfStepsStart);
         
            m_pTextCtrl_PitchHalfStepsStart = S.Id(ID_PitchHalfStepsStart)
               .AddTextBox(_("(semitones) [-12 to 12]:"), wxT(""), 12);
            m_pTextCtrl_PitchHalfStepsStart->SetValidator(vldPitchHalfStepsStart);

            FloatingPointValidator<double>
               vldPitchPercentChangeStart(3, &m_PitchPercentChangeStart, NUM_VAL_NO_TRAILING_ZEROES);
            vldPitchPercentChangeStart.SetRange(MIN_PitchPercentStart, MAX_PitchPercentStart);
         
            m_pTextCtrl_PitchPercentChangeStart = S.Id(ID_PitchPercentChangeStart)
               .AddTextBox(_("(%) [-50 to 100]:"), wxT(""), 12);
            m_pTextCtrl_PitchPercentChangeStart->SetValidator(vldPitchPercentChangeStart);
         }
         S.EndMultiColumn();
      }
      S.EndStatic();

      // Pitch End
      S.StartStatic(_("Final Pitch Shift"));
      {
         S.StartMultiColumn(2, wxCENTER);
         {
            FloatingPointValidator<double>
               vldPitchHalfStepsEnd(3, &m_PitchHalfStepsEnd, NUM_VAL_NO_TRAILING_ZEROES);
            vldPitchHalfStepsEnd.SetRange(MIN_HalfStepsEnd, MAX_HalfStepsEnd);
         
            m_pTextCtrl_PitchHalfStepsEnd = S.Id(ID_PitchHalfStepsEnd)
               .AddTextBox(_("(semitones) [-12 to 12]:"), wxT(""), 12);
            m_pTextCtrl_PitchHalfStepsEnd->SetValidator(vldPitchHalfStepsEnd);

            FloatingPointValidator<double>
               vldPitchPercentChangeEnd(3, &m_PitchPercentChangeEnd, NUM_VAL_NO_TRAILING_ZEROES);
            vldPitchPercentChangeEnd.SetRange(MIN_PitchPercentStart, MAX_PitchPercentStart);
         
            m_pTextCtrl_PitchPercentChangeEnd = S.Id(ID_PitchPercentChangeEnd)
               .AddTextBox(_("(%) [-50 to 100]:"), wxT(""), 12);
            m_pTextCtrl_PitchPercentChangeEnd->SetValidator(vldPitchPercentChangeEnd);
         }
         S.EndMultiColumn();
      }
      S.EndStatic();
   }
   S.EndMultiColumn();

   return;
}

bool EffectTimeScale::TransferDataToWindow()
{
   if (!mUIParent->TransferDataToWindow())
   {
      return false;
   }

   Update_Slider_RatePercentChangeStart();
   Update_Slider_RatePercentChangeEnd();

   return true;
}

bool EffectTimeScale::TransferDataFromWindow()
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }

   return true;
}

inline double EffectTimeScale::PercentChangeToRatio(double percentChange)
{
   return 1.0 + percentChange / 100.0;
}

inline double EffectTimeScale::HalfStepsToPercentChange(double halfSteps)
{
   return 100.0 * (pow(2.0,halfSteps/12.0) - 1.0);
}

inline double EffectTimeScale::PercentChangeToHalfSteps(double percentChange)
{
   return 12.0 * log2(PercentChangeToRatio(percentChange));
}

void EffectTimeScale::Update_Text_RatePercentChangeStart()
{
   m_pTextCtrl_RatePercentChangeStart->GetValidator()->TransferToWindow();
}

void EffectTimeScale::Update_Text_RatePercentChangeEnd()
{
   m_pTextCtrl_RatePercentChangeEnd->GetValidator()->TransferToWindow();
}

void EffectTimeScale::Update_Slider_RatePercentChangeStart()
{
   m_pSlider_RatePercentChangeStart->SetValue((int)(m_RatePercentChangeStart + 0.5));
}

void EffectTimeScale::Update_Slider_RatePercentChangeEnd()
{
   m_pSlider_RatePercentChangeEnd->SetValue((int)(m_RatePercentChangeEnd + 0.5));
}

void EffectTimeScale::Update_Text_PitchHalfStepsStart()
{
   m_pTextCtrl_PitchHalfStepsStart->GetValidator()->TransferToWindow();
}

void EffectTimeScale::Update_Text_PitchHalfStepsEnd()
{
   m_pTextCtrl_PitchHalfStepsEnd->GetValidator()->TransferToWindow();
}

void EffectTimeScale::Update_Text_PitchPercentChangeStart()
{
   m_pTextCtrl_PitchPercentChangeStart->GetValidator()->TransferToWindow();
}

void EffectTimeScale::Update_Text_PitchPercentChangeEnd()
{
   m_pTextCtrl_PitchPercentChangeEnd->GetValidator()->TransferToWindow();
}

void EffectTimeScale::OnText_RatePercentChangeStart(wxCommandEvent & WXUNUSED(evt))
{
   if (!EnableApply(mUIParent->TransferDataFromWindow()))
   {
      return;
   }

   Update_Slider_RatePercentChangeStart();
}

void EffectTimeScale::OnText_RatePercentChangeEnd(wxCommandEvent & WXUNUSED(evt))
{
   if (!EnableApply(mUIParent->TransferDataFromWindow()))
   {
      return;
   }

   Update_Slider_RatePercentChangeEnd();
}

void EffectTimeScale::OnSlider_RatePercentChangeStart(wxCommandEvent & evt)
{
   m_RatePercentChangeStart = (double) evt.GetInt();

   Update_Text_RatePercentChangeStart();
}

void EffectTimeScale::OnSlider_RatePercentChangeEnd(wxCommandEvent & evt)
{
   m_RatePercentChangeEnd = (double) evt.GetInt();

   Update_Text_RatePercentChangeEnd();
}

void EffectTimeScale::OnText_PitchHalfStepsStart(wxCommandEvent & WXUNUSED(evt))
{
   if (!EnableApply(mUIParent->TransferDataFromWindow()))
   {
      return;
   }

   m_PitchPercentChangeStart = HalfStepsToPercentChange(m_PitchHalfStepsStart);
   Update_Text_PitchPercentChangeStart();
}

void EffectTimeScale::OnText_PitchHalfStepsEnd(wxCommandEvent & WXUNUSED(evt))
{
   if (!EnableApply(mUIParent->TransferDataFromWindow()))
   {
      return;
   }

   m_PitchPercentChangeEnd = HalfStepsToPercentChange(m_PitchHalfStepsEnd);
   Update_Text_PitchPercentChangeEnd();
}

void EffectTimeScale::OnText_PitchPercentChangeStart(wxCommandEvent & WXUNUSED(evt))
{
   if (!EnableApply(mUIParent->TransferDataFromWindow()))
   {
      return;
   }

   m_PitchHalfStepsStart = PercentChangeToHalfSteps(m_PitchPercentChangeStart);
   Update_Text_PitchHalfStepsStart();
}

void EffectTimeScale::OnText_PitchPercentChangeEnd(wxCommandEvent & WXUNUSED(evt))
{
   if (!EnableApply(mUIParent->TransferDataFromWindow()))
   {
      return;
   }

   m_PitchHalfStepsEnd = PercentChangeToHalfSteps(m_PitchPercentChangeEnd);
   Update_Text_PitchHalfStepsEnd();
}

#endif
