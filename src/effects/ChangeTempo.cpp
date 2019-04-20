/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangeTempo.cpp

  Vaughan Johnson,
  Dominic Mazzoni

*******************************************************************//**

\class EffectChangeTempo
\brief An EffectSoundTouch provides speeding up or
  slowing down tempo without changing pitch.

*//*******************************************************************/

#include "../Audacity.h" // for USE_SOUNDTOUCH

#if USE_SOUNDTOUCH
#include "ChangeTempo.h"

#if USE_SBSMS
#include "../../../lib-src/header-substitutes/sbsms.h"
#include <wx/valgen.h>
#endif

#include <math.h>

#include <wx/intl.h>
#include <wx/checkbox.h>
#include <wx/slider.h>

#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "../widgets/valnum.h"
#include "TimeWarper.h"

// Soundtouch defines these as well, which are also in generated configmac.h
// and configunix.h, so get rid of them before including,
// to avoid compiler warnings, and be sure to do this
// after all other #includes, to avoid any mischief that might result
// from doing the un-definitions before seeing any wx headers.
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION
#undef PACKAGE_BUGREPORT
#undef PACKAGE
#undef VERSION
#include "SoundTouch.h"

enum
{
   ID_PercentChange = 10000,
   ID_FromBPM,
   ID_ToBPM,
   ID_FromLength,
   ID_ToLength
};

// Soundtouch is not reasonable below -99% or above 3000%.

// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name          Type     Key               Def   Min      Max      Scale
Param( Percentage,   double,  wxT("Percentage"), 0.0,  -95.0,   3000.0,  1  );
Param( UseSBSMS,     bool,    wxT("SBSMS"),     false, false,   true,    1  );

// We warp the slider to go up to 400%, but user can enter higher values.
static const double kSliderMax = 100.0;         // warped above zero to actually go up to 400%
static const double kSliderWarp = 1.30105;      // warp power takes max from 100 to 400.

//
// EffectChangeTempo
//

BEGIN_EVENT_TABLE(EffectChangeTempo, wxEvtHandler)
    EVT_TEXT(ID_PercentChange, EffectChangeTempo::OnText_PercentChange)
    EVT_SLIDER(ID_PercentChange, EffectChangeTempo::OnSlider_PercentChange)
    EVT_TEXT(ID_FromBPM, EffectChangeTempo::OnText_FromBPM)
    EVT_TEXT(ID_ToBPM, EffectChangeTempo::OnText_ToBPM)
    EVT_TEXT(ID_ToLength, EffectChangeTempo::OnText_ToLength)
END_EVENT_TABLE()

EffectChangeTempo::EffectChangeTempo()
{
   m_PercentChange = DEF_Percentage;
   m_FromBPM = 0.0; // indicates not yet set
   m_ToBPM = 0.0; // indicates not yet set
   m_FromLength = 0.0;
   m_ToLength = 0.0;

   m_bLoopDetect = false;

#if USE_SBSMS
   mUseSBSMS = DEF_UseSBSMS;
#else
   mUseSBSMS = false;
#endif

   SetLinearEffectFlag(true);
}

EffectChangeTempo::~EffectChangeTempo()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectChangeTempo::GetSymbol()
{
   return CHANGETEMPO_PLUGIN_SYMBOL;
}

wxString EffectChangeTempo::GetDescription()
{
   return _("Changes the tempo of a selection without changing its pitch");
}

wxString EffectChangeTempo::ManualPage()
{
   return wxT("Change_Tempo");
}

// EffectDefinitionInterface implementation

EffectType EffectChangeTempo::GetType()
{
   return EffectTypeProcess;
}

bool EffectChangeTempo::SupportsAutomation()
{
   return true;
}

// EffectClientInterface implementation
bool EffectChangeTempo::DefineParams( ShuttleParams & S ){
   S.SHUTTLE_PARAM( m_PercentChange, Percentage );
   S.SHUTTLE_PARAM( mUseSBSMS, UseSBSMS );
   return true;
}

bool EffectChangeTempo::GetAutomationParameters(CommandParameters & parms)
{
   parms.Write(KEY_Percentage, m_PercentChange);
   parms.Write(KEY_UseSBSMS, mUseSBSMS);

   return true;
}

bool EffectChangeTempo::SetAutomationParameters(CommandParameters & parms)
{
   ReadAndVerifyDouble(Percentage);
   m_PercentChange = Percentage;

#if USE_SBSMS
   ReadAndVerifyBool(UseSBSMS);
   mUseSBSMS = UseSBSMS;
#else
   mUseSBSMS = false;
#endif

   return true;
}

// Effect implementation

double EffectChangeTempo::CalcPreviewInputLength(double previewLength)
{
   return previewLength * (100.0 + m_PercentChange) / 100.0;
}

bool EffectChangeTempo::CheckWhetherSkipEffect()
{
   return (m_PercentChange == 0.0);
}

bool EffectChangeTempo::Init()
{
   // The selection might have changed since the last time EffectChangeTempo
   // was invoked, so recalculate the Length parameters.
   m_FromLength = mT1 - mT0;
   m_ToLength = (m_FromLength * 100.0) / (100.0 + m_PercentChange);

   mSoundTouch.reset();

   return true;
}

bool EffectChangeTempo::Process()
{
   bool success = false;

#if USE_SBSMS
   if (mUseSBSMS)
   {
      double tempoRatio = 1.0 + m_PercentChange / 100.0;
      EffectSBSMS proxy;
      proxy.mProxyEffectName = XO("High Quality Tempo Change");
      proxy.setParameters(tempoRatio, 1.0);
      success = Delegate(proxy, mUIParent, false);
   }
   else
#endif
   {
      mSoundTouch = std::make_unique<soundtouch::SoundTouch>();
      mSoundTouch->setTempoChange(m_PercentChange);
      double mT1Dashed = mT0 + (mT1 - mT0)/(m_PercentChange/100.0 + 1.0);
      RegionTimeWarper warper{ mT0, mT1,
         std::make_unique<LinearTimeWarper>(mT0, mT0, mT1, mT1Dashed )  };
      success = EffectSoundTouch::ProcessWithTimeWarper(warper);
   }

   if(success)
      mT1 = mT0 + (mT1 - mT0)/(m_PercentChange/100 + 1.);

   return success;
}

void EffectChangeTempo::PopulateOrExchange(ShuttleGui & S)
{
   S.StartVerticalLay(0);
   {
      S.AddSpace(0, 5);
      S.AddTitle(_("Change Tempo without Changing Pitch"));
      S.SetBorder(5);

      //
      S.StartMultiColumn(2, wxCENTER);
      {
         FloatingPointValidator<double> vldPercentage(3, &m_PercentChange, NumValidatorStyle::THREE_TRAILING_ZEROES);
         vldPercentage.SetRange(MIN_Percentage, MAX_Percentage);
         m_pTextCtrl_PercentChange = S.Id(ID_PercentChange)
            .AddTextBox(_("Percent Change:"), wxT(""), 12);
         m_pTextCtrl_PercentChange->SetValidator(vldPercentage);
      }
      S.EndMultiColumn();

      //
      S.StartHorizontalLay(wxEXPAND);
      {
         S.SetStyle(wxSL_HORIZONTAL);
         m_pSlider_PercentChange = S.Id(ID_PercentChange)
            .AddSlider( {}, 0, (int)kSliderMax, (int)MIN_Percentage);
         m_pSlider_PercentChange->SetName(_("Percent Change"));
      }
      S.EndHorizontalLay();

      S.StartStatic(_("Beats per minute"));
      {
         S.StartHorizontalLay(wxALIGN_CENTER);
         {
            FloatingPointValidator<double> vldFromBPM(3, &m_FromBPM, NumValidatorStyle::THREE_TRAILING_ZEROES | NumValidatorStyle::ZERO_AS_BLANK);
            m_pTextCtrl_FromBPM = S.Id(ID_FromBPM)
               .AddTextBox(_("from"), wxT(""), 12);
            m_pTextCtrl_FromBPM->SetName(_("Beats per minute, from"));
            m_pTextCtrl_FromBPM->SetValidator(vldFromBPM);

            FloatingPointValidator<double> vldToBPM(3, &m_ToBPM, NumValidatorStyle::THREE_TRAILING_ZEROES | NumValidatorStyle::ZERO_AS_BLANK);
            m_pTextCtrl_ToBPM = S.Id(ID_ToBPM)
               .AddTextBox(_("to"), wxT(""), 12);
            m_pTextCtrl_ToBPM->SetName(_("Beats per minute, to"));
            m_pTextCtrl_ToBPM->SetValidator(vldToBPM);
         }
         S.EndHorizontalLay();
      }
      S.EndStatic();

      //
      S.StartStatic(_("Length (seconds)"));
      {
         S.StartHorizontalLay(wxALIGN_CENTER);
         {
            int precission = 2;
            FloatingPointValidator<double> vldFromLength(precission, &m_FromLength, NumValidatorStyle::TWO_TRAILING_ZEROES);
            m_pTextCtrl_FromLength = S.Id(ID_FromLength)
               .AddTextBox(_("from"), wxT(""), 12);
            m_pTextCtrl_FromLength->SetValidator(vldFromLength);
            m_pTextCtrl_FromLength->Enable(false); // Disable because the value comes from the user selection.

            FloatingPointValidator<double> vldToLength(2, &m_ToLength, NumValidatorStyle::TWO_TRAILING_ZEROES);

            // min and max need same precision as what we're validating (bug 963)
            double minLength = (m_FromLength * 100.0) / (100.0 + MAX_Percentage);
            double maxLength = (m_FromLength * 100.0) / (100.0 + MIN_Percentage);
            minLength = Internat::CompatibleToDouble(Internat::ToString(minLength, precission));
            maxLength = Internat::CompatibleToDouble(Internat::ToString(maxLength, precission));

            vldToLength.SetRange(minLength, maxLength);
            m_pTextCtrl_ToLength = S.Id(ID_ToLength)
               .AddTextBox(_("to"), wxT(""), 12);
            m_pTextCtrl_ToLength->SetValidator(vldToLength);
         }
         S.EndHorizontalLay();
      }
      S.EndStatic();

#if USE_SBSMS
      S.StartMultiColumn(2);
      {
         mUseSBSMSCheckBox = S.AddCheckBox(_("Use high quality stretching (slow)"),
                                             mUseSBSMS);
         mUseSBSMSCheckBox->SetValidator(wxGenericValidator(&mUseSBSMS));
      }
      S.EndMultiColumn();
#endif

   }
   S.EndVerticalLay();

   return;
}

bool EffectChangeTempo::TransferDataToWindow()
{
   // Reset from length because it can be changed by Preview
   m_FromLength = mT1 - mT0;

   m_bLoopDetect = true;

   if (!mUIParent->TransferDataToWindow())
   {
      return false;
   }

   // percent change controls
   Update_Slider_PercentChange();
   Update_Text_ToBPM();
   Update_Text_ToLength();

   m_bLoopDetect = false;

   // Set the accessibility name here because we need m_pTextCtrl_FromLength to have had its value set
   m_pTextCtrl_ToLength->SetName(
      wxString::Format( _("Length in seconds from %s, to"),
         m_pTextCtrl_FromLength->GetValue() ) );

   return true;
}

bool EffectChangeTempo::TransferDataFromWindow()
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }

   return true;
}

// handler implementations for EffectChangeTempo

void EffectChangeTempo::OnText_PercentChange(wxCommandEvent & WXUNUSED(evt))
{
   if (m_bLoopDetect)
      return;

   m_pTextCtrl_PercentChange->GetValidator()->TransferFromWindow();

   m_bLoopDetect = true;
   Update_Slider_PercentChange();
   Update_Text_ToBPM();
   Update_Text_ToLength();
   m_bLoopDetect = false;
}

void EffectChangeTempo::OnSlider_PercentChange(wxCommandEvent & WXUNUSED(evt))
{
   if (m_bLoopDetect)
      return;

   m_PercentChange = (double)(m_pSlider_PercentChange->GetValue());
   // Warp positive values to actually go up faster & further than negatives.
   if (m_PercentChange > 0.0)
      m_PercentChange = pow(m_PercentChange, kSliderWarp);

   m_bLoopDetect = true;
   Update_Text_PercentChange();
   Update_Text_ToBPM();
   Update_Text_ToLength();
   m_bLoopDetect = false;
}

void EffectChangeTempo::OnText_FromBPM(wxCommandEvent & WXUNUSED(evt))
{
   if (m_bLoopDetect)
      return;

   m_pTextCtrl_FromBPM->GetValidator()->TransferFromWindow();

   m_bLoopDetect = true;

   Update_Text_ToBPM();

   m_bLoopDetect = false;
}

void EffectChangeTempo::OnText_ToBPM(wxCommandEvent & WXUNUSED(evt))
{
   if (m_bLoopDetect)
      return;

   m_pTextCtrl_ToBPM->GetValidator()->TransferFromWindow();

   m_bLoopDetect = true;

   // If FromBPM has already been set, then there's a NEW percent change.
   if (m_FromBPM != 0.0 && m_ToBPM != 0.0)
   {
      m_PercentChange = ((m_ToBPM * 100.0) / m_FromBPM) - 100.0;

      Update_Text_PercentChange();
      Update_Slider_PercentChange();

      Update_Text_ToLength();
   }

   m_bLoopDetect = false;
}

void EffectChangeTempo::OnText_ToLength(wxCommandEvent & WXUNUSED(evt))
{
   if (m_bLoopDetect)
      return;

   m_pTextCtrl_ToLength->GetValidator()->TransferFromWindow();

   if (m_ToLength != 0.0)
   {
      m_PercentChange = ((m_FromLength * 100.0) / m_ToLength) - 100.0;
   }

   m_bLoopDetect = true;

   Update_Text_PercentChange();
   Update_Slider_PercentChange();

   Update_Text_ToBPM();

   m_bLoopDetect = false;
}

// helper fns

void EffectChangeTempo::Update_Text_PercentChange()
{
   m_pTextCtrl_PercentChange->GetValidator()->TransferToWindow();
}

void EffectChangeTempo::Update_Slider_PercentChange()
{
   double unwarped = m_PercentChange;
   if (unwarped > 0.0)
      // Un-warp values above zero to actually go up to kSliderMax.
      unwarped = pow(m_PercentChange, (1.0 / kSliderWarp));

   // Add 0.5 to unwarped so trunc -> round.
   m_pSlider_PercentChange->SetValue((int)(unwarped + 0.5));
}

void EffectChangeTempo::Update_Text_ToBPM()
// Use m_FromBPM & m_PercentChange to set NEW m_ToBPM & control.
{
   m_ToBPM = (((m_FromBPM * (100.0 + m_PercentChange)) / 100.0));
   m_pTextCtrl_ToBPM->GetValidator()->TransferToWindow();
}

void EffectChangeTempo::Update_Text_ToLength()
// Use m_FromLength & m_PercentChange to set NEW m_ToLength & control.
{
   m_ToLength = (m_FromLength * 100.0) / (100.0 + m_PercentChange);
   m_pTextCtrl_ToLength->GetValidator()->TransferToWindow();
}

#endif // USE_SOUNDTOUCH
