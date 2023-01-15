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
#if USE_SOUNDTOUCH
#include "ChangeTempo.h"
#include "EffectEditor.h"

#if USE_SBSMS
#include <wx/valgen.h>
#endif

#include <math.h>

#include <wx/checkbox.h>
#include <wx/slider.h>

#include "../ShuttleGui.h"
#include "../widgets/valnum.h"
#include "TimeWarper.h"

#include "LoadEffects.h"

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

const EffectParameterMethods& EffectChangeTempo::Parameters() const
{
   static CapturedParameters<EffectChangeTempo,
      Percentage, UseSBSMS
   > parameters;
   return parameters;
}

// We warp the slider to go up to 400%, but user can enter higher values.
static const double kSliderMax = 100.0;         // warped above zero to actually go up to 400%
static const double kSliderWarp = 1.30105;      // warp power takes max from 100 to 400.

//
// EffectChangeTempo
//

const ComponentInterfaceSymbol EffectChangeTempo::Symbol
{ XO("Change Tempo") };

namespace{ BuiltinEffectsModule::Registration< EffectChangeTempo > reg; }

BEGIN_EVENT_TABLE(EffectChangeTempo, wxEvtHandler)
    EVT_TEXT(ID_PercentChange, EffectChangeTempo::OnText_PercentChange)
    EVT_SLIDER(ID_PercentChange, EffectChangeTempo::OnSlider_PercentChange)
    EVT_TEXT(ID_FromBPM, EffectChangeTempo::OnText_FromBPM)
    EVT_TEXT(ID_ToBPM, EffectChangeTempo::OnText_ToBPM)
    EVT_TEXT(ID_ToLength, EffectChangeTempo::OnText_ToLength)
END_EVENT_TABLE()

EffectChangeTempo::EffectChangeTempo()
{
   // mUseSBSMS always defaults to false and its value is used only if USE_SBSMS
   // is defined
   Parameters().Reset(*this);
   m_FromBPM = 0.0; // indicates not yet set
   m_ToBPM = 0.0; // indicates not yet set
   m_FromLength = 0.0;
   m_ToLength = 0.0;

   m_bLoopDetect = false;

   SetLinearEffectFlag(true);
}

EffectChangeTempo::~EffectChangeTempo()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectChangeTempo::GetSymbol() const
{
   return Symbol;
}

TranslatableString EffectChangeTempo::GetDescription() const
{
   return XO("Changes the tempo of a selection without changing its pitch");
}

ManualPageID EffectChangeTempo::ManualPage() const
{
   return L"Change_Tempo";
}

// EffectDefinitionInterface implementation

EffectType EffectChangeTempo::GetType() const
{
   return EffectTypeProcess;
}

bool EffectChangeTempo::SupportsAutomation() const
{
   return true;
}

// Effect implementation

double EffectChangeTempo::CalcPreviewInputLength(const EffectContext &,
   const EffectSettings &, double previewLength) const
{
   return previewLength * (100.0 + m_PercentChange) / 100.0;
}

bool EffectChangeTempo::CheckWhetherSkipEffect(const EffectSettings &) const
{
   return (m_PercentChange == 0.0);
}

bool EffectChangeTempo::Init()
{
   // The selection might have changed since the last time EffectChangeTempo
   // was invoked, so recalculate the Length parameters.
   m_FromLength = mT1 - mT0;
   m_ToLength = (m_FromLength * 100.0) / (100.0 + m_PercentChange);

   return true;
}

bool EffectChangeTempo::Process(EffectContext &context,
   EffectInstance &, EffectSettings &settings)
{
   bool success = false;

#if USE_SBSMS
   if (mUseSBSMS)
   {
      double tempoRatio = 1.0 + m_PercentChange / 100.0;
      EffectSBSMS proxy;
      proxy.mProxyEffectName = XO("High Quality Tempo Change");
      proxy.setParameters(tempoRatio, 1.0);
      //! Already processing; don't make a dialog
      success = Delegate(context, proxy, settings);
   }
   else
#endif
   {
      auto initer = [&](soundtouch::SoundTouch *soundtouch)
      {
         soundtouch->setTempoChange(m_PercentChange);
      };
      double mT1Dashed = mT0 + (mT1 - mT0)/(m_PercentChange/100.0 + 1.0);
      RegionTimeWarper warper{ mT0, mT1,
         std::make_unique<LinearTimeWarper>(mT0, mT0, mT1, mT1Dashed )  };
      success = EffectSoundTouch::ProcessWithTimeWarper(context,
         initer, warper, false);
   }

   if(success)
      mT1 = mT0 + (mT1 - mT0)/(m_PercentChange/100 + 1.);

   return success;
}

std::unique_ptr<EffectEditor> EffectChangeTempo::PopulateOrExchange(
   ShuttleGui & S, EffectInstance &, EffectSettingsAccess &,
   const EffectOutputs *)
{
   mUIParent = S.GetParent();

   enum { precision = 2 };

   S.StartVerticalLay(0);
   {
      S.AddSpace(0, 5);
      S.AddTitle(XO("Change Tempo without Changing Pitch"));
      S.SetBorder(5);

      //
      S.StartMultiColumn(2, wxCENTER);
      {
         m_pTextCtrl_PercentChange = S.Id(ID_PercentChange)
            .Validator<FloatingPointValidator<double>>(
               3, &m_PercentChange, NumValidatorStyle::THREE_TRAILING_ZEROES,
               Percentage.min, Percentage.max )
            .AddTextBox(XXO("Percent C&hange:"), L"", 12);
      }
      S.EndMultiColumn();

      //
      S.StartHorizontalLay(wxEXPAND);
      {
         m_pSlider_PercentChange = S.Id(ID_PercentChange)
            .Name(XO("Percent Change"))
            .Style(wxSL_HORIZONTAL)
            .AddSlider( {}, 0, (int)kSliderMax, (int)Percentage.min);
      }
      S.EndHorizontalLay();

      S.StartStatic(XO("Beats per minute"));
      {
         S.StartHorizontalLay(wxALIGN_CENTER);
         {
            m_pTextCtrl_FromBPM = S.Id(ID_FromBPM)
               /* i18n-hint: changing tempo "from" one value "to" another */
               .Name(XO("Beats per minute, from"))
               .Validator<FloatingPointValidator<double>>(
                  3, &m_FromBPM,
                  NumValidatorStyle::THREE_TRAILING_ZEROES
                     | NumValidatorStyle::ZERO_AS_BLANK)
               /* i18n-hint: changing tempo "from" one value "to" another */
               .AddTextBox(XXC("&from", "change tempo"), wxT(""), 12);

            m_pTextCtrl_ToBPM = S.Id(ID_ToBPM)
               /* i18n-hint: changing tempo "from" one value "to" another */
               .Name(XO("Beats per minute, to"))
               .Validator<FloatingPointValidator<double>>(
                  3, &m_ToBPM,
                  NumValidatorStyle::THREE_TRAILING_ZEROES
                     | NumValidatorStyle::ZERO_AS_BLANK)
               /* i18n-hint: changing tempo "from" one value "to" another */
               .AddTextBox(XXC("&to", "change tempo"), wxT(""), 12);
         }
         S.EndHorizontalLay();
      }
      S.EndStatic();

      //
      S.StartStatic(XO("Length (seconds)"));
      {
         S.StartHorizontalLay(wxALIGN_CENTER);
         {
            m_pTextCtrl_FromLength = S.Id(ID_FromLength)
               .Disable() // Disable because the value comes from the
                       // user selection.
               .Validator<FloatingPointValidator<double>>(
                  precision, &m_FromLength,
                  NumValidatorStyle::TWO_TRAILING_ZEROES)
               /* i18n-hint: changing tempo "from" one value "to" another */
               .AddTextBox(XXC("from", "change tempo"), wxT(""), 12);
            m_pTextCtrl_ToLength = S.Id(ID_ToLength)
               .Validator<FloatingPointValidator<double>>(
                  2, &m_ToLength, NumValidatorStyle::TWO_TRAILING_ZEROES,
                  // min and max need same precision as what we're validating (bug 963)
                  RoundValue( precision,
                     (m_FromLength * 100.0) / (100.0 + Percentage.max) ),
                  RoundValue( precision,
                     (m_FromLength * 100.0) / (100.0 + Percentage.min) ) )
               /* i18n-hint: changing tempo "from" one value "to" another */
               .AddTextBox(XXC("t&o", "change tempo"), wxT(""), 12);
         }
         S.EndHorizontalLay();
      }
      S.EndStatic();

#if USE_SBSMS
      S.StartMultiColumn(2);
      {
         mUseSBSMSCheckBox = S.Validator<wxGenericValidator>(&mUseSBSMS)
            .AddCheckBox(XXO("&Use high quality stretching (slow)"),
                                             mUseSBSMS);
      }
      S.EndMultiColumn();
#endif

   }
   S.EndVerticalLay();

   return nullptr;
}

bool EffectChangeTempo::TransferDataToWindow(const EffectSettings &)
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

bool EffectChangeTempo::TransferDataFromWindow(EffectSettings &)
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
