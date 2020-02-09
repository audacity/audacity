/**********************************************************************

  Audacity: A Digital Audio Editor

  Compressor2.h

  Max Maisel (based on Compressor effect)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_COMPRESSOR2__
#define __AUDACITY_EFFECT_COMPRESSOR2__

#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/event.h>
#include <wx/stattext.h>
#include <wx/string.h>
#include <wx/textctrl.h>

#include "Effect.h"

class ShuttleGui;

class EffectCompressor2 final : public Effect
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectCompressor2();
   virtual ~EffectCompressor2();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;
   wxString ManualPage() override;

   // EffectDefinitionInterface implementation

   EffectType GetType() override;

   // EffectClientInterface implementation

   bool DefineParams( ShuttleParams & S ) override;
   bool GetAutomationParameters(CommandParameters & parms) override;
   bool SetAutomationParameters(CommandParameters & parms) override;

   // Effect implementation

   bool CheckWhetherSkipEffect() override;
   bool Startup() override;
   bool Process() override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

private:
   // EffectCompressor2 implementation

   bool UpdateProgress();
   void OnUpdateUI(wxCommandEvent & evt);
   void UpdateUI();

private:
   int    mAlgorithm;
   int    mCompressBy;
   bool   mStereoInd;

   double    mThresholdDB;
   double    mRatio;
   double    mKneeWidthDB;
   double    mAttackTime;
   double    mReleaseTime;
   double    mLookaheadTime;
   double    mLookbehindTime;
   double    mMakeupGainPct;
   double    mDryWetPct;

   DECLARE_EVENT_TABLE()
};

#endif
