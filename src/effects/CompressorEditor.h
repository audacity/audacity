#pragma once

#include "DynamicRangeProcessorEditor.h"
#include "processors/CompressorProcessor.h"

class ShuttleGui;

class CompressorEditor :
    public DynamicRangeProcessorEditor<CompressorEditor, CompressorSettings>
{
public:
   CompressorEditor(
      const EffectUIServices& services, EffectSettingsAccess& access,
      const CompressorSettings& settings)
       : DynamicRangeProcessorEditor<CompressorEditor, CompressorSettings> {
          services, access, settings
       }
   {
   }

   void PopulateOrExchange(ShuttleGui& S);

   static constexpr Parameter thresholdDb {
      &CompressorSettings::thresholdDb,
      L"thresholdDb",
      CompressorSettings::thresholdDbDefault,
      -50,
      10,
      1
   };

   static constexpr Parameter kneeDb { &CompressorSettings::kneeDb,
                                       L"kneeDb",
                                       CompressorSettings::kneeDbDefault,
                                       0,
                                       30,
                                       1 };

   static constexpr Parameter lookaheadMs {
      &CompressorSettings::lookaheadMs,
      L"lookaheadMs",
      CompressorSettings::lookaheadMsDefault,
      0,
      1000,
      1
   };

   static constexpr Parameter attackMs { &CompressorSettings::attackMs,
                                         L"attackMs",
                                         CompressorSettings::attackMsDefault,
                                         0,
                                         100,
                                         1 };

   static constexpr Parameter releaseMs { &CompressorSettings::releaseMs,
                                          L"releaseMs",
                                          CompressorSettings::releaseMsDefault,
                                          0,
                                          1000,
                                          1 };

   static constexpr Parameter ratio { &CompressorSettings::ratio,
                                      L"ratio",
                                      CompressorSettings::ratioDefault,
                                      1,
                                      20,
                                      1 };

   static constexpr Parameter makeUpDb { &CompressorSettings::makeUpDb,
                                         L"makeUpDb",
                                         CompressorSettings::makeUpDbDefault,
                                         -10,
                                         20,
                                         1 };

private:
   bool UpdateUI() override;

   void OnLookaheadMsSlider(wxCommandEvent& evt);
   void OnRatioSlider(wxCommandEvent& evt);

   Control mAttackMsCtrl { attackMs, mSettings.attackMs };
   Control mRatioCtrl { ratio, mSettings.ratio };
   Control mMakeupDbCtrl { makeUpDb, mSettings.makeUpDb };
};
