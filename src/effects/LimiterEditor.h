#pragma once

#include "DynamicRangeProcessorEditor.h"
#include "processors/CompressorProcessor.h"

class ShuttleGui;

class LimiterEditor :
    public DynamicRangeProcessorEditor<LimiterEditor, LimiterSettings>
{
public:
   LimiterEditor(
      const EffectUIServices& services, EffectSettingsAccess& access,
      const LimiterSettings& settings)
       : DynamicRangeProcessorEditor<LimiterEditor, LimiterSettings> {
          services, access, settings
       }
   {
   }

   void PopulateOrExchange(ShuttleGui& S);

   static constexpr Parameter thresholdDb { &LimiterSettings::thresholdDb,
                                            L"thresholdDb",
                                            LimiterSettings::thresholdDbDefault,
                                            -20,
                                            0,
                                            1 };

   static constexpr Parameter kneeDb { &LimiterSettings::kneeDb,
                                       L"kneeDb",
                                       LimiterSettings::kneeDbDefault,
                                       0,
                                       30,
                                       1 };

   static constexpr Parameter lookaheadMs { &LimiterSettings::lookaheadMs,
                                            L"lookaheadMs",
                                            LimiterSettings::lookaheadMsDefault,
                                            0,
                                            20,
                                            1 };

   static constexpr Parameter releaseMs { &LimiterSettings::releaseMs,
                                          L"releaseMs",
                                          LimiterSettings::releaseMsDefault,
                                          0,
                                          100,
                                          1 };

   static constexpr Parameter ceilingDb { &LimiterSettings::ceilingDb,
                                          L"ceilingDb",
                                          LimiterSettings::ceilingDbDefault,
                                          -20,
                                          0,
                                          1 };

   Control mCeilingDbCtrl { ceilingDb, mSettings.ceilingDb };
};
