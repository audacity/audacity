/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  LimiterEditor.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "DynamicRangeProcessorEditor.h"
#include "SettingsVisitor.h"

class CompressorInstance;
class ShuttleGui;

using LimiterParameter = EffectParameter<LimiterSettings, double, double>;

class LimiterEditor final : public DynamicRangeProcessorEditor
{
public:
    LimiterEditor(
        wxWindow* parent, CompressorInstance& instance, bool isRealtime, const EffectUIServices& services, EffectSettingsAccess& access,
        LimiterSettings settings);

private:
    const LimiterSettings* GetLimiterSettings() const override
    {
        return &mSettings;
    }

    LimiterSettings mSettings;

public:
    // List of parameters, namely those of a compressor, less attack time
    // (hard-coded to 0) and ratio (hard-coded to infinity), and with different
    // default values.
    static constexpr LimiterParameter thresholdDb {
        &LimiterSettings::thresholdDb,
        L"thresholdDb",
        limiterThresholdDbDefault,
        -30 / dbStep,
        0,
        1 / dbStep
    };

    static constexpr LimiterParameter makeupTargetDb {
        &LimiterSettings::makeupTargetDb,
        L"makeupTargetDb",
        limiterMakeupTargetDbDefault,
        -30 / dbStep,
        0,
        1 / dbStep
    };

    static constexpr LimiterParameter kneeWidthDb {
        &LimiterSettings::kneeWidthDb,
        L"kneeWidthDb",
        limiterKneeWidthDbDefault,
        0,
        10 / dbStep,
        1 / dbStep
    };

    static constexpr LimiterParameter lookaheadMs {
        &LimiterSettings::lookaheadMs, L"lookaheadMs",
        limiterLookaheadMsDefault,     0,
        limiterMaxLookaheadMs,         1
    };

    static constexpr LimiterParameter releaseMs { &LimiterSettings::releaseMs,
                                                  L"releaseMs",
                                                  limiterReleaseMsDefault,
                                                  0,
                                                  1000,
                                                  1 };

    static constexpr LimiterParameter showInput {
        &LimiterSettings::showInput, L"showInput", showInputDefault, 0, 1, 1
    };
    static constexpr LimiterParameter showOutput {
        &LimiterSettings::showOutput, L"showOutput", showOutputDefault, 0, 1, 1
    };
    static constexpr LimiterParameter showActual {
        &LimiterSettings::showActual, L"showActual", showActualDefault, 0, 1, 1
    };
    static constexpr LimiterParameter showTarget {
        &LimiterSettings::showTarget, L"showTarget", showTargetDefault, 0, 1, 1
    };
};
