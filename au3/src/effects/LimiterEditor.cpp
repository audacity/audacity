/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  LimiterEditor.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "LimiterEditor.h"
#include "ShuttleGui.h"

namespace {
struct ParameterWrapper : public DynamicRangeProcessorParameter
{
    ParameterWrapper(const LimiterParameter& parameter)
        : mParameter{parameter}
    {
    }

    double Min() const override
    {
        return mParameter.min / mParameter.scale;
    }

    double Max() const override
    {
        return mParameter.max / mParameter.scale;
    }

    double SliderMin() const override
    {
        return mParameter.min;
    }

    double SliderMax() const override
    {
        return mParameter.max;
    }

    double TextToSlider() const override
    {
        return mParameter.scale;
    }

    const LimiterParameter& mParameter;
};
} // namespace

LimiterEditor::LimiterEditor(
    wxWindow* parent, CompressorInstance& instance, bool isRealtime,
    const EffectUIServices& services, EffectSettingsAccess& access,
    LimiterSettings settings)
    : DynamicRangeProcessorEditor{parent, instance, isRealtime, services,
                                  access}
    , mSettings{std::move(settings)}
{
    Initialize({ { mSettings.thresholdDb, limiterThresholdDbAttributes,
                   std::make_shared<ParameterWrapper>(thresholdDb),
                   ControllerCategory::CompressionCurve },
                   { mSettings.makeupTargetDb, limiterMakeupTargetDbAttributes,
                     std::make_shared<ParameterWrapper>(makeupTargetDb),
                     ControllerCategory::CompressionCurve },
                   { mSettings.kneeWidthDb, kneeWidthDbAttributes,
                     std::make_shared<ParameterWrapper>(kneeWidthDb),
                     ControllerCategory::CompressionCurve },
                   { mSettings.lookaheadMs, lookaheadMsAttributes,
                     std::make_shared<ParameterWrapper>(lookaheadMs),
                     ControllerCategory::TimeSmoothing },
                   { mSettings.releaseMs, releaseMsAttributes,
                     std::make_shared<ParameterWrapper>(releaseMs),
                     ControllerCategory::TimeSmoothing } });
}
