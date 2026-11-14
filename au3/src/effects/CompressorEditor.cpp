/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CompressorEditor.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "CompressorEditor.h"
#include "CompressorInstance.h"
#include "ShuttleGui.h"

namespace {
static constexpr auto lookaheadMinMs = 0.;
static constexpr auto lookaheadMaxMs = 1000.;
static constexpr auto compressionRatioMax = 20.;

class ParameterWrapper : public DynamicRangeProcessorParameter
{
public:
    ParameterWrapper(const CompressorParameter& parameter)
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

    const CompressorParameter& mParameter;
};
} // namespace

CompressorEditor::CompressorEditor(
    wxWindow* parent, CompressorInstance& instance, bool isRealtime,
    const EffectUIServices& services, EffectSettingsAccess& access,
    CompressorSettings settings)
    : DynamicRangeProcessorEditor{parent, instance, isRealtime, services,
                                  access}
    , mSettings{std::move(settings)}
{
    Initialize({ { mSettings.thresholdDb, compressorThresholdDbAttributes,
                   std::make_shared<ParameterWrapper>(thresholdDb),
                   ControllerCategory::CompressionCurve },
                   { mSettings.makeupGainDb, compressorMakupGainDbAttributes,
                     std::make_shared<ParameterWrapper>(makeupGainDb),
                     ControllerCategory::CompressionCurve },
                   { mSettings.kneeWidthDb, kneeWidthDbAttributes,
                     std::make_shared<ParameterWrapper>(kneeWidthDb),
                     ControllerCategory::CompressionCurve },
                   { mSettings.compressionRatio, compressionRatioAttributes,
                     std::make_shared<ParameterWrapper>(compressionRatio),
                     ControllerCategory::CompressionCurve },
                   { mSettings.lookaheadMs, lookaheadMsAttributes,
                     std::make_shared<ParameterWrapper>(lookaheadMs),
                     ControllerCategory::TimeSmoothing },
                   { mSettings.attackMs, attackMsAttributes,
                     std::make_shared<ParameterWrapper>(attackMs),
                     ControllerCategory::TimeSmoothing },
                   { mSettings.releaseMs, releaseMsAttributes,
                     std::make_shared<ParameterWrapper>(releaseMs),
                     ControllerCategory::TimeSmoothing } });
}
