/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  Limiter.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "CompressorProcessor.h"
#include "StatelessPerTrackEffect.h"

class ShuttleGui;

class EffectLimiter final : public EffectWithSettings<LimiterSettings, StatelessPerTrackEffect>
{
public:
    static const ComponentInterfaceSymbol Symbol;

    EffectLimiter();

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    // EffectDefinitionInterface implementation

    EffectType GetType() const override;
    RealtimeSince RealtimeSupport() const override;
    RegistryPaths GetFactoryPresets() const override;
    OptionalMessage
    LoadFactoryPreset(int id, EffectSettings& settings) const override;

    // Effect implementation

    std::unique_ptr<EffectEditor> MakeEditor(
        ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access, const EffectOutputs* pOutputs) const override;

    std::shared_ptr<EffectInstance> MakeInstance() const override;

    std::unique_ptr<EffectOutputs> MakeOutputs() const override;

    bool CheckWhetherSkipEffect(const EffectSettings& settings) const override;

    const EffectParameterMethods& Parameters() const override;
};
