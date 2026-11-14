/**********************************************************************

  Audacity: A Digital Audio Editor

  @file VST3Effect.h

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#pragma once

#include "VST3EffectBase.h"
#include "effects/StatelessPerTrackEffect.h"

#include <wx/wx.h>

namespace Steinberg {
namespace Vst {
class IEditController;
}
}

class NumericTextCtrl;
class VST3Instance;

class VST3ParametersWindow;

/**
 * \brief Objects of this class connect Audacity with VST3 effects
 */
class VST3Effect final : public StatelessEffectUIServices, public VST3EffectBase
{
public:
    ~VST3Effect() override;

    using VST3EffectBase::VST3EffectBase;

    int ShowClientInterface(const EffectPlugin& plugin, wxWindow& parent, wxDialog& dialog, EffectEditor* pEditor, bool forceModal)
    const override;

    std::unique_ptr<EffectEditor> PopulateUI(const EffectPlugin& plugin, ShuttleGui& S, EffectInstance& instance,
                                             EffectSettingsAccess& access, const EffectOutputs* pOutputs) const override;

    void ExportPresets(
        const EffectPlugin& plugin, const EffectSettings& settings)
    const override;
    OptionalMessage ImportPresets(
        const EffectPlugin& plugin, EffectSettings& settings) const override;

    void ShowOptions(const EffectPlugin& plugin) const override;

private:
    //! Will never be called
    virtual std::unique_ptr<EffectEditor> MakeEditor(
        ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access, const EffectOutputs* pOutputs) const final;
};
