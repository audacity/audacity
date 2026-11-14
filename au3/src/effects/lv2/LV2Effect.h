/**********************************************************************

  Audacity: A Digital Audio Editor

  LV2Effect.h

  Audacity(R) is copyright (c) 1999-2013 Audacity Team.
  License: GPL v2 or later.  See License.txt.

*********************************************************************/

#ifndef __AUDACITY_LV2_EFFECT__
#define __AUDACITY_LV2_EFFECT__

#if USE_LV2

class wxArrayString;

#include "LV2EffectBase.h"

#include "ShuttleGui.h"
#include "../StatelessPerTrackEffect.h"

// We use deprecated LV2 interfaces to remain compatible with older
// plug-ins, so disable warnings
LV2_DISABLE_DEPRECATION_WARNINGS

#define LV2EFFECTS_VERSION wxT("1.0.0.0")
/* i18n-hint: abbreviates
   "Linux Audio Developer's Simple Plugin API (LADSPA) version 2" */
#define LV2EFFECTS_FAMILY XO("LV2")

class LV2Editor;

class LV2Effect final : public StatelessEffectUIServices, public LV2EffectBase
{
public:
    using LV2EffectBase::LV2EffectBase;
    ~LV2Effect() override;

    int ShowClientInterface(const EffectPlugin& plugin, wxWindow& parent,
                            wxDialog& dialog, EffectEditor* pEditor, bool forceModal)
    const override;

    std::unique_ptr<EffectEditor> PopulateUI(const EffectPlugin& plugin,
                                             ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access,
                                             const EffectOutputs* pOutputs) const override;
    bool CloseUI() const override;

    void ExportPresets(
        const EffectPlugin& plugin, const EffectSettings& settings)
    const override;
    OptionalMessage ImportPresets(
        const EffectPlugin& plugin, EffectSettings& settings) const override;

    void ShowOptions(const EffectPlugin& plugin) const override;

private:
    //! Will never be called
    virtual std::unique_ptr<EffectEditor> MakeEditor(
        ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access,
        const EffectOutputs* pOutputs) const final;
};

#endif
#endif
