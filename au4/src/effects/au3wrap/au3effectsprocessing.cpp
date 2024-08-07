/*
* Audacity: A Digital Audio Editor
*/

#include "au3effectsprocessing.h"

#include "libraries/lib-track/Track.h"
#include "libraries/lib-wave-track/WaveTrack.h"
#include "libraries/lib-project-rate/ProjectRate.h"
#include "libraries/lib-time-frequency-selection/ViewInfo.h"
#include "libraries/lib-command-parameters/ShuttleAutomation.h"

#include "containers.h"
#include "log.h"

using namespace au::effects;

void Au3EffectsProcessing::process(const muse::String& effectId)
{
    if (!muse::contains(m_effectsMap, effectId)) {
        LOGW() << "not found effect: " << effectId;
        return;
    }

    Effect* effect = m_effectsMap[effectId];
    if (!effect) {
        LOGD() << "not found efffect: " << effectId;
        return;
    }

    AudacityProject& project = projectRef();
    auto& tracks = TrackList::Get(project);
    auto& trackFactory = WaveTrackFactory::Get(project);
    auto rate = ProjectRate::Get(project).GetRate();
    auto& selectedRegion = ViewInfo::Get(project).selectedRegion;

    EffectSettings pSettings = effect->MakeSettings();
    const auto pAccess = std::make_shared<SimpleEffectSettingsAccess>(pSettings);

    unsigned int flags = 0;
    bool success = false;

    const auto finder = [effect, pAccess](EffectSettings&)-> std::optional<std::shared_ptr<EffectInstanceEx> > {
        return effect->FindInstance(*effect);
    };

    pAccess->ModifySettings([&](EffectSettings& settings) {
        success = effect->DoEffect(
            settings, finder, rate, &tracks, &trackFactory, selectedRegion,
            flags, pAccess);
        return nullptr;
    });
}

void Au3EffectsProcessing::regEffect(const muse::String& effectId, Effect* effect)
{
    IF_ASSERT_FAILED(effect) {
        return;
    }

    IF_ASSERT_FAILED(!muse::contains(m_effectsMap, effectId)) {
        return;
    }

    m_effectsMap[effectId] = effect;
}

void Au3EffectsProcessing::unregEffect(const muse::String& effectId)
{
    m_effectsMap.erase(effectId);
}

AudacityProject& Au3EffectsProcessing::projectRef() const
{
    AudacityProject* project = reinterpret_cast<AudacityProject*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
}
