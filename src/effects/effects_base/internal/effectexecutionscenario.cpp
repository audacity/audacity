/*
* Audacity: A Digital Audio Editor
*/
#include "effectexecutionscenario.h"

#include "global/defer.h"
#include "global/realfn.h"
#include "global/translation.h"

#include "libraries/lib-project/Project.h"
#include "libraries/lib-effects/Effect.h"
#include "libraries/lib-module-manager/PluginManager.h"

#include "libraries/lib-track/Track.h"
#include "libraries/lib-wave-track/WaveTrack.h"
#include "libraries/lib-project-rate/ProjectRate.h"
#include "libraries/lib-menus/CommandManager.h"
#include "libraries/lib-effects/EffectManager.h"
#include "libraries/lib-module-manager/ConfigInterface.h"
#include "libraries/lib-numeric-formats/NumericConverterFormats.h"

#include "au3wrap/internal/wxtypes_convert.h"
#include "au3wrap/au3types.h"
#include "au3wrap/internal/domaccessor.h"

#include "../effecterrors.h"

using namespace muse;
using namespace au::effects;

static const int UNDEFINED_FREQUENCY = -1;

muse::Ret EffectExecutionScenario::performEffect(const EffectId& effectId)
{
    au3::Au3Project& project = projectRef();
    return performEffectWithShowError(project, effectId, 0);
}

au::au3::Au3Project& EffectExecutionScenario::projectRef()
{
    return *reinterpret_cast<au3::Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
}

muse::Ret EffectExecutionScenario::repeatLastProcessor()
{
    IF_ASSERT_FAILED(m_lastProcessorId) {
        return make_ret(Err::UnknownError);
    }
    au3::Au3Project& project = projectRef();
    return performEffectWithShowError(project, *m_lastProcessorId, EffectManager::kConfigured);
}

std::pair<std::string, std::string> EffectExecutionScenario::makeErrorMsg(const muse::Ret& ret,
                                                                          const EffectId& effectId)
{
    const muse::String& effect = effectsProvider()->meta(effectId).title;
    return { effect.toStdString(), ret.text() };
}

muse::Ret EffectExecutionScenario::performEffectWithShowError(au3::Au3Project& project,
                                                              const EffectId& effectId, unsigned int flags)
{
    muse::Ret ret = doPerformEffect(project, effectId, flags);
    if (!ret && muse::Ret::Code(ret.code()) != muse::Ret::Code::Cancel) {
        const auto msg = makeErrorMsg(ret, effectId);
        interactive()->error(msg.first, msg.second);
    }
    return ret;
}

muse::Ret EffectExecutionScenario::doPerformEffect(au3::Au3Project& project, const EffectId& effectId, unsigned flags)
{
    //! ============================================================================
    //! NOTE Step 1 - check input params (effect is present and available, selection)
    //! ============================================================================

    // common things used below
    PluginID ID = effectId.toStdString();
    EffectManager& em = EffectManager::Get();
    Effect* effect = nullptr;

    secs_t t0;
    secs_t t1;
    bool isSelection = false;

    const trackedit::ClipKeyList selectedClips = selectionController()->selectedClips();
    const auto numSelectedClips = selectedClips.size();

    {
        //! NOTE Step 1.2 - get effect
        effect = effectsProvider()->effect(effectId);
        if (!effect) {
            return make_ret(Err::EffectNotFound);
        }

        if (numSelectedClips > 1 && !effectsProvider()->supportsMultipleClipSelection(effectId)) {
            return make_ret(Err::EffectMultipleClipSelectionNotSupported);
        }

        if (numSelectedClips != 0) {
            // If multiple clips are selected, we have checked that the effect supports it, in which case these global time boundaries shouldn't be relevant.
            // If this is just a single-clip selection, though, that will just be start and end times of the selected clip.
            t0 = selectionController()->selectedClipStartTime();
            t1 = selectionController()->selectedClipEndTime();
        } else {
            t0 = selectionController()->dataSelectedStartTime();
            t1 = selectionController()->dataSelectedEndTime();
        }

        isSelection = t1 > t0;
        if (!isSelection && effect->GetType() != EffectTypeGenerate) {
            return make_ret(Err::EffectNoAudioSelected);
        }

        //! TODO Should we do something if there is no selection and the effect is not a generator? Maybe add a check... or automatically select all...

        // Make sure there's no activity since the effect is about to be applied
        // to the project's tracks.  Mainly for Apply during RTP, but also used
        // for batch commands
        if (flags & EffectManager::kConfigured) {
            //! TODO
            // DO stopPlayback;
        }
    }

    //! ============================================================================
    //! NOTE Step 2 - formation of settings
    //! ============================================================================

    // common things used below
    EffectSettings* settings = nullptr;
    struct EffectTimeParams {
        double projectRate = 0.0;
        double t0 = 0.0;
        double t1 = 0.0;
        double f0 = 0.0;
        double f1 = 0.0;
    } tp;

    tp.projectRate = ProjectRate::Get(project).GetRate();

    {
        //! NOTE Step 2.1 - get effect settings
        settings = em.GetDefaultSettings(ID);
        IF_ASSERT_FAILED(settings) {
            return make_ret(Err::UnknownError);
        }

        //! NOTE Step 2.2 - get oldDuration for EffectTypeGenerate
        double duration = 0.0;
        if (effect->GetType() == EffectTypeGenerate) {
            GetConfig(effect->GetDefinition(), PluginSettings::Private,
                      CurrentSettingsGroup(),
                      EffectSettingsExtra::DurationKey(), duration, effect->GetDefaultDuration());
        }

        //! NOTE Step 2.3 - check selected time
        double quantizedDuration = duration;
        tp.t0 = t0;
        tp.t1 = t1;
        if (tp.t1 > tp.t0) {
            // there is a selection: let's fit in there...
            // MJS: note that this is just for the TTC and is independent of the track rate
            // but we do need to make sure we have the right number of samples at the project rate
            double quantMT0 = QUANTIZED_TIME(tp.t0, tp.projectRate);
            double quantMT1 = QUANTIZED_TIME(tp.t1, tp.projectRate);
            quantizedDuration = quantMT1 - quantMT0;
            tp.t1 = tp.t0 + quantizedDuration;
        }

        //! TODO when we support spectral display and selection
        //   tp.f0 = f0;
        //   tp.f1 = f1;

        //! NOTE Step 2.4 - update settings
        wxString newFormat = (isSelection
                              ? NumericConverterFormats::TimeAndSampleFormat()
                              : NumericConverterFormats::DefaultSelectionFormat()
                              ).Internal();

        settings->extra.SetDuration(quantizedDuration);
        settings->extra.SetDurationFormat(newFormat);
    }

    //! ============================================================================
    //! NOTE Step 3 - setup effect
    //! (must be before creating an instance and initializing it)
    //! ============================================================================
    unsigned oldFlags = 0;
    {
        //! NOTE Step 3.1 - setup effect
        oldFlags = effect->mUIFlags;
        effect->mUIFlags = flags;
        effect->mFactory = &WaveTrackFactory::Get(project);
        effect->mProjectRate = tp.projectRate;
        effect->mT0 = tp.t0;
        effect->mT1 = tp.t1;

        effect->SetTracks(&au3::Au3TrackList::Get(project));
        // Update track/group counts
        effect->CountWaveTracks();

        //! NOTE Step 3.2 - check frequency params
        effect->mF0 = tp.f0;
        effect->mF1 = tp.f1;
        if (effect->mF0 != UNDEFINED_FREQUENCY) {
            effect->mPresetNames.push_back(L"control-f0");
        }
        if (effect->mF1 != UNDEFINED_FREQUENCY) {
            effect->mPresetNames.push_back(L"control-f1");
        }
    }

    //! ============================================================================
    //! NOTE Step 4 - Make and init instance
    //! ============================================================================
    std::shared_ptr<EffectInstanceEx> pInstanceEx;
    {
        pInstanceEx = std::dynamic_pointer_cast<EffectInstanceEx>(effect->MakeInstance());
        if (!pInstanceEx || !pInstanceEx->Init()) {
            return make_ret(Err::UnknownError);
        }
    }

    //! ============================================================================
    //! NOTE Step 5 - modify settings by user
    //! ============================================================================
    {
        if (effect->IsInteractive() && (flags& EffectManager::kConfigured) == 0) {
            const auto access = std::make_shared<SimpleEffectSettingsAccess>(*settings);
            EffectInstanceId instanceId = effectInstancesRegister()->regInstance(effectId, pInstanceEx, access);
            muse::Ret ret = effectsProvider()->showEffect(effectId, instanceId);
            effectInstancesRegister()->requestUpdateSettings(instanceId);
            effectInstancesRegister()->unregInstance(instanceId);
            if (ret) {
                effect->SaveUserPreset(CurrentSettingsGroup(), *settings);
            } else {
                LOGE() << "failed show effect: " << effectId << ", ret: " << ret.toString();
                return ret;
            }
        }

        //! TODO
        em.SetSkipStateFlag(false);
    }

    //! ============================================================================
    //! NOTE Step 6 - perform effect
    //! ============================================================================
    // common things used below
    Ret success;
    if (numSelectedClips == 0) {
        success = performEffectOnTimeSelection(project, *effect, pInstanceEx, *settings);
    } else if (numSelectedClips == 1) {
        performEffectOnSingleClip(project, *effect, pInstanceEx, *settings, selectedClips.front().trackId, success);
    } else {
        success = performEffectOnEachSelectedClip(project, *effect, pInstanceEx, *settings);
    }

    //! ============================================================================
    //! NOTE Step 7 - cleanup
    //! ============================================================================

    {
        //! NOTE Step 7.1 - cleanup effect
        // Don't hold a dangling pointer when done
        effect->SetTracks(nullptr);
        effect->mPresetNames.clear();
        effect->mUIFlags = oldFlags;

        //! NOTE Step 7.2 - update selected region after process

        //! Generators, and even some processors (e.g. tempo change), need an update of the selection.
        if (success && numSelectedClips == 0 && (effect->mT1 >= effect->mT0)) {
            selectionController()->setDataSelectedStartTime(effect->mT0, true);
            selectionController()->setDataSelectedEndTime(effect->mT1, true);
        }
    }

    //! NOTE break if not success
    if (!success) {
        return success;
    }

    //! ============================================================================
    //! NOTE Step 8 - write history
    //! ============================================================================

    {
        //! NOTE Step 8.1 - write project history if need
        if (em.GetSkipStateFlag()) {
            flags = flags | EffectManager::kSkipState;
        }

        if (!(flags & EffectManager::kSkipState)) {
            const auto shortDesc = PluginManager::Get().GetName(ID).Translation().ToStdString();
            const auto longDesc = muse::mtrc("effects", "Applied effect: %1").arg(muse::String { shortDesc.c_str() }).toStdString();
            projectHistory()->pushHistoryState(longDesc, shortDesc);
        }

        //! NOTE Step 8.2 - remember a successful effect
        if (!(flags & EffectManager::kDontRepeatLast) && effect->GetType() == EffectTypeProcess) {
            if (m_lastProcessorId != effectId) {
                const auto firstTime = !m_lastProcessorId.has_value();
                m_lastProcessorId = effectId;
                m_lastProcessorIdChanged.send(effectId);
                if (firstTime) {
                    m_lastProcessorIsAvailableChanged.notify();
                }
            }
        }

        //! NOTE Step 8.3 - update plugin registry for next use
        if (effect->GetType() == EffectTypeGenerate) {
            SetConfig(effect->GetDefinition(), PluginSettings::Private,
                      CurrentSettingsGroup(),
                      EffectSettingsExtra::DurationKey(), effect->mT1 - effect->mT0);
        }
    }

    return true;
}

muse::Ret EffectExecutionScenario::performEffectOnTimeSelection(au3::Au3Project& project, Effect& effect,
                                                                const std::shared_ptr<EffectInstanceEx>& instance,
                                                                EffectSettings& settings)
{
    return effectsProvider()->performEffect(project, &effect, instance, settings);
}

std::optional<au::trackedit::ClipId> EffectExecutionScenario::performEffectOnSingleClip(au3::Au3Project& project, Effect& effect,
                                                                                        const std::shared_ptr<EffectInstanceEx>& instance,
                                                                                        EffectSettings& settings,
                                                                                        trackedit::TrackId trackId,
                                                                                        muse::Ret& success)
{
    success = effectsProvider()->performEffect(project, &effect, instance, settings);
    if (!success) {
        return std::nullopt;
    }
    // It is possible that the backend decides to replace the processed clip with a new one.
    // Since the selection spans only one clip, we want the originally selected clip to remain selected in appearance.
    // Look for the clip on that track at that time - we should find the new one.
    return selectionController()->setSelectedClip(trackId, effect.mT0);
}

muse::Ret EffectExecutionScenario::performEffectOnEachSelectedClip(au3::Au3Project& project, Effect& effect,
                                                                   const std::shared_ptr<EffectInstanceEx>& instance,
                                                                   EffectSettings& settings)
{
    // We are going to set the time and track selection to one clip at a time and apply the effect.

    // Make a copy of the selection state and restore it when leaving this scope.
    const trackedit::ClipKeyList clipsToProcess = selectionController()->selectedClips();
    const trackedit::TrackIdList tracksToProcess = selectionController()->selectedTracks();
    trackedit::ClipKeyList clipsToReselect;
    clipsToReselect.reserve(clipsToProcess.size());

    constexpr bool complete = true;

    Defer restoreTrackSelection([&] {
        selectionController()->setSelectedClips(clipsToReselect, complete);
    });

    // Perform the effect on each selected clip
    Ret success = true;
    for (const auto& clip : clipsToProcess) {
        selectionController()->setSelectedClips({ clip }, complete);
        selectionController()->setSelectedTracks({ clip.trackId }, complete);

        WaveTrack* waveTrack = au3::DomAccessor::findWaveTrack(project, ::TrackId(clip.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            continue;
        }

        const std::shared_ptr<WaveClip> waveClip = au3::DomAccessor::findWaveClip(waveTrack, clip.clipId);
        IF_ASSERT_FAILED(waveClip) {
            continue;
        }

        effect.mT0 = waveClip->GetPlayStartTime();
        effect.mT1 = waveClip->GetPlayEndTime();

        muse::Ret thisSuccess;
        const auto newClipId = performEffectOnSingleClip(project, effect, instance, settings, clip.trackId, thisSuccess);

        // Keep the error message from the first failure, that should do.
        if (success && !thisSuccess) {
            success = thisSuccess;
        }

        if (newClipId) {
            clipsToReselect.emplace_back(clip.trackId, *newClipId);
        }
    }
    return success;
}

bool EffectExecutionScenario::lastProcessorIsAvailable() const
{
    return m_lastProcessorId.has_value();
}

muse::async::Notification EffectExecutionScenario::lastProcessorIsNowAvailable() const
{
    return m_lastProcessorIsAvailableChanged;
}

muse::async::Channel<EffectId> EffectExecutionScenario::lastProcessorIdChanged() const
{
    return m_lastProcessorIdChanged;
}

muse::Ret EffectExecutionScenario::previewEffect(const EffectInstanceId& effectInstanceId, EffectSettings& settings)
{
    au3::Au3Project& project = projectRef();
    EffectId effectId = effectInstancesRegister()->effectIdByInstanceId(effectInstanceId);
    Effect* effect = effectsProvider()->effect(effectId);
    return effectsProvider()->previewEffect(project, effect, settings);
}
