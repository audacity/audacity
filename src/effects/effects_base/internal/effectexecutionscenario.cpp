/*
* Audacity: A Digital Audio Editor
*/
#include "effectexecutionscenario.h"

#include "framework/global/async/async.h"
#include "framework/global/defer.h"
#include "framework/global/realfn.h"
#include "framework/global/translation.h"

#include "playback/iplayer.h"

#include "au3-project/Project.h"
#include "au3-effects/Effect.h"
#include "au3-effects/MixAndRender.h"

#include "au3-track/Track.h"
#include "au3-wave-track/WaveTrack.h"
#include "au3-project-rate/ProjectRate.h"
#include "au3-menus/CommandManager.h"
#include "au3-effects/EffectManager.h"
#include "au3-module-manager/ConfigInterface.h"
#include "au3-numeric-formats/NumericConverterFormats.h"
#include "au3-numeric-formats/ProjectTimeSignature.h"
#include "au3-transactions/TransactionScope.h"
#include "au3-stretching-sequence/TempoChange.h"

#include "au3-command-parameters/ShuttleAutomation.h"
#include "au3-components/EffectAutomationParameters.h"

#include "au3wrap/au3types.h"
#include "au3wrap/internal/domaccessor.h"
#include "au3wrap/internal/domconverter.h"
#include "au3wrap/internal/progressdialog.h"
#include "trackedit/trackeditutils.h"

#include "../effecterrors.h"

using namespace muse;
using namespace au::effects;

static const int UNDEFINED_FREQUENCY = -1;

muse::Ret EffectExecutionScenario::performEffect(const EffectId& effectId)
{
    au3::Au3Project& project = projectRef();
    return performEffectWithShowError(project, effectId, 0);
}

muse::Ret EffectExecutionScenario::performEffect(const EffectId& effectId, const std::string& params)
{
    EffectId resolved;
    EffectId titleFallback;
    // Search effect by id with a conviniece fallback to title for scripting
    for (const auto& meta : effectsProvider()->effectMetaList()) {
        if (meta.id == effectId) {
            resolved = meta.id;
            break;
        }
        if (titleFallback.empty() && meta.title == effectId) {
            titleFallback = meta.id;
        }
    }
    if (resolved.empty()) {
        resolved = titleFallback;
    }
    if (resolved.empty()) {
        LOGE() << "no effect found for symbol: " << effectId;
        return make_ret(Err::EffectNotFound);
    }

    au3::Au3Project& project = projectRef();
    return performEffectWithShowError(project, resolved, EffectManager::kConfigured, params);
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
                                                              const EffectId& effectId, unsigned int flags,
                                                              const std::string& params)
{
    muse::Ret ret = doPerformEffect(project, effectId, flags, params);
    if (!ret && muse::Ret::Code(ret.code()) != muse::Ret::Code::Cancel) {
        const auto msg = makeErrorMsg(ret, effectId);
        interactive()->error(msg.first, msg.second);
    }
    return ret;
}

muse::Ret EffectExecutionScenario::doPerformEffect(au3::Au3Project& project, const EffectId& effectId, unsigned flags,
                                                   const std::string& params)
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
    bool isTimeSelection = false;
    const spectrogram::FrequencySelection frequencySelection = frequencySelectionController()->frequencySelection();

    const trackedit::ClipKeyList selectedClips = selectionController()->selectedClips();
    const auto numSelectedClips = selectedClips.size();

    {
        //! NOTE Step 1.2 - get effect
        effect = effectsProvider()->effect(effectId);
        if (!effect) {
            return make_ret(Err::EffectNotFound);
        }

        if (numSelectedClips > 1 && !effectsProvider()->paramsAreInputAgnostic(effectId)) {
            return make_ret(Err::EffectMultipleClipSelectionNotSupported);
        }

        if (numSelectedClips != 0) {
            // If multiple clips are selected, we have checked that the effect supports it, in which case these global time boundaries shouldn't be relevant.
            // If this is just a single-clip selection, though, that will just be start and end times of the selected clip.
            t0 = selectionController()->selectedClipStartTime().value_or(0.0);
            t1 = selectionController()->selectedClipEndTime().value_or(0.0);
        } else {
            t0 = selectionController()->dataSelectedStartTime();
            t1 = selectionController()->dataSelectedEndTime();
        }

        isTimeSelection = t1 > t0;
        auto isTrackSelection = !selectionController()->selectedTracks().empty();

        if (effectsConfiguration()->applyEffectToAllAudio() && effect->applyEffectToAllAudio()
            && effect->GetType() != EffectTypeGenerate) {
            if (!isTimeSelection) {
                if (!isTrackSelection) {
                    // Select everything
                    selectionController()->setSelectedAllAudioData();
                } else {
                    // Select all time without modifying track selection
                    const auto prj = globalContext()->currentTrackeditProject();
                    selectionController()->setDataSelectedStartTime(0.0, true);
                    selectionController()->setDataSelectedEndTime(prj->totalTime(), true);
                }
                t0 = selectionController()->dataSelectedStartTime();
                t1 = selectionController()->dataSelectedEndTime();
                isTimeSelection = !selectionController()->timeSelectionIsEmpty();
            } else if (!isTrackSelection) {
                // Select all tracks without modifying time selection
                const auto prj = globalContext()->currentTrackeditProject();
                selectionController()->setSelectedTracks(prj->trackIdList());
            }
            isTrackSelection = !selectionController()->selectedTracks().empty();
        }

        if ((!isTimeSelection || !isTrackSelection) && (effect->GetType() != EffectTypeGenerate
                                                        && effect->GetType() != EffectTypeTool)) {
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
    struct EffectParams {
        double projectRate = 0.0;
        double t0 = 0.0;
        double t1 = 0.0;
        double f0 = 0.0;
        double f1 = 0.0;
        double centerFrequency = 0.0;
        bool spectralSelectionEnabled = false;
    } tp;

    tp.projectRate = ProjectRate::Get(project).GetRate();

    {
        //! NOTE Step 2.1 - get effect settings
        settings = em.GetDefaultSettings(ID, [&](const PluginID& id) -> EffectSettingsManager* {
            return effect;
        });
        IF_ASSERT_FAILED(settings) {
            return make_ret(Err::UnknownError);
        }

        //! NOTE Step 2.1b - apply script params if provided
        if (!params.empty()) {
            LOGD() << "Loading effect params: " << params;
            CommandParameters eap(wxString::FromUTF8(params));
            ShuttleSetAutomation shuttle;
            shuttle.SetForValidating(&eap);
            effect->VisitSettings(shuttle, *settings);
            if (!shuttle.bOK) {
                LOGE() << "Failed to load effect params: " << params;
                return make_ret(Err::UnknownError, "Failed to load effect settings from serialized params");
            }
            shuttle.SetForWriting(&eap);
            effect->VisitSettings(shuttle, *settings);
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

        const auto selectedTracks = selectionController()->selectedTracks();
        if (!selectedTracks.empty()) {
            const auto trackId = selectedTracks.front();
            // Just as spectral selection is a per-track thing, so are spectral effects.
            // Only pass spectral selection context if all selected clips belong to the same track.
            if (std::all_of(selectedTracks.begin(), selectedTracks.end(), [trackId](const trackedit::TrackId& id) {
                return id == trackId;
            })) {
                tp.f0 = frequencySelection.startFrequency();
                tp.f1 = frequencySelection.endFrequency();
                tp.centerFrequency = frequencySelection.centerFrequency();
            }
            tp.spectralSelectionEnabled = spectrogramConfiguration()->spectralSelectionEnabled();
        }

        //! NOTE Step 2.4 - update settings
        wxString newFormat = (isTimeSelection
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
        effect->mSpectralSelectionEnabled = tp.spectralSelectionEnabled;
        effect->mF0 = tp.f0;
        effect->mF1 = tp.f1;
        effect->mCenterFrequency = tp.centerFrequency;
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
            return pInstanceEx
                   && !pInstanceEx->GetLastError().empty() ? make_ret(Err::EffectProcessFailed, pInstanceEx->GetLastError()) : make_ret(
                Err::UnknownError);
        }
    }

    //! ============================================================================
    //! NOTE Step 5 - modify settings by user
    //! ============================================================================
    {
        if (effect->IsInteractive() && (flags& EffectManager::kConfigured) == 0) {
            const auto access = std::make_shared<SimpleEffectSettingsAccess>(*settings);
            EffectInstanceId instanceId = effectInstancesRegister()->regInstance(effectId, pInstanceEx, access);
            muse::Ret ret = effectViewController()->showEffect(effectId, instanceId);
            effectInstancesRegister()->requestUpdateSettings(instanceId);
            effectInstancesRegister()->unregInstance(instanceId);
            if (ret) {
                effect->SaveUserPreset(CurrentSettingsGroup(), *settings);
            } else {
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
        success = performGenerator(project, *effect, pInstanceEx, *settings);
    } else if (numSelectedClips == 1) {
        performEffectOnSingleClip(project, *effect, pInstanceEx, *settings, selectedClips.front().trackId, success);
    } else {
        success = performEffectOnEachSelectedClip(project, *effect, pInstanceEx, *settings);
    }

    //! ============================================================================
    //! NOTE Step 7 - cleanup
    //! ============================================================================

    // Don't hold a dangling pointer when done
    effect->SetTracks(nullptr);
    effect->mPresetNames.clear();
    effect->mUIFlags = oldFlags;

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
            const auto shortDesc = effectsProvider()->effectName(effectId.toStdString());
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

namespace {
auto getAllClips(const au::trackedit::ITrackeditProject& prj)
{
    const auto tracks = prj.trackIdList();
    au::trackedit::Clips clips;
    for (const auto& trackId : tracks) {
        const auto trackClips = prj.clipList(trackId);
        clips.insert(clips.end(), trackClips.begin(), trackClips.end());
    }
    return clips;
}
}

muse::Ret EffectExecutionScenario::performGenerator(au3::Au3Project& project, Effect& effect,
                                                    const std::shared_ptr<EffectInstanceEx>& instance,
                                                    EffectSettings& settings)
{
    const auto prj = globalContext()->currentTrackeditProject();
    const auto clipsBefore = getAllClips(*prj);

    const auto ret = performEffectInternal(project, &effect, instance, settings);

    if (ret) {
        const auto clipsAfter = getAllClips(*prj);
        const std::vector<const au::trackedit::Clip*> newClips = trackedit::utils::clipSetDifference(clipsAfter, clipsBefore);
        if (!newClips.empty()) {
            trackedit::ClipKeyList newClipsKeys;
            std::transform(newClips.begin(), newClips.end(), std::back_inserter(newClipsKeys),
                           [](const auto clip) { return clip->key; });
            selectionController()->resetDataSelection();
            selectionController()->setSelectedClips(newClipsKeys, true);
        }
    }

    return ret;
}

std::optional<au::trackedit::ClipId> EffectExecutionScenario::performEffectOnSingleClip(au3::Au3Project& project, Effect& effect,
                                                                                        const std::shared_ptr<EffectInstanceEx>& instance,
                                                                                        EffectSettings& settings,
                                                                                        trackedit::TrackId trackId,
                                                                                        muse::Ret& success)
{
    success = performEffectInternal(project, &effect, instance, settings);
    if (!success) {
        return std::nullopt;
    }
    // It is possible that the backend decides to replace the processed clip with a new one.
    // Since the selection spans only one clip, we want the originally selected clip to remain selected in appearance.
    // Look for the clip on that track at that time - we should find the new one.
    const auto waveClip = au3::DomAccessor::findWaveClip(project, trackId, effect.mT0);
    IF_ASSERT_FAILED(waveClip) {
        return std::nullopt;
    }
    selectionController()->setSelectedClips({ trackedit::ClipKey { trackId, waveClip->GetId() } });
    return waveClip->GetId();
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
        trackNavigationController()->setFocusedTrack(clip.trackId);

        WaveTrack* waveTrack = au3::DomAccessor::findWaveTrack(project, ::TrackId(clip.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            continue;
        }

        const std::shared_ptr<WaveClip> waveClip = au3::DomAccessor::findWaveClip(waveTrack, clip.itemId);
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
    EffectId effectId = effectInstancesRegister()->effectIdByInstanceId(effectInstanceId);
    return doPreviewEffect(effectId, settings);
}

namespace {
std::vector<au::trackedit::Track> trackListDifference(const std::vector<au::trackedit::Track>& a,
                                                      const std::vector<au::trackedit::Track>& b)
{
    std::vector<au::trackedit::Track> result;
    for (const au::trackedit::Track& item : a) {
        if (std::find_if(b.begin(), b.end(), [&item](const au::trackedit::Track& other) { return item.id == other.id; }) == b.end()) {
            result.push_back(item);
        }
    }
    return result;
}

void notifyIfTracksWereAdded(au::au3::Au3Project& au3Prj, const std::vector<au::trackedit::Track>& before,
                             au::trackedit::ITrackeditProject& trackeditPrj)
{
    const auto& trackList = ::TrackList::Get(au3Prj);
    std::vector<au::trackedit::Track> tracksAfter;
    auto it = trackList.begin();
    while (it != trackList.end()) {
        tracksAfter.push_back(au::au3::DomConverter::track(*it));
        ++it;
    }

    const std::vector<au::trackedit::Track> addedTracks = trackListDifference(tracksAfter, before);
    for (const auto& track : addedTracks) {
        trackeditPrj.notifyAboutTrackAdded(track);
    }
}
}

muse::Ret EffectExecutionScenario::performEffectInternal(au3::Au3Project& project, Effect* effect,
                                                         std::shared_ptr<EffectInstance> pInstanceEx,
                                                         EffectSettings& settings)
{
    //! ============================================================================
    //! NOTE Step 1 - add new a track if need
    //! ============================================================================

    // common things used below
    au3::Au3WaveTrack* newTrack = nullptr;
    {
        // We don't yet know the effect type for code in the Nyquist Prompt, so
        // assume it requires a track and handle errors when the effect runs.
        if ((effect->GetType() == EffectTypeGenerate || effect->GetPath() == NYQUIST_PROMPT_ID) && (effect->mNumTracks == 0)) {
            auto track = effect->mFactory->Create();
            track->SetName(effect->mTracks->MakeUniqueTrackName(au3::Au3WaveTrack::GetDefaultAudioTrackNamePreference()));
            // The track-added event should be issued synchronously.
            newTrack = effect->mTracks->Add(
                track, TrackList::DoAssignId::Yes,
                TrackList::EventPublicationSynchrony::Synchronous);
            newTrack->SetSelected(true);
            globalContext()->currentTrackeditProject()->notifyAboutTrackAdded(au3::DomConverter::track(newTrack));
        }
    }

    //! ============================================================================
    //! NOTE Step 2 - process
    //! ============================================================================

    // common things used below
    muse::Ret success = make_ret(Ret::Code::Ok);
    {
        //! NOTE Step 2.3 - open transaction
        TransactionScope trans(project, "Effect");

        //! NOTE Step 2.4 - do process

        //! TODO It is not clear what the skip flag is and why it can be set,
        //! in what cases when calling this function
        //! it is not necessary to call the main thing - the process
        bool skipFlag = static_cast<EffectBase*>(effect)->CheckWhetherSkipEffect(settings);
        if (skipFlag == false) {
            using namespace BasicUI;
            auto name = effect->GetName();

            const std::string title
                = (effect->GetType()
                   == EffectTypeGenerate ? muse::qtrc("effects", "Generating %1…") : muse::qtrc("effects", "Applying %1…")).arg(
                      QString::fromUtf8(name.Translation().ToUTF8().data())).toStdString();

            ::ProgressDialog progress{ iocContext(), title };
            auto vr = valueRestorer<BasicUI::ProgressDialog*>(effect->mProgress, &progress);

            assert(pInstanceEx); // null check above
            try {
                // Get tracklist now and compare it with after to see if some tracks were added (such as a label track being added by the beat finder analyzer).
                const auto prj = globalContext()->currentTrackeditProject();
                const std::vector<trackedit::Track> tracksBefore = prj->trackList();
                if (pInstanceEx->Process(settings) == false) {
                    if (progress.cancelled()) {
                        success = make_ret(Err::EffectProcessCancelled);
                    } else {
                        success = make_ret(Err::EffectProcessFailed, pInstanceEx->GetLastError());
                    }
                }
                notifyIfTracksWereAdded(project, tracksBefore, *prj);
            } catch (::AudacityException& e) {
                success = make_ret(Err::EffectProcessFailed);
                if (const auto box = dynamic_cast<MessageBoxException*>(&e)) {
                    std::string message = box->ErrorMessage().Translation().ToStdString();
                    if (!message.empty()) {
                        success.setText(message);
                    }
                }
            }
        }

        //! NOTE Step 2.5 - commit transaction on success
        if (success) {
            trans.Commit();
        }
    }

    //! ============================================================================
    //! NOTE Step 3 - cleanup
    //! ============================================================================

    {
        if (!success && newTrack) {
            const auto au4Track = au3::DomConverter::track(newTrack);
            // This decreases the reference count of the track, so it may be deleted.
            effect->mTracks->Remove(*newTrack);
            globalContext()->currentTrackeditProject()->notifyAboutTrackRemoved(au4Track);
        }
    }

    return success;
}

namespace {
void restoreEffectStateHack(EffectBase& effect)
{
    if (auto pInstance = std::dynamic_pointer_cast<EffectInstanceEx>(effect.MakeInstance())) {
        pInstance->Init();
    }
}
}

muse::Ret EffectExecutionScenario::doPreviewEffect(const EffectId& effectId, EffectSettings& settings)
{
    ::EffectBase* pEffect = effectsProvider()->effect(effectId);
    if (!pEffect) {
        return muse::make_ret(muse::Ret::Code::InternalError);
    }
    auto& effect = *pEffect;

    const bool isNyquist = effect.GetFamily() == NYQUISTEFFECTS_FAMILY;
    const bool isGenerator = effect.GetType() == EffectTypeGenerate;

    //! ============================================================================
    //! NOTE Step 1 - check conditions
    //! ============================================================================
    if (effect.mNumTracks == 0 && !isGenerator) {     // nothing to preview
        return muse::make_ret(muse::Ret::Code::InternalError);
    }

    auto player = playback()->player();
    if (player->isBusy()) {
        LOGW() << "can't play, maybe audio is busy";
        return muse::make_ret(muse::Ret::Code::InternalError);
    }

    //! ============================================================================
    //! NOTE Step 2 - save origin context (state)
    //! ============================================================================
    const EffectContext originCtx = { effect.mT0, effect.mT1, effect.mTracks, effect.mProgress, effect.mIsPreview };

    // restore internal effect state on return (if needed)
    auto cleanup0 = effect.BeginPreview(settings);

    // Effect is already inited; we will call Process and then Init
    // again, so the state is exactly the way it was before Preview
    // was called.
    auto cleanup1 = finally([&] {
        restoreEffectStateHack(effect);
    });

    //! ============================================================================
    //! NOTE Step 3 - make new context (state)
    //! ============================================================================

    EffectContext newCtx;

    //! Step 3.1 - prepare time
    newCtx.t0 = originCtx.t0;
    if (effect.PreviewsFullSelection()) {
        newCtx.t1 = originCtx.t1;
    } else {
        // Limit preview time:
        // We need to pre-render the audio, which would take a long time and lots of memory for long selections.
        // On the other hand, preview isn't typically something users would listen to for more than a few seconds.
        // (Au3 used to read `previewLen` from the `/AudioIO/EffectsPreviewLen` setting.
        // There is no plan at the moment to reintroduce it in Au4.)
        const double maxPreviewLen = effectsConfiguration()->previewMaxDuration();
        const double previewLen = std::min(originCtx.t1 - originCtx.t0, maxPreviewLen);
        double previewDuration = 0.0;
        if (isNyquist && isGenerator) {
            previewDuration = effect.CalcPreviewInputLength(settings, previewLen);
        } else {
            previewDuration = std::min(settings.extra.GetDuration(), effect.CalcPreviewInputLength(settings, previewLen));
        }
        newCtx.t1 = originCtx.t0 + previewDuration;
    }

    if ((newCtx.t1 > originCtx.t1) && !isGenerator) {
        newCtx.t1 = originCtx.t1;
    }

    if (muse::RealIsEqualOrLess(newCtx.t1, newCtx.t0)) {
        return muse::make_ret(muse::Ret::Code::InternalError);
    }

    //! Step 3.2 - make new tracks
    {
        // Build NEW tracklist from rendering tracks
        // Set the same owning project, so FindProject() can see it within Process()
        const auto pProject = effect.mTracks->GetOwner();
        newCtx.tracks = au::au3::Au3TrackList::Create(pProject);

        // Linear Effect preview optimised by pre-mixing to one track.
        // Generators need to generate per track.
        const bool isLinearEffect = effect.IsLinearEffect();
        if (isLinearEffect && !isGenerator) {
            auto newTrack = MixAndRender(
                originCtx.tracks->Selected<const au::au3::Au3WaveTrack>(),
                Mixer::WarpOptions { pProject },
                wxString {}, // Don't care about the name of the temporary tracks
                effect.mFactory, effect.mProjectRate, floatSample, newCtx.t0, newCtx.t1);

            if (!newTrack) {
                return muse::make_ret(muse::Ret::Code::InternalError);
            }

            newCtx.tracks->Add(newTrack);

            newTrack->MoveTo(0);
            newTrack->SetSelected(true);
            std::static_pointer_cast<WaveTrack>(newTrack)->SetMute(false);
        } else {
            if (effect.mNumTracks > 0) {
                for (const WaveTrack* src : originCtx.tracks->Selected<const au::au3::Au3WaveTrack>()) {
                    const auto dest = std::static_pointer_cast<WaveTrack>(src->Copy(newCtx.t0, newCtx.t1));
                    dest->SetSelected(true);
                    dest->SetMute(false);
                    newCtx.tracks->Add(dest);
                }
            } else {
                // If we are previewing a generator without a given track, we can just give mono.
                constexpr auto nChannels = 1;
                const auto dest = WaveTrackFactory::Get(*pProject).Create(nChannels);
                dest->SetSelected(true);
                const auto tempo = ProjectTimeSignature::Get(*pProject).GetTempo();
                DoProjectTempoChange(*dest, tempo);
                newCtx.tracks->Add(dest);
            }
        }
    }

    // NEW tracks start at time zero.
    // Adjust T0 and T1 to be the times to process, and to
    // play back in these tracks
    double startOffset = newCtx.t0;
    newCtx.t1 -= newCtx.t0;
    newCtx.t0 = 0.0;

    //! ============================================================================
    //! NOTE Step 4 - process
    //! ============================================================================
    {
        using namespace BasicUI;
        auto progress = MakeProgress(
            effect.GetName(),
            XO("Preparing preview"),
            ProgressShowStop
            ); // Have only "Stop" button.

        newCtx.preparingPreviewProgress = progress.get();
        newCtx.isPreview = true;

        // apply new context
        {
            effect.mT0 = newCtx.t0;
            effect.mT1 = newCtx.t1;
            effect.mTracks = newCtx.tracks;
            effect.mProgress = newCtx.preparingPreviewProgress;
            effect.mIsPreview = newCtx.isPreview;

            // Update track/group counts
            effect.CountWaveTracks();
        }

        // Apply effect to new tracks

        auto pInstance = std::dynamic_pointer_cast<EffectInstanceEx>(effect.MakeInstance());
        IF_ASSERT_FAILED(pInstance) {
            return muse::make_ret(muse::Ret::Code::InternalError);
        }

        bool success = pInstance->Process(settings);
        if (!success) {
            return muse::make_ret(muse::Ret::Code::InternalError);
        }

        // Time-warping effects (e.g. Paulstretch) may update mT1 during
        // Process() to reflect the new selection end after stretching.
        // Read it back so the playback region covers the processed output.
        newCtx.t1 = effect.mT1;
    }

    //! ============================================================================
    //! NOTE Step 4 - play new processed tracks
    //! ============================================================================
    {
        playback::PlayTracksOptions opt;
        opt.selectedOnly = true;
        opt.startOffset = startOffset;
        opt.isDefaultPolicy = false;

        // Setting looping to `false` ensures that the loop region won't interfere with preview.
        // Also, looping is of course not effective during preview. Having it visually disabled
        // makes it clear to the user.
        const auto loopWasActive = player->isLoopRegionActive();
        player->setLoopRegionActive(false);
        player->setPlaybackRegion({ startOffset + newCtx.t0, startOffset + newCtx.t1 });

        m_effectPreviewState.emplace(effectId, originCtx, newCtx.tracks, loopWasActive);

        // Listen for playback stop to clean up preview state when audio finishes
        // before user manually stops it.
        player->playbackStatusChanged().onReceive(this, [this](playback::PlaybackStatus status) {
            if (status == playback::PlaybackStatus::Stopped) {
                // Wait for other observers of this signal to be finished, in particular the audio engine,
                // which should stop playback synchronously. Only then we may delete the preview tracks.
                async::Async::call(this, [this] {
                    stopPreview();
                });
            }
        });

        muse::Ret ret = player->playTracks(*newCtx.tracks, newCtx.t0, newCtx.t1, opt);
        if (!ret) {
            player->playbackStatusChanged().disconnect(this);
            m_effectPreviewState.reset();
            return ret;
        }
    }

    return muse::make_ok();
}

void EffectExecutionScenario::stopPreview()
{
    if (!m_effectPreviewState) {
        return;
    }

    const auto player = playback()->player();

    IF_ASSERT_FAILED(player) {
        return;
    }

    player->playbackStatusChanged().disconnect(this);

    if (player->playbackStatus() == playback::PlaybackStatus::Running) {
        player->stop();
    }

    EffectBase* effect = effectsProvider()->effect(m_effectPreviewState->effectId);
    if (effect) {
        const EffectContext& originCtx = m_effectPreviewState->originContext;
        effect->mT0 = originCtx.t0;
        effect->mT1 = originCtx.t1;
        effect->mTracks = originCtx.tracks;
        effect->mProgress = originCtx.preparingPreviewProgress;
        effect->mIsPreview = originCtx.isPreview;
    }

    player->setLoopRegionActive(m_effectPreviewState->loopWasActive);

    m_effectPreviewState.reset();
}
