/*
* Audacity: A Digital Audio Editor
*/
#include "effectsprovider.h"
#include "effecterrors.h"

#include "global/translation.h"
#include "au3wrap/internal/domconverter.h"
#include "au3wrap/internal/wxtypes_convert.h"
#include "au3wrap/internal/progressdialog.h"

#include "libraries/lib-effects/Effect.h"
#include "libraries/lib-components/EffectInterface.h"
#include "libraries/lib-effects/EffectManager.h"
#include "libraries/lib-effects/MixAndRender.h"
#include "libraries/lib-numeric-formats/ProjectTimeSignature.h"
#include "libraries/lib-stretching-sequence/TempoChange.h"
#include "libraries/lib-realtime-effects/RealtimeEffectState.h"
#include "libraries/lib-wave-track/WaveTrack.h"
#include "libraries/lib-transactions/TransactionScope.h"
#include "libraries/lib-exceptions/AudacityException.h"

#include "libraries/lib-module-manager/PluginManager.h" // for NYQUIST_PROMPT_ID
#include "libraries/lib-basic-ui/BasicUI.h"

#include "libraries/lib-audio-io/AudioIO.h"

#include "au3wrap/au3types.h"
#include "playback/iplayer.h"

#include "log.h"

using namespace muse;
using namespace au::effects;

static const char16_t* REALTIME_VIEWER_URI
    = u"audacity://effects/realtime_viewer?type=%1&instanceId=%2&effectState=%3&sync=false&modal=false&floating=true";

bool EffectsProvider::isVstSupported() const
{
    return vstEffectsRepository() ? true : false;
}

bool EffectsProvider::isNyquistSupported() const
{
    return nyquistEffectsRepository() ? true : false;
}

void EffectsProvider::reloadEffects()
{
    m_effects.clear();
    m_effectsCategories.clear();

    // built-in
    {
        EffectMetaList metaList = builtinEffectsRepository()->effectMetaList();
        for (EffectMeta meta : metaList) {
            m_effects.push_back(std::move(meta));
        }
    }

    // VST
    if (isVstSupported()) {
        EffectMetaList metaList = vstEffectsRepository()->effectMetaList();
        for (EffectMeta meta : metaList) {
            m_effects.push_back(std::move(meta));
        }
    }

    // Nyquist
    if (isNyquistSupported()) {
        EffectMetaList metaList = nyquistEffectsRepository()->effectMetaList();
        for (EffectMeta meta : metaList) {
            m_effects.push_back(std::move(meta));
        }
    }

    m_effectsChanged.notify();
}

EffectMetaList EffectsProvider::effectMetaList() const
{
    return m_effects;
}

muse::async::Notification EffectsProvider::effectMetaListChanged() const
{
    return m_effectsChanged;
}

EffectCategoryList EffectsProvider::effectsCategoryList() const
{
    EffectCategoryList list;
    list.push_back({ BUILTIN_CATEGORY_ID, muse::mtrc("effects", "Built-in") });
    if (isVstSupported()) {
        list.push_back({ VST_CATEGORY_ID, muse::mtrc("effects", "VST") });
    }

    return list;
}

EffectMeta EffectsProvider::meta(const EffectId& effectId) const
{
    for (const EffectMeta& meta : m_effects) {
        if (meta.id == effectId) {
            return meta;
        }
    }

    LOGE() << "not found meta: " << effectId;
    return EffectMeta();
}

std::string EffectsProvider::effectName(const std::string& effectId) const
{
    const auto desc = PluginManager::Get().GetPlugin(effectId);
    if (!desc) {
        return "";
    }
    return desc->GetSymbol().Msgid().Translation().ToStdString();
}

std::string EffectsProvider::effectName(const effects::RealtimeEffectState& state) const
{
    return effectName(state.GetID().ToStdString());
}

std::string EffectsProvider::effectSymbol(const std::string& effectId) const
{
    const auto desc = PluginManager::Get().GetPlugin(effectId);
    if (!desc) {
        return "";
    }
    return desc->GetSymbol().Internal().ToStdString();
}

bool EffectsProvider::supportsMultipleClipSelection(const EffectId& effectId) const
{
    for (const EffectMeta& meta : m_effects) {
        if (meta.id == effectId) {
            return meta.supportsMultipleClipSelection;
        }
    }

    LOGE() << "not found meta: " << effectId;
    return false;
}

Effect* EffectsProvider::effect(const EffectId& effectId) const
{
    PluginID pluginID = effectId.toStdString();
    const PluginDescriptor* plug = PluginManager::Get().GetPlugin(pluginID);
    if (!plug || !PluginManager::IsPluginAvailable(*plug)) {
        LOGE() << "plugin not available, effectId: " << effectId;
        return nullptr;
    }

    Effect* effect = dynamic_cast<Effect*>(EffectManager::Get().GetEffect(pluginID));
    IF_ASSERT_FAILED(effect) {
        LOGE() << "effect not available, effectId: " << effectId;
        return nullptr;
    }

    return effect;
}

muse::Ret EffectsProvider::showEffect(const EffectId& effectId, const EffectInstanceId& instanceId)
{
    LOGD() << "try open effect: " << effectId << ", instanceId: " << instanceId;

    PluginID pluginID = effectId.toStdString();
    const PluginDescriptor* plug = PluginManager::Get().GetPlugin(pluginID);
    if (!plug || !PluginManager::IsPluginAvailable(*plug)) {
        LOGE() << "plugin not available, effectId: " << effectId;
        return muse::make_ret(muse::Ret::Code::UnknownError);
    }

    std::string family = au3::wxToStdSting(plug->GetEffectFamily());

    LOGD() << "effect family: " << family;

    IEffectViewLauncherPtr launcher = viewLaunchRegister()->launcher(family);
    IF_ASSERT_FAILED(launcher) {
        LOGE() << "not found launcher for family:" << family;
        return muse::make_ret(muse::Ret::Code::NotSupported);
    }

    Ret ret = launcher->showEffect(effectId, instanceId);

    LOGD() << "open ret: " << ret.toString();
    return ret;
}

void EffectsProvider::showEffect(const RealtimeEffectStatePtr& state) const
{
    const auto effectId = state->GetID().ToStdString();
    const auto type = muse::String::fromStdString(effectSymbol(effectId));
    const auto instance = std::dynamic_pointer_cast<effects::EffectInstance>(state->GetInstance());
    if (!instance) {
        LOGW() << "Could not get instance for " << effectId;
        return;
    }
    const auto instanceId = instance->id();

    const UriQuery query{ String(REALTIME_VIEWER_URI).arg(type).arg(size_t(instanceId)).arg(size_t(state.get())) };

    // If the dialog for this specific instance is opened, just raise it.
    if (interactive()->isOpened(query).val) {
        // Note: at the time of writing, `raise` doesn't seem to be working for QML dialogs (although it does for QtWidget dialogs)
        // Some changes are needed in the Muse framework.
        interactive()->raise(query);
        return;
    }

    // At the time of writing, despite the `alwaysOnTop: true` property set on the dialog, whenever a new dialog is spawned,
    // the other dialog isn't always on top anymore. UX-wise this is very confusing, so until this problem is solved in the framework,
    // we only allow one effect dialog open at all times.
    const Uri genericUri { REALTIME_VIEWER_URI };
    if (interactive()->isOpened(genericUri).val) {
        interactive()->close(genericUri);
    }

    interactive()->open(query);
}

void EffectsProvider::hideEffect(const RealtimeEffectStatePtr& state) const
{
    const auto effectId = state->GetID().ToStdString();
    const auto type = muse::String::fromStdString(effectSymbol(effectId));
    const auto instance = std::dynamic_pointer_cast<effects::EffectInstance>(state->GetInstance());
    if (!instance) {
        LOGW() << "Could not get instance for " << effectId;
        return;
    }
    const auto instanceId = instance->id();

    const UriQuery query{ String(REALTIME_VIEWER_URI).arg(type).arg(size_t(instanceId)).arg(size_t(state.get())) };

    if (interactive()->isOpened(query).val) {
        interactive()->close(query);
    }
}

void EffectsProvider::toggleShowEffect(const RealtimeEffectStatePtr& state) const
{
    const auto effectId = state->GetID().ToStdString();
    const auto type = muse::String::fromStdString(effectSymbol(effectId));
    const auto instance = std::dynamic_pointer_cast<effects::EffectInstance>(state->GetInstance());
    if (!instance) {
        // This could happen if e.g. a VST plugin was uninstalled since the last run.
        LOGW() << "Could not get instance for " << effectId;
        return;
    }
    const auto instanceId = instance->id();

    const UriQuery query{ String(REALTIME_VIEWER_URI).arg(type).arg(size_t(instanceId)).arg(size_t(state.get())) };

    if (interactive()->isOpened(query).val) {
        interactive()->close(query);
    } else {
        showEffect(state);
    }
}

muse::Ret EffectsProvider::performEffect(au3::Au3Project& project, Effect* effect, std::shared_ptr<EffectInstance> pInstanceEx,
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
            ::ProgressDialog progress{};
            auto vr = valueRestorer<BasicUI::ProgressDialog*>(effect->mProgress, &progress);

            assert(pInstanceEx); // null check above
            try {
                if (pInstanceEx->Process(settings) == false) {
                    if (progress.cancelled()) {
                        success = make_ret(Err::EffectProcessCancelled);
                    } else {
                        success = make_ret(Err::EffectProcessFailed, pInstanceEx->GetLastError());
                    }
                }
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
        if (success) {
            if (newTrack) {
                const auto trackeditProject = globalContext()->currentTrackeditProject();
                trackeditProject->notifyAboutTrackAdded(au3::DomConverter::track(*effect->mTracks->rbegin()));
            }
        } else if (newTrack) {
            effect->mTracks->Remove(*newTrack);
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

muse::Ret EffectsProvider::doEffectPreview(EffectBase& effect, EffectSettings& settings)
{
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
    struct EffectContext {
        double t0 = 0.0;
        double t1 = 0.0;
        std::shared_ptr<TrackList> tracks;
        BasicUI::ProgressDialog* progress = nullptr;
        bool isPreview = false;
    };

    const EffectContext originCtx = { effect.mT0, effect.mT1, effect.mTracks, effect.mProgress, effect.mIsPreview };
    auto restoreCtx = finally([&] {
        effect.mT0 = originCtx.t0;
        effect.mT1 = originCtx.t1;
        effect.mTracks = originCtx.tracks;
        effect.mProgress = originCtx.progress;
        effect.mIsPreview = originCtx.isPreview;
    });

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

    //const bool previewFullSelection = effect.PreviewsFullSelection(); not used at the moment
    const double previewLen = originCtx.t1 - originCtx.t0;
    double previewDuration = 0.0;
    if (isNyquist && isGenerator) {
        previewDuration = effect.CalcPreviewInputLength(settings, previewLen);
    } else {
        previewDuration = std::min(settings.extra.GetDuration(), effect.CalcPreviewInputLength(settings, previewLen));
    }

    newCtx.t0 = originCtx.t0;
    newCtx.t1 = originCtx.t0 + previewDuration;
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
        } else {
            if (effect.mNumTracks > 0) {
                for (const WaveTrack* src : originCtx.tracks->Selected<const au::au3::Au3WaveTrack>()) {
                    auto dest = src->Copy(newCtx.t0, newCtx.t1);
                    dest->SetSelected(true);
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

        newCtx.progress = progress.get();
        newCtx.isPreview = true;

        // apply new context
        {
            effect.mT0 = newCtx.t0;
            effect.mT1 = newCtx.t1;
            effect.mTracks = newCtx.tracks;
            effect.mProgress = newCtx.progress;
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
    }

    //! ============================================================================
    //! NOTE Step 4 - play new processed tracks
    //! ============================================================================
    {
        playback::PlayTracksOptions opt;
        opt.selectedOnly = true;
        opt.startOffset = startOffset;
        opt.isDefaultPolicy = false;

        muse::Ret ret = player->playTracks(*newCtx.tracks, newCtx.t0, newCtx.t1, opt);
        if (!ret) {
            return ret;
        }

        using namespace BasicUI;

        // The progress dialog must be deleted before stopping the stream
        // to allow events to flow to the app during StopStream processing.
        // The progress dialog blocks these events.
        {
            auto progress = MakeProgress(effect.GetName(), XO("Previewing"), ProgressShowStop);
            while (player->isRunning()) {
                using namespace std::chrono;
                std::this_thread::sleep_for(100ms);
                muse::secs_t playPos = player->playbackPosition() - startOffset;
                auto previewing = progress->Poll(playPos, newCtx.t1);

                if (previewing != BasicUI::ProgressResult::Success || player->reachedEnd().val) {
                    break;
                }
            }

            player->stop();
        }
    }

    return muse::make_ok();
}

muse::Ret EffectsProvider::previewEffect(au3::Au3Project&, Effect* effect, EffectSettings& settings)
{
    return doEffectPreview(*effect, settings);
}
