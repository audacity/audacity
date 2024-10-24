/*
* Audacity: A Digital Audio Editor
*/
#include "effectsprovider.h"

#include "global/translation.h"

#include "libraries/lib-effects/Effect.h"
#include "libraries/lib-components/EffectInterface.h"
#include "libraries/lib-effects/EffectManager.h"
#include "libraries/lib-effects/MixAndRender.h"
#include "libraries/lib-wave-track/WaveTrack.h"
#include "libraries/lib-transactions/TransactionScope.h"

#include "libraries/lib-module-manager/PluginManager.h" // for NYQUIST_PROMPT_ID
#include "libraries/lib-basic-ui/BasicUI.h"

#include "libraries/lib-audio-io/AudioIO.h"
#include "libraries/lib-stretching-sequence/StretchingSequence.h"
#include "libraries/lib-audio-io/ProjectAudioIO.h"

#include "au3wrap/au3types.h"
#include "playback/iplayer.h"

#include "log.h"

using namespace muse;
using namespace au::effects;

static const char16_t* VIEWER_URI = u"audacity://effects/viewer?type=%1&instanceId=%2";

static const int UNDEFINED_FREQUENCY = -1;

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

    // build-in
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

EffectMeta EffectsProvider::meta(const muse::String& effectId) const
{
    for (const EffectMeta& meta : m_effects) {
        if (meta.id == effectId) {
            return meta;
        }
    }

    LOGE() << "not found meta: " << effectId;
    return EffectMeta();
}

muse::Ret EffectsProvider::showEffect(const muse::String& type, const EffectInstanceId& instanceId)
{
    LOGD() << "try open effect: " << type << ", instanceId: " << instanceId;

    RetVal<Val> rv = interactive()->open(String(VIEWER_URI).arg(type).arg(size_t(instanceId)).toStdString());

    LOGD() << "open ret: " << rv.ret.toString();

    return rv.ret;
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
            newTrack = effect->mTracks->Add(track);
            newTrack->SetSelected(true);
        }
    }

    //! ============================================================================
    //! NOTE Step 2 - process
    //! ============================================================================

    // common things used below
    bool success = false;
    {
        //! NOTE Step 2.3 - open transaction
        TransactionScope trans(project, "Effect");

        //! NOTE Step 2.4 - do process

        //! TODO It is not clear what the skip flag is and why it can be set,
        //! in what cases when calling this function
        //! it is not necessary to call the main thing - the process
        bool returnVal = true;
        bool skipFlag = static_cast<EffectBase*>(effect)->CheckWhetherSkipEffect(settings);
        if (skipFlag == false) {
            using namespace BasicUI;
            auto name = effect->GetName();
            auto progress = MakeProgress(
                name,
                XO("Applying %s...").Format(name),
                ProgressShowCancel
                );
            auto vr = valueRestorer(effect->mProgress, progress.get());

            assert(pInstanceEx); // null check above
            returnVal = pInstanceEx->Process(settings);
        }

        success = returnVal;

        //! NOTE Step 2.5 - commit transaction on success
        if (success) {
            trans.Commit();
        }
    }

    //! ============================================================================
    //! NOTE Step 3 - cleanup
    //! ============================================================================

    {
        if (!success) {
            if (newTrack) {
                effect->mTracks->Remove(*newTrack);
            }
        }
    }

    return success ? muse::make_ok() : muse::make_ret(Ret::Code::UnknownError);
}

void EffectsProvider::doEffectPreview(EffectBase& effect,
                                      EffectSettingsAccess& access, std::function<void()> updateUI, bool dryOnly)
{
    auto cleanup0 = effect.BeginPreview(access.Get());

    // These are temporary state in the Effect object that are meant be moved to
    // a new class EffectContext
    const auto numTracks = effect.mNumTracks;
    const auto rate = effect.mProjectRate;
    const auto& factory = effect.mFactory;
    auto& mT0 = effect.mT0;
    auto& mT1 = effect.mT1;
    auto& mTracks = effect.mTracks;
    auto& mProgress = effect.mProgress;
    auto& mIsPreview = effect.mIsPreview;

    // Get certain immutable properties of the effect
    const auto previewFullSelection = effect.PreviewsFullSelection();
    const auto isLinearEffect = effect.IsLinearEffect();

    if (numTracks == 0) { // nothing to preview
        return;
    }

    auto player = playback()->player();
    if (!player->canPlay()) {
        LOGW() << "can't play, maybe audio is busy";
        return;
    }

    const auto FocusDialog = BasicUI::FindFocus();
    assert(FocusDialog); // postcondition

    double previewDuration;
    bool isNyquist = effect.GetFamily() == NYQUISTEFFECTS_FAMILY;
    bool isGenerator = effect.GetType() == EffectTypeGenerate;

    // Mix a few seconds of audio from all of the tracks
    double previewLen;
    gPrefs->Read(wxT("/AudioIO/EffectsPreviewLen"), &previewLen, 6.0);

    const auto& settings = access.Get();
    if (isNyquist && isGenerator) {
        previewDuration = effect.CalcPreviewInputLength(settings, previewLen);
    } else {
        previewDuration = std::min(settings.extra.GetDuration(),
                                   effect.CalcPreviewInputLength(settings, previewLen));
    }

    double t1 = mT0 + previewDuration;

    if ((t1 > mT1) && !isGenerator) {
        t1 = mT1;
    }

    if (t1 <= mT0) {
        return;
    }

    bool success = true;

    auto cleanup = finally([&] {
        // Effect is already inited; we will call Process and then Init
        // again, so the state is exactly the way it was before Preview
        // was called.
        if (!dryOnly) {
            // TODO remove this reinitialization of state within the Effect object
            // It is done indirectly via Effect::Instance
            if (auto pInstance = std::dynamic_pointer_cast<EffectInstanceEx>(effect.MakeInstance())) {
                pInstance->Init();
            }
        }
    });

    auto vr0 = valueRestorer(mT0);
    auto vr1 = valueRestorer(mT1);
    // Most effects should stop at t1.
    if (!previewFullSelection) {
        mT1 = t1;
    }

    // In case any dialog control depends on mT1 or mDuration:
    if (updateUI) {
        updateUI();
    }

    // Save the original track list
    auto saveTracks = mTracks;

    auto cleanup2 = finally([&] {
        mTracks = saveTracks;
        if (*FocusDialog) {
            BasicUI::SetFocus(*FocusDialog);
        }
    });

    // Build NEW tracklist from rendering tracks
    // Set the same owning project, so FindProject() can see it within Process()
    const auto pProject = saveTracks->GetOwner();
    mTracks = au::au3::Au3TrackList::Create(pProject);

    // Linear Effect preview optimised by pre-mixing to one track.
    // Generators need to generate per track.
    if (isLinearEffect && !isGenerator) {
        auto newTrack = MixAndRender(
            saveTracks->Selected<const au::au3::Au3WaveTrack>(),
            Mixer::WarpOptions { saveTracks->GetOwner() },
            wxString {}, // Don't care about the name of the temporary tracks
            factory, rate, floatSample, mT0, t1);
        if (!newTrack) {
            return;
        }
        mTracks->Add(newTrack);

        newTrack->MoveTo(0);
        newTrack->SetSelected(true);
    } else {
        for (auto src : saveTracks->Selected<const au::au3::Au3WaveTrack>()) {
            auto dest = src->Copy(mT0, t1);
            dest->SetSelected(true);
            mTracks->Add(dest);
        }
    }

    // NEW tracks start at time zero.
    // Adjust mT0 and mT1 to be the times to process, and to
    // play back in these tracks
    double startOffset = mT0;
    mT1 -= mT0;
    mT0 = 0.0;

    // Update track/group counts
    effect.CountWaveTracks();

    // Apply effect
    if (!dryOnly) {
        using namespace BasicUI;
        auto progress = MakeProgress(
            effect.GetName(),
            XO("Preparing preview"),
            ProgressShowStop
            ); // Have only "Stop" button.
        auto vr = valueRestorer(mProgress, progress.get());

        auto vr2 = valueRestorer(mIsPreview, true);

        access.ModifySettings([&](EffectSettings& settings){
            // Preview of non-realtime effect
            auto pInstance = std::dynamic_pointer_cast<EffectInstanceEx>(effect.MakeInstance());
            success = pInstance && pInstance->Process(settings);
            return nullptr;
        });
    }

    if (success) {
        // Some effects (Paulstretch) may need to generate more
        // than previewLen, so take the min.
        t1 = std::min(mT0 + previewLen, mT1);

        // Start audio playing
        playback::PlayTracksOptions opt = { .selectedOnly = true,
                                            .startOffset = startOffset };
        muse::Ret ret = player->playTracks(*mTracks, mT0, t1, opt);

        if (ret) {
            using namespace BasicUI;
            auto previewing = BasicUI::ProgressResult::Success;
            // The progress dialog must be deleted before stopping the stream
            // to allow events to flow to the app during StopStream processing.
            // The progress dialog blocks these events.
            {
                auto progress = MakeProgress(effect.GetName(), XO("Previewing"), ProgressShowStop);

                while (player->isRunning() && previewing == BasicUI::ProgressResult::Success) {
                    using namespace std::chrono;
                    std::this_thread::sleep_for(100ms);
                    previewing = progress->Poll(player->playbackPosition() - mT0 - startOffset, t1 - mT0);
                }
            }

            player->stop();

            while (!player->canPlay()) {
                using namespace std::chrono;
                std::this_thread::sleep_for(100ms);
            }
        } else {
            using namespace BasicUI;
            ShowErrorDialog(
                *FocusDialog, XO("Error"),
                XO("Error opening sound device.\nTry changing the audio host, playback device and the project sample rate."),
                wxT("Error_opening_sound_device"),
                ErrorDialogOptions { ErrorDialogType::ModalErrorReport });
        }
    }
}

muse::Ret EffectsProvider::previewEffect(au3::Au3Project&, Effect* effect, EffectSettings& settings)
{
    std::shared_ptr<SimpleEffectSettingsAccess> pAccess = std::make_shared<SimpleEffectSettingsAccess>(settings);
    doEffectPreview(*effect, *pAccess.get(), nullptr, false);

    return muse::make_ok();
}
