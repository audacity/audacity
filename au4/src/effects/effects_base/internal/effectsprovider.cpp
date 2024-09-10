/*
* Audacity: A Digital Audio Editor
*/
#include "effectsprovider.h"

#include "global/translation.h"

#include "libraries/lib-effects/Effect.h"
#include "libraries/lib-components/EffectInterface.h"
#include "libraries/lib-audacity-application-logic/EffectManager.h"
#include "libraries/lib-wave-track/WaveTrack.h"
#include "libraries/lib-transactions/TransactionScope.h"

#include "libraries/lib-module-manager/PluginManager.h" // for NYQUIST_PROMPT_ID
#include "libraries/lib-basic-ui/BasicUI.h"

#include "log.h"

using namespace muse;
using namespace au::effects;

static const char16_t* VIEWER_URI = u"audacity://effects/viewer?type=%1&instanceId=%2";

static const int UNDEFINED_FREQUENCY = -1;

bool EffectsProvider::isVstSupported() const
{
    return vstEffectsRepository() ? true : false;
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

    // vst
    if (isVstSupported()) {
        EffectMetaList metaList = vstEffectsRepository()->effectMetaList();
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
    list.push_back({ BUILTIN_CATEGORY_ID, muse::mtrc("effects", "Build-in") });
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

muse::Ret EffectsProvider::performEffect(AudacityProject& project, Effect* effect, EffectSettings& settings,
                                         const EffectTimeParams& timeParams)
{
    unsigned flags = 0; // not used at the monent

    //! ============================================================================
    //! NOTE Step 1 - setup effect
    //! ============================================================================

    // common things used below
    unsigned oldFlags = 0;
    WaveTrack* newTrack = nullptr;

    {
        //! NOTE Step 1.1 - setup effect
        oldFlags = effect->mUIFlags;
        effect->mUIFlags = flags;
        effect->mFactory = &WaveTrackFactory::Get(project);
        effect->mProjectRate = timeParams.projectRate;
        effect->mT0 = timeParams.t0;
        effect->mT1 = timeParams.t1;

        effect->SetTracks(&TrackList::Get(project));
        // Update track/group counts
        effect->CountWaveTracks();

        //! NOTE Step 1.2 - add new a track if need
        // We don't yet know the effect type for code in the Nyquist Prompt, so
        // assume it requires a track and handle errors when the effect runs.
        if ((effect->GetType() == EffectTypeGenerate || effect->GetPath() == NYQUIST_PROMPT_ID) && (effect->mNumTracks == 0)) {
            auto track = effect->mFactory->Create();
            track->SetName(effect->mTracks->MakeUniqueTrackName(WaveTrack::GetDefaultAudioTrackNamePreference()));
            newTrack = effect->mTracks->Add(track);
            newTrack->SetSelected(true);
        }

        //! NOTE Step 1.3 - check frequency params
        effect->mF0 = timeParams.f0;
        effect->mF1 = timeParams.f1;
        if (effect->mF0 != UNDEFINED_FREQUENCY) {
            effect->mPresetNames.push_back(L"control-f0");
        }
        if (effect->mF1 != UNDEFINED_FREQUENCY) {
            effect->mPresetNames.push_back(L"control-f1");
        }
    }

    //! ============================================================================
    //! NOTE Step 2 - process
    //! ============================================================================

    // common things used below
    bool success = false;
    {
        //! NOTE Step 2.1 - find instance
        std::shared_ptr<EffectInstanceEx> pInstanceEx;

        //! TODO It is not obvious why we only look for an instance
        //! for interactive and unconfigured effects,
        //! and always create a new one for the others
        if (effect->IsInteractive() && (flags& EffectManager::kConfigured) == 0) {
            const std::optional<EffectPlugin::InstancePointer> result = EffectBase::FindInstance(*effect);
            if (result.has_value()) {
                pInstanceEx = *result;
            }
        }

        //! NOTE Step 2.2 - make new instance if not found
        if (!pInstanceEx) {
            // Path that skipped the dialog factory -- effect may be non-interactive
            // or this is batch mode processing or repeat of last effect with stored
            // settings.
            pInstanceEx = std::dynamic_pointer_cast<EffectInstanceEx>(effect->MakeInstance());
            // Note: Init may read parameters from preferences
            if (!pInstanceEx || !pInstanceEx->Init()) {
                return muse::make_ret(Ret::Code::InternalError);
            }
        }

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

        // Don't hold a dangling pointer when done
        effect->SetTracks(nullptr);
        effect->mPresetNames.clear();
        effect->mUIFlags = oldFlags;
    }

    return success ? muse::make_ok() : muse::make_ret(Ret::Code::UnknownError);
}
