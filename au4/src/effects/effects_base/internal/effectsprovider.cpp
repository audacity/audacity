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

muse::Ret EffectsProvider::performEffect(AudacityProject& project, Effect* effect, std::shared_ptr<EffectInstance> pInstanceEx,
                                         EffectSettings& settings)
{
    //! ============================================================================
    //! NOTE Step 1 - add new a track if need
    //! ============================================================================

    // common things used below
    WaveTrack* newTrack = nullptr;
    {
        // We don't yet know the effect type for code in the Nyquist Prompt, so
        // assume it requires a track and handle errors when the effect runs.
        if ((effect->GetType() == EffectTypeGenerate || effect->GetPath() == NYQUIST_PROMPT_ID) && (effect->mNumTracks == 0)) {
            auto track = effect->mFactory->Create();
            track->SetName(effect->mTracks->MakeUniqueTrackName(WaveTrack::GetDefaultAudioTrackNamePreference()));
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
