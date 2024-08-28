/*
* Audacity: A Digital Audio Editor
*/
#include "effectexecutionscenario.h"

#include "global/realfn.h"

#include "libraries/lib-project/Project.h"
//#include "libraries/lib-audacity-application-logic/AudacityApplicationLogic.h"
#include "libraries/lib-effects/Effect.h"
#include "libraries/lib-module-manager/PluginManager.h"
#include "libraries/lib-command-parameters/ShuttleAutomation.h"

#include "libraries/lib-track/Track.h"
#include "libraries/lib-wave-track/WaveTrack.h"
#include "libraries/lib-project-rate/ProjectRate.h"
#include "libraries/lib-time-frequency-selection/ViewInfo.h"
#include "libraries/lib-menus/CommandManager.h"
#include "libraries/lib-audacity-application-logic/EffectManager.h"
#include "libraries/lib-project-history/ProjectHistory.h"
#include "libraries/lib-transactions/TransactionScope.h"
#include "libraries/lib-module-manager/ConfigInterface.h"
#include "libraries/lib-numeric-formats/NumericConverterFormats.h"
#include "libraries/lib-basic-ui/BasicUI.h"

#include "au3wrap/internal/wxtypes_convert.h"

using namespace muse;
using namespace au::effects;

muse::Ret EffectExecutionScenario::performEffect(const EffectId& effectId)
{
    AudacityProject& project = *reinterpret_cast<::AudacityProject*>(globalContext()->currentProject()->au3ProjectPtr());
    return doPerformEffect(project, effectId, 0);
}

muse::Ret EffectExecutionScenario::doPerformEffect(AudacityProject& project, const EffectId& effectId, unsigned flags)
{
    PluginID ID = effectId.toStdString();
    EffectType type = EffectType::EffectTypeNone;

    //! NOTE Step 1 - check that the effect is present and available
    {
        const PluginDescriptor* plug = PluginManager::Get().GetPlugin(ID);
        if (!plug || !PluginManager::IsPluginAvailable(*plug)) {
            return muse::make_ret(Ret::Code::UnknownError);
        }

        //! NOTE Step 1.2 get effect type
        type = plug->GetEffectType();
    }

    //! NOTE Step 2 - stop play, select all if need
    // Make sure there's no activity since the effect is about to be applied
    // to the project's tracks.  Mainly for Apply during RTP, but also used
    // for batch commands
    if (flags & EffectManager::kConfigured) {
        // DO stopPlayback;

        // Don't Select All if repeating Generator Effect
        if (!(flags & EffectManager::kConfigured)) {
            // DO selectAllIfNone;
        }
    }

    //! NOTE Step 3 - get effect (maybe better to change step 2 and step 3)
    EffectManager& em = EffectManager::Get();
    Effect* effect = dynamic_cast<Effect*>(em.GetEffect(ID));
    IF_ASSERT_FAILED(effect) {
        return muse::make_ret(Ret::Code::InternalError);
    }

    //! TODO
    em.SetSkipStateFlag(false);

    //! NOTE Step 4 - get effect settings
    EffectSettings* pSettings = em.GetDefaultSettings(ID);
    IF_ASSERT_FAILED(pSettings) {
        return muse::make_ret(Ret::Code::InternalError);
    }
    const auto pAccess = std::make_shared<SimpleEffectSettingsAccess>(*pSettings);

    //! NOTE Step 5 - setup effect

    NotifyingSelectedRegion& selectedRegion = ViewInfo::Get(project).selectedRegion;
    IF_ASSERT_FAILED(muse::RealIsEqualOrMore(selectedRegion.duration(), 0.0)) {
        return muse::make_ret(Ret::Code::InternalError);
    }

    auto cleanup0 = valueRestorer(effect->mUIFlags, flags);
    effect->mFactory = &WaveTrackFactory::Get(project);
    effect->mProjectRate = ProjectRate::Get(project).GetRate();

    effect->SetTracks(&TrackList::Get(project));
    // Don't hold a dangling pointer when done
    Finally Do([effect]{ effect->SetTracks(nullptr); });

    //! NOTE Step 6 - open transaction
    TransactionScope trans(project, "Effect");

    //! NOTE Step 7 - some pre action
    // Update track/group counts
    effect->CountWaveTracks();

    //! NOTE Step 8 - get duration for EffectTypeGenerate
    double duration = 0.0;
    if (effect->GetType() == EffectTypeGenerate) {
        GetConfig(effect->GetDefinition(), PluginSettings::Private,
                  CurrentSettingsGroup(),
                  EffectSettingsExtra::DurationKey(), duration, effect->GetDefaultDuration());
    }
    double oldDuration = duration;

    //! NOTE Step 9 - add new a track if need
    // We don't yet know the effect type for code in the Nyquist Prompt, so
    // assume it requires a track and handle errors when the effect runs.
    WaveTrack* newTrack = nullptr;
    if ((effect->GetType() == EffectTypeGenerate || effect->GetPath() == NYQUIST_PROMPT_ID) && (effect->mNumTracks == 0)) {
        auto track = effect->mFactory->Create();
        track->SetName(effect->mTracks->MakeUniqueTrackName(WaveTrack::GetDefaultAudioTrackNamePreference()));
        newTrack = effect->mTracks->Add(track);
        newTrack->SetSelected(true);
    }

    //! NOTE Step 10 - check selected time
    bool isSelection = false;
    effect->mT0 = selectedRegion.t0();
    effect->mT1 = selectedRegion.t1();
    if (effect->mT1 > effect->mT0) {
        // there is a selection: let's fit in there...
        // MJS: note that this is just for the TTC and is independent of the track rate
        // but we do need to make sure we have the right number of samples at the project rate
        double quantMT0 = QUANTIZED_TIME(effect->mT0, effect->mProjectRate);
        double quantMT1 = QUANTIZED_TIME(effect->mT1, effect->mProjectRate);
        duration = quantMT1 - quantMT0;
        isSelection = true;
        effect->mT1 = effect->mT0 + duration;
    }

    //! NOTE Step 11.1 - get local settings
    EffectSettings settings = pAccess->Get();

    //! NOTE Step 11.2 - update settings
    wxString newFormat = (isSelection
                          ? NumericConverterFormats::TimeAndSampleFormat()
                          : NumericConverterFormats::DefaultSelectionFormat()
                          ).Internal();
    auto updater = [&](EffectSettings& settings) {
        settings.extra.SetDuration(duration);
        settings.extra.SetDurationFormat(newFormat);
        return nullptr;
    };
    // Update our copy of settings; update the EffectSettingsAccess too,
    // if we are going to show a dialog
    updater(settings);
    if (pAccess) {
        pAccess->ModifySettings(updater);
    }

    //! NOTE Step 12 - check frequency params
    effect->mF0 = selectedRegion.f0();
    effect->mF1 = selectedRegion.f1();
    if (effect->mF0 != SelectedRegion::UndefinedFrequency) {
        effect->mPresetNames.push_back(L"control-f0");
    }
    if (effect->mF1 != SelectedRegion::UndefinedFrequency) {
        effect->mPresetNames.push_back(L"control-f1");
    }

    //! NOTE Step 13 - some pre action again
    effect->CountWaveTracks();

    //! NOTE Step 14 - find instance
    // Allow the dialog factory to fill this in, but it might not
    std::shared_ptr<EffectInstanceEx> pInstanceEx;

    // Prompting will be bypassed when applying an effect that has
    // already been configured, e.g. repeating the last effect on a
    // different selection.  Prompting may call EffectPreview
    if (effect->IsInteractive() && (flags& EffectManager::kConfigured) == 0) {
        const std::optional<EffectPlugin::InstancePointer> result = EffectBase::FindInstance(*effect);
        if (result.has_value()) {
            pInstanceEx = *result;
        }

        //! NOTE Step 14.2 - modify settings
        muse::String type = au3::wxToString(effect->GetSymbol().Internal());
        EffectInstanceId instanceId = effectInstancesRegister()->regInstance(effect);
        muse::Ret ret = effectsProvider()->showEffect(type, instanceId);
        effectInstancesRegister()->unregInstance(effect);
        if (ret) {
            effect->SaveUserPreset(CurrentSettingsGroup(), pAccess->Get());
        } else {
            LOGE() << "failed show effect: " << type << ", ret: " << ret.toString();
        }

        //! NOTE Step 14.3 - update local settings
        if (pInstanceEx && ret) {
            settings = pAccess->Get();
        }
    }

    //! NOTE Step 15 - make new instance if not found
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

    //! NOTE Step 16 - check skip and process
    // If the dialog was shown, then it has been closed without errors or
    // cancellation, and any change of duration has been saved in the config file

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

    bool success = returnVal;

    //! NOTE Step 17 - update selected region after process
    if (returnVal && (effect->mT1 >= effect->mT0)) {
        selectedRegion.setTimes(effect->mT0, effect->mT1);
    }

    //! NOTE Step 18 - commit transaction on success
    if (success) {
        trans.Commit();
    }

    //! NOTE Step 19 - cleanup
    if (!success) {
        if (newTrack) {
            effect->mTracks->Remove(*newTrack);
        }
        // On failure, restore the old duration setting
        settings.extra.SetDuration(oldDuration);
    }
    effect->mPresetNames.clear();

    //! NOTE Step 20 - update settings
    pAccess->Set(std::move(settings), nullptr);

    //! NOTE Step 21 - break if not success
    if (!success) {
        return muse::make_ret(Ret::Code::UnknownError);
    }

    //! NOTE Step 22 - write project history if need
    if (em.GetSkipStateFlag()) {
        flags = flags | EffectManager::kSkipState;
    }

    if (!(flags & EffectManager::kSkipState)) {
        auto shortDesc = PluginManager::Get().GetName(ID);
        const auto longDesc = XO("Applied effect: %s").Format(shortDesc);
        ProjectHistory::Get(project).PushState(longDesc, shortDesc);
    }

    //! NOTE Step 23 - remember a successful generator, effect, analyzer, or tool Process

    if (!(flags & EffectManager::kDontRepeatLast)) {
        auto& commandManager = CommandManager::Get(project);

        // Remember a successful generator, effect, analyzer, or tool Process
        auto shortDesc = PluginManager::Get().GetName(ID);
        /* i18n-hint: %s will be the name of the effect which will be
       * repeated if this menu item is chosen */
        auto lastEffectDesc = XO("Repeat %s").Format(shortDesc);
        switch (type) {
        case EffectTypeGenerate:
            commandManager.Modify(wxT("RepeatLastGenerator"), lastEffectDesc);
            commandManager.mLastGenerator = ID;
            commandManager.mRepeatGeneratorFlags = EffectManager::kConfigured;
            break;
        case EffectTypeProcess:
            commandManager.Modify(wxT("RepeatLastEffect"), lastEffectDesc);
            commandManager.mLastEffect = ID;
            commandManager.mRepeatEffectFlags = EffectManager::kConfigured;
            break;
        case EffectTypeAnalyze:
            commandManager.Modify(wxT("RepeatLastAnalyzer"), lastEffectDesc);
            commandManager.mLastAnalyzer = ID;
            commandManager.mLastAnalyzerRegistration = CommandManager::repeattypeplugin;
            commandManager.mRepeatAnalyzerFlags = EffectManager::kConfigured;
            break;
        case EffectTypeTool:
            commandManager.Modify(wxT("RepeatLastTool"), lastEffectDesc);
            commandManager.mLastTool = ID;
            commandManager.mLastToolRegistration = CommandManager::repeattypeplugin;
            commandManager.mRepeatToolFlags = EffectManager::kConfigured;
            if (shortDesc == NYQUIST_PROMPT_NAME) {
                commandManager.mRepeatToolFlags =EffectManager::kRepeatNyquistPrompt; // Nyquist Prompt is not configured
            }
            break;
        }
    }

    //! TODO There should probably be a saving of commandManager here, but it is not explicitly there.

    return true;
}
