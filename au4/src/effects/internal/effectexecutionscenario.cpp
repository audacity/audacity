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
    //! ============================================================================
    //! NOTE Step 1 - check input params (effect is present and available, selection)
    //! ============================================================================

    // common things used below
    PluginID ID = effectId.toStdString();
    EffectManager& em = EffectManager::Get();
    Effect* effect = nullptr;

    NotifyingSelectedRegion& selectedRegion = ViewInfo::Get(project).selectedRegion;
    bool isSelection = false;

    {
        //! NOTE Step 1.1 - check plugin
        const PluginDescriptor* plug = PluginManager::Get().GetPlugin(ID);
        if (!plug || !PluginManager::IsPluginAvailable(*plug)) {
            return muse::make_ret(Ret::Code::UnknownError);
        }

        //! NOTE Step 1.2 - get effect
        effect = dynamic_cast<Effect*>(em.GetEffect(ID));
        IF_ASSERT_FAILED(effect) {
            return muse::make_ret(Ret::Code::InternalError);
        }

        //! NOTE Step 1.3 - check selection

        IF_ASSERT_FAILED(muse::RealIsEqualOrMore(selectedRegion.duration(), 0.0)) {
            return muse::make_ret(Ret::Code::InternalError);
        }

        isSelection = selectedRegion.t1() > selectedRegion.t0();
        if (flags & EffectManager::kConfigured) {
            // Don't Select All if repeating Generator Effect
            if (!(flags & EffectManager::kConfigured)) {
                //! TODO
                // DO selectAllIfNone;
            }
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
    std::shared_ptr<SimpleEffectSettingsAccess> pAccess;
    EffectSettings settings;

    double t0 = 0.0;
    double t1 = 0.0;
    double oldDuration = 0.0;
    double projectRate = ProjectRate::Get(project).GetRate();

    {
        //! NOTE Step 2.1 - get effect settings
        EffectSettings* pSettings = em.GetDefaultSettings(ID);
        IF_ASSERT_FAILED(pSettings) {
            return muse::make_ret(Ret::Code::InternalError);
        }
        pAccess = std::make_shared<SimpleEffectSettingsAccess>(*pSettings);

        //! NOTE Step 2.2 - get oldDuration for EffectTypeGenerate
        if (effect->GetType() == EffectTypeGenerate) {
            GetConfig(effect->GetDefinition(), PluginSettings::Private,
                      CurrentSettingsGroup(),
                      EffectSettingsExtra::DurationKey(), oldDuration, effect->GetDefaultDuration());
        }

        //! NOTE Step 3.3 - check selected time
        double duration = 0.0;
        t0 = selectedRegion.t0();
        t1 = selectedRegion.t1();
        if (t1 > t0) {
            // there is a selection: let's fit in there...
            // MJS: note that this is just for the TTC and is independent of the track rate
            // but we do need to make sure we have the right number of samples at the project rate
            double quantMT0 = QUANTIZED_TIME(t0, projectRate);
            double quantMT1 = QUANTIZED_TIME(t1, projectRate);
            duration = quantMT1 - quantMT0;
            t1 = t0 + duration;
        }

        //! NOTE Step 2.3 - update settings
        wxString newFormat = (isSelection
                              ? NumericConverterFormats::TimeAndSampleFormat()
                              : NumericConverterFormats::DefaultSelectionFormat()
                              ).Internal();

        //! NOTE Step 2.4 - get current settings (make local settings)
        settings = pAccess->Get();

        //! NOTE Step 2.5 - update settings
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

        //! NOTE Step 2.6 - modify settings
        if (effect->IsInteractive() && (flags& EffectManager::kConfigured) == 0) {
            muse::String type = au3::wxToString(effect->GetSymbol().Internal());
            EffectInstanceId instanceId = effectInstancesRegister()->regInstance(effect);
            muse::Ret ret = effectsProvider()->showEffect(type, instanceId);
            effectInstancesRegister()->unregInstance(effect);
            if (ret) {
                effect->SaveUserPreset(CurrentSettingsGroup(), pAccess->Get());
            } else {
                LOGE() << "failed show effect: " << type << ", ret: " << ret.toString();
            }

            //! NOTE Step 2.6.2 - update local settings
            if (ret) {
                settings = pAccess->Get();
            }
        }
    }

    //! ============================================================================
    //! NOTE Step 3 - setup
    //! ============================================================================

    // common things used below
    unsigned oldFlags = 0;
    WaveTrack* newTrack = nullptr;

    {
        //! TODO
        em.SetSkipStateFlag(false);

        //! NOTE Step 3.1 - setup effect
        oldFlags = effect->mUIFlags;
        effect->mUIFlags = flags;
        effect->mFactory = &WaveTrackFactory::Get(project);
        effect->mProjectRate = projectRate;
        effect->mT0 = t0;
        effect->mT1 = t1;

        effect->SetTracks(&TrackList::Get(project));
        // Update track/group counts
        effect->CountWaveTracks();

        //! NOTE Step 3.2 - add new a track if need
        // We don't yet know the effect type for code in the Nyquist Prompt, so
        // assume it requires a track and handle errors when the effect runs.
        if ((effect->GetType() == EffectTypeGenerate || effect->GetPath() == NYQUIST_PROMPT_ID) && (effect->mNumTracks == 0)) {
            auto track = effect->mFactory->Create();
            track->SetName(effect->mTracks->MakeUniqueTrackName(WaveTrack::GetDefaultAudioTrackNamePreference()));
            newTrack = effect->mTracks->Add(track);
            newTrack->SetSelected(true);
        }

        //! NOTE Step 3.4 - check frequency params
        effect->mF0 = selectedRegion.f0();
        effect->mF1 = selectedRegion.f1();
        if (effect->mF0 != SelectedRegion::UndefinedFrequency) {
            effect->mPresetNames.push_back(L"control-f0");
        }
        if (effect->mF1 != SelectedRegion::UndefinedFrequency) {
            effect->mPresetNames.push_back(L"control-f1");
        }
    }

    //! ============================================================================
    //! NOTE Step 4 - process
    //! ============================================================================

    // common things used below
    bool success = false;
    {
        //! NOTE Step 4.1 - find instance
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

        //! NOTE Step 4.2 - make new instance if not found
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

        //! NOTE Step 4.3 - open transaction
        TransactionScope trans(project, "Effect");

        //! NOTE Step 4.4 - do process

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

        //! NOTE Step 4.5 - commit transaction on success
        if (success) {
            trans.Commit();
        }
    }

    //! ============================================================================
    //! NOTE Step 5 - cleanup
    //! ============================================================================

    {
        //! NOTE Step 5.1 - update selected region after process

        //! TODO It is not clear, can the effect change the selected region?
        //! Or is it done because the selected region changes when
        //! setting the parameters (because of QUANTIZED_TIME)
        //! Should we notify the UI about the change of the selected region and show the user its change?
        if (success && (effect->mT1 >= effect->mT0)) {
            selectedRegion.setTimes(effect->mT0, effect->mT1);
        }

        //! NOTE Step 5.2 - cleanup
        if (!success) {
            if (newTrack) {
                effect->mTracks->Remove(*newTrack);
            }
            // On failure, restore the old duration setting
            settings.extra.SetDuration(oldDuration);
        }

        // Don't hold a dangling pointer when done
        effect->SetTracks(nullptr);
        effect->mPresetNames.clear();
        effect->mUIFlags = oldFlags;

        //! TODO Should we do this only if it is not successful? (restore the oldDuration).
        //! If it is successful, according to logic, there is nothing to update, everything is up to date
        pAccess->Set(std::move(settings), nullptr);

        //! NOTE Step 5.3 - break if not success
        if (!success) {
            return muse::make_ret(Ret::Code::UnknownError);
        }
    }

    //! ============================================================================
    //! NOTE Step 6 - write history
    //! ============================================================================

    {
        //! NOTE Step 6.1 - write project history if need
        if (em.GetSkipStateFlag()) {
            flags = flags | EffectManager::kSkipState;
        }

        if (!(flags & EffectManager::kSkipState)) {
            auto shortDesc = PluginManager::Get().GetName(ID);
            const auto longDesc = XO("Applied effect: %s").Format(shortDesc);
            ProjectHistory::Get(project).PushState(longDesc, shortDesc);
        }

        //! NOTE Step 6.2 - remember a successful generator, effect, analyzer, or tool Process

        if (!(flags & EffectManager::kDontRepeatLast)) {
            auto& commandManager = CommandManager::Get(project);
            EffectType type = effect->GetType();

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
    }

    return true;
}
