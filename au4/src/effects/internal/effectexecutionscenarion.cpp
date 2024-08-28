/*
* Audacity: A Digital Audio Editor
*/
#include "effectexecutionscenarion.h"

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

muse::Ret EffectExecutionScenarion::performEffect(const EffectId& effectId)
{
    auto& project = *reinterpret_cast<::AudacityProject*>(globalContext()->currentProject()->au3ProjectPtr());

    auto showEffectHostInterfaceCb = [this](Effect& effect,
                                            std::shared_ptr<EffectInstance>& instance,
                                            SimpleEffectSettingsAccess& settings)
    {
        const std::optional<EffectPlugin::InstancePointer> result = EffectBase::FindInstance(effect);
        if (!result.has_value()) {
            return false;
        }
        instance = *result;
        if (!instance) {
            return false;
        }

        muse::String type = au3::wxToString(effect.GetSymbol().Internal());
        //! NOTE The goal is that we need to pass the instance ID to the view model
        //! and get a pointer to the effect instance there.
        //! For built-in effects, we can register and unregister the instance
        //! in the constructor and destructor.
        //! But now I'm not sure we can do this for all effects.
        //! Therefore, we register here and immediately unregister here.
        //! This is a hack...
        //! But it looks like later everything will be different, at some point we will remove it

        EffectInstanceId instanceId = effectInstancesRegister()->regInstance(&effect);
        muse::Ret ret = effectsProvider()->showEffect(type, instanceId);
        effectInstancesRegister()->unregInstance(&effect);
        if (!ret) {
            LOGE() << "failed show effect: " << type << ", ret: " << ret.toString();
            return false;
        }

        effect.SaveUserPreset(CurrentSettingsGroup(), settings.Get());
        return true;
    };

    auto stopPlaybackCb = [] {};
    auto selectAllIfNoneCb = [] {};

    DoEffect(
        effectId, project, 0,
        std::move(showEffectHostInterfaceCb), std::move(stopPlaybackCb),
        std::move(selectAllIfNoneCb));

    return muse::make_ok();
}

bool EffectExecutionScenarion::DoEffect(
    const EffectId& effectId, AudacityProject& project, unsigned flags,
    ShowEffectHostInterfaceCb showEffectHostInterfaceCb,
    StopPlaybackCb stopPlaybackCb, SelectAllIfNoneCb selectAllIfNoneCb)
{
    PluginID ID = effectId.toStdString();

    auto& tracks = TrackList::Get(project);
    auto& trackFactory = WaveTrackFactory::Get(project);
    auto rate = ProjectRate::Get(project).GetRate();
    auto& selectedRegion = ViewInfo::Get(project).selectedRegion;
    auto& commandManager = CommandManager::Get(project);
    //auto& viewport = Viewport::Get(project);

    const PluginDescriptor* plug = PluginManager::Get().GetPlugin(ID);

    if (!plug || !PluginManager::IsPluginAvailable(*plug)) {
        // BasicUI::ShowMessageBox(
        //     XO("This plugin could not be loaded.\nIt may have been deleted."),
        //     BasicUI::MessageBoxOptions().Caption(XO("Plugin Error")));

        return false;
    }

    EffectType type = plug->GetEffectType();

    // Make sure there's no activity since the effect is about to be applied
    // to the project's tracks.  Mainly for Apply during RTP, but also used
    // for batch commands
    if (flags & EffectManager::kConfigured) {
        stopPlaybackCb();
        // Don't Select All if repeating Generator Effect
        if (!(flags & EffectManager::kConfigured)) {
            selectAllIfNoneCb();
        }
    }

    bool success = false;
    auto cleanup = finally([&] {
        if (!success) {
            // For now, we're limiting realtime preview to a single effect, so
            // make sure the menus reflect that fact that one may have just been
            // opened.
            CommandManager::Get(project).UpdateMenus(false);
        }
    });

    const auto range = tracks.Selected<const WaveTrack>();
    EffectManager& em = EffectManager::Get();

    em.SetSkipStateFlag(false);
    success = false;
    if (auto effect = dynamic_cast<Effect*>(em.GetEffect(ID))) {
        if (const auto pSettings = em.GetDefaultSettings(ID)) {
            const auto pAccess = std::make_shared<SimpleEffectSettingsAccess>(*pSettings);

            const auto finder = [effect, pAccess, flags,
                                 cb = std::move(showEffectHostInterfaceCb)](
                EffectSettings& settings)
                                -> std::optional<std::shared_ptr<EffectInstanceEx> > {
                // Prompting will be bypassed when applying an effect that has
                // already been configured, e.g. repeating the last effect on a
                // different selection.  Prompting may call EffectPreview
                std::shared_ptr<EffectInstance> pInstance;
                std::shared_ptr<EffectInstanceEx> pInstanceEx;
                if ((flags& EffectManager::kConfigured) == 0 && pAccess) {
                    if (!cb(*effect, pInstance, *pAccess)) {
                        return {};
                    } else if (!(pInstanceEx =std::dynamic_pointer_cast<EffectInstanceEx>(pInstance))) {
                        return {};
                    } else {
                        // Retrieve again after the dialog modified settings
                        settings = pAccess->Get();
                    }
                }
                return { pInstanceEx };
            };
            pAccess->ModifySettings([&](EffectSettings& settings) {
                // success = effect->DoEffect(
                //     settings, finder, rate, &tracks, &trackFactory, selectedRegion,
                //     flags, pAccess);
                success = EffectBaseDoEffect(effect, settings, finder, rate, &tracks, &trackFactory, selectedRegion, flags, pAccess);
                return nullptr;
            });
        }
    }

    if (!success) {
        return false;
    }

    if (em.GetSkipStateFlag()) {
        flags = flags | EffectManager::kSkipState;
    }

    if (!(flags & EffectManager::kSkipState)) {
        auto shortDesc = PluginManager::Get().GetName(ID);
        const auto longDesc = XO("Applied effect: %s").Format(shortDesc);
        ProjectHistory::Get(project).PushState(longDesc, shortDesc);
    }

    if (!(flags & EffectManager::kDontRepeatLast)) {
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

    return true;
}

bool EffectExecutionScenarion::EffectBaseDoEffect(EffectBase* effect,
                                                  EffectSettings& settings,
                                                  const EffectPlugin::InstanceFinder& finder,
                                                  double projectRate,
                                                  TrackList* list,
                                                  WaveTrackFactory* factory,
                                                  NotifyingSelectedRegion& selectedRegion,
                                                  unsigned flags,
                                                  const EffectPlugin::EffectSettingsAccessPtr& pAccess)
{
    auto cleanup0 = valueRestorer(effect->mUIFlags, flags);
    wxASSERT(selectedRegion.duration() >= 0.0);

    effect->mFactory = factory;
    effect->mProjectRate = projectRate;

    effect->SetTracks(list);
    // Don't hold a dangling pointer when done
    Finally Do([effect]{ effect->SetTracks(nullptr); });

    // This is for performance purposes only, no additional recovery implied
    auto& pProject = *const_cast<AudacityProject*>(effect->FindProject()); // how to remove this const_cast?
    TransactionScope trans(pProject, "Effect");

    // Update track/group counts
    effect->CountWaveTracks();

    bool isSelection = false;

    auto duration = 0.0;
    if (effect->GetType() == EffectTypeGenerate) {
        GetConfig(effect->GetDefinition(), PluginSettings::Private,
                  CurrentSettingsGroup(),
                  EffectSettingsExtra::DurationKey(), duration, effect->GetDefaultDuration());
    }

    WaveTrack* newTrack{};
    bool success = false;
    auto oldDuration = duration;

    auto cleanup = finally([&] {
        if (!success) {
            if (newTrack) {
                effect->mTracks->Remove(*newTrack);
            }
            // On failure, restore the old duration setting
            settings.extra.SetDuration(oldDuration);
        } else {
            trans.Commit();
        }

        effect->mPresetNames.clear();
    });

    // We don't yet know the effect type for code in the Nyquist Prompt, so
    // assume it requires a track and handle errors when the effect runs.
    if ((effect->GetType() == EffectTypeGenerate || effect->GetPath() == NYQUIST_PROMPT_ID) && (effect->mNumTracks == 0)) {
        auto track = effect->mFactory->Create();
        track->SetName(effect->mTracks->MakeUniqueTrackName(WaveTrack::GetDefaultAudioTrackNamePreference()));
        newTrack = effect->mTracks->Add(track);
        newTrack->SetSelected(true);
    }

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

    // This is happening inside EffectSettingsAccess::ModifySettings
    auto newFormat = (isSelection
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

    effect->mF0 = selectedRegion.f0();
    effect->mF1 = selectedRegion.f1();
    if (effect->mF0 != SelectedRegion::UndefinedFrequency) {
        effect->mPresetNames.push_back(L"control-f0");
    }
    if (effect->mF1 != SelectedRegion::UndefinedFrequency) {
        effect->mPresetNames.push_back(L"control-f1");
    }

    effect->CountWaveTracks();

    // Allow the dialog factory to fill this in, but it might not
    std::shared_ptr<EffectInstance> pInstance;

    if (effect->IsInteractive()) {
        if (!finder) {
            return false;
        } else if (auto result = finder(settings)) {
            pInstance = *result;
        } else {
            return false;
        }
    }

    auto pInstanceEx = std::dynamic_pointer_cast<EffectInstanceEx>(pInstance);
    if (!pInstanceEx) {
        // Path that skipped the dialog factory -- effect may be non-interactive
        // or this is batch mode processing or repeat of last effect with stored
        // settings.
        pInstanceEx = std::dynamic_pointer_cast<EffectInstanceEx>(effect->MakeInstance());
        // Note: Init may read parameters from preferences
        if (!pInstanceEx || !pInstanceEx->Init()) {
            return false;
        }
    }

    // If the dialog was shown, then it has been closed without errors or
    // cancellation, and any change of duration has been saved in the config file

    bool returnVal = true;
    bool skipFlag = effect->CheckWhetherSkipEffect(settings);
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

    if (returnVal && (effect->mT1 >= effect->mT0)) {
        selectedRegion.setTimes(effect->mT0, effect->mT1);
    }

    success = returnVal;
    return returnVal;
}
