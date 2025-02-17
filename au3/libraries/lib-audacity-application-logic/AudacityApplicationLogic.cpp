/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudacityApplicationLogic.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "AudacityApplicationLogic.h"
#include "BasicUI.h"
#include "CommandManager.h"
#include "Effect.h"
#include "EffectManager.h"
#include "EffectPlugin.h"
#include "PluginManager.h"
#include "Project.h"
#include "ProjectHistory.h"
#include "ProjectRate.h"
#include "TrackFocus.h"
#include "ViewInfo.h"
#include "Viewport.h"
#include "WaveTrack.h"

/// DoEffect() takes a PluginID and executes the associated effect.
///
/// At the moment flags are used only to indicate whether to prompt for
//  parameters, whether to save the state to history and whether to allow
/// 'Repeat Last Effect'.

bool AudacityApplicationLogic::DoEffect(
    const PluginID& ID, AudacityProject& project, unsigned flags,
    ShowEffectHostInterfaceCb showEffectHostInterfaceCb,
    StopPlaybackCb stopPlaybackCb, SelectAllIfNoneCb selectAllIfNoneCb)
{
    auto& tracks = TrackList::Get(project);
    auto& trackFactory = WaveTrackFactory::Get(project);
    auto rate = ProjectRate::Get(project).GetRate();
    auto& selectedRegion = ViewInfo::Get(project).selectedRegion;
    auto& commandManager = CommandManager::Get(project);
    auto& viewport = Viewport::Get(project);

    const PluginDescriptor* plug = PluginManager::Get().GetPlugin(ID);

    if (!plug || !PluginManager::IsPluginAvailable(*plug)) {
        BasicUI::ShowMessageBox(
            XO("This plugin could not be loaded.\nIt may have been deleted."),
            BasicUI::MessageBoxOptions().Caption(XO("Plugin Error")));

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

    auto nTracksOriginally = tracks.Size();
    // wxWindow* focus = wxWindow::FindFocus();
    // wxWindow* parent = nullptr;
    // if (focus != nullptr)
    // {
    //    parent = focus->GetParent();
    // }

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
    bool anyTracks = !range.empty();
    bool clean = std::all_of(range.begin(), range.end(), [](const WaveTrack* t) {
        return t->GetEndTime() == 0;
    });

    EffectManager& em = EffectManager::Get();

    em.SetSkipStateFlag(false);
    success = false;
    if (auto effect = dynamic_cast<Effect*>(em.GetEffect(ID))) {
        if (const auto pSettings = em.GetDefaultSettings(ID)) {
            const auto pAccess
                =std::make_shared<SimpleEffectSettingsAccess>(*pSettings);
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
                    } else if (!(pInstanceEx
                                     =std::dynamic_pointer_cast<EffectInstanceEx>(
                                           pInstance))) {
                        return {};
                    } else {
                        // Retrieve again after the dialog modified settings
                        settings = pAccess->Get();
                    }
                }
                return { pInstanceEx };
            };
            pAccess->ModifySettings([&](EffectSettings& settings) {
                success = effect->DoEffect(
                    settings, finder, rate, &tracks, &trackFactory, selectedRegion,
                    flags, pAccess);
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
            commandManager.mLastAnalyzerRegistration
                =CommandManager::repeattypeplugin;
            commandManager.mRepeatAnalyzerFlags = EffectManager::kConfigured;
            break;
        case EffectTypeTool:
            commandManager.Modify(wxT("RepeatLastTool"), lastEffectDesc);
            commandManager.mLastTool = ID;
            commandManager.mLastToolRegistration
                =CommandManager::repeattypeplugin;
            commandManager.mRepeatToolFlags = EffectManager::kConfigured;
            if (shortDesc == NYQUIST_PROMPT_NAME) {
                commandManager.mRepeatToolFlags
                    =EffectManager::kRepeatNyquistPrompt; // Nyquist Prompt is not
                                                          // configured
            }
            break;
        }
    }

    // STM:
    // The following automatically re-zooms after sound was generated.
    // IMO, it was disorienting, removing to try out without re-fitting
    // mchinen:12/14/08 reapplying for generate effects
    if (type == EffectTypeGenerate) {
        if (!anyTracks || (clean && selectedRegion.t0() == 0.0)) {
            viewport.ZoomFitHorizontally();
        }
    }

    // PRL:  Redraw explicitly because sometimes history push is skipped
    viewport.Redraw();

    // if (focus != nullptr && focus->GetParent() == parent)
    // {
    //    focus->SetFocus();
    // }

    // A fix for Bug 63
    // New tracks added?  Scroll them into view so that user sees them.
    // Don't care what track type.  An analyser might just have added a
    // Label track and we want to see it.
    if (tracks.Size() > nTracksOriginally) {
        viewport.ScrollToBottom();
    } else {
        auto pTrack = *tracks.Selected().begin();
        if (!pTrack) {
            pTrack = *tracks.begin();
        }
        if (pTrack) {
            TrackFocus::Get(project).Set(pTrack);
            Viewport::Get(project).ShowTrack(*pTrack);
        }
    }

    return true;
}
