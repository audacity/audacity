#include "../Benchmark.h"
#include "../CommonCommandFlags.h"
#include "../MenuCreator.h"
#include "../PluginRegistrationDialog.h"
#include "../ProjectWindows.h"
#include "../commands/CommandDispatch.h"
#include "../effects/EffectUI.h"
#include "../prefs/PrefsDialog.h"
#include "../toolbars/SelectionBar.h"
#include "../toolbars/ToolManager.h"
#include "AudacityMessageBox.h"
#include "AudioIO.h"
#include "CommandContext.h"
#include "CommandManager.h"
#include "EffectManager.h"
#include "HelpSystem.h"
#include "Journal.h"
#include "MenuHelper.h"
#include "PluginManager.h"
#include "Prefs.h"
#include "Project.h"
#include "ProjectRate.h"
#include "ProjectSnap.h"
#include "RealtimeEffectManager.h"
#include "RealtimeEffectPanel.h"
#include "SampleTrack.h"
#include "SyncLock.h"
#include "TempDirectory.h"
#include "TrackFocus.h"
#include "UndoManager.h"
#include "Viewport.h"
#include "prefs/EffectsPrefs.h"
#include "DoEffect.h"
#include "prefs/GUIPrefs.h"

// private helper classes and functions
namespace {
bool ShowManager(wxWindow* parent, int effectsCategory)
{
    PluginRegistrationDialog dlg(parent, effectsCategory);
    return dlg.ShowModal() == wxID_OK;
}

void DoManagePluginsMenu(AudacityProject& project, int effectsCategory)
{
    auto& window = GetProjectFrame(project);
    auto& pm = PluginManager::Get();
    if (ShowManager(&window, effectsCategory)) {
        MenuCreator::RebuildAllMenuBars();
    }
}

void DoManageRealtimeEffectsSidePanel(AudacityProject& project)
{
    auto& trackFocus = TrackFocus::Get(project);
    auto& panel = RealtimeEffectPanel::Get(project);
    if (panel.IsShown()) {
        panel.HidePanel();
    } else if (auto pTrack = dynamic_cast<SampleTrack*>(trackFocus.Get())) {
        panel.ShowPanel(pTrack, true);
    }
}
}

namespace {
// Menu handler functions

void OnResetConfig(const CommandContext& context)
{
    auto& project = context.project;
    auto& commandManager = CommandManager::Get(project);
    commandManager.mLastAnalyzerRegistration = CommandManager::repeattypenone;
    commandManager.mLastToolRegistration = CommandManager::repeattypenone;
    commandManager.mLastGenerator = "";
    commandManager.mLastEffect = "";
    commandManager.mLastAnalyzer = "";
    commandManager.mLastTool = "";

    ResetPreferences();

    // Directory will be reset on next restart.
    FileNames::UpdateDefaultPath(FileNames::Operation::Temp, TempDirectory::DefaultTempDir());

    // There are many more things we could reset here.
    // Beeds discussion as to which make sense to.
    // Maybe in future versions?
    // - Reset Effects
    // - Reset Recording and Playback volumes
    // - Reset Selection formats (and for spectral too)
    // - Reset Play-at-speed speed to x1
    // - Stop playback/recording and unapply pause.
    // - Set Zoom sensibly.
    SyncLockTracks.Reset();
    SoundActivatedRecord.Reset();
    SelectionToolbarMode.Reset();

    ShowRMSPref().Reset();
    ShowClippingPref().Reset();

    gPrefs->Flush();

    DoReloadPreferences(project);

    Viewport::Get(project).SetToDefaultSize();
    ToolManager::OnResetToolBars(context);

    // These are necessary to preserve the newly correctly laid out toolbars.
    // In particular the Device Toolbar ends up short on next restart,
    // if they are left out.
    gPrefs->Write(wxT("/PrefsVersion"), wxString(wxT(AUDACITY_PREFS_VERSION_STRING)));

    // write out the version numbers to the prefs file for future checking
    gPrefs->Write(wxT("/Version/Major"), AUDACITY_VERSION);
    gPrefs->Write(wxT("/Version/Minor"), AUDACITY_RELEASE);
    gPrefs->Write(wxT("/Version/Micro"), AUDACITY_REVISION);

    gPrefs->Flush();

    ProjectSnap::Get(project).SetSnapTo(SnapToSetting.Read());
    ProjectSnap::Get(project).SetSnapMode(SnapModeSetting.ReadEnum());

    ProjectRate::Get(project)
    .SetRate(gPrefs->ReadDouble("/DefaultProjectSampleRate", 44100.0));
}

void OnManageGenerators(const CommandContext& context)
{
    auto& project = context.project;
    DoManagePluginsMenu(project, EffectTypeGenerate);
}

void OnEffect(const CommandContext& context)
{
    // using GET to interpret parameter as a PluginID
    EffectUI::DoEffect(context.parameter.GET(), context.project, 0);
}

void OnManageEffects(const CommandContext& context)
{
    auto& project = context.project;
    DoManagePluginsMenu(project, EffectTypeProcess);
}

void OnAddRealtimeEffects(const CommandContext& context)
{
    auto& project = context.project;
    DoManageRealtimeEffectsSidePanel(project);
}

void OnAnalyzer2(wxCommandEvent& evt) { return; }

void OnRepeatLastGenerator(const CommandContext& context)
{
    auto& commandManager = CommandManager::Get(context.project);
    auto lastEffect = commandManager.mLastGenerator;
    if (!lastEffect.empty()) {
        EffectUI::DoEffect(
            lastEffect, context.project,
            commandManager.mRepeatGeneratorFlags | EffectManager::kRepeatGen);
    }
}

void OnRepeatLastEffect(const CommandContext& context)
{
    auto& commandManager = CommandManager::Get(context.project);
    auto lastEffect = commandManager.mLastEffect;
    if (!lastEffect.empty()) {
        EffectUI::DoEffect(lastEffect, context.project, commandManager.mRepeatEffectFlags);
    }
}

void OnRepeatLastAnalyzer(const CommandContext& context)
{
    auto& commandManager = CommandManager::Get(context.project);
    switch (commandManager.mLastAnalyzerRegistration) {
    case CommandManager::repeattypeplugin:
    {
        auto lastEffect = commandManager.mLastAnalyzer;
        if (!lastEffect.empty()) {
            EffectUI::DoEffect(
                lastEffect, context.project, commandManager.mRepeatAnalyzerFlags);
        }
    }
    break;
    case CommandManager::repeattypeunique:
        CommandManager::Get(context.project).DoRepeatProcess(context,
                                                             commandManager.mLastAnalyzerRegisteredId);
        break;
    }
}

void OnManageAnalyzers(const CommandContext& context)
{
    auto& project = context.project;
    DoManagePluginsMenu(project, EffectTypeAnalyze);
}

void OnManageTools(const CommandContext& context)
{
    auto& project = context.project;
    DoManagePluginsMenu(project, EffectTypeTool);
}

void OnBenchmark(const CommandContext& context)
{
    auto& project = context.project;
    CommandManager::Get(project).RegisterLastTool(context); //Register Run Benchmark as Last Tool
    auto& window = GetProjectFrame(project);
    ::RunBenchmark(&window, project);
}

void OnSimulateRecordingErrors(const CommandContext& context)
{
    auto& project = context.project;
    auto& commandManager = CommandManager::Get(project);

    auto gAudioIO = AudioIO::Get();
    bool& setting = gAudioIO->mSimulateRecordingErrors;
    commandManager.Check(wxT("SimulateRecordingErrors"), !setting);
    setting = !setting;
}

void OnDetectUpstreamDropouts(const CommandContext& context)
{
    auto& project = context.project;
    auto& commandManager = CommandManager::Get(project);

    auto gAudioIO = AudioIO::Get();
    auto& setting = gAudioIO->mDetectUpstreamDropouts;
    auto oldValue = setting.load(std::memory_order_relaxed);
    commandManager.Check(wxT("DetectUpstreamDropouts"), !oldValue);
    setting.store(!oldValue, std::memory_order_relaxed);
}

void OnWriteJournal(const CommandContext&)
{
    auto OnMessage
        =/* i18n-hint a "journal" is a text file that records
       the user's interactions with the application */
          XO("A journal will be recorded after Audacity restarts.");
    auto OffMessage
        =/* i18n-hint a "journal" is a text file that records
       the user's interactions with the application */
          XO("No journal will be recorded after Audacity restarts.");

    using namespace Journal;
    bool enabled = RecordEnabled();
    if (SetRecordEnabled(!enabled)) {
        enabled = !enabled;
    }
    if (enabled) {
        AudacityMessageBox(OnMessage);
    } else {
        AudacityMessageBox(OffMessage);
    }
}
}

// Menu definitions

// Under /MenuBar
using namespace MenuRegistry;

namespace {
const ReservedCommandFlag&
HasLastGeneratorFlag()
{
    static ReservedCommandFlag flag{
        [](const AudacityProject& project){
            return !CommandManager::Get(project).mLastGenerator.empty();
        }
    };
    return flag;
}

auto GenerateMenu()
{
    // All of this is a bit hacky until we can get more things connected into
    // the plugin manager...sorry! :-(
    static auto menu = std::shared_ptr{
        Menu(wxT("Generate"), XXO("&Generate"),
             Section("Manage",
                     Command(wxT("ManageGenerators"), XXO("Plugin Manager"),
                             OnManageGenerators, AudioIONotBusyFlag())
                     ),

             Section("RepeatLast",
                     // Delayed evaluation:
                     [](AudacityProject& project)
        {
            const auto& lastGenerator
                =CommandManager::Get(project).mLastGenerator;
            TranslatableString buildMenuLabel;
            if (!lastGenerator.empty()) {
                buildMenuLabel
                    =XO("Repeat %s")
                      .Format(PluginManager::Get().GetName(lastGenerator));
            } else {
                buildMenuLabel = XO("Repeat Last Generator");
            }

            return Command(wxT("RepeatLastGenerator"), buildMenuLabel,
                           OnRepeatLastGenerator,
                           AudioIONotBusyFlag()
                           | HasLastGeneratorFlag(),
                           Options {}.IsGlobal());
        }
                     ),

             Section("Generators",
                     // Delayed evaluation:
                     [](AudacityProject&) {
            auto result = Items("");
            MenuHelper::PopulateEffectsMenu(
                *result,
                EffectTypeGenerate,
                AudioIONotBusyFlag(),
                EffectsGroupBy.Read(),
                &OnEffect);
            return result;
        }
                     )
             ) };
    return menu;
}

static const ReservedCommandFlag
& IsRealtimeNotActiveFlag()
{
    static ReservedCommandFlag flag{
        [](const AudacityProject& project){
            return !RealtimeEffectManager::Get(project).IsActive();
        }
    };
    return flag;
}                  //lll

AttachedItem sAttachment1{ Indirect(GenerateMenu()) };

const ReservedCommandFlag&
HasLastEffectFlag()
{
    static ReservedCommandFlag flag{
        [](const AudacityProject& project) {
            return !CommandManager::Get(project).mLastEffect.empty();
        }
    };
    return flag;
}

static const ReservedCommandFlag&
HasTrackFocusFlag()
{
    static ReservedCommandFlag flag{
        [](const AudacityProject& project) {
            auto& trackFocus = TrackFocus::Get(const_cast<AudacityProject&>(project));
            return trackFocus.Get() != nullptr;
        }
    };
    return flag;
}

auto EffectMenu()
{
    // All of this is a bit hacky until we can get more things connected into
    // the plugin manager...sorry! :-(
    static auto menu = std::shared_ptr { Menu(
                                             wxT("Effect"), XXO("Effe&ct"),
                                             Section(
                                                 "Manage", Command(
                                                     wxT("ManageEffects"), XXO("Plugin Manager"),
                                                     OnManageEffects, AudioIONotBusyFlag())),

                                             Section(
                                                 "RealtimeEffects",
                                                 Command(
                                                     wxT("AddRealtimeEffects"), XXO("Add Realtime Effects"),
                                                     OnAddRealtimeEffects, HasTrackFocusFlag(), wxT("E"))
#if defined(__WXMSW__) || defined(__WXMAC__)
                                                 , Command(
                                                     wxT("GetMoreEffects"), XXO("Get more effects..."),
                                                     [](const CommandContext&) {
            OpenInDefaultBrowser("https://www.musehub.com");
        },
                                                     AlwaysEnabledFlag)
#endif
#if defined(__WXMSW__)
                                                 , Command(
                                                     wxT("GetAIEffects"), XXO("Get AI effects..."),
                                                     [](const CommandContext&) {
            OpenInDefaultBrowser("https://audacityteam.org/download/openvino");
        },
                                                     AlwaysEnabledFlag)
#endif
                                                 ),
                                             Section(
                                                 "RepeatLast",
                                                 // Delayed evaluation:
                                                 [](AudacityProject& project) {
            const auto& lastEffect = CommandManager::Get(project).mLastEffect;
            TranslatableString buildMenuLabel;
            if (!lastEffect.empty()) {
                buildMenuLabel
                    =XO("Repeat %s")
                      .Format(PluginManager::Get().GetName(lastEffect));
            } else {
                buildMenuLabel = XO("Repeat Last Effect");
            }

            return Command(wxT("RepeatLastEffect"), buildMenuLabel,
                           OnRepeatLastEffect,
                           AudioIONotBusyFlag() | TimeSelectedFlag()
                           | WaveTracksSelectedFlag() | HasLastEffectFlag(),
                           wxT("Ctrl+R"));
        }),

                                             Section(
                                                 "Effects",
                                                 // Delayed evaluation:
                                                 [](AudacityProject&) {
            auto result = Items("");
            MenuHelper::PopulateEffectsMenu(
                *result,
                EffectTypeProcess,
                AudioIONotBusyFlag() | TimeSelectedFlag() | WaveTracksSelectedFlag(),
                EffectsGroupBy.Read(),
                &OnEffect);
            return result;
        }))
    };
    return menu;
}

AttachedItem sAttachment2{ Indirect(EffectMenu()) };

const ReservedCommandFlag&
HasLastAnalyzerFlag()
{
    static ReservedCommandFlag flag{
        [](const AudacityProject& project) {
            if (CommandManager::Get(project).mLastAnalyzerRegistration
                == CommandManager::repeattypeunique) {
                return true;
            }
            return !CommandManager::Get(project).mLastAnalyzer.empty();
        }
    };
    return flag;
}

auto AnalyzeMenu()
{
    // All of this is a bit hacky until we can get more things connected into
    // the plugin manager...sorry! :-(
    static auto menu = std::shared_ptr{
        Menu(wxT("Analyze"), XXO("&Analyze"),
             Section("Manage",
                     Command(wxT("ManageAnalyzers"), XXO("Plugin Manager"),
                             OnManageAnalyzers, AudioIONotBusyFlag())
                     ),

             Section("RepeatLast",
                     // Delayed evaluation:
                     [](AudacityProject& project)
        {
            const auto& lastAnalyzer
                =CommandManager::Get(project).mLastAnalyzer;
            TranslatableString buildMenuLabel;
            if (!lastAnalyzer.empty()) {
                buildMenuLabel
                    =XO("Repeat %s")
                      .Format(PluginManager::Get().GetName(lastAnalyzer));
            } else {
                buildMenuLabel = XO("Repeat Last Analyzer");
            }

            return Command(wxT("RepeatLastAnalyzer"), buildMenuLabel,
                           OnRepeatLastAnalyzer,
                           AudioIONotBusyFlag() | TimeSelectedFlag()
                           | WaveTracksSelectedFlag() | HasLastAnalyzerFlag(),
                           Options {}.IsGlobal());
        }
                     ),

             Section("Analyzers",
                     Items("Windows"),

                     // Delayed evaluation:
                     [](AudacityProject&) {
            auto result = Items("");
            MenuHelper::PopulateEffectsMenu(
                *result,
                EffectTypeAnalyze,
                AudioIONotBusyFlag() | TimeSelectedFlag() | WaveTracksSelectedFlag(),
                EffectsGroupBy.Read(),
                &OnEffect);
            return result;
        }
                     )
             ) };
    return menu;
}

AttachedItem sAttachment3{ Indirect(AnalyzeMenu()) };

auto ToolsMenu()
{
    static auto menu = std::shared_ptr{
        Menu(wxT("Tools"), XXO("T&ools"),
             Section("Manage",
                     Command(wxT("ManageTools"), XXO("Plugin Manager"),
                             OnManageTools, AudioIONotBusyFlag())

                     //Separator(),
                     ),

             Section("Other",
                     Command(wxT("ConfigReset"), XXO("Reset &Configuration"),
                             OnResetConfig,
                             AudioIONotBusyFlag()),

                     // PRL: team consensus for 2.2.0 was, we let end users have this diagnostic,
                     // as they used to in 1.3.x
                     //#ifdef IS_ALPHA
                     // TODO: What should we do here?  Make benchmark a plug-in?
                     // Easy enough to do.  We'd call it mod-self-test.
                     Command(wxT("Benchmark"), XXO("&Run Benchmark..."),
                             OnBenchmark, AudioIONotBusyFlag())
                     //#endif
                     ),

             Section("Tools",
                     // Delayed evaluation:
                     [](AudacityProject&) {
            auto result = Items("");
            MenuHelper::PopulateEffectsMenu(
                *result,
                EffectTypeTool,
                AudioIONotBusyFlag(),
                EffectsGroupBy.Read(),
                &OnEffect);
            return result;
        }
                     )

#ifdef IS_ALPHA
             ,
             Section("",
                     Command(wxT("SimulateRecordingErrors"),
                             XXO("Simulate Recording Errors"),
                             OnSimulateRecordingErrors,
                             AudioIONotBusyFlag(),
                             Options {}.CheckTest(
                                 [](AudacityProject&){
            return AudioIO::Get()->mSimulateRecordingErrors;
        })),
                     Command(wxT("DetectUpstreamDropouts"),
                             XXO("Detect Upstream Dropouts"),
                             OnDetectUpstreamDropouts,
                             AudioIONotBusyFlag(),
                             Options {}.CheckTest(
                                 [](AudacityProject&){
            return AudioIO::Get()->mDetectUpstreamDropouts
                   .load(std::memory_order_relaxed);
        }))
                     )
#endif

#if defined(IS_ALPHA) || defined(END_USER_JOURNALLING)
             ,
             Section("",
                     Command(wxT("WriteJournal"),
                             /* i18n-hint a "journal" is a text file that records
                              the user's interactions with the application */
                             XXO("Write Journal"),
                             OnWriteJournal,
                             AlwaysEnabledFlag,
                             Options {}.CheckTest([](AudacityProject&){
            return Journal::RecordEnabled();
        }))
                     )
#endif

             ) };
    return menu;
}

AttachedItem sAttachment4{ Indirect(ToolsMenu()) };
}
