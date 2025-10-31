#pragma once

#include "ineededmodulesetup.h"

// Framework
#include "muse_framework_config.h"
#include "diagnostics/diagnosticsmodule.h"
#include "framework/draw/drawmodule.h"
#include "framework/actions/actionsmodule.h"
#include "framework/audioplugins/audiopluginsmodule.h"
#include "framework/ui/uimodule.h"
#include "framework/shortcuts/shortcutsmodule.h"
#include "framework/accessibility/accessibilitymodule.h"
#include "framework/uicomponents/uicomponentsmodule.h"
#include "framework/dockwindow/dockmodule.h"
#include "framework/cloud/cloudmodule.h"
#include "framework/network/networkmodule.h"
#include "framework/learn/learnmodule.h"
#include "framework/languages/languagesmodule.h"
#include "framework/workspace/workspacemodule.h"

// need stubs
#include "framework/stubs/multiinstances/multiinstancesstubmodule.h"

// -----
#include "appshell/appshellmodule.h"
#include "context/contextmodule.h"
#include "project/projectmodule.h"
#include "projectscene/projectscenemodule.h"
#include "au3audio/audiomodule.h"
#include "playback/playbackmodule.h"
#include "trackedit/trackeditmodule.h"
#include "record/recordmodule.h"
#include "effects/effects_base/effectsmodule.h"
#include "effects/builtin/builtineffectsmodule.h"
#ifdef AU_MODULE_EFFECTS_LV2
#include "effects/lv2/lv2effectsmodule.h"
#else
#include "stubs/lv2/lv2effectsstubmodule.h"
#endif
#ifdef AU_MODULE_EFFECTS_VST
#include "effects/vst/vsteffectsmodule.h"
#else
#include "stubs/vst/vsteffectsstubmodule.h"
#endif
#ifdef AU_MODULE_EFFECTS_AUDIO_UNIT
#include "effects/audio_unit/audiouniteffectsmodule.h"
#else
#include "stubs/audio_unit/audiouniteffectsstubmodule.h"
#endif
#include "effects/nyquist/nyquisteffectsmodule.h"
#include "importexport/import/importermodule.h"
#include "importexport/export/exportermodule.h"

#ifdef MUSE_MODULE_AUTOBOT
#include "autobot/autobotmodule.h"
#endif

#ifdef MUSE_MODULE_EXTENSIONS
#include "framework/extensions/extensionsmodule.h"
#else
#include "framework/stubs/extensions/extensionsstubmodule.h"
#endif

#include "au3wrap/au3wrapmodule.h"

namespace au {
class NeededDiagnosticsModule : public INeededModuleSetup
{
public:
    NeededDiagnosticsModule()
        : INeededModuleSetup(new muse::diagnostics::DiagnosticsModule()) {}
};

class NeededAudioPluginsModule : public INeededModuleSetup
{
public:
    NeededAudioPluginsModule()
        : INeededModuleSetup(new muse::audioplugins::AudioPluginsModule()) {}
};

class NeededDrawModule : public INeededModuleSetup
{
public:
    NeededDrawModule()
        : INeededModuleSetup(new muse::draw::DrawModule()) {}
};

class NeededActionsModule : public INeededModuleSetup
{
public:
    NeededActionsModule()
        : INeededModuleSetup(new muse::actions::ActionsModule()) {}
};

class NeededWorkspaceModule : public INeededModuleSetup
{
public:
    NeededWorkspaceModule()
        : INeededModuleSetup(new muse::workspace::WorkspaceModule()) {}
};

class NeededAccessibilityModule : public INeededModuleSetup
{
public:
    NeededAccessibilityModule()
        : INeededModuleSetup(new muse::accessibility::AccessibilityModule()) {}
};

class NeededMultiInstancesModule : public INeededModuleSetup
{
public:
    NeededMultiInstancesModule()
        : INeededModuleSetup(new muse::mi::MultiInstancesModule()) {}
};

class NeededLearnModule : public INeededModuleSetup
{
public:
    NeededLearnModule()
        : INeededModuleSetup(new muse::learn::LearnModule()) {}
};

class NeededLanguagesModule : public INeededModuleSetup
{
public:
    NeededLanguagesModule()
        : INeededModuleSetup(new muse::languages::LanguagesModule()) {}
};

class NeededUiModule : public INeededModuleSetup
{
public:
    NeededUiModule()
        : INeededModuleSetup(new muse::ui::UiModule()) {}
};

class NeededUiComponentsModule : public INeededModuleSetup
{
public:
    NeededUiComponentsModule()
        : INeededModuleSetup(new muse::uicomponents::UiComponentsModule()) {}
};

class NeededDockModule : public INeededModuleSetup
{
public:
    NeededDockModule()
        : INeededModuleSetup(new muse::dock::DockModule()) {}
};

class NeededShortcutsModule : public INeededModuleSetup
{
public:
    NeededShortcutsModule()
        : INeededModuleSetup(new muse::shortcuts::ShortcutsModule()) {}
};

class NeededCloudModule : public INeededModuleSetup
{
public:
    NeededCloudModule()
        : INeededModuleSetup(new muse::cloud::CloudModule()) {}
};

class NeededNetworkModule : public INeededModuleSetup
{
public:
    NeededNetworkModule()
        : INeededModuleSetup(new muse::network::NetworkModule()) {}
};

class NeededAppShellModule : public INeededModuleSetup
{
public:
    NeededAppShellModule()
        : INeededModuleSetup(new au::appshell::AppShellModule()) {}
};

class NeededAudioUnitEffectsModule : public INeededModuleSetup
{
public:
    NeededAudioUnitEffectsModule()
        : INeededModuleSetup(new au::effects::AudioUnitEffectsModule()) {}
};

class NeededLv2EffectsModule : public INeededModuleSetup
{
public:
    NeededLv2EffectsModule()
        : INeededModuleSetup(new au::effects::Lv2EffectsModule()) {}
};

class NeededVstEffectsModule : public INeededModuleSetup
{
public:
    NeededVstEffectsModule()
        : INeededModuleSetup(new au::effects::VstEffectsModule()) {}
};

class NeededExtensionsModule : public INeededModuleSetup
{
public:
    NeededExtensionsModule()
        : INeededModuleSetup(new muse::extensions::ExtensionsModule()) {}
};

#ifdef MUSE_MODULE_AUTOBOT
class NeededAutobotModule : public INeededModuleSetup
{
public:
    NeededAutobotModule()
        : INeededModuleSetup(new muse::autobot::AutobotModule()) {}
};
#endif

class NeededContextModule : public INeededModuleSetup
{
public:
    NeededContextModule()
        : INeededModuleSetup(new au::context::ContextModule()) {}
};

class NeededAudioModule : public INeededModuleSetup
{
public:
    NeededAudioModule()
        : INeededModuleSetup(new au::audio::AudioModule()) {}
};

class NeededProjectSceneModule : public INeededModuleSetup
{
public:
    NeededProjectSceneModule()
        : INeededModuleSetup(new au::projectscene::ProjectSceneModule()) {}
};

class NeededPlaybackModule : public INeededModuleSetup
{
public:
    NeededPlaybackModule()
        : INeededModuleSetup(new au::playback::PlaybackModule()) {}
};

class NeededRecordModule : public INeededModuleSetup
{
public:
    NeededRecordModule()
        : INeededModuleSetup(new au::record::RecordModule()) {}
};

class NeededTrackeditModule : public INeededModuleSetup
{
public:
    NeededTrackeditModule()
        : INeededModuleSetup(new au::trackedit::TrackeditModule()) {}
};

class NeededProjectModule : public INeededModuleSetup
{
public:
    NeededProjectModule()
        : INeededModuleSetup(new au::project::ProjectModule()) {}
};

class NeededExporterModule : public INeededModuleSetup
{
public:
    NeededExporterModule()
        : INeededModuleSetup(new au::importexport::ExporterModule()) {}
};

class NeededImporterModule : public INeededModuleSetup
{
public:
    NeededImporterModule()
        : INeededModuleSetup(new au::importexport::ImporterModule()) {}
};

class NeededAu3WrapModule : public INeededModuleSetup
{
public:
    NeededAu3WrapModule()
        : INeededModuleSetup(new au::au3::Au3WrapModule()) {}
};

class NeededEffectsModule : public INeededModuleSetup
{
public:
    NeededEffectsModule()
        : INeededModuleSetup(new au::effects::EffectsModule()) {}
};

class NeededBuiltinEffectsModule : public INeededModuleSetup
{
public:
    NeededBuiltinEffectsModule()
        : INeededModuleSetup(new au::effects::BuiltinEffectsModule()) {}
};

class NeededNyquistEffectsModule : public INeededModuleSetup
{
public:
    NeededNyquistEffectsModule()
        : INeededModuleSetup(new au::effects::NyquistEffectsModule()) {}
};
}
