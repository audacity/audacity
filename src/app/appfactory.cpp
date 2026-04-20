/*
 * Audacity: A Digital Audio Editor
 */
#include "appfactory.h"

#include "guiapp.h"
#include "pluginregistrationapp.h"

#include "muse_framework_config.h"
#include "app_config.h"

#include "framework/diagnostics/diagnosticsmodule.h"
#include "framework/draw/drawmodule.h"
#include "framework/actions/actionsmodule.h"
#include "framework/audioplugins/audiopluginsmodule.h"
#include "framework/interactive/interactivemodule.h"
#include "framework/ui/uimodule.h"
#include "framework/shortcuts/shortcutsmodule.h"
#include "framework/accessibility/accessibilitymodule.h"
#include "framework/uicomponents/uicomponentsmodule.h"
#include "framework/dockwindow/dockmodule.h"
#include "framework/cloud/cloudmodule.h"
#include "framework/network/networkmodule.h"
#ifdef MUSE_MODULE_UPDATE
#include "framework/update/updatemodule.h"
#else
#include "framework/stubs/update/updatestubmodule.h"
#endif
#include "framework/learn/learnmodule.h"
#include "framework/languages/languagesmodule.h"
#include "framework/workspace/workspacemodule.h"

#include "framework/multiwindows/multiwindowsmodule.h"

#include "appshell/appshellmodule.h"
#include "context/contextmodule.h"
#include "preferences/preferencesmodule.h"
#include "project/projectmodule.h"
#include "projectscene/projectscenemodule.h"
#include "audio/audiomodule.h"
#include "au3audio/au3audiomodule.h"
#include "playback/playbackmodule.h"
#include "trackedit/trackeditmodule.h"
#include "spectrogram/spectrogrammodule.h"
#include "record/recordmodule.h"
#include "uicomponents/uicomponentsmodule.h"
#include "toast/toastmodule.h"
#include "effects/effects_base/effectsmodule.h"
#include "effects/builtin/builtineffectsmodule.h"
#include "effects/builtin_collection/builtineffectscollectionmodule.h"
#include "importexport/import/importermodule.h"
#include "importexport/export/exportermodule.h"
#include "importexport/labels/labelsmodule.h"
#ifdef AU_BUILD_CLOUD_AUDIOCOM
#include "au3cloud/au3cloudmodule.h"
#else
#include "stubs/au3cloud/au3cloudstubmodule.h"
#endif
#include "automation/automationmodule.h"

#if AU_MODULE_EFFECTS_NYQUIST
#include "effects/nyquist/nyquisteffectsmodule.h"
#endif

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

#ifdef MUSE_MODULE_TESTFLOW
#include "testflow/testflowmodule.h"
#endif

#ifdef MUSE_MODULE_EXTENSIONS
#include "framework/extensions/extensionsmodule.h"
#else
#include "framework/stubs/extensions/extensionsstubmodule.h"
#endif

#include "au3wrap/au3wrapmodule.h"

using namespace muse;
using namespace au::app;

std::shared_ptr<muse::IApplication> AppFactory::newApp(const std::shared_ptr<AudacityCmdOptions>& options) const
{
    IF_ASSERT_FAILED(options) {
        return nullptr;
    }

    switch (options->runMode) {
    case IApplication::RunMode::GuiApp:
    case IApplication::RunMode::ConsoleApp:
        // No console mode for now
        return newGuiApp(options);
    case IApplication::RunMode::AudioPluginRegistration:
        return newPluginRegistrationApp(options);
    }

    return newGuiApp(options);
}

std::shared_ptr<muse::IApplication> AppFactory::newGuiApp(const std::shared_ptr<AudacityCmdOptions>& options) const
{
    std::shared_ptr<GuiApp> app = std::make_shared<GuiApp>(options);

    //! NOTE `diagnostics` must be first, because it installs the crash handler.
    //! For other modules, the order is (and should be) unimportant.
    app->addModule(new muse::diagnostics::DiagnosticsModule());

    app->addModule(new muse::audioplugins::AudioPluginsModule());
    app->addModule(new muse::actions::ActionsModule());
    app->addModule(new muse::draw::DrawModule());
    app->addModule(new muse::workspace::WorkspaceModule());
    app->addModule(new muse::accessibility::AccessibilityModule());
    app->addModule(new muse::interactive::InteractiveModule());
    app->addModule(new muse::mi::MultiWindowsModule());
    app->addModule(new muse::learn::LearnModule());
    app->addModule(new muse::languages::LanguagesModule());
    app->addModule(new muse::ui::UiModule());
    app->addModule(new muse::uicomponents::UiComponentsModule());
    app->addModule(new muse::dock::DockModule());
#ifdef MUSE_MODULE_SHORTCUTS
    app->addModule(new muse::shortcuts::ShortcutsModule());
#endif
    app->addModule(new muse::cloud::CloudModule());
    app->addModule(new muse::network::NetworkModule());
    app->addModule(new muse::update::UpdateModule());
    app->addModule(new muse::extensions::ExtensionsModule());
#ifdef MUSE_MODULE_TESTFLOW
    app->addModule(new muse::testflow::TestflowModule());
#endif

    // Audacity modules
    app->addModule(new au::appshell::AppShellModule());
    app->addModule(new au::preferences::PreferencesModule());
    app->addModule(new au::uicomponents::UiComponentsModule());
    app->addModule(new au::effects::AudioUnitEffectsModule());
    app->addModule(new au::effects::Lv2EffectsModule());
    app->addModule(new au::effects::VstEffectsModule());
    app->addModule(new au::effects::NyquistEffectsModule());
    app->addModule(new au::context::ContextModule());
    app->addModule(new au::audio::AudioModule());
    app->addModule(new au::au3audio::Au3AudioModule());
    app->addModule(new au::projectscene::ProjectSceneModule());
    app->addModule(new au::playback::PlaybackModule());
    app->addModule(new au::record::RecordModule());
    app->addModule(new au::trackedit::TrackeditModule());
    app->addModule(new au::spectrogram::SpectrogramModule());
    app->addModule(new au::toast::ToastModule());
    app->addModule(new au::project::ProjectModule());
    app->addModule(new au::importexport::ExporterModule());
    app->addModule(new au::importexport::ImporterModule());
    app->addModule(new au::importexport::LabelsModule());
    app->addModule(new au::au3::Au3WrapModule());
    app->addModule(new au::au3cloud::Au3CloudModule());
    app->addModule(new au::effects::EffectsModule());
    app->addModule(new au::effects::BuiltinEffectsModule());
    app->addModule(new au::effects::BuiltinEffectsCollectionModule());
    app->addModule(new au::automation::AutomationModule());

    return app;
}

std::shared_ptr<muse::IApplication> AppFactory::newPluginRegistrationApp(const std::shared_ptr<AudacityCmdOptions>& options) const
{
    std::shared_ptr<PluginRegistrationApp> app = std::make_shared<PluginRegistrationApp>(options);

    app->addModule(new muse::audioplugins::AudioPluginsModule());
    app->addModule(new muse::actions::ActionsModule());

    app->addModule(new au::effects::AudioUnitEffectsModule());
    app->addModule(new au::effects::Lv2EffectsModule());
    app->addModule(new au::effects::VstEffectsModule());
    app->addModule(new au::effects::NyquistEffectsModule());

    return app;
}
