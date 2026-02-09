/*
 * Audacity: A Digital Audio Editor
 */
#include "appfactory.h"

#include "guiapp.h"
#include "pluginregistrationapp.h"

#include "muse_framework_config.h"

#include "framework/global/modularity/ioc.h"
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
#include "framework/learn/learnmodule.h"
#include "framework/languages/languagesmodule.h"
#include "framework/workspace/workspacemodule.h"

#include "framework/stubs/multiwindows/multiwindowsstubmodule.h"

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
#include "effects/nyquist/nyquisteffectsmodule.h"
#include "importexport/import/importermodule.h"
#include "importexport/export/exportermodule.h"
#include "importexport/labels/labelsmodule.h"

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

#ifdef MUSE_MODULE_AUTOBOT
#include "autobot/autobotmodule.h"
#endif

#ifdef MUSE_MODULE_EXTENSIONS
#include "framework/extensions/extensionsmodule.h"
#else
#include "framework/stubs/extensions/extensionsstubmodule.h"
#endif

#include "au3wrap/au3wrapmodule.h"

using namespace muse;
using namespace au::app;

std::shared_ptr<muse::IApplication> AppFactory::newApp(const CommandLineParser& parser) const
{
    IApplication::RunMode runMode = parser.runMode();

    switch (runMode) {
    case IApplication::RunMode::GuiApp:
        return newGuiApp(parser.options());
    case IApplication::RunMode::ConsoleApp:
        // No console mode for now
        return newGuiApp(parser.options());
    case IApplication::RunMode::AudioPluginRegistration:
        return newPluginRegistrationApp(parser.audioPluginRegistration());
    }

    return newGuiApp(parser.options());
}

std::shared_ptr<muse::IApplication> AppFactory::newGuiApp(const CommandLineParser::Options& options) const
{
    modularity::ContextPtr ctx = modularity::globalCtx();
    std::shared_ptr<GuiApp> app = std::make_shared<GuiApp>(options, ctx);

    //! NOTE `diagnostics` must be first, because it installs the crash handler.
    //! For other modules, the order is (and should be) unimportant.
    app->addModule(new muse::diagnostics::DiagnosticsModule());

    app->addModule(new muse::audioplugins::AudioPluginsModule());
    app->addModule(new muse::actions::ActionsModule());
    app->addModule(new muse::draw::DrawModule());
    app->addModule(new muse::workspace::WorkspaceModule());
    app->addModule(new muse::accessibility::AccessibilityModule());
    app->addModule(new muse::interactive::InteractiveModule());
    app->addModule(new muse::mi::MultiInstancesModule());
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
    app->addModule(new muse::extensions::ExtensionsModule());
#ifdef MUSE_MODULE_AUTOBOT
    app->addModule(new muse::autobot::AutobotModule());
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
    app->addModule(new au::effects::EffectsModule());
    app->addModule(new au::effects::BuiltinEffectsModule());

    return app;
}

std::shared_ptr<muse::IApplication> AppFactory::newPluginRegistrationApp(const CommandLineParser::AudioPluginRegistration& task) const
{
    modularity::ContextPtr ctx = modularity::globalCtx();
    std::shared_ptr<PluginRegistrationApp> app = std::make_shared<PluginRegistrationApp>(task, ctx);

    //! NOTE `diagnostics` must be first, because it installs the crash handler.
    app->addModule(new muse::diagnostics::DiagnosticsModule());

    app->addModule(new muse::audioplugins::AudioPluginsModule());
    app->addModule(new muse::actions::ActionsModule());

    app->addModule(new au::appshell::AppShellModule());
    app->addModule(new au::preferences::PreferencesModule());
    app->addModule(new au::uicomponents::UiComponentsModule());
    app->addModule(new au::effects::AudioUnitEffectsModule());
    app->addModule(new au::effects::Lv2EffectsModule());
    app->addModule(new au::effects::VstEffectsModule());
    app->addModule(new au::effects::NyquistEffectsModule());

    return app;
}
