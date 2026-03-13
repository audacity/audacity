#include "spectrogrammodule.h"

#include "internal/spectraleffectsregister.h"
#include "internal/spectrogramuiactions.h"
#include "internal/spectrogramactionscontroller.h"
#include "internal/globalspectrogramconfiguration.h"
#include "internal/spectrogramservice.h"
#include "internal/frequencyselectioncontroller.h"
#include "internal/au3/au3spectrogrampainter.h"
#include "internal/au3/au3peakfinderfactory.h"

#include "view/abstractspectrogramsettingsmodel.h"
#include "view/algorithmsectionparameterlistmodel.h"
#include "view/colorsectionparameterlistmodel.h"
#include "view/spectrogramchannelrulermodel.h"
#include "view/spectrogramviewservice.h"
#include "view/scalesectionparameterlistmodel.h"
#include "view/globalspectrogramsettingsmodel.h"
#include "view/trackspectrogramcontextmenumodel.h"
#include "view/trackspectrogramsettingsmodel.h"

#include "view/spectrogramhit.h"
#include "view/clipchannelspectrogramview.h"
#include "view/channelspectralselectionmodel.h"

#include "framework/interactive/iinteractiveuriregister.h"

static void spectrogram_init_qrc()
{
    Q_INIT_RESOURCE(spectrogram);
}

namespace au::spectrogram {
static const std::string mname("spectrogram");

std::string SpectrogramModule::moduleName() const
{
    return mname;
}

void SpectrogramModule::registerResources()
{
    spectrogram_init_qrc();
}

void SpectrogramModule::registerExports()
{
    m_configuration = std::make_shared<GlobalSpectrogramConfiguration>();

    globalIoc()->registerExport<IGlobalSpectrogramConfiguration>(mname, m_configuration);
}

void SpectrogramModule::resolveImports()
{
    auto ir = globalIoc()->resolve<muse::interactive::IInteractiveUriRegister>(mname);
    if (ir) {
        ir->registerQmlUri(muse::Uri(TRACK_SPECTROGRAM_SETTINGS_ACTION), "Audacity/Spectrogram/TrackSpectrogramSettingsDialog.qml");
    }
}

void SpectrogramModule::registerUiTypes()
{
    qmlRegisterUncreatableType<AbstractSpectrogramSettingsModel>("Audacity.Spectrogram", 1, 0, "AbstractSpectrogramSettingsModel",
                                                                 "Abstract base class");
    qmlRegisterType<GlobalSpectrogramSettingsModel>("Audacity.Spectrogram", 1, 0, "GlobalSpectrogramSettingsModel");
    qmlRegisterType<AlgorithmSectionParameterListModel>("Audacity.Spectrogram", 1, 0, "AlgorithmSectionParameterListModel");
    qmlRegisterType<ColorSectionParameterListModel>("Audacity.Spectrogram", 1, 0, "ColorSectionParameterListModel");
    qmlRegisterType<ScaleSectionParameterListModel>("Audacity.Spectrogram", 1, 0, "ScaleSectionParameterListModel");
    qmlRegisterType<ClipChannelSpectrogramView>("Audacity.Spectrogram", 1, 0, "ClipChannelSpectrogramView");
    qmlRegisterType<ChannelSpectralSelectionModel>("Audacity.Spectrogram", 1, 0, "ChannelSpectralSelectionModel");
    qmlRegisterType<SpectrogramHit>("Audacity.Spectrogram", 1, 0, "SpectrogramHit");
    qmlRegisterType<SpectrogramChannelRulerModel>("Audacity.Spectrogram", 1, 0, "SpectrogramChannelRulerModel");
    qmlRegisterType<TrackSpectrogramContextMenuModel>("Audacity.Spectrogram", 1, 0, "TrackSpectrogramContextMenuModel");
    qmlRegisterType<TrackSpectrogramSettingsModel>("Audacity.Spectrogram", 1, 0, "TrackSpectrogramSettingsModel");
    qmlRegisterSingletonType<SpectrogramHitFactory>("Audacity.Spectrogram", 1, 0, "SpectrogramHitFactory",
                                                    [](QQmlEngine*, QJSEngine*) -> QObject* {
        return new SpectrogramHitFactory();
    });
}

void SpectrogramModule::onInit(const muse::IApplication::RunMode&)
{
    m_configuration->init();
}

muse::modularity::IContextSetup* SpectrogramModule::newContext(const muse::modularity::ContextPtr& ctx) const
{
    return new SpectrogramContext(ctx);
}

// =====================================================
// SpectrogramContext
// =====================================================

void SpectrogramContext::registerExports()
{
    m_au3SpectrogramPainter = std::make_shared<Au3SpectrogramPainter>(iocContext());
    m_spectrogramService = std::make_shared<SpectrogramService>(iocContext());
    m_spectrogramActionsController = std::make_shared<SpectrogramActionsController>(iocContext());
    m_spectrogramViewService = std::make_shared<SpectrogramViewService>(iocContext());

    ioc()->registerExport<ISpectralEffectsRegister>(mname, new SpectralEffectsRegister);
    ioc()->registerExport<ISpectrogramPainter>(mname, m_au3SpectrogramPainter);
    ioc()->registerExport<ISpectrogramService>(mname, m_spectrogramService);
    ioc()->registerExport<IPeakFinderFactory>(mname, new Au3PeakFinderFactory(iocContext()));
    ioc()->registerExport<IFrequencySelectionController>(mname, new FrequencySelectionController(iocContext()));
    ioc()->registerExport<ISpectrogramViewService>(mname, m_spectrogramViewService);
}

void SpectrogramContext::resolveImports()
{
    auto ar = ioc()->resolve<muse::ui::IUiActionsRegister>(mname);
    if (ar) {
        ar->reg(std::make_shared<SpectrogramUiActions>(iocContext()));
    }
}

void SpectrogramContext::onInit(const muse::IApplication::RunMode&)
{
    m_spectrogramActionsController->init();
    m_au3SpectrogramPainter->init();
    m_spectrogramService->init();
    m_spectrogramViewService->init();
}
}
