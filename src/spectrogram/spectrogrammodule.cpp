#include "spectrogrammodule.h"

#include "internal/globalspectrogramconfiguration.h"
#include "internal/spectrogramservice.h"
#include "internal/au3/au3spectrogrampainter.h"

#include "view/abstractspectrogramsettingsmodel.h"
#include "view/algorithmsectionparameterlistmodel.h"
#include "view/colorsectionparameterlistmodel.h"
#include "view/scalesectionparameterlistmodel.h"
#include "view/globalspectrogramsettingsmodel.h"
#include "view/spectrogramhit.h"
#include "view/clipchannelspectrogramview.h"

static void spectrogram_init_qrc()
{
    Q_INIT_RESOURCE(spectrogram);
}

namespace au::spectrogram {
SpectrogramModule::SpectrogramModule()
    : m_au3SpectrogramPainter(std::make_shared<Au3SpectrogramPainter>()),
    m_configuration(std::make_shared<GlobalSpectrogramConfiguration>()),
    m_spectrogramService(std::make_shared<SpectrogramService>())
{
}

std::string SpectrogramModule::moduleName() const
{
    return "spectrogram";
}

void SpectrogramModule::registerResources()
{
    spectrogram_init_qrc();
}

void SpectrogramModule::registerExports()
{
    ioc()->registerExport<IGlobalSpectrogramConfiguration>(moduleName(), m_configuration);
    ioc()->registerExport<ISpectrogramPainter>(moduleName(), m_au3SpectrogramPainter);
    ioc()->registerExport<ISpectrogramService>(moduleName(), m_spectrogramService);
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

    qmlRegisterType<SpectrogramHit>("Audacity.Spectrogram", 1, 0, "SpectrogramHit");
    qmlRegisterSingletonType<SpectrogramHitFactory>("Audacity.Spectrogram", 1, 0, "SpectrogramHitFactory",
                                                    [](QQmlEngine*, QJSEngine*) -> QObject* {
        return new SpectrogramHitFactory();
    });
}

void SpectrogramModule::onInit(const muse::IApplication::RunMode&)
{
    m_au3SpectrogramPainter->init();
    m_configuration->init();
    m_spectrogramService->init();
}
}
