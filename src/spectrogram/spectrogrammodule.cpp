#include "spectrogrammodule.h"

#include "internal/globalspectrogramconfiguration.h"
#include "internal/au3/au3spectrogrampainter.h"

#include "view/abstractspectrogramsettingsmodel.h"
#include "view/algorithmsectionparameterlistmodel.h"
#include "view/colorsectionparameterlistmodel.h"
#include "view/globalspectrogramsettingsmodel.h"

namespace au::spectrogram {
SpectrogramModule::SpectrogramModule()
    : m_au3SpectrogramPainter(std::make_shared<Au3SpectrogramPainter>()),
    m_configuration(std::make_shared<GlobalSpectrogramConfiguration>())
{
}

std::string SpectrogramModule::moduleName() const
{
    return "spectrogram";
}

void SpectrogramModule::registerExports()
{
    ioc()->registerExport<IGlobalSpectrogramConfiguration>(moduleName(), m_configuration);
    ioc()->registerExport<ISpectrogramPainter>(moduleName(), m_au3SpectrogramPainter);
}

void SpectrogramModule::registerUiTypes()
{
    qmlRegisterUncreatableType<AbstractSpectrogramSettingsModel>("Audacity.Spectrogram", 1, 0, "AbstractSpectrogramSettingsModel",
                                                                 "Abstract base class");
    qmlRegisterType<GlobalSpectrogramSettingsModel>("Audacity.Spectrogram", 1, 0, "GlobalSpectrogramSettingsModel");
    qmlRegisterType<AlgorithmSectionParameterListModel>("Audacity.Spectrogram", 1, 0, "AlgorithmSectionParameterListModel");
    qmlRegisterType<ColorSectionParameterListModel>("Audacity.Spectrogram", 1, 0, "ColorSectionParameterListModel");
}

void SpectrogramModule::onInit(const muse::IApplication::RunMode&)
{
    m_au3SpectrogramPainter->init();
    m_configuration->init();
}
}
