#include "spectrogrammodule.h"

#include "internal/spectrogramconfiguration.h"
#include "view/abstractspectrogramsettingsmodel.h"
#include "view/algorithmsectionparameterlistmodel.h"
#include "view/colorsectionparameterlistmodel.h"
#include "view/globalspectrogramsettingsmodel.h"

#include "framework/ui/iinteractiveuriregister.h"

static void spectrogram_init_qrc()
{
    Q_INIT_RESOURCE(spectrogram);
}

namespace au::spectrogram {
SpectrogramModule::SpectrogramModule()
    : m_configuration(std::make_shared<SpectrogramConfiguration>())
{
}

std::string SpectrogramModule::moduleName() const
{
    return "spectrogram";
}

void SpectrogramModule::registerExports()
{
    ioc()->registerExport<ISpectrogramConfiguration>(moduleName(), m_configuration);
}

void SpectrogramModule::registerUiTypes()
{
    qmlRegisterUncreatableType<AbstractSpectrogramSettingsModel>("Audacity.Spectrogram", 1, 0, "AbstractSpectrogramSettingsModel",
                                                                 "Abstract base class");
    qmlRegisterType<GlobalSpectrogramSettingsModel>("Audacity.Spectrogram", 1, 0, "GlobalSpectrogramSettingsModel");
    qmlRegisterType<AlgorithmSectionParameterListModel>("Audacity.Spectrogram", 1, 0, "AlgorithmSectionParameterListModel");
    qmlRegisterType<ColorSectionParameterListModel>("Audacity.Spectrogram", 1, 0, "ColorSectionParameterListModel");
}

void SpectrogramModule::registerResources()
{
    spectrogram_init_qrc();
}

void SpectrogramModule::onInit(const muse::IApplication::RunMode&)
{
    m_configuration->init();
}
}
