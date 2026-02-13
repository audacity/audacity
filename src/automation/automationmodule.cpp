#include "automationmodule.h"

#include "internal/au3/au3clipgaininteraction.h"

#include "view/clipenvelopemodel.h"

using namespace au::automation;

AutomationModule::AutomationModule()
{}

std::string AutomationModule::moduleName() const
{
    return "automation";
}

void AutomationModule::registerExports()
{
    ioc()->registerExport<IClipGainInteraction>(moduleName(), new Au3ClipGainInteraction(iocContext()));
}

void AutomationModule::registerUiTypes()
{
    qmlRegisterType<ClipEnvelopeModel>("Audacity.Automation", 1, 0, "ClipEnvelopeModel");
}
