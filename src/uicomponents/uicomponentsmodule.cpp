/*
* Audacity: A Digital Audio Editor
*/

#include <QQmlEngine>
#include <QtQml>

#include "iapplication.h"

#include "uicomponentsmodule.h"

#include "components/timecodemodeselector.h"
#include "components/timecodemodel.h"
#include "components/bpmmodel.h"

#include "log.h"

using namespace au::uicomponents;

static void uicomponents_init_qrc()
{
    Q_INIT_RESOURCE(au_uicomponents);
}

std::string UiComponentsModule::moduleName() const
{
    return "au::uicomponents";
}

void UiComponentsModule::registerResources()
{
    uicomponents_init_qrc();
}

void UiComponentsModule::registerUiTypes()
{
    qmlRegisterUncreatableType<TimecodeModeSelector>("Audacity.UiComponents", 1, 0, "TimecodeModeSelector",
                                                     "TimecodeModeSelector is a simple enum");
    qmlRegisterType<TimecodeModel>("Audacity.UiComponents", 1, 0, "TimecodeModel");
    qmlRegisterType<BPMModel>("Audacity.UiComponents", 1, 0, "BPMModel");
}

void UiComponentsModule::onInit(const muse::IApplication::RunMode& mode)
{
    UNUSED(mode);
}

void UiComponentsModule::onDeinit()
{
}
