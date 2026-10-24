/*
* Audacity: A Digital Audio Editor
*/

#include "iapplication.h"

#include "uicomponentsmodule.h"

#include "log.h"

using namespace au::uicomponents;

std::string UiComponentsModule::moduleName() const
{
    return "au::uicomponents";
}

void UiComponentsModule::onInit(const muse::IApplication::RunMode& mode)
{
    UNUSED(mode);
}

void UiComponentsModule::onDeinit()
{
}
