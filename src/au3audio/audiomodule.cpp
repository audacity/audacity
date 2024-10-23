/*
* Audacity: A Digital Audio Editor
*/
#include "audiomodule.h"

#include "modularity/ioc.h"

using namespace au::audio;
using namespace muse::modularity;

std::string AudioModule::moduleName() const
{
    return "audio";
}

void AudioModule::registerExports()
{
}

void AudioModule::resolveImports()
{
}

void AudioModule::registerResources()
{
}

void AudioModule::registerUiTypes()
{
}

void AudioModule::onInit(const muse::IApplication::RunMode&)
{
}

void AudioModule::onDeinit()
{
}
