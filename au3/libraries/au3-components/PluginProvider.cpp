/**********************************************************************

  Audacity: A Digital Audio Editor

  @file PluginProvider.cpp

**********************************************************************/
#include "PluginProvider.h"

PluginProvider::Validator::~Validator() = default;

PluginProvider::~PluginProvider() = default;

bool PluginProvider::SupportsCustomModulePaths() const
{
    return false;
}

std::unique_ptr<PluginProvider::Validator> PluginProvider::MakeValidator() const
{
    return {};
}
