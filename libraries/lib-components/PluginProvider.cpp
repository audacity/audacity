/**********************************************************************

  Audacity: A Digital Audio Editor

  @file PluginProvider.cpp

**********************************************************************/
#include "PluginProvider.h"

PluginProvider::~PluginProvider() = default;

std::unique_ptr<PluginProvider::Validator> PluginProvider::MakeValidator() const
{
   return {};
}
