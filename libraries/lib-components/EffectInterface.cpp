/**********************************************************************

  Audacity: A Digital Audio Editor

  @file EffectInterface.cpp

**********************************************************************/
#include "EffectInterface.h"

EffectDefinitionInterface::~EffectDefinitionInterface() = default;

bool EffectDefinitionInterface::EnablesDebug()
{
   return false;
}

EffectClientInterface::~EffectClientInterface() = default;

EffectUIClientInterface::~EffectUIClientInterface() = default;
