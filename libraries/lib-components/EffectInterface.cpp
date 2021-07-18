/**********************************************************************

  Audacity: A Digital Audio Editor

  @file EffectInterface.cpp

**********************************************************************/
#include "EffectInterface.h"

EffectDefinitionInterface::~EffectDefinitionInterface() = default;

EffectType EffectDefinitionInterface::GetClassification()
{
   return GetType();
}

bool EffectDefinitionInterface::EnablesDebug()
{
   return false;
}

ManualPageID EffectDefinitionInterface::ManualPage()
{
   return {};
}

FilePath EffectDefinitionInterface::HelpPage()
{
   return {};
}

bool EffectDefinitionInterface::IsHiddenFromMenus()
{
   return false;
}

//bool EffectDefinitionInterface::DefineParams(ShuttleParams & S)
//{
//   return false;
//}

EffectClientInterface::~EffectClientInterface() = default;

EffectUIClientInterface::~EffectUIClientInterface() = default;
