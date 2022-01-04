/*!********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file AudioIOExt.cpp
 
 Paul Licameli
 
 **********************************************************************/

#include "AudioIOExt.h"

AudioIOExt::~AudioIOExt() = default;

auto AudioIOExt::GetFactories() -> Factories &
{
   static Factories factories;
   return factories;
}

AudioIOExt::RegisteredFactory::RegisteredFactory(Factory factory)
{
   wxASSERT(!AudioIOBase::Get());
   GetFactories().push_back( move(factory) );
}

AudioIOExt::RegisteredFactory::~RegisteredFactory()
{
   GetFactories().pop_back();
}
