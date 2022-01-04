/**********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioIOExtensions.cpp

  Paul Licameli

**********************************************************************/

#include "AudioIOExtensions.h"

AudioIOExtensions *AudioIOExtensions::Get()
{
   return static_cast< AudioIOExtensions* >( AudioIOBase::Get() );
}

AudioIOExtensions::AudioIOExtensions()
{
}

AudioIOExtensions::~AudioIOExtensions()
{
}

void AudioIOExtensions::Initialize(PlaybackSchedule &schedule)
{
   auto &factories = AudioIOExt::GetFactories();
   for (auto &factory: factories)
      if (auto pExt = factory(schedule))
         mAudioIOExt.push_back( move(pExt) );
}

auto AudioIOExtensions::AudioIOExtIterator::operator *() const -> AudioIOExt &
{
   // Down-cast and dereference are safe because only AudioIOExtensions
   // populates the array
   return *static_cast<AudioIOExt*>(mIterator->get());
}
