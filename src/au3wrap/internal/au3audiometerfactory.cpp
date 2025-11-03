/*
 * Audacity: A Digital Audio Editor
 */
#include "au3audiometerfactory.h"
#include "auaudio/iaudiometer.h"

std::shared_ptr<au::au3::Au3AudioMeter> au::au3::createAudioMeter()
{
    return std::make_shared<Au3AudioMeter>(auaudio::IAudioMeter::create());
}
