/*
 * Audacity: A Digital Audio Editor
 */
#include "audiothreadsecurer.h"

using namespace au::audio;

void AudioThreadSecurer::setupMainThread()
{
    m_mainThreadID = std::this_thread::get_id();
}

void AudioThreadSecurer::setupAudioEngineThread()
{
    m_audioEngineThreadID = std::this_thread::get_id();
}

bool AudioThreadSecurer::isMainThread() const
{
    return m_mainThreadID == std::this_thread::get_id();
}

std::thread::id AudioThreadSecurer::mainThreadId() const
{
    return m_mainThreadID;
}

bool AudioThreadSecurer::isAudioEngineThread() const
{
    return m_audioEngineThreadID == std::this_thread::get_id();
}

std::thread::id AudioThreadSecurer::audioEngineThreadId() const
{
    return m_audioEngineThreadID;
}
