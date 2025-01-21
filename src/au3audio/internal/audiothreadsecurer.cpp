/*
 * Audacity: A Digital Audio Editor
 */
#include "audiothreadsecurer.h"

using namespace au::audio;

void AudioThreadSecurer::setupMainThread()
{
    m_mainThreadID = std::this_thread::get_id();
}

void AudioThreadSecurer::setupWorkerThread()
{
    m_workerThreadID = std::this_thread::get_id();
}

bool AudioThreadSecurer::isMainThread() const
{
    return m_mainThreadID == std::this_thread::get_id();
}

std::thread::id AudioThreadSecurer::mainThreadId() const
{
    return m_mainThreadID;
}

bool AudioThreadSecurer::isAudioWorkerThread() const
{
    return m_workerThreadID == std::this_thread::get_id();
}

std::thread::id AudioThreadSecurer::workerThreadId() const
{
    return m_workerThreadID;
}
