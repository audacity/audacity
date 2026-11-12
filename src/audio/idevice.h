/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>
#include <string>

#include "audio/audioconfigurationtypes.h"

namespace au::audio {
//! What a device calls back on its thread, with a buffer per channel; a null buffer is a channel to ignore
class IDeviceCallback
{
public:
    virtual ~IDeviceCallback() = default;

    //! The output is to be written, all of it
    virtual void process(const float* const* input, int numInputChannels, float* const* output, int numOutputChannels, int frames) = 0;
};

//! An open audio device; closed when destroyed
class IDevice
{
public:
    virtual ~IDevice() = default;

    virtual double sampleRate() const = 0;
    //! The most frames it calls back with at once, as far as it knows: callbacks may be longer
    virtual int blockSize() const = 0;
    virtual int numInputChannels() const = 0;
    virtual int numOutputChannels() const = 0;
    //! As the device reports them, in seconds
    virtual double inputLatency() const = 0;
    virtual double outputLatency() const = 0;

    //! Calls `callback` back until stopped
    virtual void start(IDeviceCallback& callback) = 0;
    //! Once returned, it calls back no more
    virtual void stop() = 0;
    //! Underruns of the output, and overflows of the input if open, so far
    virtual int xrunCount() const = 0;
};

//! Opens devices of an audio library, as the audio settings say
class IDeviceOpener
{
public:
    virtual ~IDeviceOpener() = default;

    //! In logs
    virtual std::string name() const = 0;
    //! With as many of the requested channels as the devices have, at `sampleRate` or the closest the devices support; null
    //! if it can't, with `error` saying why
    virtual std::unique_ptr<IDevice> open(const audio::AudioConfiguration& configuration, int numInputChannels, int numOutputChannels,
                                          double sampleRate, std::string& error) = 0;
};
}
