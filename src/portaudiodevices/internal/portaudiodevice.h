/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <atomic>
#include <memory>
#include <string>

#include "portaudio.h"

#include "au3-audio-devices/DeviceMixer.h"

#include "audio/idevice.h"

namespace au::portaudiodevices {
//! A PortAudio stream, opened as AudioIO opens it: see PortAudioStream
class PortAudioDevice final : public audio::IDevice
{
public:
    //! `mixer`, of the selected devices, must outlive this
    explicit PortAudioDevice(DeviceMixer& mixer);
    ~PortAudioDevice() override;

    //! With as many of the requested channels as the devices have; false if it can't, with `error` saying why
    bool open(int numInputChannels, int numOutputChannels, double sampleRate, std::string& error);

    double sampleRate() const override;
    int blockSize() const override;
    int numInputChannels() const override { return m_numInputChannels; }
    int numOutputChannels() const override { return m_numOutputChannels; }
    double inputLatency() const override { return m_inputLatency; }
    double outputLatency() const override { return m_outputLatency; }

    void start(audio::IDeviceCallback& callback) override;
    void stop() override;
    int xrunCount() const override { return m_xruns; }

private:
    static int callback(const void* input, void* output, unsigned long frames, const PaStreamCallbackTimeInfo*,
                        PaStreamCallbackFlags flags, void* userData);

    DeviceMixer& m_mixer;
    PaStream* m_stream = nullptr;
    int m_numInputChannels = 0;
    int m_numOutputChannels = 0;
    double m_inputLatency = 0.0;
    double m_outputLatency = 0.0;
    bool m_started = false;
    audio::IDeviceCallback* m_callback = nullptr;
    std::atomic<int> m_xruns { 0 };
};

//! The hardware volume of the devices of AudioIO's settings, through their mixer, as AudioIO controls it
class PortAudioHardwareVolume final : public audio::IHardwareVolume
{
public:
    void reopen() override;
    bool inputVolumeWorks() const override;
    float inputVolume() const override;
    void setInputVolume(float volume) override;

    DeviceMixer& deviceMixer();

private:
    void openOnce() const;

    mutable DeviceMixer m_mixer;
    mutable bool m_opened = false;
};

//! Opens PortAudio streams on the devices of AudioIO's settings
class PortAudioDeviceOpener final : public audio::IDeviceOpener
{
public:
    audio::IHardwareVolume* hardwareVolume() override { return &m_hardwareVolume; }

    std::string name() const override { return "PortAudio"; }
    //! `configuration` is that of AudioIO's settings, which PortAudioStream reads
    std::unique_ptr<audio::IDevice> open(const audio::AudioConfiguration& configuration, int numInputChannels, int numOutputChannels,
                                         double sampleRate, std::string& error) override;

private:
    PortAudioHardwareVolume m_hardwareVolume;
};
}
