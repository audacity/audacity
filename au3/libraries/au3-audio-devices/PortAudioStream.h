/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "portaudio.h"

//! Opens PortAudio streams on the devices and with the latency of the audio settings, minding each host's quirks
namespace PortAudioStream {
struct Request {
    unsigned numPlaybackChannels = 0;
    unsigned numCaptureChannels = 0;
    //! paNonInterleaved may be added
    PaSampleFormat playbackFormat = paFloat32;
    PaSampleFormat captureFormat = paFloat32;
    //! 0 for a bidirectional stream whose devices have no rate in common
    double rate = 0.0;
    //! Where the input is played through: the devices' default latencies, except with MME, rather than the settings'
    bool softwarePlaythrough = false;
};

//! How to open the stream, once the devices are known
struct Parameters {
    PaStreamParameters playback {};
    PaStreamParameters capture {};
    bool usePlayback = false;
    bool useCapture = false;
    //! The requested rate, or, where WASAPI converts it for the playback, the capture device's
    double rate = 0.0;
    bool wasapiAutoConvert = false;
    bool usingAlsa = false;
    bool usingJack = false;
};

//! On the devices of the audio settings; paNoError, or paInvalidDevice
PaError Prepare(const Request& request, Parameters& parameters);

struct Opened {
    PaStream* stream = nullptr;
    //! In seconds, as reported; with JACK, the output's is the latency asked for, PortAudio's being wrong
    double inputLatency = 0.0;
    double outputLatency = 0.0;
};

//! On Linux, shortly after a rescan, retries while the ALSA devices come back
PaError Open(const Parameters& parameters, PaStreamCallback* callback, void* userData, Opened& opened);

//! The ASIO device's current rate, if the stream is on one and the settings say to open at it; else 0
double AsioRate(bool playing);

//! The rate to open at: AsioRate(), else the closest to `requested` the devices support, or 0 if none
double BestRate(bool capturing, bool playing, double requested);
}
