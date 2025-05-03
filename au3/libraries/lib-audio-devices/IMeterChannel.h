/*
* Audacity: A Digital Audio Editor
*/
#pragma once

constexpr int64_t MAIN_METER_KEY = -1;

class AUDIO_DEVICES_API IMeterChannel
{
public:
    virtual ~IMeterChannel() = default;
    virtual void push(uint8_t channel, float signal, int64_t key = MAIN_METER_KEY) = 0;
    virtual void sendAll() = 0;
    virtual void reset() = 0;
};
