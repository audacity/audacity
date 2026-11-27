/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "au3-preferences/Prefs.h"

namespace AudioDeviceSettings {
extern StringSetting Host;
extern DoubleSetting LatencyCompensation;
extern BoolSetting AutomaticLatencyCompensation;
extern DoubleSetting LatencyDuration;
extern StringSetting PlaybackDevice;
extern StringSetting PlaybackSource;
extern DoubleSetting PlaybackVolume;
extern IntSetting RecordChannels;
extern StringSetting RecordingDevice;
extern StringSetting RecordingSource;
extern IntSetting RecordingSourceIndex;
}
