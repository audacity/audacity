/*
 * Audacity: A Digital Audio Editor
 */
#include "AudioDeviceSettings.h"

namespace AudioDeviceSettings {

StringSetting Host{
    L"/AudioIO/Host", L"" };
BoolSetting AutomaticLatencyCompensation{
    L"/AudioIO/AutomaticLatencyCompensation", true };
DoubleSetting LatencyCompensation{
    L"/AudioIO/LatencyCompensation", -130.0 };
DoubleSetting LatencyDuration{
    L"/AudioIO/LatencyDuration", 100.0 };
StringSetting PlaybackDevice{
    L"/AudioIO/PlaybackDevice", L"" };
StringSetting PlaybackSource{
    L"/AudioIO/PlaybackSource", L"" };
DoubleSetting PlaybackVolume {
    L"/AudioIO/PlaybackVolume", 1.0 };
IntSetting RecordChannels{
    L"/AudioIO/RecordChannels", 2 };
StringSetting RecordingDevice{
    L"/AudioIO/RecordingDevice", L"" };
StringSetting RecordingSource{
    L"/AudioIO/RecordingSource", L"" };
IntSetting RecordingSourceIndex{
    L"/AudioIO/RecordingSourceIndex", -1 };
}
