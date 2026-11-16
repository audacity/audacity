/*
 * Audacity: A Digital Audio Editor
 */
#include "AudioDeviceSettings.h"

StringSetting AudioIOHost{
    L"/AudioIO/Host", L"" };
BoolSetting AudioIOAutomaticLatencyCompensation{
    L"/AudioIO/AutomaticLatencyCompensation", true };
DoubleSetting AudioIOLatencyCompensation{
    L"/AudioIO/LatencyCompensation", -130.0 };
DoubleSetting AudioIOLatencyDuration{
    L"/AudioIO/LatencyDuration", 100.0 };
StringSetting AudioIOPlaybackDevice{
    L"/AudioIO/PlaybackDevice", L"" };
StringSetting AudioIOPlaybackSource{
    L"/AudioIO/PlaybackSource", L"" };
DoubleSetting AudioIOPlaybackVolume {
    L"/AudioIO/PlaybackVolume", 1.0 };
IntSetting AudioIORecordChannels{
    L"/AudioIO/RecordChannels", 2 };
StringSetting AudioIORecordingDevice{
    L"/AudioIO/RecordingDevice", L"" };
StringSetting AudioIORecordingSource{
    L"/AudioIO/RecordingSource", L"" };
IntSetting AudioIORecordingSourceIndex{
    L"/AudioIO/RecordingSourceIndex", -1 };
