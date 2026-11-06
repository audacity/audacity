/*
* Audacity: A Digital Audio Editor
*/
#pragma once

class AudacityProject;
class ChannelGroup;

namespace au::au4juce {
//! The latency, in samples, of the enabled realtime effects of a group (MasterGroup for the master effects)
int effectsLatency(const AudacityProject& project, const ChannelGroup* group, double sampleRate);
}
