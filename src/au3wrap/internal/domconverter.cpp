#include "domconverter.h"

#include "au3types.h"
#include "trackcolor.h"
#include "trackviewtypeattachment.h"
#include "au3-track/Track.h"
#include "au3-wave-track/WaveClip.h"
#include "au3-wave-track/WaveTrack.h"
#include "au3-label-track/LabelTrack.h"

#include "wxtypes_convert.h"

#include "../au3types.h"

using namespace au::au3;

namespace {
au::trackedit::TrackType trackType(const Au3Track* track)
{
    if (dynamic_cast<const LabelTrack*>(track)) {
        return au::trackedit::TrackType::Label;
    }

    switch (track->NChannels()) {
    case 1:
        return au::trackedit::TrackType::Mono;
    case 2:
        return au::trackedit::TrackType::Stereo;
    default:
        break;
    }

    return au::trackedit::TrackType::Undefined;
}

au::trackedit::TrackFormat trackFormat(const Au3Track* track)
{
    const WaveTrack* waveTrack = dynamic_cast<const WaveTrack*>(track);
    if (!waveTrack) {
        return au::trackedit::TrackFormat::Undefined;
    }

    switch (waveTrack->GetSampleFormat()) {
    case sampleFormat::int16Sample:
        return au::trackedit::TrackFormat::Int16;
    case sampleFormat::int24Sample:
        return au::trackedit::TrackFormat::Int24;
    case sampleFormat::floatSample:
        return au::trackedit::TrackFormat::Float32;
    default:
        return au::trackedit::TrackFormat::Undefined;
    }
}

int trackRate(const Au3Track* track)
{
    const WaveTrack* waveTrack = dynamic_cast<const WaveTrack*>(track);
    if (!waveTrack) {
        return -1;
    }
    return waveTrack->GetRate();
}

au::trackedit::TrackViewType trackViewType(const Au3Track* track)
{
    const auto type = TrackViewTypeAttachment::Get(track).GetTrackViewType();
    if (type == au::trackedit::TrackViewType::Unspecified && dynamic_cast<const WaveTrack*>(track)) {
        // Default to Waveform for WaveTracks
        return au::trackedit::TrackViewType::Waveform;
    } else {
        return type;
    }
}

bool trackSolo(const Au3Track* track)
{
    const auto* playableTrack = dynamic_cast<const PlayableTrack*>(track);
    if (playableTrack == nullptr) {
        return false;
    }

    return playableTrack->GetSolo();
}

bool trackMute(const Au3Track* track)
{
    const auto* playableTrack = dynamic_cast<const PlayableTrack*>(track);
    if (playableTrack == nullptr) {
        return false;
    }

    return playableTrack->GetMute();
}
}

au::trackedit::Clip DomConverter::clip(const Au3WaveTrack* waveTrack, const Au3WaveClip* au3clip)
{
    au::trackedit::Clip clip;
    clip.key.trackId = waveTrack->GetId();
    clip.key.itemId = au3clip->GetId();
    clip.clipVersion = au3clip->GetVersion();

    clip.title = wxToString(au3clip->GetName());
    clip.startTime = au3clip->GetPlayStartTime();
    clip.endTime = au3clip->GetPlayEndTime();
    clip.color = (!wxToString(au3clip->GetColor()).isEmpty()) ? muse::draw::Color(au3clip->GetColor()) : TrackColor::Get(
        waveTrack).GetColor();
    clip.groupId = au3clip->GetGroupId();
    clip.hasCustomColor = !wxToString(au3clip->GetColor()).isEmpty();
    clip.stereo = au3clip->NChannels() > 1;

    clip.pitch = au3clip->GetCentShift();
    clip.speed = au3clip->GetStretchRatio();
    clip.optimizeForVoice = au3clip->GetPitchAndSpeedPreset() == PitchAndSpeedPreset::OptimizeForVoice;

    clip.stretchToMatchTempo = au3clip->GetStretchToMatchProjectTempo();

    return clip;
}

au::trackedit::Track DomConverter::track(const Au3Track* track)
{
    trackedit::Track au4t;
    au4t.id = track->GetId();
    au4t.title = wxToString(track->GetName());
    au4t.type = trackType(track);

    au4t.color = TrackColor::Get(track).GetColor();
    if (!au4t.color.isValid()) { // todo
        TrackColor::Get(track).assignColor();
        au4t.color = TrackColor::Get(track).GetColor();
    }

    au4t.format = trackFormat(track);
    au4t.viewType = trackViewType(track);
    au4t.rate = trackRate(track);
    au4t.solo = trackSolo(track);
    au4t.mute = trackMute(track);
    return au4t;
}

au::trackedit::Track DomConverter::labelTrack(const Au3LabelTrack* labelTrack)
{
    return track(labelTrack);
}

au::trackedit::Label DomConverter::label(const Au3LabelTrack* labelTrack, const Au3Label* au3label)
{
    au::trackedit::Label label;
    label.key.trackId = labelTrack->GetId();
    label.key.itemId = au3label->GetId();

    label.title = wxToString(au3label->title);
    label.startTime = au3label->getT0();
    label.endTime = au3label->getT1();
    label.lowFrequency = au3label->getLowFrequency();
    label.highFrequency = au3label->getHighFrequency();

    label.color = TrackColor::Get(labelTrack).GetColor();

    return label;
}
