/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <cstdint>
#include <vector>

class AudacityProject;

class TrackId;
class Track;
class TrackList;
class WaveTrack;
class WaveTrackFactory;

class WaveClip;

class LabelTrack;
class LabelStruct;
using LabelTrackList = std::vector<LabelTrack*>;

namespace au::au3 {
using Au3Project = ::AudacityProject;

using Au3TrackId = ::TrackId;
using Au3Track = ::Track;
using Au3TrackList = ::TrackList;
using Au3WaveTrack = ::WaveTrack;
using Au3WaveTrackFactory = ::WaveTrackFactory;

using Au3ClipId = int64_t;
using Au3WaveClip = ::WaveClip;

using Au3LabelTrackId = ::TrackId;
using Au3LabelTrack = ::LabelTrack;
using Au3LabelTrackList = ::LabelTrackList;
using Au3Label = ::LabelStruct;
}
