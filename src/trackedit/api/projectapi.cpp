/*
 * Audacity: A Digital Audio Editor
 */
#include "projectapi.h"

#include <algorithm>
#include <limits>

#include "log.h"
#include "trackedit/itrackeditproject.h"

using namespace au::trackedit;
using namespace au::trackedit::api;

namespace {
constexpr double INVALID_TIME = std::numeric_limits<double>::quiet_NaN();
}

ProjectApi::ProjectApi(muse::api::IApiEngine* e)
    : ApiObject(e)
{
}

ITrackeditProjectPtr ProjectApi::trackeditProject() const
{
    return globalContext() ? globalContext()->currentTrackeditProject() : nullptr;
}

Clips ProjectApi::sortedClips(int trackIndex) const
{
    auto prj = trackeditProject();
    if (!prj) {
        return {};
    }
    auto ids = prj->trackIdList();
    if (trackIndex < 0 || trackIndex >= static_cast<int>(ids.size())) {
        LOGE() << "trackIndex out of range: " << trackIndex << " (have " << ids.size() << " tracks)";
        return {};
    }
    auto clipList = prj->clipList(ids[trackIndex]);
    Clips clips(clipList.begin(), clipList.end());
    std::sort(clips.begin(), clips.end(), [](const Clip& a, const Clip& b) {
        return a.startTime < b.startTime;
    });
    return clips;
}

int ProjectApi::trackCount() const
{
    auto prj = trackeditProject();
    return prj ? static_cast<int>(prj->trackIdList().size()) : 0;
}

int ProjectApi::clipCount(int trackIndex) const
{
    return static_cast<int>(sortedClips(trackIndex).size());
}

double ProjectApi::clipStartTime(int trackIndex, int clipIndex) const
{
    auto clips = sortedClips(trackIndex);
    if (clipIndex < 0 || clipIndex >= static_cast<int>(clips.size())) {
        LOGE() << "clipIndex out of range: " << clipIndex << " on track " << trackIndex;
        return INVALID_TIME;
    }
    return clips[clipIndex].startTime;
}

double ProjectApi::clipEndTime(int trackIndex, int clipIndex) const
{
    auto clips = sortedClips(trackIndex);
    if (clipIndex < 0 || clipIndex >= static_cast<int>(clips.size())) {
        LOGE() << "clipIndex out of range: " << clipIndex << " on track " << trackIndex;
        return INVALID_TIME;
    }
    return clips[clipIndex].endTime;
}

QJSValue ProjectApi::clipsOnTrack(int trackIndex) const
{
    const auto clips = sortedClips(trackIndex);
    QJSValue arr = engine()->newArray(clips.size());
    for (size_t i = 0; i < clips.size(); ++i) {
        QJSValue obj = engine()->newObject();
        obj.setProperty("start", static_cast<double>(clips[i].startTime));
        obj.setProperty("end", static_cast<double>(clips[i].endTime));
        arr.setProperty(static_cast<quint32>(i), obj);
    }
    return arr;
}

double ProjectApi::totalTime() const
{
    auto prj = trackeditProject();
    if (!prj) {
        return INVALID_TIME;
    }
    return static_cast<double>(prj->totalTime());
}

double ProjectApi::selectionStart() const
{
    if (!trackeditProject() || !selectionController()) {
        return INVALID_TIME;
    }
    return static_cast<double>(selectionController()->dataSelectedStartTime());
}

double ProjectApi::selectionEnd() const
{
    if (!trackeditProject() || !selectionController()) {
        return INVALID_TIME;
    }
    return static_cast<double>(selectionController()->dataSelectedEndTime());
}

bool ProjectApi::hasSelection() const
{
    if (!trackeditProject() || !selectionController()) {
        return false;
    }
    return !selectionController()->timeSelectionIsEmpty();
}
