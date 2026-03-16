/*
* Audacity: A Digital Audio Editor
*/

#include "au3clipgaininteraction.h"

#include "au3-mixer/Envelope.h"
#include "au3-wave-track/WaveClip.h"
#include "au3-wave-track/WaveTrack.h"
#include "au3wrap/au3types.h"

#include "au3wrap/internal/domaccessor.h"
#include "au3wrap/internal/domconverter.h"

#include "automationtypes.h"

#include <algorithm>

using namespace au::automation;
using namespace au::au3;

namespace {
// NOTE: newly recorded clips doesn't have offset/sequenceLength synced with envelope.
// We need to sync it before interacting with clip gain
void syncEnvelopeOffset(const std::shared_ptr<Au3WaveClip>& clip, Envelope& env)
{
    const double sequenceStartTime = clip->GetSequenceStartTime();
    if (env.GetOffset() != sequenceStartTime) {
        env.SetOffset(sequenceStartTime);
    }

    // Recording path may keep clip envelope length stale (e.g. still zero)
    // while sequence data grows via linked pending clip updates.
    // Ensure envelope domain matches current sequence duration.
    const double sequenceLen = clip->GetSequenceEndTime() - sequenceStartTime;
    if (env.GetTrackLen() != sequenceLen) {
        env.SetTrackLen(sequenceLen, 1.0 / clip->GetRate());
    }
}

void restoreEnvelopeFromSnapshot(Envelope& env, const AutomationDragSession& session)
{
    env.Clear();
    for (const auto& p : session.originalPoints) {
        env.Insert(p.xValue, p.yValue);
    }
}

// How many original points have we crossed while dragging to new time?
int computeRightCrossedIndex(const AutomationDragSession& session, double time)
{
    int idx = session.originalIndex;
    for (int i = session.originalIndex + 1; i < static_cast<int>(session.originalPoints.size()); ++i) {
        const double pointTime = session.originalPoints[i].xValue;
        if (pointTime > session.originalXValue && pointTime <= time) {
            idx = i;
        } else {
            break;
        }
    }
    return idx;
}

// How many original points have we crossed while dragging to new time?
int computeLeftCrossedIndex(const AutomationDragSession& session, double time)
{
    int idx = session.originalIndex;
    for (int i = session.originalIndex - 1; i >= 0; --i) {
        const double pointTime = session.originalPoints[i].xValue;
        if (pointTime < session.originalXValue && pointTime >= time) {
            idx = i;
        } else {
            break;
        }
    }
    return idx;
}

void applyDragWithCrossingRemoval(Envelope& env, AutomationDragSession& session, double newTime, double newValue)
{
    const int desiredRight = computeRightCrossedIndex(session, newTime);
    const int desiredLeft = computeLeftCrossedIndex(session, newTime);

    while (session.lastConsumedRight < desiredRight) {
        env.Delete(session.currentDragIndex + 1);
        ++session.lastConsumedRight;
    }
    while (session.lastConsumedRight > desiredRight) {
        const auto& restore = session.originalPoints[session.lastConsumedRight];
        env.Insert(session.currentDragIndex + 1, EnvPoint { restore.xValue, restore.yValue });
        --session.lastConsumedRight;
    }

    while (session.lastConsumedLeft > desiredLeft) {
        env.Delete(session.currentDragIndex - 1);
        --session.currentDragIndex;
        --session.lastConsumedLeft;
    }
    while (session.lastConsumedLeft < desiredLeft) {
        const auto& restore = session.originalPoints[session.lastConsumedLeft];
        env.Insert(session.currentDragIndex, EnvPoint { restore.xValue, restore.yValue });
        ++session.currentDragIndex;
        ++session.lastConsumedLeft;
    }

    env.SetDragPoint(session.currentDragIndex);
    env.MoveDragPoint(newTime, newValue);
}
}

Au3ClipGainInteraction::Au3ClipGainInteraction(const muse::modularity::ContextPtr& ctx)
    : muse::Injectable(ctx) {}

std::optional<AutomationInfo> Au3ClipGainInteraction::clipGainInfo(
    const trackedit::ClipKey& clipKey) const
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    if (!waveTrack) {
        return {};
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
    if (!clip) {
        return {};
    }

    auto& env = clip->GetEnvelope();
    syncEnvelopeOffset(clip, env);

    AutomationInfo info;
    info.minValue = env.GetMinValue();
    info.maxValue = env.GetMaxValue();
    info.defaultValue = env.GetDefaultValue();
    info.exponential = env.GetExponential();
    info.version = env.GetVersion();

    return info;
}

AutomationPoints Au3ClipGainInteraction::clipGainPoints(const trackedit::ClipKey& clipKey) const
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    if (!waveTrack) {
        return {};
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
    if (!clip) {
        return {};
    }

    auto& env = clip->GetEnvelope();
    syncEnvelopeOffset(clip, env);

    AutomationPoints pts;
    const auto n = env.GetNumberOfPoints();
    pts.reserve(n);

    const double offset = env.GetOffset();
    for (size_t i = 0; i < n; ++i) {
        const auto& p = env[int(i)];
        pts.push_back({ offset + p.GetT(), p.GetVal() });
    }
    return pts;
}

bool Au3ClipGainInteraction::setClipGainPoint(const trackedit::ClipKey& clipKey, double time, double value, bool completed)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    auto& env = clip->GetEnvelope();
    syncEnvelopeOffset(clip, env);

    const double clampedTime = std::clamp(time, clip->GetPlayStartTime(), clip->GetPlayEndTime());
    const double v = std::clamp(value, env.GetMinValue(), env.GetMaxValue());
    env.InsertOrReplace(clampedTime, v);

    if (auto prj = globalContext()->currentTrackeditProject()) {
        prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, clip.get()));
    }
    projectHistory()->pushHistoryState(muse::trc("trackedit", "Added enveloped point"), muse::trc("trackedit",
                                                                                                  "Clip envelope edit"));

    m_clipEnvelopeChanged.send(clipKey, completed);
    return true;
}

bool Au3ClipGainInteraction::removeClipGainPoint(const trackedit::ClipKey& clipKey, int index, bool completed)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    auto& env = clip->GetEnvelope();
    syncEnvelopeOffset(clip, env);
    if (index < 0 || index >= int(env.GetNumberOfPoints())) {
        return false;
    }

    env.Delete(index);

    if (auto prj = globalContext()->currentTrackeditProject()) {
        prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, clip.get()));
    }
    projectHistory()->pushHistoryState(muse::trc("trackedit", "Removed enveloped point"), muse::trc("trackedit",
                                                                                                    "Clip envelope edit"));

    m_clipEnvelopeChanged.send(clipKey, completed);
    return true;
}

bool Au3ClipGainInteraction::setClipGainPointAtIndex(
    const trackedit::ClipKey& key, int index, double time, double value, bool completed)
{
    const auto points = clipGainPoints(key);
    if (index < 0 || index >= static_cast<int>(points.size())) {
        return false;
    }

    const double oldTime = points.at(index).xValue;
    if (muse::is_equal(oldTime, time)) {
        return setClipGainPoint(key, time, value, completed);
    }

    return setClipGainPoint(key, time, value, completed);
}

bool Au3ClipGainInteraction::beginClipGainPointDrag(const trackedit::ClipKey& clipKey, int pointIndex)
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    auto& env = clip->GetEnvelope();
    syncEnvelopeOffset(clip, env);

    const int n = static_cast<int>(env.GetNumberOfPoints());
    if (pointIndex < 0 || pointIndex >= n) {
        return false;
    }

    if (m_envDrag && m_envDrag->active) {
        endClipGainPointDrag(m_envDrag->clip, true);
    }

    AutomationDragSession s;
    s.clip = clipKey;
    s.originalIndex = pointIndex;
    s.originalXValue = env[pointIndex].GetT();
    s.originalYValue = env[pointIndex].GetVal();
    s.active = true;

    s.currentDragIndex = pointIndex;
    s.lastConsumedLeft = pointIndex;
    s.lastConsumedRight = pointIndex;
    s.originalPoints.reserve(n);
    for (int i = 0; i < n; ++i) {
        const auto& p = env[i];
        s.originalPoints.push_back({ p.GetT(), p.GetVal() });
    }

    env.SetDragPoint(pointIndex);

    m_envDrag = s;
    return true;
}

bool Au3ClipGainInteraction::updateClipGainPointDrag(const trackedit::ClipKey& clipKey, double tAbs, double value)
{
    if (!m_envDrag || !m_envDrag->active || !(m_envDrag->clip == clipKey)) {
        return false;
    }

    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    auto& env = clip->GetEnvelope();
    syncEnvelopeOffset(clip, env);
    const double clampedTAbs = std::clamp(tAbs, clip->GetPlayStartTime(), clip->GetPlayEndTime());
    const double clampedValue = std::clamp(value, env.GetMinValue(), env.GetMaxValue());
    applyDragWithCrossingRemoval(env, *m_envDrag, clampedTAbs - env.GetOffset(), clampedValue);

    if (auto prj = globalContext()->currentTrackeditProject()) {
        prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, clip.get()));
    }

    m_clipEnvelopeChanged.send(clipKey, false);
    return true;
}

bool Au3ClipGainInteraction::endClipGainPointDrag(const trackedit::ClipKey& clipKey, bool commit)
{
    if (!m_envDrag || !m_envDrag->active || !(m_envDrag->clip == clipKey)) {
        return false;
    }

    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    auto& env = clip->GetEnvelope();
    syncEnvelopeOffset(clip, env);

    if (commit) {
        env.ClearDragPoint();
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Dragged enveloped point"), muse::trc("trackedit",
                                                                                                        "Clip envelope edit"));
    } else {
        restoreEnvelopeFromSnapshot(env, *m_envDrag);
        env.ClearDragPoint();
    }

    if (auto prj = globalContext()->currentTrackeditProject()) {
        prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, clip.get()));
    }

    m_clipEnvelopeChanged.send(clipKey, true);
    m_envDrag.reset();

    return true;
}

muse::async::Channel<au::trackedit::ClipKey, bool> Au3ClipGainInteraction::clipGainChanged() const
{
    return m_clipEnvelopeChanged;
}

au::au3::Au3Project& Au3ClipGainInteraction::projectRef() const
{
    Au3Project* project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
}
