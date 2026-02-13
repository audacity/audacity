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

using namespace au::automation;
using namespace au::au3;

Au3ClipGainInteraction::Au3ClipGainInteraction(const muse::modularity::ContextPtr& ctx)
    : muse::Injectable(ctx) {}

std::optional<ClipEnvelopeInfo> Au3ClipGainInteraction::clipEnvelopeInfo(
    const trackedit::ClipKey& clipKey) const
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return {};
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
    IF_ASSERT_FAILED(clip) {
        return {};
    }

    auto& env = clip->GetEnvelope();

    ClipEnvelopeInfo info;
    info.minValue = env.GetMinValue();
    info.maxValue = env.GetMaxValue();
    info.defaultValue = env.GetDefaultValue();
    info.exponential = env.GetExponential();
    info.version = env.GetVersion();

    return info;
}

ClipEnvelopePoints Au3ClipGainInteraction::clipEnvelopePoints(const trackedit::ClipKey& clipKey) const
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return {};
    }

    std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
    IF_ASSERT_FAILED(clip) {
        return {};
    }

    auto& env = clip->GetEnvelope();

    ClipEnvelopePoints pts;
    const auto n = env.GetNumberOfPoints();
    pts.reserve(n);

    const double offset = env.GetOffset();
    for (size_t i = 0; i < n; ++i) {
        const auto& p = env[int(i)];
        pts.push_back({ offset + p.GetT(), p.GetVal() });
    }
    return pts;
}

bool Au3ClipGainInteraction::setClipEnvelopePoint(const trackedit::ClipKey& clipKey, double time, double value, bool completed)
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

    const double v = std::clamp(value, env.GetMinValue(), env.GetMaxValue());

    env.InsertOrReplace(time, v);

    if (auto prj = globalContext()->currentTrackeditProject()) {
        prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, clip.get()));
    }
    projectHistory()->pushHistoryState(muse::trc("trackedit", "Added enveloped point"), muse::trc("trackedit",
                                                                                                  "Clip envelope edit"));

    m_clipEnvelopeChanged.send(clipKey, completed);
    return true;
}

bool Au3ClipGainInteraction::removeClipEnvelopePoint(const trackedit::ClipKey& clipKey, int index, bool completed)
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

bool Au3ClipGainInteraction::flattenClipEnvelope(const trackedit::ClipKey& clipKey, double value, bool completed)
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
    const double v = std::clamp(value, env.GetMinValue(), env.GetMaxValue());
    env.Flatten(v);

    if (auto prj = globalContext()->currentTrackeditProject()) {
        prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, clip.get()));
    }

    m_clipEnvelopeChanged.send(clipKey, completed);
    return true;
}

bool Au3ClipGainInteraction::setClipEnvelopePointAtIndex(
    const trackedit::ClipKey& key, int index, double time, double value, bool completed)
{
    const auto points = clipEnvelopePoints(key);
    if (index < 0 || index >= static_cast<int>(points.size())) {
        return false;
    }

    const double oldTime = points.at(index).time;
    if (muse::is_equal(oldTime, time)) {
        return setClipEnvelopePoint(key, time, value, completed);
    }

    return setClipEnvelopePoint(key, time, value, completed);
}

bool Au3ClipGainInteraction::beginClipEnvelopePointDrag(const trackedit::ClipKey& clipKey, int pointIndex)
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

    const int n = static_cast<int>(env.GetNumberOfPoints());
    if (pointIndex < 0 || pointIndex >= n) {
        return false;
    }

    QVector<double> times(n);
    QVector<double> values(n);
    env.GetPoints(times.data(), values.data(), n);

    if (m_envDrag && m_envDrag->active) {
        endClipEnvelopePointDrag(m_envDrag->clip, true);
    }

    EnvelopeDragSession s;
    s.clip = clipKey;
    s.index = pointIndex;
    s.active = true;
    s.origTime = times[pointIndex];
    s.origValue = values[pointIndex];

    env.SetDragPoint(pointIndex);

    m_envDrag = s;
    return true;
}

bool Au3ClipGainInteraction::updateClipEnvelopePointDrag(const trackedit::ClipKey& clipKey, double tAbs, double value)
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
    env.MoveDragPoint(tAbs, value);

    if (auto prj = globalContext()->currentTrackeditProject()) {
        prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, clip.get()));
    }

    m_clipEnvelopeChanged.send(clipKey, false);
    return true;
}

bool Au3ClipGainInteraction::endClipEnvelopePointDrag(const trackedit::ClipKey& clipKey, bool commit)
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

    if (commit) {
        env.ClearDragPoint();
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Dragged enveloped point"), muse::trc("trackedit",
                                                                                                        "Clip envelope edit"));
    } else {
        // cancel path: restore orig values
        env.MoveDragPoint(m_envDrag->origTime + clip->GetPlayStartTime(), m_envDrag->origValue);
        env.ClearDragPoint();
    }

    if (auto prj = globalContext()->currentTrackeditProject()) {
        prj->notifyAboutClipChanged(DomConverter::clip(waveTrack, clip.get()));
    }

    m_clipEnvelopeChanged.send(clipKey, true);
    m_envDrag.reset();

    return true;
}

muse::async::Channel<au::trackedit::ClipKey, bool> Au3ClipGainInteraction::clipEnvelopeChanged() const
{
    return m_clipEnvelopeChanged;
}

au::au3::Au3Project& Au3ClipGainInteraction::projectRef() const
{
    Au3Project* project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
}
