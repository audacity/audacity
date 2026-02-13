/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "async/channel.h"

#include "automationtypes.h"
#include "trackedit/trackedittypes.h"

#include "modularity/imoduleinterface.h"

namespace au::automation {
class IClipGainInteraction : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IClipGainInteraction)

public:
    virtual ~IClipGainInteraction() = default;

    virtual std::optional<ClipEnvelopeInfo> clipEnvelopeInfo(const trackedit::ClipKey& key) const = 0;
    virtual ClipEnvelopePoints clipEnvelopePoints(const trackedit::ClipKey& key) const = 0;
    virtual bool setClipEnvelopePoint(const trackedit::ClipKey& key, double tAbs, double value, bool completed) = 0;
    virtual bool removeClipEnvelopePoint(const trackedit::ClipKey& key, int index, bool completed) = 0;
    virtual bool flattenClipEnvelope(const trackedit::ClipKey& key, double value, bool completed) = 0;
    virtual bool setClipEnvelopePointAtIndex(const trackedit::ClipKey& key, int index, double tAbs, double value, bool completed) = 0;
    virtual bool beginClipEnvelopePointDrag(const trackedit::ClipKey& clip, int pointIndex) = 0;
    virtual bool updateClipEnvelopePointDrag(const trackedit::ClipKey& clip, double tAbs, double value) = 0;
    virtual bool endClipEnvelopePointDrag(const trackedit::ClipKey& clip, bool commit) = 0;
    virtual muse::async::Channel<trackedit::ClipKey, bool> clipEnvelopeChanged() const = 0;
};
}
