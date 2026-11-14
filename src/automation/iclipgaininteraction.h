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

    virtual std::optional<AutomationInfo> clipGainInfo(const trackedit::ClipKey& key) const = 0;
    virtual AutomationPoints clipGainPoints(const trackedit::ClipKey& key) const = 0;
    virtual bool setClipGainPoint(const trackedit::ClipKey& key, double tAbs, double value, bool completed) = 0;
    virtual bool removeClipGainPoint(const trackedit::ClipKey& key, int index, bool completed) = 0;
    virtual bool setClipGainPointAtIndex(const trackedit::ClipKey& key, int index, double tAbs, double value, bool completed) = 0;
    virtual bool beginClipGainPointDrag(const trackedit::ClipKey& clip, int pointIndex) = 0;
    virtual bool updateClipGainPointDrag(const trackedit::ClipKey& clip, double tAbs, double value) = 0;
    virtual bool endClipGainPointDrag(const trackedit::ClipKey& clip, bool commit) = 0;
    virtual muse::async::Channel<trackedit::ClipKey, bool> clipGainChanged() const = 0;
};
}
