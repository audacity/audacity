/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>
#include <memory>

#include "modularity/imoduleinterface.h"
#include "global/io/path.h"
#include "async/notifylist.h"
#include "async/channel.h"

#include "trackedit/dom/track.h"

namespace au::au3 {
class IAu3Project
{
public:

    virtual ~IAu3Project() = default;

    virtual bool load(const muse::io::path_t& filePath) = 0;
    virtual bool save(const muse::io::path_t& fileName) = 0;
    virtual void close() = 0;

    virtual std::string title() const = 0;

    virtual std::vector<trackedit::TrackId> trackIdList() const = 0;
    virtual muse::async::NotifyList<trackedit::Track> trackList() const = 0;
    virtual muse::async::NotifyList<trackedit::Clip> clipList(const trackedit::TrackId& trackId) const = 0;

    virtual trackedit::TimeSignature timeSignature() const = 0;
    virtual void setTimeSignature(const trackedit::TimeSignature& timeSignature) = 0;
    virtual muse::async::Channel<au::trackedit::TimeSignature> timeSignatureChanged() const = 0;

    virtual void pushHistoryState(const std::string& longDescription, const std::string& shortDescription) = 0;

    // internal
    virtual uintptr_t au3ProjectPtr() const = 0;
};

class IAu3ProjectCreator : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IAu3ProjectCreator)
public:
    virtual ~IAu3ProjectCreator() = default;

    virtual std::shared_ptr<IAu3Project> create() const = 0;
};
}
