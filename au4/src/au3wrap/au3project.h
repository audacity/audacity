#ifndef AU_AU3WRAP_AU3PROJECT_H
#define AU_AU3WRAP_AU3PROJECT_H

#include <memory>
#include <string>

#include "global/io/path.h"
#include "async/notifylist.h"

#include "processing/dom/track.h"

namespace au::au3 {
struct Au3ProjectData;
class Au3Project
{
public:

    Au3Project();

    static std::shared_ptr<Au3Project> create();

    bool load(const muse::io::path_t& filePath);
    bool save(const muse::io::path_t& fileName, const bool fromSaveAs);
    void close();

    std::string title() const;

    std::vector<processing::TrackId> trackIdList() const;
    muse::async::NotifyList<processing::Track> trackList() const;
    muse::async::NotifyList<processing::Clip> clipList(const processing::TrackId& trackId) const;

    uintptr_t au3ProjectPtr() const;

private:

    std::shared_ptr<Au3ProjectData> m_data;
    mutable muse::async::ChangedNotifier<processing::Track> m_trackChangedNotifier;
};
}

#endif // AU_AU3WRAP_AU3PROJECT_H
