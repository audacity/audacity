#ifndef AU_AU3WRAP_AUDACITY3PROJECT_H
#define AU_AU3WRAP_AUDACITY3PROJECT_H

#include <memory>
#include <string>

#include "global/io/path.h"
#include "async/notifylist.h"

#include "processing/dom/track.h"

namespace au::au3 {
struct Audacity3ProjectData;
class Audacity3Project
{
public:

    Audacity3Project();

    static std::shared_ptr<Audacity3Project> create();

    bool load(const muse::io::path_t& filePath);
    bool save(const muse::io::path_t& fileName, const bool fromSaveAs);
    void close();

    std::string title() const;

    muse::async::NotifyList<processing::Track> trackList() const;

private:

    std::shared_ptr<Audacity3ProjectData> m_data;
    mutable muse::async::ChangedNotifier<processing::Track> m_trackChangedNotifier;
};
}

#endif // AU_AU3WRAP_AUDACITY3PROJECT_H
