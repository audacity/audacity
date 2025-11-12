/*
* Audacity: A Digital Audio Editor
*/

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "iinteractive.h"

#include "libraries/lib-tags/Tags.h"

#include "project/types/projectmeta.h"

#include "project/imetadata.h"

namespace au::project {
class Au3Metadata : public IMetadata
{
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<muse::IInteractive> interactive;

public:
    void init();

    project::ProjectMeta tags() const override;
    void setTags(project::ProjectMeta) override;

    std::string buildXml(const project::ProjectMeta&) const override;
    project::ProjectMeta parseXml(const std::string& xml) const override;

private:
    inline static const std::array<std::string, 6> kStdTags =
    {
        muse::trc("metadata", "Artist name"),
        muse::trc("metadata", "Track title"),
        muse::trc("metadata", "Album title"),
        muse::trc("metadata", "Track number"),
        muse::trc("metadata", "Year"),
        muse::trc("metadata", "Comments"),
    };
};
}
