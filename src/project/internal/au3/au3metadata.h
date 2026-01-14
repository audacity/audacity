/*
* Audacity: A Digital Audio Editor
*/

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "iinteractive.h"

#include "au3-tags/Tags.h"

#include "project/types/projectmeta.h"

#include "project/imetadata.h"

namespace au::project {
class Au3Metadata : public IMetadata, public muse::Injectable
{
    muse::Inject<au::context::IGlobalContext> globalContext { this };
    muse::Inject<muse::IInteractive> interactive { this };

public:
    void init();

    project::ProjectMeta tags() const override;
    void setTags(project::ProjectMeta) override;

    std::string buildXml(const project::ProjectMeta&) const override;
    project::ProjectMeta parseXml(const std::string& xml) const override;

private:
    inline static const std::array<std::string, 6> kStdTags =
    {
        muse::trc("metadata", "TITLE"),
        muse::trc("metadata", "ARTIST"),
        muse::trc("metadata", "ALBUM"),
        muse::trc("metadata", "TRACKNUMBER"),
        muse::trc("metadata", "YEAR"),
        // muse::trc("metadata", "GENRE"), // TODO
        muse::trc("metadata", "COMMENTS"),
    };
};
}
