/*
* Audacity: A Digital Audio Editor
*/

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "libraries/lib-tags/Tags.h"
#include "project/types/projectmeta.h"

#include "project/imetadata.h"

namespace au::project {
class Au3Metadata : public IMetadata
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    void init();

    project::ProjectMeta tags() const override;
    void setTags(project::ProjectMeta) override;
};
}
