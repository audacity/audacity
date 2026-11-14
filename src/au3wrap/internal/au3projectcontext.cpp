/*
* Audacity: A Digital Audio Editor
*/
#include "../au3projectcontext.h"

#include <memory>

#include "au3-project/Project.h"

namespace au::au3 {
namespace {
class ProjectIocContext final : public ClientData::Base
{
public:
    std::weak_ptr<muse::modularity::Context> context;
};

const AttachedProjectObjects::RegisteredFactory contextKey{ [](AudacityProject&) {
        return std::make_shared<ProjectIocContext>();
    } };
} // namespace

void setProjectIocContext(AudacityProject& project, const muse::modularity::ContextPtr& context)
{
    project.AttachedObjects::Get<ProjectIocContext>(contextKey).context = context;
}

muse::modularity::ContextPtr projectIocContext(AudacityProject& project)
{
    const auto* attachment = project.AttachedObjects::Find<ProjectIocContext>(contextKey);
    return attachment ? attachment->context.lock() : nullptr;
}
} // namespace au::au3
