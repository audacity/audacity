/*
* Audacity: A Digital Audio Editor
*/
#include "projectcreator.h"

#include "audacityproject.h"

using namespace au::project;

IAudacityProjectPtr ProjectCreator::newProject() const
{
    return std::make_shared<Audacity4Project>(iocContext());
}

muse::RetVal<IAudacityProjectPtr> ProjectCreator::loadProject(const muse::io::path_t& path) const
{
    IAudacityProjectPtr project = std::make_shared<Audacity4Project>(iocContext());

    const muse::Ret ret = project->load(path);
    if (!ret) {
        return muse::RetVal<IAudacityProjectPtr>::make_ret(ret);
    }

    return muse::RetVal<IAudacityProjectPtr>::make_ok(project);
}
