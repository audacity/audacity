#include "audacity3project.h"

#include "libraries/lib-project/Project.h"

using namespace au::au3;

struct au::au3::Audacity3ProjectData
{
    std::shared_ptr<AudacityProject> project;
};

Audacity3Project::Audacity3Project()
{
    m_data = std::make_shared<Audacity3ProjectData>();
}

std::shared_ptr<Audacity3Project> Audacity3Project::create()
{
    std::shared_ptr<Audacity3Project> p = std::make_shared<Audacity3Project>();
    p->m_data->project = AudacityProject::Create();
    return p;
}
