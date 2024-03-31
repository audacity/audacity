#include "audacity3project.h"

#include "libraries/lib-project/Project.h"
#include "libraries/lib-project-file-io/ProjectFileIO.h"

#include "log.h"

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

bool Audacity3Project::load(const mu::io::path_t& filePath)
{
    auto &projectFileIO = ProjectFileIO::Get( *m_data->project.get() );
    std::string sstr = filePath.toStdString();
    FilePath fileName = wxString::FromUTF8(sstr.c_str(), sstr.size());
    auto parseResult = projectFileIO.LoadProject(fileName, true);
    const bool bParseSuccess = parseResult.has_value();
    if (!bParseSuccess) {
        LOGE() << "failed load project: " << filePath;
    }
    return bParseSuccess;
}
