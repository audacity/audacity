#include "audacityproject.h"

#include "au3wrap/audacity3project.h"

#include "log.h"

using namespace mu;
using namespace au::project;
using namespace au::processing;

Audacity4Project::Audacity4Project()
{
    m_processingProject = std::make_shared<ProcessingProject>();
}

mu::Ret Audacity4Project::load(const mu::io::path_t& path, bool forceMode, const std::string& format_)
{
    TRACEFUNC;

    std::string format = format_.empty() ? io::suffix(path) : format_;

    LOGD() << "try load: " << path << ", format: " << format;

    //setupProject();
    setPath(path);

    //! TODO AU4
    // if (!isAudacityFile(format)) {
    //     Ret ret = doImport(path, forceMode);
    //     if (ret) {
    //         listenIfNeedSaveChanges();
    //     }

    //     return ret;
    // }

    Ret ret = doLoad(path, forceMode, format);
    if (!ret) {
        LOGE() << "failed load, err: " << ret.toString();
        return ret;
    }

    //! TODO AU4
    // listenIfNeedSaveChanges();

    return ret;
}

mu::Ret Audacity4Project::doLoad(const io::path_t& path, bool forceMode, const std::string& format)
{
    TRACEFUNC;

    UNUSED(forceMode);
    UNUSED(format);

    m_au3Project = au3::Audacity3Project::create();
    bool isLoaded = m_au3Project->load(path);
    if (!isLoaded) {
        LOGE() << "Failed load:" << path;
        return mu::make_ret(mu::Ret::Code::UnknownError);
    }

    LOGI() << "success loaded au3 project: " << m_au3Project->title();

    m_processingProject = std::make_shared<processing::ProcessingProject>();
    m_processingProject->setAudacity3Project(m_au3Project);

    m_processingProject->dump();

    return mu::make_ret(Ret::Code::Ok);
}

void Audacity4Project::close()
{
    m_au3Project->close();
}

void Audacity4Project::setPath(const io::path_t& path)
{
    if (m_path == path) {
        return;
    }

    m_path = path;
    m_pathChanged.notify();
}

const ProcessingProjectPtr Audacity4Project::processingProject() const
{
    return m_processingProject;
}
