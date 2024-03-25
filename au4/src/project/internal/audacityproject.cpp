#include "audacityproject.h"

using namespace mu::project;
using namespace au::processing;

const ProcessingProject& AudacityProject::processingProject() const
{
    return m_processingProject;
}
