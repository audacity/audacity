#ifndef AU_PROJECT_AUDACITYPROJECT_H
#define AU_PROJECT_AUDACITYPROJECT_H

#include "../iaudacityproject.h"

namespace mu::project {
class AudacityProject : public IAudacityProject
{
public:
    AudacityProject() = default;

    const au::processing::ProcessingProject& processingProject() const override;

private:
    au::processing::ProcessingProject m_processingProject;
};
}

#endif // AU_PROJECT_AUDACITYPROJECT_H
