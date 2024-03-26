#ifndef AU_PROJECT_AUDACITYPROJECT_H
#define AU_PROJECT_AUDACITYPROJECT_H

#include "../iaudacityproject.h"

namespace au::project {
class AudacityProject : public IAudacityProject
{
public:
    AudacityProject();

    static IAudacityProjectPtr makeMock();

    const au::processing::ProcessingProjectPtr processingProject() const override;

private:
    au::processing::ProcessingProjectPtr m_processingProject;
};
}

#endif // AU_PROJECT_AUDACITYPROJECT_H
