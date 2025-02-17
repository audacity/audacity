/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file ActiveProject.cpp

 Paul Licameli split from Project.cpp

 **********************************************************************/

#include "ActiveProject.h"
#include "KeyboardCapture.h"
#include "Project.h"
#include "ProjectWindows.h"

#include <wx/app.h>
#include <wx/frame.h>

//This is a pointer to the currently-active project.
static std::weak_ptr<AudacityProject> gActiveProject;

AUDACITY_DLL_API std::weak_ptr<AudacityProject> GetActiveProject()
{
    return gActiveProject;
}

void SetActiveProject(AudacityProject* project)
{
    auto pProject = project ? project->shared_from_this() : nullptr;
    if (gActiveProject.lock() != pProject) {
        gActiveProject = pProject;
        KeyboardCapture::Capture(nullptr);
    }
    wxTheApp->SetTopWindow(FindProjectFrame(project));
}
