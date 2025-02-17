/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file ActiveProject.h
 @brief Handle changing of active project and keep global project pointer

 Paul Licameli split from Project.h

 **********************************************************************/

#ifndef __AUDACITY_ACTIVE_PROJECT__
#define __AUDACITY_ACTIVE_PROJECT__

#include <wx/event.h> // to declare custom event type
#include <memory>

class AudacityProject;

AUDACITY_DLL_API std::weak_ptr<AudacityProject> GetActiveProject();
// For use by ProjectManager only:
AUDACITY_DLL_API void SetActiveProject(AudacityProject* project);

#endif
