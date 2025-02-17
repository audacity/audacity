/**********************************************************************

Audacity: A Digital Audio Editor

ProjectWindowBase.h

Paul Licameli split from ProjectWindow.h

**********************************************************************/

#ifndef __AUDACITY_PROJECT_WINDOW_BASE__
#define __AUDACITY_PROJECT_WINDOW_BASE__

#include <wx/frame.h> // to inherit
#include <memory>

class AudacityProject;

///\brief A top-level window associated with a project
class ProjectWindowBase /* not final */ : public wxFrame
{
public:
    explicit ProjectWindowBase(
        wxWindow* parent, wxWindowID id, const wxPoint& pos, const wxSize& size, AudacityProject& project);

    ~ProjectWindowBase() override;

    std::shared_ptr<AudacityProject> FindProject()
    { return mwProject.lock(); }
    std::shared_ptr<const AudacityProject> FindProject() const
    { return mwProject.lock(); }

protected:
    std::weak_ptr<AudacityProject> mwProject;
};

AUDACITY_DLL_API AudacityProject* FindProjectFromWindow(wxWindow* pWindow);
const AudacityProject* FindProjectFromWindow(const wxWindow* pWindow);

#endif
