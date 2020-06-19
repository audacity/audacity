/**********************************************************************

Audacity: A Digital Audio Editor

ProjectWindowBase.h

Paul Licameli split from ProjectWindow.h

**********************************************************************/

#ifndef __AUDACITY_PROJECT_WINDOW_BASE__
#define __AUDACITY_PROJECT_WINDOW_BASE__

#include <wx/frame.h> // to inherit

class AudacityProject;

///\brief A top-level window associated with a project
class ProjectWindowBase /* not final */ : public wxFrame
{
public:
   explicit ProjectWindowBase(
      wxWindow * parent, wxWindowID id,
      const wxPoint & pos, const wxSize &size,
      AudacityProject &project );

   ~ProjectWindowBase() override;

   AudacityProject &GetProject() { return mProject; }
   const AudacityProject &GetProject() const { return mProject; }

protected:
   AudacityProject &mProject;
};

AudacityProject *FindProjectFromWindow( wxWindow *pWindow );
const AudacityProject *FindProjectFromWindow( const wxWindow *pWindow );

#endif

