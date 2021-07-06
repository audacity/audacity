/**********************************************************************

Sneedacity: A Digital Audio Editor

ProjectWindowBase.h

Paul Licameli split from ProjectWindow.h

**********************************************************************/

#ifndef __SNEEDACITY_PROJECT_WINDOW_BASE__
#define __SNEEDACITY_PROJECT_WINDOW_BASE__

#include <wx/frame.h> // to inherit

class SneedacityProject;

///\brief A top-level window associated with a project
class ProjectWindowBase /* not final */ : public wxFrame
{
public:
   explicit ProjectWindowBase(
      wxWindow * parent, wxWindowID id,
      const wxPoint & pos, const wxSize &size,
      SneedacityProject &project );

   ~ProjectWindowBase() override;

   SneedacityProject &GetProject() { return mProject; }
   const SneedacityProject &GetProject() const { return mProject; }

protected:
   SneedacityProject &mProject;
};

SNEEDACITY_DLL_API SneedacityProject *FindProjectFromWindow( wxWindow *pWindow );
const SneedacityProject *FindProjectFromWindow( const wxWindow *pWindow );

#endif

