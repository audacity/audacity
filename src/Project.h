/**********************************************************************

  Audacity: A Digital Audio Editor

  Project.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_PROJECT__
#define __AUDACITY_PROJECT__

#include "Identifier.h"

#include "ClientData.h" // to inherit

#include <memory>
#include <mutex>
#include <wx/weakref.h> // member variable
#include <wx/window.h> // MSVC wants this

class wxFrame;
class wxWindow;
namespace BasicUI { class WindowPlacement; }

class AudacityProject;

AUDACITY_DLL_API AudacityProject *GetActiveProject();
// For use by ProjectManager only:
AUDACITY_DLL_API void SetActiveProject(AudacityProject * project);

/// \brief an object of class AllProjects acts like a standard library
/// container, but refers to a global array of open projects.  So you can
/// iterate easily over shared pointers to them with range-for :
/// for (auto pProject : AllProjects{}) { ... }
/// The pointers are never null.
class AUDACITY_DLL_API AllProjects
{
   // Use shared_ptr to projects, because elsewhere we need weak_ptr
   using AProjectHolder = std::shared_ptr< AudacityProject >;
   using Container = std::vector< AProjectHolder >;
   static Container gAudacityProjects;

public:
   AllProjects() = default;

   size_t size() const;
   bool empty() const { return size() == 0; }

   using const_iterator = Container::const_iterator;
   const_iterator begin() const;
   const_iterator end() const;

   using const_reverse_iterator = Container::const_reverse_iterator;
   const_reverse_iterator rbegin() const;
   const_reverse_iterator rend() const;

   using value_type = Container::value_type;

   // If the project is present, remove it from the global array and return
   // a shared pointer, else return null.  This invalidates any iterators.
   value_type Remove( AudacityProject &project );

   // This invalidates iterators
   void Add( const value_type &pProject );

   /// In case you must iterate in a non-main thread, use this to prevent
   /// changes in the set of open projects
   static std::mutex &Mutex();

   // Return true if all projects do close (always so if force == true)
   // But if return is false, that means the user cancelled close of at least
   // one un-saved project.
   static bool Close( bool force = false );

   static bool Closing() { return sbClosing; }

private:
   static bool sbClosing;
};

// Container of various objects associated with the project, which is
// responsible for destroying them
using AttachedProjectObjects = ClientData::Site<
   AudacityProject, ClientData::Base, ClientData::SkipCopying, std::shared_ptr
>;
// Container of pointers to various windows associated with the project, which
// is not responsible for destroying them -- wxWidgets handles that instead
using AttachedProjectWindows = ClientData::Site<
   AudacityProject, wxWindow, ClientData::SkipCopying, ClientData::BarePtr
>;

wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API,
                         EVT_TRACK_PANEL_TIMER, wxCommandEvent);

// This event is emitted by the application object when there is a change
// in the activated project
wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API,
                         EVT_PROJECT_ACTIVATION, wxCommandEvent);

///\brief The top-level handle to an Audacity project.  It serves as a source
/// of events that other objects can bind to, and a container of associated
/// sub-objects that it treats opaquely.  It stores a filename and a status
/// message and a few other things.
/// There is very little in this class, most of the intelligence residing in
/// the cooperating attached objects.
class AUDACITY_DLL_API AudacityProject final
   : public wxEvtHandler
   , public AttachedProjectObjects
   , public AttachedProjectWindows
   , public std::enable_shared_from_this<AudacityProject>
{
 public:
   using AttachedObjects = ::AttachedProjectObjects;
   using AttachedWindows = ::AttachedProjectWindows;

   AudacityProject();
   virtual ~AudacityProject();

   wxFrame *GetFrame() { return mFrame; }
   const wxFrame *GetFrame() const { return mFrame; }
   void SetFrame( wxFrame *pFrame );
 
   wxWindow *GetPanel() { return mPanel; }
   const wxWindow *GetPanel() const { return mPanel; }
   void SetPanel( wxWindow *pPanel );
 
   int GetProjectNumber(){ return mProjectNo;}

   // Project name can be either empty or have the name of the project.
   //
   // If empty, it signifies that the project is empty/unmodified or
   // that the project hasn't yet been saved to a permanent project
   // file.
   //
   // If a name has been assigned, it is merely used to identify
   // the project and should not be used for any other purposes.
   const wxString &GetProjectName() const;
   void SetProjectName(const wxString &name);

   // Used exclusively in batch mode, this allows commands to remember
   // and use the initial import path
   FilePath GetInitialImportPath() const;
   void SetInitialImportPath(const FilePath &path);

private:

   // The project's name
   wxString mName;

   static int mProjectCounter;// global counter.
   int mProjectNo; // count when this project was created.

   FilePath mInitialImportPath;

 public:
   bool mbBusyImporting{ false }; // used to fix bug 584
   int mBatchMode{ 0 };// 0 means not, >0 means in batch mode.

 private:
   wxWeakRef< wxFrame > mFrame{};
   wxWeakRef< wxWindow > mPanel{};
};

///\brief Get the top-level window associated with the project (as a wxFrame
/// only, when you do not need to use the subclass ProjectWindow)
AUDACITY_DLL_API wxFrame &GetProjectFrame( AudacityProject &project );
AUDACITY_DLL_API const wxFrame &GetProjectFrame( const AudacityProject &project );

///\brief Get a pointer to the window associated with a project, or null if
/// the given pointer is null.
inline wxFrame *FindProjectFrame( AudacityProject *project ) {
   return project ? &GetProjectFrame( *project ) : nullptr;
}
inline const wxFrame *FindProjectFrame( const AudacityProject *project ) {
   return project ? &GetProjectFrame( *project ) : nullptr;
}

//! Make a WindowPlacement object suitable for `project` (which may be null)
/*! @post return value is not null */
AUDACITY_DLL_API std::unique_ptr<const BasicUI::WindowPlacement>
ProjectFramePlacement( AudacityProject *project );

///\brief Get the main sub-window of the project frame that displays track data
// (as a wxWindow only, when you do not need to use the subclass TrackPanel)
AUDACITY_DLL_API wxWindow &GetProjectPanel( AudacityProject &project );
AUDACITY_DLL_API const wxWindow &GetProjectPanel(
   const AudacityProject &project );

#endif
