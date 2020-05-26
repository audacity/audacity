/**********************************************************************

  Audacity: A Digital Audio Editor

  Project.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_PROJECT__
#define __AUDACITY_PROJECT__

#include "Audacity.h"

#include "ClientData.h" // to inherit

#include <memory>
#include <wx/weakref.h> // member variable
#include <wx/window.h> // MSVC wants this

class wxFrame;
class wxWindow;

class AudacityProject;
class ODLock;


AUDACITY_DLL_API AudacityProject *GetActiveProject();
// For use by ProjectManager only:
extern void SetActiveProject(AudacityProject * project);

/// \brief an object of class AllProjects acts like a standard library
/// container, but refers to a global array of open projects.  So you can
/// iterate easily over shared pointers to them with range-for :
/// for (auto pProject : AllProjects{}) { ... }
/// The pointers are never null.
class AllProjects
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
   static ODLock &Mutex();

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
using AttachedObjects = ClientData::Site<
   AudacityProject, ClientData::Base, ClientData::SkipCopying, std::shared_ptr
>;
// Container of pointers to various windows associated with the project, which
// is not responsible for destroying them -- wxWidgets handles that instead
using AttachedWindows = ClientData::Site<
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
   , public AttachedObjects
   , public AttachedWindows
{
 public:
   using AttachedObjects = ::AttachedObjects;
   using AttachedWindows = ::AttachedWindows;

   AudacityProject();
   virtual ~AudacityProject();

   wxFrame *GetFrame() { return mFrame; }
   const wxFrame *GetFrame() const { return mFrame; }
   void SetFrame( wxFrame *pFrame );
 
   wxWindow *GetPanel() { return mPanel; }
   const wxWindow *GetPanel() const { return mPanel; }
   void SetPanel( wxWindow *pPanel );
 
   wxString GetProjectName() const;

   const FilePath &GetFileName() { return mFileName; }
   void SetFileName( const FilePath &fileName ) { mFileName = fileName; }

   int GetProjectNumber(){ return mProjectNo;}
   
 private:

   // The project's name and file info
   FilePath mFileName; // Note: extension-less

   static int mProjectCounter;// global counter.
   int mProjectNo; // count when this project was created.

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

///\brief Get the main sub-window of the project frame that displays track data
// (as a wxWindow only, when you do not need to use the subclass TrackPanel)
AUDACITY_DLL_API wxWindow &GetProjectPanel( AudacityProject &project );
AUDACITY_DLL_API const wxWindow &GetProjectPanel(
   const AudacityProject &project );

#endif
