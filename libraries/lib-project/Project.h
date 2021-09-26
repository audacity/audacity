/**********************************************************************

  Audacity: A Digital Audio Editor

  Project.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_PROJECT__
#define __AUDACITY_PROJECT__

#include "ClientData.h" // to inherit

#include <memory>
#include <mutex>

#include <wx/event.h>
using FilePath = wxString;

class AudacityProject;

//! Like a standard library container of all open projects.
//! @invariant pointers accessible through the iterators are not null
/*!
So you can iterate easily over shared pointers to them with range-for :
for (auto pProject : AllProjects{}) { ... }
The pointers are never null.

However iterators will be invalid if addition or deletion of projects occur
during traversal.
*/
class PROJECT_API AllProjects
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

   //! This invalidates iterators
   /*!
    @pre pProject is not null
    */
   void Add( const value_type &pProject );

   /// In case you must iterate in a non-main thread, use this to prevent
   /// changes in the set of open projects
   static std::mutex &Mutex();
};

// Container of various objects associated with the project, which is
// responsible for destroying them
using AttachedProjectObjects = ClientData::Site<
   AudacityProject, ClientData::Base, ClientData::SkipCopying, std::shared_ptr
>;

wxDECLARE_EXPORTED_EVENT(PROJECT_API,
                         EVT_TRACK_PANEL_TIMER, wxCommandEvent);

///\brief The top-level handle to an Audacity project.  It serves as a source
/// of events that other objects can bind to, and a container of associated
/// sub-objects that it treats opaquely.  It stores a filename and a status
/// message and a few other things.
/// There is very little in this class, most of the intelligence residing in
/// the cooperating attached objects.
class PROJECT_API AudacityProject final
   : public wxEvtHandler
   , public AttachedProjectObjects
   , public std::enable_shared_from_this<AudacityProject>
{
 public:
   using AttachedObjects = ::AttachedProjectObjects;

   AudacityProject();
   virtual ~AudacityProject();

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
};

// Generate a registry for serialized data attached to the project
#include "XMLMethodRegistry.h"
class AudacityProject;
using ProjectFileIORegistry = XMLMethodRegistry<AudacityProject>;
DECLARE_XML_METHOD_REGISTRY( PROJECT_API, ProjectFileIORegistry );

#endif
