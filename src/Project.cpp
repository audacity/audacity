/**********************************************************************

  Audacity: A Digital Audio Editor

  Project.cpp

  Dominic Mazzoni
  Vaughan Johnson

*//*******************************************************************/


#include "Project.h"

#include "FileNames.h"
#include "KeyboardCapture.h"
#include "TempDirectory.h"
#include "widgets/wxWidgetsBasicUI.h"

#include <wx/app.h>
#include <wx/display.h>
#include <wx/filename.h>
#include <wx/frame.h>

wxDEFINE_EVENT(EVT_TRACK_PANEL_TIMER, wxCommandEvent);
wxDEFINE_EVENT(EVT_PROJECT_ACTIVATION, wxCommandEvent);

size_t AllProjects::size() const
{
   return gAudacityProjects.size();
}

auto AllProjects::begin() const -> const_iterator
{
   return gAudacityProjects.begin();
}

auto AllProjects::end() const -> const_iterator
{
   return gAudacityProjects.end();
}

auto AllProjects::rbegin() const -> const_reverse_iterator
{
   return gAudacityProjects.rbegin();
}

auto AllProjects::rend() const -> const_reverse_iterator
{
   return gAudacityProjects.rend();
}

auto AllProjects::Remove( AudacityProject &project ) -> value_type
{
   std::lock_guard<std::mutex> guard{ Mutex() };
   auto start = begin(), finish = end(), iter = std::find_if(
      start, finish,
      [&]( const value_type &ptr ){ return ptr.get() == &project; }
   );
   if (iter == finish)
      return nullptr;
   auto result = *iter;
   gAudacityProjects.erase( iter );
   return result;
}

void AllProjects::Add( const value_type &pProject )
{
   std::lock_guard<std::mutex> guard{ Mutex() };
   gAudacityProjects.push_back( pProject );
}

bool AllProjects::sbClosing = false;

bool AllProjects::Close( bool force )
{
   ValueRestorer<bool> cleanup{ sbClosing, true };
   while (AllProjects{}.size())
   {
      // Closing the project has global side-effect
      // of deletion from gAudacityProjects
      if ( force )
      {
         GetProjectFrame( **AllProjects{}.begin() ).Close(true);
      }
      else
      {
         if (! GetProjectFrame( **AllProjects{}.begin() ).Close())
            return false;
      }
   }
   return true;
}

std::mutex &AllProjects::Mutex()
{
   static std::mutex theMutex;
   return theMutex;
}

int AudacityProject::mProjectCounter=0;// global counter.

/* Define Global Variables */
//This is a pointer to the currently-active project.
static AudacityProject *gActiveProject;
//This array holds onto all of the projects currently open
AllProjects::Container AllProjects::gAudacityProjects;

AUDACITY_DLL_API AudacityProject *GetActiveProject()
{
   return gActiveProject;
}

void SetActiveProject(AudacityProject * project)
{
   if ( gActiveProject != project ) {
      gActiveProject = project;
      KeyboardCapture::Capture( nullptr );
      wxTheApp->QueueEvent( safenew wxCommandEvent{ EVT_PROJECT_ACTIVATION } );
   }
   wxTheApp->SetTopWindow( FindProjectFrame( project ) );
}

AudacityProject::AudacityProject()
{
   mProjectNo = mProjectCounter++; // Bug 322
   AttachedObjects::BuildAll();
   // But not for the attached windows.  They get built only on demand, such as
   // from menu items.

   // Make sure there is plenty of space for Sqlite files
   wxLongLong freeSpace = 0;

   auto path = TempDirectory::TempDir();
   if (wxGetDiskSpace(path, NULL, &freeSpace)) {
      if (freeSpace < wxLongLong(wxLL(100 * 1048576))) {
         auto volume = FileNames::AbbreviatePath( path );
         /* i18n-hint: %s will be replaced by the drive letter (on Windows) */
         BasicUI::ShowErrorDialog( {},
            XO("Warning"),
            XO("There is very little free disk space left on %s\n"
               "Please select a bigger temporary directory location in\n"
               "Directories Preferences.").Format( volume ),
            "Error:_Disk_full_or_not_writable"
            );
      }
   }

}

AudacityProject::~AudacityProject()
{
}

void AudacityProject::SetFrame( wxFrame *pFrame )
{
   mFrame = pFrame;
}

void AudacityProject::SetPanel( wxWindow *pPanel )
{
   mPanel = pPanel;
}

const wxString &AudacityProject::GetProjectName() const
{
   return mName;
}

void AudacityProject::SetProjectName(const wxString &name)
{
   mName = name;
}

FilePath AudacityProject::GetInitialImportPath() const
{
   return mInitialImportPath;
}

void AudacityProject::SetInitialImportPath(const FilePath &path)
{
   if (mInitialImportPath.empty())
   {
      mInitialImportPath = path;
   }
}

AUDACITY_DLL_API wxFrame &GetProjectFrame( AudacityProject &project )
{
   auto ptr = project.GetFrame();
   if ( !ptr )
      THROW_INCONSISTENCY_EXCEPTION;
   return *ptr;
}

AUDACITY_DLL_API const wxFrame &GetProjectFrame( const AudacityProject &project )
{
   auto ptr = project.GetFrame();
   if ( !ptr )
      THROW_INCONSISTENCY_EXCEPTION;
   return *ptr;
}

std::unique_ptr<const BasicUI::WindowPlacement>
ProjectFramePlacement( AudacityProject *project )
{
   if (!project)
      return std::make_unique<BasicUI::WindowPlacement>();
   return std::make_unique<wxWidgetsWindowPlacement>(
      &GetProjectFrame(*project));
}

AUDACITY_DLL_API wxWindow &GetProjectPanel( AudacityProject &project )
{
   auto ptr = project.GetPanel();
   if ( !ptr )
      THROW_INCONSISTENCY_EXCEPTION;
   return *ptr;
}

AUDACITY_DLL_API const wxWindow &GetProjectPanel(
   const AudacityProject &project )
{
   auto ptr = project.GetPanel();
   if ( !ptr )
      THROW_INCONSISTENCY_EXCEPTION;
   return *ptr;
}
