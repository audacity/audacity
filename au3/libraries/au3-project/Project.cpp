/**********************************************************************

  Audacity: A Digital Audio Editor

  Project.cpp

  Dominic Mazzoni
  Vaughan Johnson

*//*******************************************************************/

#include "Project.h"

#include <wx/display.h>
#include <wx/filename.h>

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

auto AllProjects::Remove(AudacityProject& project) -> value_type
{
    std::lock_guard<std::mutex> guard{ Mutex() };
    auto start = begin(), finish = end(), iter = std::find_if(
        start, finish,
        [&]( const value_type& ptr ){ return ptr.get() == &project; }
        );
    if (iter == finish) {
        return nullptr;
    }
    auto result = *iter;
    gAudacityProjects.erase(iter);
    return result;
}

void AllProjects::Add(const value_type& pProject)
{
    if (!pProject) {
        wxASSERT(false);
        return;
    }
    std::lock_guard<std::mutex> guard{ Mutex() };
    gAudacityProjects.push_back(pProject);
}

std::mutex& AllProjects::Mutex()
{
    static std::mutex theMutex;
    return theMutex;
}

int AudacityProject::mProjectCounter=0;// global counter.

/* Define Global Variables */
//This array holds onto all of the projects currently open
AllProjects::Container AllProjects::gAudacityProjects;

std::shared_ptr<AudacityProject> AudacityProject::Create()
{
    // Must complete make_shared before using shared_from_this() or
    // weak_from_this()
    auto result = std::make_shared<AudacityProject>(CreateToken {});
    // Only now build the attached objects, which also causes the project window
    // to be built on demand
    result->AttachedObjects::BuildAll();
    // But not for all the attached windows.  They get built on demand only
    // later.
    return result;
}

AudacityProject::AudacityProject(CreateToken)
{
    mProjectNo = mProjectCounter++; // Bug 322
}

AudacityProject::~AudacityProject()
{
}

const wxString& AudacityProject::GetProjectName() const
{
    return mName;
}

void AudacityProject::SetProjectName(const wxString& name)
{
    mName = name;
}

FilePath AudacityProject::GetInitialImportPath() const
{
    return mInitialImportPath;
}

void AudacityProject::SetInitialImportPath(const FilePath& path)
{
    if (mInitialImportPath.empty()) {
        mInitialImportPath = path;
    }
}

// Generate the needed, linkable registry functions
DEFINE_XML_METHOD_REGISTRY(ProjectFileIORegistry);

#include "BasicUI.h"

std::unique_ptr<const BasicUI::WindowPlacement>
ProjectFramePlacement(AudacityProject* project)
{
    auto& factory = WindowPlacementFactory::Get();
    std::unique_ptr<const BasicUI::WindowPlacement> result;
    if (project && factory && (result = factory(*project)).get()) {
        return result;
    } else {
        return std::make_unique<BasicUI::WindowPlacement>();
    }
}
