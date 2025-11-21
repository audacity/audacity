/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ProjectFileIOExtension.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cstdint>
#include <functional>
#include <string>

class AudacityProject;
class ProjectSerializer;

using ProjectSaveCallback
    =std::function<bool (const std::string& path, bool nameChanged)>;

//! Action the ProjectManager should take after the save hooks were called
enum class OnSaveAction
{
    //! Save was handled by the extension
    Handled,
    //! Save was cancelled by the extension
    Cancelled,
    //! Save was not handled by the extension
    Continue
};

//! Action the ProjectManager should take after the close hooks were called
enum class OnCloseAction
{
    //! Extension vetoed the close
    Veto,
    //! Extension did not veto the close
    Continue
};

//! Action the ProjectManager should take after the open hooks were called
enum class OnOpenAction
{
    //! Open was cancelled by the extension
    Cancel,
    //! ProjectManager should continue with the open
    Continue
};

//! Set of hooks for project file I/O
class PROJECT_FILE_IO_API ProjectFileIOExtension /* not final */
{
public:
    virtual ~ProjectFileIOExtension();

    //! This hook is called before the project is opened
    virtual OnOpenAction
    OnOpen(AudacityProject& project, const std::string& path) = 0;
    //! This hook is called after the project is loaded
    virtual void OnLoad(AudacityProject& project)             = 0;
    //! This hook is called before the project is saved
    virtual OnSaveAction OnSave(
        AudacityProject& project, const ProjectSaveCallback& projectSaveCallback)      = 0;
    //! This hook is called before the project is closed
    virtual OnCloseAction OnClose(AudacityProject& project) = 0;
    //! This hook is called after the project blob is saved
    virtual void OnUpdateSaved(
        AudacityProject& project, const ProjectSerializer& serializer) = 0;
    //! This hook is called to check if a block is locked
    virtual bool
    IsBlockLocked(const AudacityProject& project, int64_t blockId) const = 0;
};

//! Extension registry for project file I/O extensions
struct PROJECT_FILE_IO_API ProjectFileIOExtensionRegistry final
{
    struct PROJECT_FILE_IO_API Extension final
    {
        explicit Extension(ProjectFileIOExtension& extension);
    };

    static OnOpenAction
    OnOpen(AudacityProject& project, const std::string& path);
    static void OnLoad(AudacityProject& project);
    static OnSaveAction OnSave(
        AudacityProject& project, const ProjectSaveCallback& projectSaveCallback);
    static OnCloseAction OnClose(AudacityProject& project);
    static void
    OnUpdateSaved(AudacityProject& project, const ProjectSerializer& serializer);
    static bool IsBlockLocked(const AudacityProject& project, int64_t blockId);
};
