/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <string>

#include "au3-registries/ClientData.h"
#include "au3-project/Project.h"

class AudacityProject;

namespace au::au3 {
//! Remembers which video file a project has attached, so it comes back when
//! the project is reopened.
//!
//! Project-scoped rather than attached to a track or clip. Clip identifiers
//! are never written to the project file and are reassigned on split, delete
//! and paste, so there is nothing stable to anchor to; and a video is a
//! property of the session being edited, not of one piece of audio in it.
//!
//! Both an absolute and a project-relative path are kept. This is the project
//! format's first dependency on a file outside the .aup3, and an absolute path
//! alone breaks the moment the project moves. The duration and frame rate are
//! recorded alongside so a path that still resolves after the media was
//! replaced or re-encoded can be noticed rather than silently trusted.
class ProjectVideoRef : public ClientData::Base
{
public:
    static ProjectVideoRef& Get(AudacityProject& project);
    static const ProjectVideoRef& Get(const AudacityProject& project);

    ProjectVideoRef();
    ~ProjectVideoRef();

    const std::string& path() const;
    void setPath(const std::string& path);

    const std::string& relativePath() const;
    void setRelativePath(const std::string& path);

    double duration() const;
    void setDuration(double duration);

    double frameRate() const;
    void setFrameRate(double frameRate);

    bool isEmpty() const;
    void clear();

private:
    std::string m_path;
    std::string m_relativePath;
    double m_duration { 0.0 };
    double m_frameRate { 0.0 };
};
}
