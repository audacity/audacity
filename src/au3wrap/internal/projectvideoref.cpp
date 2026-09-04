/*
 * Audacity: A Digital Audio Editor
 */
#include "projectvideoref.h"

#include "au3-xml/XMLAttributeValueView.h"
#include "au3-xml/XMLWriter.h"

using namespace au::au3;

static const AttachedProjectObjects::RegisteredFactory key
{
    [](AudacityProject&) {
        return std::make_shared<ProjectVideoRef>();
    }
};

ProjectVideoRef& ProjectVideoRef::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get<ProjectVideoRef&>(key);
}

const ProjectVideoRef& ProjectVideoRef::Get(const AudacityProject& project)
{
    return Get(const_cast<AudacityProject&>(project));
}

ProjectVideoRef::ProjectVideoRef() {}

ProjectVideoRef::~ProjectVideoRef() {}

const std::string& ProjectVideoRef::path() const
{
    return m_path;
}

void ProjectVideoRef::setPath(const std::string& path)
{
    m_path = path;
}

const std::string& ProjectVideoRef::relativePath() const
{
    return m_relativePath;
}

void ProjectVideoRef::setRelativePath(const std::string& path)
{
    m_relativePath = path;
}

double ProjectVideoRef::duration() const
{
    return m_duration;
}

void ProjectVideoRef::setDuration(double duration)
{
    m_duration = duration;
}

double ProjectVideoRef::frameRate() const
{
    return m_frameRate;
}

void ProjectVideoRef::setFrameRate(double frameRate)
{
    m_frameRate = frameRate;
}

bool ProjectVideoRef::isEmpty() const
{
    return m_path.empty() && m_relativePath.empty();
}

void ProjectVideoRef::clear()
{
    m_path.clear();
    m_relativePath.clear();
    m_duration = 0.0;
    m_frameRate = 0.0;
}

static ProjectFileIORegistry::AttributeWriterEntry entry {
    [](const AudacityProject& project, XMLWriter& xmlFile){
        auto& ref = ProjectVideoRef::Get(project);
        if (ref.isEmpty()) {
            return;
        }

        xmlFile.WriteAttr(wxT("video_path"), wxString::FromUTF8(ref.path()));
        xmlFile.WriteAttr(wxT("video_path_rel"), wxString::FromUTF8(ref.relativePath()));
        xmlFile.WriteAttr(wxT("video_duration"), ref.duration());
        xmlFile.WriteAttr(wxT("video_frame_rate"), ref.frameRate());
    }
};

static ProjectFileIORegistry::AttributeReaderEntries entries {
    (ProjectVideoRef & (*)(AudacityProject&)) & ProjectVideoRef::Get, {
        { "video_path", [](auto& ref, auto value) {
                ref.setPath(value.ToWString().ToStdString());
            } },
        { "video_path_rel", [](auto& ref, auto value) {
                ref.setRelativePath(value.ToWString().ToStdString());
            } },
        { "video_duration", [](auto& ref, auto value) {
                ref.setDuration(value.Get(ref.duration()));
            } },
        { "video_frame_rate", [](auto& ref, auto value) {
                ref.setFrameRate(value.Get(ref.frameRate()));
            } }
    }
};
