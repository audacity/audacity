/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <cstdint>
#include <functional>
#include <memory>
#include <string>
#include <vector>

#include <QObject>
#include <QString>

#include "trackedit/iprojecthistory.h"
#include "trackedit/trackedittypes.h"

class AudacityProject;
class LabelTrack;
class TrackList;
class WaveTrack;
class WaveTrackFactory;

namespace au::trackedit::api {
struct ProjectAudioClip {
    int64_t nativeId = 0;
    double start = 0.0;
    double end = 0.0;
    std::string title;
    bool selected = false;
};

struct ProjectAudioTrack {
    std::shared_ptr<WaveTrack> source;
    std::shared_ptr<WaveTrack> destination;
    std::string name;
    bool selected = true;
    std::vector<ProjectAudioClip> clips;
};

struct ProjectLabel {
    int64_t nativeId = 0;
    double start = 0.0;
    double end = 0.0;
    std::string text;
    bool selected = false;
};

struct ProjectLabelTrack {
    std::shared_ptr<LabelTrack> track;
    std::string name;
    std::vector<ProjectLabel> labels;
    bool selected = true;
};

struct ProjectEditWorkspace {
    std::shared_ptr<AudacityProject> project;
    ::TrackList* tracks = nullptr;
    WaveTrackFactory* factory = nullptr;
    std::vector<ProjectAudioTrack> audioTracks;
    std::vector<ProjectLabelTrack> labelTracks;
    std::shared_ptr<WaveTrack> preferredAudioDestination;
    std::function<bool()> projectAvailable;
    double selectionStart = 0.0;
    double selectionEnd = 0.0;
};

namespace detail {
struct EditState;
}

class ProjectEditSession : public QObject
{
public:
    explicit ProjectEditSession(ProjectEditWorkspace workspace, QObject* parent = nullptr);
    ~ProjectEditSession() override;

    QObject* beginEdit(QObject* parent);
    bool finish(std::string& error);
    void invalidate();

private:
    std::shared_ptr<detail::EditState> m_state;
};

class ProjectApiScope final
{
public:
    explicit ProjectApiScope(ProjectEditSession* session = nullptr);
    ~ProjectApiScope();

    ProjectApiScope(const ProjectApiScope&) = delete;
    ProjectApiScope& operator=(const ProjectApiScope&) = delete;

private:
    ProjectEditSession* m_previousSession = nullptr;
    bool m_previousActive = false;

    friend class ProjectApi;
};

bool projectApiScopeActive();
ProjectEditSession* currentProjectEditSession();
ProjectLabelTrack snapshotLabelTrack(LabelTrack& track, double start, double end);
QObject* makeCurrentProjectSelection(
    AudacityProject* project, double start, double end, const ClipKeyList& selectedClips, QObject* parent);
QObject* beginRootProjectEdit(
    AudacityProject* project, double start, double end, const ClipKeyList& selectedClips, const QString& name, IProjectHistoryPtr history,
    std::function<bool()> projectAvailable, std::function<void(double, double, const std::vector<int64_t>&)> setSelection, QObject* parent);
void throwProjectError(QObject* object, const QString& message);
} // namespace au::trackedit::api
