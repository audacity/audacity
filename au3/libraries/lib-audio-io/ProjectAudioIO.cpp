/**********************************************************************

Audacity: A Digital Audio Editor

ProjectAudioIO.cpp

Paul Licameli split from AudacityProject.cpp

**********************************************************************/

#include "ProjectAudioIO.h"

#include "AudioIOBase.h"
#include "Mix.h"
#include "Project.h"
#include "ProjectRate.h"

AudioIOStartStreamOptions
ProjectAudioIO::DefaultOptionsFactory(AudacityProject& project, bool)
{
    auto& projectAudioIO = Get(project);
    AudioIOStartStreamOptions options{
        project.shared_from_this(), ProjectRate::Get(project).GetRate()
    };
    options.captureMeter = projectAudioIO.GetCaptureMeter();
    options.playbackMeter = projectAudioIO.GetPlaybackMeter();
    options.envelope
        =Mixer::WarpOptions::DefaultWarp::Call(&project);
    // options.listener remains null
    // boolean argument is ignored
    return options;
}

AudioIOStartStreamOptions ProjectAudioIO::GetDefaultOptions(
    AudacityProject& project, bool newDefaults)
{
    return DefaultOptions::Call(project, newDefaults);
}

static const AudacityProject::AttachedObjects::RegisteredFactory sAudioIOKey{
    []( AudacityProject& parent ){
        return std::make_shared< ProjectAudioIO >(parent);
    }
};

ProjectAudioIO& ProjectAudioIO::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get< ProjectAudioIO >(sAudioIOKey);
}

const ProjectAudioIO& ProjectAudioIO::Get(const AudacityProject& project)
{
    return Get(const_cast<AudacityProject&>(project));
}

ProjectAudioIO::ProjectAudioIO(AudacityProject& project)
    : mProject{project}
{
}

ProjectAudioIO::~ProjectAudioIO()
{
}

int ProjectAudioIO::GetAudioIOToken() const
{
    return mAudioIOToken;
}

void ProjectAudioIO::SetAudioIOToken(int token)
{
    mAudioIOToken = token;
}

bool ProjectAudioIO::IsAudioActive() const
{
    auto gAudioIO = AudioIOBase::Get();
    return GetAudioIOToken() > 0
           && gAudioIO->IsStreamActive(GetAudioIOToken());
}

const std::shared_ptr<Meter>& ProjectAudioIO::GetPlaybackMeter() const
{
    return mPlaybackMeter;
}

void ProjectAudioIO::SetPlaybackMeter(
    const std::shared_ptr<Meter>& playback)
{
    auto& project = mProject;
    mPlaybackMeter = playback;
    auto gAudioIO = AudioIOBase::Get();
    if (gAudioIO) {
        gAudioIO->SetPlaybackMeter(project.shared_from_this(), mPlaybackMeter);
    }
}

const std::shared_ptr<Meter>& ProjectAudioIO::GetCaptureMeter() const
{
    return mCaptureMeter;
}

void ProjectAudioIO::SetCaptureMeter(
    const std::shared_ptr<Meter>& capture)
{
    auto& project = mProject;
    mCaptureMeter = capture;

    auto gAudioIO = AudioIOBase::Get();
    if (gAudioIO) {
        gAudioIO->SetCaptureMeter(project.shared_from_this(), mCaptureMeter);
    }
}

void ProjectAudioIO::SetPlaySpeed(double value)
{
    if (auto oldValue = GetPlaySpeed(); value != oldValue) {
        mPlaySpeed.store(value, std::memory_order_relaxed);
        Publish({});
    }
}
