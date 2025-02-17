/**********************************************************************

  Audacity: A Digital Audio Editor

  @file TransportUtilities.cpp
  @brief implements some UI related to starting and stopping play and record

  Paul Licameli split from TransportMenus.cpp

**********************************************************************/

#include "TransportUtilities.h"

#include <thread>
#include "AudioIO.h"
#include "AudioIOSequences.h"
#include "CommandContext.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "ProjectAudioManager.h"
#include "SampleTrack.h"
#include "StretchingSequence.h"
#include "ViewInfo.h"
#include "toolbars/ControlToolBar.h"
#include "ProgressDialog.h"
#include "WaveTrack.h"

void TransportUtilities::PlayCurrentRegionAndWait(
    const CommandContext& context,
    bool newDefault,
    bool cutpreview)
{
    auto& project = context.project;
    auto& projectAudioManager = ProjectAudioManager::Get(project);

    const auto& playRegion = ViewInfo::Get(project).playRegion;
    double t0 = playRegion.GetStart();
    double t1 = playRegion.GetEnd();

    projectAudioManager.PlayCurrentRegion(newDefault, cutpreview);

    if (project.mBatchMode > 0 && t0 != t1 && !newDefault) {
        wxYieldIfNeeded();

        /* i18n-hint: This title appears on a dialog that indicates the progress
           in doing something.*/
        ProgressDialog progress(XO("Progress"), XO("Playing"), pdlgHideCancelButton);
        auto gAudioIO = AudioIO::Get();

        while (projectAudioManager.Playing()) {
            ProgressResult result = progress.Update(gAudioIO->GetStreamTime() - t0, t1 - t0);
            if (result != ProgressResult::Success) {
                projectAudioManager.Stop();
                if (result != ProgressResult::Stopped) {
                    context.Error(wxT("Playing interrupted"));
                }
                break;
            }

            using namespace std::chrono;
            std::this_thread::sleep_for(100ms);
            wxYieldIfNeeded();
        }

        projectAudioManager.Stop();
        wxYieldIfNeeded();
    }
}

void TransportUtilities::PlayPlayRegionAndWait(
    const CommandContext& context,
    const SelectedRegion& selectedRegion,
    const AudioIOStartStreamOptions& options,
    PlayMode mode)
{
    auto& project = context.project;
    auto& projectAudioManager = ProjectAudioManager::Get(project);

    double t0 = selectedRegion.t0();
    double t1 = selectedRegion.t1();

    projectAudioManager.PlayPlayRegion(selectedRegion, options, mode);

    if (project.mBatchMode > 0) {
        wxYieldIfNeeded();

        /* i18n-hint: This title appears on a dialog that indicates the progress
           in doing something.*/
        ProgressDialog progress(XO("Progress"), XO("Playing"), pdlgHideCancelButton);
        auto gAudioIO = AudioIO::Get();

        while (projectAudioManager.Playing()) {
            ProgressResult result = progress.Update(gAudioIO->GetStreamTime() - t0, t1 - t0);
            if (result != ProgressResult::Success) {
                projectAudioManager.Stop();
                if (result != ProgressResult::Stopped) {
                    context.Error(wxT("Playing interrupted"));
                }
                break;
            }

            using namespace std::chrono;
            std::this_thread::sleep_for(100ms);
            wxYieldIfNeeded();
        }

        projectAudioManager.Stop();
        wxYieldIfNeeded();
    }
}

void TransportUtilities::RecordAndWait(
    const CommandContext& context, bool altAppearance)
{
    auto& project = context.project;
    auto& projectAudioManager = ProjectAudioManager::Get(project);

    const auto& selectedRegion = ViewInfo::Get(project).selectedRegion;
    double t0 = selectedRegion.t0();
    double t1 = selectedRegion.t1();

    projectAudioManager.OnRecord(altAppearance);

    if (project.mBatchMode > 0 && t1 != t0) {
        wxYieldIfNeeded();

        /* i18n-hint: This title appears on a dialog that indicates the progress
           in doing something.*/
        ProgressDialog progress(XO("Progress"), XO("Recording"), pdlgHideCancelButton);
        auto gAudioIO = AudioIO::Get();

        while (projectAudioManager.Recording()) {
            ProgressResult result = progress.Update(gAudioIO->GetStreamTime() - t0, t1 - t0);
            if (result != ProgressResult::Success) {
                projectAudioManager.Stop();
                if (result != ProgressResult::Stopped) {
                    context.Error(wxT("Recording interrupted"));
                }
                break;
            }

            using namespace std::chrono;
            std::this_thread::sleep_for(100ms);
            wxYieldIfNeeded();
        }

        projectAudioManager.Stop();
        wxYieldIfNeeded();
    }
}

// Returns true if this project was stopped, otherwise false.
// (it may though have stopped another project playing)
bool TransportUtilities::DoStopPlaying(const CommandContext& context)
{
    auto& project = context.project;
    auto& projectAudioManager = ProjectAudioManager::Get(project);
    auto gAudioIO = AudioIOBase::Get();
    auto& toolbar = ControlToolBar::Get(project);
    auto token = ProjectAudioIO::Get(project).GetAudioIOToken();

    //If this project is playing, stop playing, make sure everything is unpaused.
    if (gAudioIO->IsStreamActive(token)) {
        toolbar.SetStop();       //Pushes stop down
        projectAudioManager.Stop();
        // Playing project was stopped.  All done.
        return true;
    }

    // This project isn't playing.
    // If some other project is playing, stop playing it
    if (gAudioIO->IsStreamActive()) {
        //find out which project we need;
        auto start = AllProjects{}.begin(), finish = AllProjects{}.end(),
             iter = std::find_if(start, finish,
                                 [&](const AllProjects::value_type& ptr) {
            return gAudioIO->IsStreamActive(
                ProjectAudioIO::Get(*ptr).GetAudioIOToken());
        });

        //stop playing the other project
        if (iter != finish) {
            auto otherProject = *iter;
            auto& otherToolbar = ControlToolBar::Get(*otherProject);
            auto& otherProjectAudioManager
                =ProjectAudioManager::Get(*otherProject);
            otherToolbar.SetStop();      //Pushes stop down
            otherProjectAudioManager.Stop();
        }
    }
    return false;
}

void TransportUtilities::DoStartPlaying(
    const CommandContext& context, bool newDefault)
{
    auto& project = context.project;
    auto gAudioIO = AudioIOBase::Get();
    //play the front project
    if (!gAudioIO->IsBusy()) {
        //Otherwise, start playing (assuming audio I/O isn't busy)

        // Will automatically set mLastPlayMode
        PlayCurrentRegionAndWait(context, newDefault);
    }
}

TransportSequences MakeTransportTracks(
    TrackList& trackList, bool selectedOnly, bool nonWaveToo)
{
    TransportSequences result;
    {
        const auto range = trackList.Any<WaveTrack>()
                           + (selectedOnly ? &Track::IsSelected : &Track::Any);
        for (auto pTrack : range) {
            result.playbackSequences.push_back(
                StretchingSequence::Create(*pTrack, pTrack->GetClipInterfaces()));
        }
    }
    if (nonWaveToo) {
        const auto range = trackList.Any<const PlayableTrack>()
                           + (selectedOnly ? &Track::IsSelected : &Track::Any);
        for (auto pTrack : range) {
            if (!track_cast<const SampleTrack*>(pTrack)) {
                if (auto pSequence
                        =std::dynamic_pointer_cast<const OtherPlayableSequence>(
                              pTrack->shared_from_this())
                        ) {
                    result.otherPlayableSequences.push_back(pSequence);
                }
            }
        }
    }
    return result;
}
