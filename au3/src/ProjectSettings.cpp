/**********************************************************************

Audacity: A Digital Audio Editor

ProjectSettings.cpp

Paul Licameli split from AudacityProject.cpp

**********************************************************************/

#include "ProjectSettings.h"

#include "AudioIOBase.h"
#include "Project.h"
#include "QualitySettings.h"
#include "widgets/NumericTextCtrl.h"

static const AudacityProject::AttachedObjects::RegisteredFactory
    sProjectSettingsKey{
    []( AudacityProject& project ){
        auto result = std::make_shared< ProjectSettings >(project);
        return result;
    }
};

ProjectSettings& ProjectSettings::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get< ProjectSettings >(
        sProjectSettingsKey);
}

const ProjectSettings& ProjectSettings::Get(const AudacityProject& project)
{
    return Get(const_cast< AudacityProject& >(project));
}

ProjectSettings::ProjectSettings(AudacityProject& project)
    : mProject{project}
    , mCurrentBrushRadius(5)
{
    bool multiToolActive = false;
    gPrefs->Read(wxT("/GUI/ToolBars/Tools/MultiToolActive"), &multiToolActive);

    if (multiToolActive) {
        mCurrentTool = ToolCodes::multiTool;
    } else {
        mCurrentTool = ToolCodes::selectTool;
    }

    UpdatePrefs();
}

void ProjectSettings::UpdatePrefs()
{
    gPrefs->Read(wxT("/GUI/ShowSplashScreen"), &mShowSplashScreen, true);
    //   gPrefs->Read(wxT("/GUI/UpdateSpectrogram"),
    //     &mViewInfo.bUpdateSpectrogram, true);

    // This code to change an empty projects rate is currently disabled, after
    // discussion.  The rule 'Default sample rate' only affects newly created
    // projects was felt to be simpler and better.
#if 0
    // The DefaultProjectSample rate is the rate for new projects.
    // Do not change this project's rate, unless there are no tracks.
    if (TrackList::Get(*this).size() == 0) {
        mRate = QualityDefaultSampleRate.Read();
        // If necessary, we change this rate in the selection toolbar too.
        auto bar = SelectionBar::Get(*this);
        bar.SetRate(mRate);
    }
#endif
}

void ProjectSettings::SetTool(int tool)
{
    if (auto oldValue = mCurrentTool; tool != oldValue) {
        mCurrentTool = tool;
        Publish({ ProjectSettingsEvent::ChangedTool, oldValue, tool });
    }
}
