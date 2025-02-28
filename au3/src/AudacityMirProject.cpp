/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudacityMirProject.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "AudacityMirProject.h"
#include "AdornedRulerPanel.h"
#include "AudacityDontAskAgainMessageDialog.h"
#include "Project.h"
#include "ProjectHistory.h"
#include "ProjectTimeSignature.h"
#include "ProjectWindows.h"
#include "TimeDisplayMode.h"
#include "WaveTrack.h"
#include "prefs/ImportExportPrefs.h"

namespace {
auto FormatTempo(double value)
{
    auto rounded = std::to_string(std::round(value * 100) / 100);
    rounded.erase(rounded.find_last_not_of('0') + 1, std::string::npos);
    rounded.erase(rounded.find_last_not_of('.') + 1, std::string::npos);
    return rounded;
}
} // namespace

AudacityMirProject::AudacityMirProject(AudacityProject& project)
    : mProject{project}
    , mImportedOnEmptyProject{TrackList::Get(mProject)
                              .Any<WaveTrack>()
                              .empty()}
    , mProjectTempo{ProjectTimeSignature::Get(mProject).GetTempo()}
{
}

AudacityMirProject::~AudacityMirProject()
{
    if (!mProjectWasModified) {
        return;
    }
    assert(mMostSignificantModification.has_value());
    auto& ph = ProjectHistory::Get(mProject);
    if (mMostSignificantModification == ModificationType::Automatic) {
        ph.ModifyState(true);
    } else if (mMostSignificantModification == ModificationType::Manual) {
        ph.PushState(
            XO("Configure Project from Music File"),
            XO("Automatic Music Configuration"));
    }
}

bool AudacityMirProject::ViewIsBeatsAndMeasures() const
{
    return AdornedRulerPanel::Get(mProject).GetTimeDisplayMode()
           == TimeDisplayMode::BeatsAndMeasures;
}

void AudacityMirProject::ReconfigureMusicGrid(
    double newTempo, std::optional<MIR::TimeSignature> timeSignature)
{
    AdornedRulerPanel::Get(mProject).SetTimeDisplayMode(
        TimeDisplayMode::BeatsAndMeasures);
    auto& projTimeSignature = ProjectTimeSignature::Get(mProject);
    projTimeSignature.SetTempo(newTempo);
    if (timeSignature.has_value()) {
        projTimeSignature.SetUpperTimeSignature(
            MIR::GetNumerator(*timeSignature));
        projTimeSignature.SetLowerTimeSignature(
            MIR::GetDenominator(*timeSignature));
    }
    mProjectWasModified = true;
}

double AudacityMirProject::GetTempo() const
{
    return mProjectTempo;
}

bool AudacityMirProject::ShouldBeReconfigured(
    double newTempo, bool isSingleFileImport)
{
    const auto policy = ImportExportPrefs::MusicFileImportSetting.Read();
    if (policy == wxString("Ask")) {
        const auto displayedTempo = FormatTempo(newTempo);
        const auto message
            =isSingleFileImport
              ? XO("Audacity detected this file to be %s bpm.\nWould you like to enable music view and set the project tempo to %s?")
              .Format(displayedTempo, displayedTempo)
              : XO("Audacity detected one or more files to be %s bpm.\nWould you like to enable music view and set the project tempo to %s?")
              .Format(displayedTempo, displayedTempo);
        AudacityDontAskAgainMessageDialog m(
            &GetProjectPanel(mProject), XO("Music Import"), message);
        const auto yes = m.ShowDialog();
        if (m.IsChecked()) {
            ImportExportPrefs::MusicFileImportSetting.Write(
                yes ? wxString("Yes") : wxString("No"));
        }
        if (yes) {
            mMostSignificantModification = ModificationType::Manual;
        }
        return yes;
    }
    const auto yes = policy == wxString("Yes");
    if (yes) {
        mMostSignificantModification = ModificationType::Automatic;
    }
    return yes;
}

void AudacityMirProject::OnClipsSynchronized()
{
    mProjectWasModified = true;
    if (mMostSignificantModification != ModificationType::Manual) {
        mMostSignificantModification = ModificationType::Automatic;
    }
}
