/**********************************************************************

Audacity: A Digital Audio Editor

ProjectSelectionManager.cpp

Paul Licameli split from ProjectManager.cpp

**********************************************************************/
#include "ProjectSelectionManager.h"

#include "Project.h"
#include "ProjectHistory.h"
#include "ProjectWindows.h"
#include "ProjectNumericFormats.h"
#include "ProjectRate.h"
#include "ProjectSnap.h"
#include "ProjectTimeSignature.h"
#include "Snap.h"
#include "ViewInfo.h"
#include "WaveTrack.h"
#include "toolbars/SpectralSelectionBar.h"
#include "toolbars/TimeToolBar.h"

static AudacityProject::AttachedObjects::RegisteredFactory
sProjectSelectionManagerKey {
   []( AudacityProject &project ) {
      return std::make_shared< ProjectSelectionManager >( project );
   }
};

ProjectSelectionManager &ProjectSelectionManager::Get(
   AudacityProject &project )
{
   return project.AttachedObjects::Get< ProjectSelectionManager >(
      sProjectSelectionManagerKey );
}

const ProjectSelectionManager &ProjectSelectionManager::Get(
   const AudacityProject &project )
{
   return Get( const_cast< AudacityProject & >( project ) );
}

ProjectSelectionManager::ProjectSelectionManager(AudacityProject& project)
    : mProject { project }
    , mSnappingChangedSubscription { ProjectSnap::Get(project).Subscribe(
         [this](auto&)
         {
            SnapSelection();
         })}
    , mTimeSignatureChangedSubscription {
       ProjectTimeSignature::Get(project).Subscribe([this](auto&)
                                                    { SnapSelection(); }) }
    , mProjectRateChangedSubscription { ProjectRate::Get(project).Subscribe(
         [this](auto&) { SnapSelection(); }) } 

{
   // Be consistent with ProjectNumericFormats
   auto &formats = ProjectNumericFormats::Get(mProject);
   SetSelectionFormat(formats.GetSelectionFormat());

   // And stay consistent
   mFormatsSubscription = ProjectNumericFormats::Get(project)
      .Subscribe(*this, &ProjectSelectionManager::OnFormatsChanged);
}

ProjectSelectionManager::~ProjectSelectionManager() = default;

void ProjectSelectionManager::SnapSelection()
{   
   auto& project = mProject;
   auto& projectSnap = ProjectSnap::Get(mProject);

   if (projectSnap.GetSnapMode() == SnapMode::SNAP_OFF)
      return;

   auto& viewInfo = ViewInfo::Get(project);
   auto& selectedRegion = viewInfo.selectedRegion;

   const double oldt0 = selectedRegion.t0();
   const double oldt1 = selectedRegion.t1();

   const double t0 = projectSnap.SnapTime(oldt0).time;
   const double t1 = projectSnap.SnapTime(oldt1).time;

   if (t0 != oldt0 || t1 != oldt1)
      selectedRegion.setTimes(t0, t1);
}

void ProjectSelectionManager::OnFormatsChanged(ProjectNumericFormatsEvent evt)
{
   auto &formats = ProjectNumericFormats::Get(mProject);
   switch (evt.type) {
   case ProjectNumericFormatsEvent::ChangedSelectionFormat:
      return SetSelectionFormat(formats.GetSelectionFormat());
   default:
      break;
   }
}

void ProjectSelectionManager::SetSelectionFormat(const NumericFormatID & format)
{
   gPrefs->Write(wxT("/SelectionFormat"), format);
   gPrefs->Flush();
}

void
ProjectSelectionManager::TT_SetAudioTimeFormat(const NumericFormatID & format)
{
   auto &project = mProject;
   auto &formats = ProjectNumericFormats::Get( project );
   formats.SetAudioTimeFormat( format );

   gPrefs->Write(wxT("/AudioTimeFormat"), format.GET());
   gPrefs->Flush();

   TimeToolBar::Get( project ).SetAudioTimeFormat(format);
}

void ProjectSelectionManager::ModifySelection(
   double &start, double &end, bool done)
{
   auto &project = mProject;
   auto &history = ProjectHistory::Get( project );
   auto &viewInfo = ViewInfo::Get( project );
   viewInfo.selectedRegion.setTimes(start, end);
   if (done)
      history.ModifyState(false);
}

void ProjectSelectionManager::SSBL_SetFrequencySelectionFormatName(
   const NumericFormatID &formatName)
{
   auto &project = mProject;
   auto &formats = ProjectNumericFormats::Get( project );

   formats.SetFrequencySelectionFormatName( formatName );

   gPrefs->Write(wxT("/FrequencySelectionFormatName"),
                 formatName.GET());
   gPrefs->Flush();

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   SpectralSelectionBar::Get( project ).SetFrequencySelectionFormatName(formatName);
#endif
}

void ProjectSelectionManager::SSBL_SetBandwidthSelectionFormatName(
   const NumericFormatID & formatName)
{
   auto &project = mProject;
   auto &formats = ProjectNumericFormats::Get( project );

   formats.SetBandwidthSelectionFormatName( formatName );

   gPrefs->Write(wxT("/BandwidthSelectionFormatName"), formatName.GET());
   gPrefs->Flush();

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   SpectralSelectionBar::Get(project)
      .SetBandwidthSelectionFormatName(formatName.GET());
#endif
}

void ProjectSelectionManager::ModifySpectralSelection(
   double &bottom, double &top, bool done)
{
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   auto &project = mProject;
   auto &history = ProjectHistory::Get(project);
   auto &viewInfo = ViewInfo::Get(project);
   auto &tracks = TrackList::Get(mProject);
   auto nyq = std::max(ProjectRate::Get(project).GetRate(),
      tracks.Any<const WaveTrack>().max(&WaveTrack::GetRate))
      / 2.0;
   if (bottom >= 0.0)
      bottom = std::min(nyq, bottom);
   if (top >= 0.0)
      top = std::min(nyq, top);
   viewInfo.selectedRegion.setFrequencies(bottom, top);
   if (done)
      history.ModifyState(false);
#else
   bottom; top; done;
#endif
}

