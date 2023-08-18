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
#include "ProjectSettings.h"
#include "ProjectWindow.h"
#include "Snap.h"
#include "TrackPanel.h"
#include "ViewInfo.h"
#include "WaveTrack.h"
#include "toolbars/SelectionBar.h"
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
   {
      selectedRegion.setTimes(t0, t1);
      TrackPanel::Get(mProject).Refresh(false);
   }
}

const NumericFormatSymbol & ProjectSelectionManager::AS_GetSelectionFormat()
{
   auto &project = mProject;
   return ProjectNumericFormats::Get(project).GetSelectionFormat();
}

void ProjectSelectionManager::AS_SetSelectionFormat(
   const NumericFormatSymbol & format)
{
   auto &project = mProject;
   auto &formats = ProjectNumericFormats::Get( project );
   formats.SetSelectionFormat( format );

   gPrefs->Write(wxT("/SelectionFormat"), format.Internal());
   gPrefs->Flush();

   SelectionBar::Get( project ).SetSelectionFormat(format);
}

const NumericFormatSymbol & ProjectSelectionManager::TT_GetAudioTimeFormat()
{
   auto &project = mProject;
   auto &formats = ProjectNumericFormats::Get( project );
   return formats.GetAudioTimeFormat();
}

void ProjectSelectionManager::TT_SetAudioTimeFormat(
   const NumericFormatSymbol & format)
{
   auto &project = mProject;
   auto &formats = ProjectNumericFormats::Get( project );
   formats.SetAudioTimeFormat( format );

   gPrefs->Write(wxT("/AudioTimeFormat"), format.Internal());
   gPrefs->Flush();

   TimeToolBar::Get( project ).SetAudioTimeFormat(format);
}

void ProjectSelectionManager::AS_ModifySelection(
   double &start, double &end, bool done)
{
   auto &project = mProject;
   auto &history = ProjectHistory::Get( project );
   auto &trackPanel = TrackPanel::Get( project );
   auto &viewInfo = ViewInfo::Get( project );
   viewInfo.selectedRegion.setTimes(start, end);
   trackPanel.Refresh(false);
   if (done) {
      history.ModifyState(false);
   }
}

double ProjectSelectionManager::SSBL_GetRate() const
{
   auto &project = mProject;
   auto &tracks = TrackList::Get( project );
   // Return maximum of project rate and all track rates.
   return std::max( ProjectRate::Get( project ).GetRate(),
      tracks.Any<const WaveTrack>().max(&WaveTrack::GetRate));
}

const NumericFormatSymbol &
ProjectSelectionManager::SSBL_GetFrequencySelectionFormatName()
{
   auto &project = mProject;
   auto &formats = ProjectNumericFormats::Get( project );
   return formats.GetFrequencySelectionFormatName();
}

void ProjectSelectionManager::SSBL_SetFrequencySelectionFormatName(
   const NumericFormatSymbol & formatName)
{
   auto &project = mProject;
   auto &formats = ProjectNumericFormats::Get( project );

   formats.SetFrequencySelectionFormatName( formatName );

   gPrefs->Write(wxT("/FrequencySelectionFormatName"),
                 formatName.Internal());
   gPrefs->Flush();

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   SpectralSelectionBar::Get( project ).SetFrequencySelectionFormatName(formatName);
#endif
}

const NumericFormatSymbol &
ProjectSelectionManager::SSBL_GetBandwidthSelectionFormatName()
{
   auto &project = mProject;
   auto &formats = ProjectNumericFormats::Get( project );
   return formats.GetBandwidthSelectionFormatName();
}

void ProjectSelectionManager::SSBL_SetBandwidthSelectionFormatName(
   const NumericFormatSymbol & formatName)
{
   auto &project = mProject;
   auto &formats = ProjectNumericFormats::Get( project );

   formats.SetBandwidthSelectionFormatName( formatName );

   gPrefs->Write(wxT("/BandwidthSelectionFormatName"),
      formatName.Internal());
   gPrefs->Flush();

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   SpectralSelectionBar::Get( project ).SetBandwidthSelectionFormatName(formatName);
#endif
}

void ProjectSelectionManager::SSBL_ModifySpectralSelection(
   double &bottom, double &top, bool done)
{
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   auto &project = mProject;
   auto &history = ProjectHistory::Get( project );
   auto &trackPanel = TrackPanel::Get( project );
   auto &viewInfo = ViewInfo::Get( project );

   double nyq = SSBL_GetRate() / 2.0;
   if (bottom >= 0.0)
      bottom = std::min(nyq, bottom);
   if (top >= 0.0)
      top = std::min(nyq, top);
   viewInfo.selectedRegion.setFrequencies(bottom, top);
   trackPanel.Refresh(false);
   if (done) {
      history.ModifyState(false);
   }
#else
   bottom; top; done;
#endif
}

