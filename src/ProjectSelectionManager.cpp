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

ProjectSelectionManager::ProjectSelectionManager( AudacityProject &project )
   : mProject{ project }
{
}

ProjectSelectionManager::~ProjectSelectionManager() = default;

bool ProjectSelectionManager::SnapSelection()
{
   auto &project = mProject;
   auto &formats = ProjectNumericFormats::Get( project );
   auto &settings = ProjectSettings::Get( project );
   auto &window = ProjectWindow::Get( project );
   auto snapTo = settings.GetSnapTo();
   if (snapTo != SNAP_OFF) {
      auto &viewInfo = ViewInfo::Get( project );
      auto &selectedRegion = viewInfo.selectedRegion;
      NumericConverter nc(NumericConverter::TIME,
         formats.GetSelectionFormat(), 0, ProjectRate::Get(project).GetRate());
      const bool nearest = (snapTo == SNAP_NEAREST);

      const double oldt0 = selectedRegion.t0();
      const double oldt1 = selectedRegion.t1();

      nc.ValueToControls(oldt0, nearest);
      nc.ControlsToValue();
      const double t0 = nc.GetValue();

      nc.ValueToControls(oldt1, nearest);
      nc.ControlsToValue();
      const double t1 = nc.GetValue();

      if (t0 != oldt0 || t1 != oldt1) {
         selectedRegion.setTimes(t0, t1);
         return true;
      }
   }

   return false;
}

double ProjectSelectionManager::AS_GetRate()
{
   return ProjectRate::Get(mProject).GetRate();
}

void ProjectSelectionManager::AS_SetRate(double rate)
{
   auto &project = mProject;
   ProjectRate::Get( project ).SetRate( rate );
   SelectionBar::Get( project ).SetRate(rate);
}

int ProjectSelectionManager::AS_GetSnapTo()
{
   auto &project = mProject;
   auto &settings = ProjectSettings::Get( project );
   return settings.GetSnapTo();
}

void ProjectSelectionManager::AS_SetSnapTo(int snap)
{
   auto &project = mProject;
   auto &settings = ProjectSettings::Get( project );
   auto &window = ProjectWindow::Get( project );

   settings.SetSnapTo( snap );

// LLL: TODO - what should this be changed to???
// GetCommandManager()->Check(wxT("Snap"), mSnapTo);
   gPrefs->Write(wxT("/SnapTo"), snap);
   gPrefs->Flush();

   SnapSelection();

   window.RedrawProject();

   SelectionBar::Get( project ).SetSnapTo(snap);
}

const NumericFormatSymbol & ProjectSelectionManager::AS_GetSelectionFormat()
{
   auto &project = mProject;
   auto &formats = ProjectNumericFormats::Get( project );
   return formats.GetSelectionFormat();
}

void ProjectSelectionManager::AS_SetSelectionFormat(
   const NumericFormatSymbol & format)
{
   auto &project = mProject;
   auto &formats = ProjectNumericFormats::Get( project );
   formats.SetSelectionFormat( format );

   gPrefs->Write(wxT("/SelectionFormat"), format.Internal());
   gPrefs->Flush();

   if (SnapSelection())
      TrackPanel::Get( project ).Refresh(false);

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
      tracks.Any<const WaveTrack>().max( &WaveTrack::GetRate ) );
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
