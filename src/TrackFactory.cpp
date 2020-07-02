/**********************************************************************

Audacity: A Digital Audio Editor

TrackFactory.h

Paul Licameli -- split from Track.h

**********************************************************************/

#include "TrackFactory.h"

#include "LabelTrack.h"
#include "NoteTrack.h"
#include "TimeTrack.h"
#include "ViewInfo.h"
#include "WaveTrack.h"

LabelTrack::Holder TrackFactory::NewLabelTrack()
{
   return std::make_shared<LabelTrack>();
}

NoteTrack::Holder TrackFactory::NewNoteTrack()
{
   return std::make_shared<NoteTrack>();
}

std::shared_ptr<TimeTrack> TrackFactory::NewTimeTrack()
{
   return std::make_shared<TimeTrack>(mZoomInfo);
}

WaveTrack::Holder TrackFactory::DuplicateWaveTrack(const WaveTrack &orig)
{
   return std::static_pointer_cast<WaveTrack>( orig.Duplicate() );
}

WaveTrack::Holder TrackFactory::NewWaveTrack(sampleFormat format, double rate)
{
   if (format == (sampleFormat)0)
      format = QualityPrefs::SampleFormatChoice();
   if (rate == 0)
      rate = mSettings.GetRate();
   return std::make_shared<WaveTrack> ( &mProject, format, rate );
}

static auto TrackFactoryFactory = []( AudacityProject &project ) {
   auto &viewInfo = ViewInfo::Get( project );
   return std::make_shared< TrackFactory >(
      ProjectSettings::Get( project ),
      project, &viewInfo );
};

static const AudacityProject::AttachedObjects::RegisteredFactory key2{
   TrackFactoryFactory
};

TrackFactory &TrackFactory::Get( AudacityProject &project )
{
   return project.AttachedObjects::Get< TrackFactory >( key2 );
}

const TrackFactory &TrackFactory::Get( const AudacityProject &project )
{
   return Get( const_cast< AudacityProject & >( project ) );
}

TrackFactory &TrackFactory::Reset( AudacityProject &project )
{
   auto result = TrackFactoryFactory( project );
   project.AttachedObjects::Assign( key2, result );
   return *result;
}

void TrackFactory::Destroy( AudacityProject &project )
{
   project.AttachedObjects::Assign( key2, nullptr );
}
