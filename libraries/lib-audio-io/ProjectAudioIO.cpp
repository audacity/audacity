/**********************************************************************

Audacity: A Digital Audio Editor

ProjectAudioIO.cpp

Paul Licameli split from AudacityProject.cpp

**********************************************************************/

#include "ProjectAudioIO.h"

#include "AudioIO.h"
#include "Mix.h"
#include "Project.h"
#include "ProjectRate.h"

AudioIOStartStreamOptions
ProjectAudioIO::DefaultOptionsFactory(AudacityProject &project, bool)
{
   auto &projectAudioIO = Get(project);
   AudioIOStartStreamOptions options{
      project.shared_from_this(), ProjectRate::Get(project).GetRate()
   };
   auto &captureMeters = projectAudioIO.GetCaptureMeters();
   options.captureMeters = { captureMeters.begin(), captureMeters.end() };
   auto &playbackMeters = projectAudioIO.GetPlaybackMeters();
   options.playbackMeters = { playbackMeters.begin(), playbackMeters.end() };
   options.envelope =
      Mixer::WarpOptions::DefaultWarp::Call(&project);
   // options.listener remains null
   // boolean argument is ignored
   return options;
}

AudioIOStartStreamOptions ProjectAudioIO::GetDefaultOptions(
   AudacityProject &project, bool newDefaults)
{
   return DefaultOptions::Call(project, newDefaults);
}

static const AudacityProject::AttachedObjects::RegisteredFactory sAudioIOKey{
  []( AudacityProject &parent ){
     return std::make_shared< ProjectAudioIO >( parent );
   }
};

ProjectAudioIO &ProjectAudioIO::Get( AudacityProject &project )
{
   return project.AttachedObjects::Get< ProjectAudioIO >( sAudioIOKey );
}

const ProjectAudioIO &ProjectAudioIO::Get( const AudacityProject &project )
{
   return Get( const_cast<AudacityProject &>(project) );
}

ProjectAudioIO::ProjectAudioIO( AudacityProject &project )
: mProject{ project }
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
   return GetAudioIOToken() > 0 &&
      gAudioIO->IsStreamActive(GetAudioIOToken());
}

auto ProjectAudioIO::GetPlaybackMeters() const -> const Meters &
{
   return mPlaybackMeters;
}

bool ProjectAudioIO::HasPlaybackMeter( Meter *pMeter ) const
{
   auto end = mPlaybackMeters.end(),
   iter = std::find_if( mPlaybackMeters.begin(), end,
      [=]( const MeterPtr &ptr ){ return ptr.get() == pMeter; } );
   return iter != end;
}

void ProjectAudioIO::AddPlaybackMeter( const MeterPtr &playback )
{
   if ( HasPlaybackMeter( playback.get() ) )
      return;

   auto &project = mProject;
   mPlaybackMeters.push_back( playback );
   auto gAudioIO = AudioIOBase::Get();
   if (gAudioIO)
   {
      gAudioIO->SetPlaybackMeters(project.shared_from_this(),
         AudioIO::Get()->GetRate(),
         { mPlaybackMeters.begin(), mPlaybackMeters.end() });
   }
}

void ProjectAudioIO::RemovePlaybackMeter( Meter *playback )
{
   auto begin = mPlaybackMeters.begin(), end = mPlaybackMeters.end(),
      newEnd = std::remove_if( begin, end,
         [=](const MeterPtr &ptr){ return ptr.get() == playback; } );
   if (newEnd != end) {
      mPlaybackMeters.erase( newEnd, end );
      auto gAudioIO = AudioIOBase::Get();
      if (gAudioIO)
      {
         gAudioIO->SetPlaybackMeters( mProject.shared_from_this(),
            AudioIO::Get()->GetRate(),
            { mPlaybackMeters.begin(), mPlaybackMeters.end() } );
      }
   }
}

auto ProjectAudioIO::GetCaptureMeters() const -> const Meters &
{
   return mCaptureMeters;
}

bool ProjectAudioIO::HasCaptureMeter( Meter *pMeter ) const
{
   auto end = mCaptureMeters.end(),
   iter = std::find_if( mCaptureMeters.begin(), end,
      [=]( const MeterPtr &ptr ){ return ptr.get() == pMeter; } );
   return iter != end;
}

void ProjectAudioIO::AddCaptureMeter( const MeterPtr &capture )
{
   if ( HasCaptureMeter( capture.get() ) )
      return ;

   auto &project = mProject;
   mCaptureMeters.push_back( capture );

   auto gAudioIO = AudioIOBase::Get();
   if (gAudioIO)
   {
      gAudioIO->SetCaptureMeters(project.shared_from_this(),
         AudioIO::Get()->GetRate(),
         { mCaptureMeters.begin(), mCaptureMeters.end() });
   }
}

void ProjectAudioIO::RemoveCaptureMeter( Meter *playback )
{
   auto begin = mCaptureMeters.begin(), end = mCaptureMeters.end(),
      newEnd = std::remove_if( begin, end,
         [=](const MeterPtr &ptr){ return ptr.get() == playback; } );
   if (newEnd != end) {
      mCaptureMeters.erase( newEnd, end );
      auto gAudioIO = AudioIOBase::Get();
      if (gAudioIO)
      {
         gAudioIO->SetCaptureMeters(mProject.shared_from_this(),
            AudioIO::Get()->GetRate(),
            { mCaptureMeters.begin(), mCaptureMeters.end() });
      }
   }
}

void ProjectAudioIO::SetPlaySpeed(double value)
{
   if (auto oldValue = GetPlaySpeed(); value != oldValue) {
      mPlaySpeed.store( value, std::memory_order_relaxed );
      Publish({});
   }
}
