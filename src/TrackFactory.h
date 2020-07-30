/**********************************************************************

Audacity: A Digital Audio Editor

TrackFactory.h

Paul Licameli -- split from Track.h

**********************************************************************/

#ifndef __AUDACITY_TRACK_FACTORY__
#define __AUDACITY_TRACK_FACTORY__

#include "ClientData.h" // to inherit

class AudacityProject;

class AUDACITY_DLL_API TrackFactory final
   : public ClientData::Base
{
 public:
   static TrackFactory &Get( AudacityProject &project );
   static const TrackFactory &Get( const AudacityProject &project );
   static TrackFactory &Reset( AudacityProject &project );
   static void Destroy( AudacityProject &project );

   TrackFactory( const ProjectSettings &settings,
      AudacityProject &project, const ZoomInfo *zoomInfo)
      : mSettings{ settings }
      , mProject(project)
      , mZoomInfo(zoomInfo)
   {
   }
   TrackFactory( const TrackFactory & ) PROHIBITED;
   TrackFactory &operator=( const TrackFactory & ) PROHIBITED;

 private:
   const ProjectSettings &mSettings;
   AudacityProject &mProject;
   const ZoomInfo *const mZoomInfo;
   friend class AudacityProject;
 public:
   // These methods are defined in WaveTrack.cpp, NoteTrack.cpp,
   // LabelTrack.cpp, and TimeTrack.cpp respectively
   std::shared_ptr<WaveTrack> DuplicateWaveTrack(const WaveTrack &orig);
   std::shared_ptr<WaveTrack> NewWaveTrack(sampleFormat format = (sampleFormat)0,
                           double rate = 0);
   std::shared_ptr<LabelTrack> NewLabelTrack();
   std::shared_ptr<TimeTrack> NewTimeTrack();
#if defined(USE_MIDI)
   std::shared_ptr<NoteTrack> NewNoteTrack();
#endif
};

#endif

