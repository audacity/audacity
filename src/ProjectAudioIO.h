/**********************************************************************

Audacity: A Digital Audio Editor

ProjectAudioIO.h

Paul Licameli split from AudacityProject.h

**********************************************************************/

#ifndef __PROJECT_AUDIO_IO__
#define __PROJECT_AUDIO_IO__

#include "ClientData.h" // to inherit
#include <wx/weakref.h>

#include <memory>
class AudacityProject;
class Meter;

///\ brief Holds per-project state needed for interaction with AudioIO,
/// including the audio stream token and pointers to meters
class AUDACITY_DLL_API ProjectAudioIO final
   : public ClientData::Base
{
public:
   static ProjectAudioIO &Get( AudacityProject &project );
   static const ProjectAudioIO &Get( const AudacityProject &project );

   explicit ProjectAudioIO( AudacityProject &project );
   ProjectAudioIO( const ProjectAudioIO & ) PROHIBITED;
   ProjectAudioIO &operator=( const ProjectAudioIO & ) PROHIBITED;
   ~ProjectAudioIO();

   int GetAudioIOToken() const;
   bool IsAudioActive() const;
   void SetAudioIOToken(int token);

   const std::shared_ptr<Meter> &GetPlaybackMeter() const;
   void SetPlaybackMeter(
      const std::shared_ptr<Meter> &playback);
   const std::shared_ptr<Meter> &GetCaptureMeter() const;
   void SetCaptureMeter(
      const std::shared_ptr<Meter> &capture);

private:
   AudacityProject &mProject;

   // Project owned meters
   std::shared_ptr<Meter> mPlaybackMeter;
   std::shared_ptr<Meter> mCaptureMeter;

   int  mAudioIOToken{ -1 };
};

#endif
