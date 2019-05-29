/**********************************************************************

Audacity: A Digital Audio Editor

ProjectAudioIO.h

Paul Licameli split from AudacityProject.h

**********************************************************************/

#ifndef __PROJECT_AUDIO_IO__
#define __PROJECT_AUDIO_IO__

#include "ClientData.h" // to inherit

class AudacityProject;
class MeterPanel;

///\ brief Holds per-project state needed for interaction with AudioIO,
/// including the audio stream token and pointers to meters
class ProjectAudioIO final
   : public ClientData::Base
{
public:
   static ProjectAudioIO &Get( AudacityProject &project );
   static const ProjectAudioIO &Get( const AudacityProject &project );

   explicit ProjectAudioIO( AudacityProject &project );
   ~ProjectAudioIO();

   int GetAudioIOToken() const;
   bool IsAudioActive() const;
   void SetAudioIOToken(int token);

   MeterPanel *GetPlaybackMeter();
   void SetPlaybackMeter(MeterPanel *playback);
   MeterPanel *GetCaptureMeter();
   void SetCaptureMeter(MeterPanel *capture);

private:
   AudacityProject &mProject;

   // Project owned meters
   MeterPanel *mPlaybackMeter{};
   MeterPanel *mCaptureMeter{};

   int  mAudioIOToken{ -1 };
};

#endif
