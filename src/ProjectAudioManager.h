/**********************************************************************

Audacity: A Digital Audio Editor

ProjectAudioManager.h

Paul Licameli split from ProjectManager.h

**********************************************************************/

#ifndef __AUDACITY_PROJECT_AUDIO_MANAGER__
#define __AUDACITY_PROJECT_AUDIO_MANAGER__

#include "AudioIOListener.h" // to inherit
#include "ClientData.h" // to inherit

class AudacityProject;
struct AudioIOStartStreamOptions;

class ProjectAudioManager final
   : public ClientData::Base
   , public AudioIOListener
{
public:
   static ProjectAudioManager &Get( AudacityProject &project );
   static const ProjectAudioManager &Get( const AudacityProject &project );

   explicit ProjectAudioManager( AudacityProject &project )
      : mProject{ project }
   {}
   ~ProjectAudioManager() override;

   bool IsTimerRecordCancelled() { return mTimerRecordCanceled; }
   void SetTimerRecordCancelled() { mTimerRecordCanceled = true; }
   void ResetTimerRecordCancelled() { mTimerRecordCanceled = false; }

private:
   // Audio IO callback methods
   void OnAudioIORate(int rate) override;
   void OnAudioIOStartRecording() override;
   void OnAudioIOStopRecording() override;
   void OnAudioIONewBlockFiles(const AutoSaveFile & blockFileLog) override;

   AudacityProject &mProject;

   //flag for cancellation of timer record.
   bool mTimerRecordCanceled{ false };
};

AudioIOStartStreamOptions DefaultPlayOptions( AudacityProject &project );
AudioIOStartStreamOptions DefaultSpeedPlayOptions( AudacityProject &project );

#endif
