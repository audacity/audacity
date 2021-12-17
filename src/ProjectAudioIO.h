/**********************************************************************

Audacity: A Digital Audio Editor

ProjectAudioIO.h

Paul Licameli split from AudacityProject.h

**********************************************************************/

#ifndef __PROJECT_AUDIO_IO__
#define __PROJECT_AUDIO_IO__

#include "ClientData.h" // to inherit
#include <wx/weakref.h>
#include <wx/event.h> // to declare custom event type

#include <atomic>
#include <memory>
class AudacityProject;
class Meter;

// Sent to the project when the play speed changes
wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API,
   EVT_PLAY_SPEED_CHANGE, wxCommandEvent);

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

   // Speed play
   double GetPlaySpeed() const {
      return mPlaySpeed.load( std::memory_order_relaxed ); }
   void SetPlaySpeed( double value );

private:
   AudacityProject &mProject;

   // Project owned meters
   std::shared_ptr<Meter> mPlaybackMeter;
   std::shared_ptr<Meter> mCaptureMeter;

   // This is atomic because scrubber may read it in a separate thread from
   // the main
   std::atomic<double> mPlaySpeed{};

   int  mAudioIOToken{ -1 };
};

#endif
