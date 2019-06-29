/**********************************************************************

Audacity: A Digital Audio Editor

ProjectAudioIO.h

Paul Licameli split from AudacityProject.h

**********************************************************************/

#ifndef __PROJECT_AUDIO_IO__
#define __PROJECT_AUDIO_IO__

#include "ClientData.h" // to inherit
#include "GlobalVariable.h"
#include "Observer.h" // to inherit
#include <wx/weakref.h>

#include <atomic>
#include <memory>
#include <vector>
class AudacityProject;
struct AudioIOStartStreamOptions;
class Meter;

struct SpeedChangeMessage {};

///\ brief Holds per-project state needed for interaction with AudioIO,
/// including the audio stream token and pointers to meters
class AUDIO_IO_API ProjectAudioIO final
   : public ClientData::Base
   , public Observer::Publisher<SpeedChangeMessage>
{
public:
   //! Default factory function ignores the second argument
   static AudioIOStartStreamOptions
   DefaultOptionsFactory(AudacityProject &project, bool newDefaults);

   //! Global hook making AudioIOStartStreamOptions for a project, which
   //! has a non-trivial default implementation
   struct AUDIO_IO_API DefaultOptions : DefaultedGlobalHook<DefaultOptions,
      DefaultOptionsFactory
   >{};

   //! Invoke the global hook, supplying a default argument
   static AudioIOStartStreamOptions GetDefaultOptions(
      AudacityProject &project,
      bool newDefaults = false /*!< if true, policy is meant to respond to
         looping region; but specifying that is outside this library's scope
      */
   );

   static ProjectAudioIO &Get( AudacityProject &project );
   static const ProjectAudioIO &Get( const AudacityProject &project );

   explicit ProjectAudioIO( AudacityProject &project );
   ProjectAudioIO( const ProjectAudioIO & ) = delete;
   ProjectAudioIO &operator=( const ProjectAudioIO & ) = delete;
   ~ProjectAudioIO();

   int GetAudioIOToken() const;
   bool IsAudioActive() const;
   void SetAudioIOToken(int token);

   using MeterPtr = std::shared_ptr< Meter >;
   using Meters = std::vector< MeterPtr >;

   const Meters &GetPlaybackMeters() const;
   bool HasPlaybackMeter( Meter *pMeter ) const;
   void AddPlaybackMeter(const MeterPtr &playback);
   void RemovePlaybackMeter(Meter *playback);

   const Meters &GetCaptureMeters() const;
   bool HasCaptureMeter( Meter *pMeter ) const;
   void AddCaptureMeter(const MeterPtr &capture);
   void RemoveCaptureMeter(Meter *capture);

   // Speed play
   double GetPlaySpeed() const {
      return mPlaySpeed.load( std::memory_order_relaxed ); }
   void SetPlaySpeed( double value );

private:
   AudacityProject &mProject;

   // Project owned meters
   Meters mPlaybackMeters;
   Meters mCaptureMeters;

   // This is atomic because scrubber may read it in a separate thread from
   // the main
   std::atomic<double> mPlaySpeed{};

   int  mAudioIOToken{ -1 };
};

#endif
