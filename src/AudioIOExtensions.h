/**********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioIOExtensions.h

  @brief Extends AudioIOBase with an array of AudioIOExt objects

  Paul Licameli split from AudioIO.h

**********************************************************************/

#ifndef __AUDACITY_AUDIO_IO_EXTENSIONS__
#define __AUDACITY_AUDIO_IO_EXTENSIONS__

#include "AudioIOExt.h"

class AUDACITY_DLL_API AudioIOExtensions /* not final */
   : public AudioIOBase
{
public:
   // This might return null during application startup or shutdown
   static AudioIOExtensions *Get();

   explicit AudioIOExtensions();
   ~AudioIOExtensions();

   void Initialize(PlaybackSchedule &schedule);

   //! @name iteration over extensions, supporting range-for syntax
   //! @{
   class AUDACITY_DLL_API AudioIOExtIterator {
   public:
      using difference_type = ptrdiff_t;
      using value_type = AudioIOExt &;
      using pointer = AudioIOExt *;
      using reference = AudioIOExt &;
      using iterator_category = std::forward_iterator_tag;

      explicit AudioIOExtIterator( AudioIOExtensions &audioIO, bool end )
         : mIterator{ end
            ? audioIO.mAudioIOExt.end()
            : audioIO.mAudioIOExt.begin() }
      {}
      AudioIOExtIterator &operator ++ () { ++mIterator; return *this; }
      auto operator *() const -> AudioIOExt &;
      friend inline bool operator == (
         const AudioIOExtIterator &xx, const AudioIOExtIterator &yy)
      {
         return xx.mIterator == yy.mIterator;
      }
      friend inline bool operator != (
         const AudioIOExtIterator &xx, const AudioIOExtIterator &yy)
      {
         return !(xx == yy);
      }
   private:
      std::vector<std::unique_ptr<AudioIOExtBase>>::const_iterator mIterator;
   };
   struct AudioIOExtRange {
      AudioIOExtIterator first;
      AudioIOExtIterator second;
      AudioIOExtIterator begin() const { return first; }
      AudioIOExtIterator end() const { return second; }
   };

   AudioIOExtRange Extensions() {
      return {
         AudioIOExtIterator{ *this, false },
         AudioIOExtIterator{ *this, true }
      };
   }
   //! @}

private:
   /*!
    Privatize the inherited array but give access by Extensions().
    This class guarantees that this array is populated only with non-null
    pointers to the subtype AudioIOExt
    */
   using AudioIOBase::mAudioIOExt;
};

#endif
