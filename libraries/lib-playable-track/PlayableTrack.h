/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file PlayableTrack.h
  @brief Extends Track with notions of mute and solo setting

  Dominic Mazzoni

  Paul Licameli split from Track.h

**********************************************************************/
#ifndef __AUDACITY_PLAYABLE_TRACK__
#define __AUDACITY_PLAYABLE_TRACK__

#include "Track.h"

//! Track subclass holding data representing sound (as notes, or samples, or ...)
class PLAYABLE_TRACK_API AudioTrack /* not final */ : public Track
{
public:
   AudioTrack();
   AudioTrack(const Track &orig, ProtectedCreationArg &&a);

   static const TypeInfo &ClassTypeInfo();

   // Serialize, not with tags of its own, but as attributes within a tag.
   void WriteXMLAttributes(XMLWriter &WXUNUSED(xmlFile)) const {}

   // Return true iff the attribute is recognized.
   bool HandleXMLAttribute(const std::string_view & /*attr*/, const XMLAttributeValueView &/*value*/)
   { return false; }
};

ENUMERATE_TRACK_TYPE(AudioTrack);

//! AudioTrack subclass that can also be audibly replayed by the program
class PLAYABLE_TRACK_API PlayableTrack /* not final */ : public AudioTrack
{
public:
   PlayableTrack();
   PlayableTrack(const PlayableTrack &orig, ProtectedCreationArg&&);

   static const TypeInfo &ClassTypeInfo();

   bool GetMute    () const { return DoGetMute();     }
   bool GetSolo    () const { return DoGetSolo();     }
   bool GetNotMute () const { return !DoGetMute();     }
   bool GetNotSolo () const { return !DoGetSolo();     }
   void SetMute    (bool m);
   void SetSolo    (bool s);

   void Init( const PlayableTrack &init );

   // Called when this track is merged to stereo with another, and should
   // take on some parameters of its partner.
   virtual void Merge( const Track &init );

   // Serialize, not with tags of its own, but as attributes within a tag.
   void WriteXMLAttributes(XMLWriter &xmlFile) const;

   // Return true iff the attribute is recognized.
   bool HandleXMLAttribute(const std::string_view &attr, const XMLAttributeValueView &value);

protected:
   // These just abbreviate load and store with relaxed memory ordering
   bool DoGetMute() const;
   void DoSetMute(bool value);
   bool DoGetSolo() const;
   void DoSetSolo(bool value);

   //! Atomic because it may be read by worker threads in playback
   std::atomic<bool>  mMute { false };
   //! Atomic because it may be read by worker threads in playback
   std::atomic<bool>  mSolo { false };
};

ENUMERATE_TRACK_TYPE(PlayableTrack);
#endif
