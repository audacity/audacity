/*!********************************************************************

  Audacity: A Digital Audio Editor

  PlayableTrack.cpp

  Dominic Mazzoni

  Paul Licameli split from Track.cpp

*******************************************************************//**

\class AudioTrack
\brief A Track that can load/save audio data to/from XML.

\class PlayableTrack
\brief An AudioTrack that can be played and stopped.

*//*******************************************************************/
#include "PlayableTrack.h"

AudioTrack::AudioTrack() : Track{}
{
}

AudioTrack::AudioTrack(const Track &orig, ProtectedCreationArg &&a)
   : Track{ orig, std::move(a) }
{
}

PlayableTrack::PlayableTrack() : AudioTrack{}
{
}

PlayableTrack::PlayableTrack(
   const PlayableTrack &orig, ProtectedCreationArg &&a
)  : AudioTrack{ orig, std::move(a) }
{
}

void PlayableTrack::Init( const PlayableTrack &orig )
{
   DoSetMute(orig.DoGetMute());
   DoSetSolo(orig.DoGetSolo());
   AudioTrack::Init( orig );
}

void PlayableTrack::Merge( const Track &orig )
{
   auto pOrig = dynamic_cast<const PlayableTrack *>(&orig);
   wxASSERT( pOrig );
   DoSetMute(pOrig->DoGetMute());
   DoSetSolo(pOrig->DoGetSolo());
}

void PlayableTrack::SetMute( bool m )
{
   if ( DoGetMute() != m ) {
      DoSetMute(m);
      Notify(false);
   }
}

void PlayableTrack::SetSolo( bool s  )
{
   if ( DoGetSolo() != s ) {
      DoSetSolo(s);
      Notify(false);
   }
}

bool PlayableTrack::DoGetMute() const
{
   return mMute.load(std::memory_order_relaxed);
}

void PlayableTrack::DoSetMute(bool value)
{
   mMute.store(value, std::memory_order_relaxed);
}

bool PlayableTrack::DoGetSolo() const
{
   return mSolo.load(std::memory_order_relaxed);
}

void PlayableTrack::DoSetSolo(bool value)
{
   mSolo.store(value, std::memory_order_relaxed);
}

// Serialize, not with tags of its own, but as attributes within a tag.
void PlayableTrack::WriteXMLAttributes(XMLWriter &xmlFile) const
{
   xmlFile.WriteAttr(wxT("mute"), DoGetMute());
   xmlFile.WriteAttr(wxT("solo"), DoGetSolo());
   AudioTrack::WriteXMLAttributes(xmlFile);
}

// Return true iff the attribute is recognized.
bool PlayableTrack::HandleXMLAttribute(const std::string_view &attr, const XMLAttributeValueView &value)
{
   long nValue;

   if (attr == "mute" && value.TryGet(nValue)) {
      DoSetMute(nValue != 0);
      return true;
   }
   else if (attr == "solo" && value.TryGet(nValue)) {
      DoSetSolo(nValue != 0);
      return true;
   }

   return AudioTrack::HandleXMLAttribute(attr, value);
}

auto AudioTrack::ClassTypeInfo() -> const TypeInfo &
{
   static Track::TypeInfo info{
      { "audio", "audio", XO("Audio Track") },
      false, &Track::ClassTypeInfo() };
   return info;
}

auto PlayableTrack::ClassTypeInfo() -> const TypeInfo &
{
   static Track::TypeInfo info{
      { "playable", "playable", XO("Playable Track") },
      false, &AudioTrack::ClassTypeInfo() };
   return info;
}

EnumSetting<SoloBehavior> TracksBehaviorsSolo{
   wxT("/GUI/Solo"),
   {
      ByColumns,
      { XO("Simple"),  XO("Multi-track"), XO("None") },
      { wxT("Simple"), wxT("Multi"),      wxT("None") }
   },
   0, // "Simple"
   { SoloBehaviorSimple, SoloBehaviorMulti, SoloBehaviorNone },
};
