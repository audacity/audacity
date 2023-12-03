/**********************************************************************

  Audacity: A Digital Audio Editor

  @file ChannelAttachments.cpp

  Dominic Mazzoni

  Paul Licameli split from Track.cpp

*//*******************************************************************/
#include "ChannelAttachments.h"

TrackAttachment &ChannelAttachmentsBase::Get(
   const AttachedTrackObjects::RegisteredFactory &key,
   Track &track, size_t iChannel)
{
   // Precondition of this function; satisfies precondition of factory below
   assert(iChannel < track.NChannels());
   auto &attachments = track.AttachedObjects::Get<ChannelAttachmentsBase>(key);
   auto &objects = attachments.mAttachments;
   if (iChannel >= objects.size())
      objects.resize(iChannel + 1);
   auto &pObject = objects[iChannel];
   if (!pObject) {
      // Create on demand
      pObject = attachments.mFactory(track, iChannel);
      assert(pObject); // Precondition of constructor
   }
   return *pObject;
}

TrackAttachment *ChannelAttachmentsBase::Find(
   const AttachedTrackObjects::RegisteredFactory &key,
   Track *pTrack, size_t iChannel)
{
   assert(!pTrack || iChannel < pTrack->NChannels());
   if (!pTrack)
      return nullptr;
   const auto pAttachments =
      pTrack->AttachedObjects::Find<ChannelAttachmentsBase>(key);
   // do not create on demand
   if (!pAttachments || iChannel >= pAttachments->mAttachments.size())
      return nullptr;
   return pAttachments->mAttachments[iChannel].get();
}

ChannelAttachmentsBase::ChannelAttachmentsBase(Track &track, Factory factory)
   : mFactory{ move(factory) }
{
   // Always construct one channel view
   // TODO wide wave tracks -- number of channels will be known earlier, and
   // they will all be constructed
   mAttachments.push_back(mFactory(track, 0));
}

ChannelAttachmentsBase::~ChannelAttachmentsBase() = default;

void ChannelAttachmentsBase::CopyTo(Track &track) const
{
   for (auto &pAttachment : mAttachments)
      if (pAttachment)
         pAttachment->CopyTo(track);
}

void ChannelAttachmentsBase::Reparent(const std::shared_ptr<Track> &parent)
{
   for (auto &pAttachment : mAttachments)
      if (pAttachment)
         pAttachment->Reparent(parent);
}

void ChannelAttachmentsBase::WriteXMLAttributes(XMLWriter &writer) const
{
   for (auto &pAttachment : mAttachments)
      if (pAttachment)
         pAttachment->WriteXMLAttributes(writer);
}

bool ChannelAttachmentsBase::HandleXMLAttribute(
   const std::string_view& attr, const XMLAttributeValueView& valueView)
{
   return std::any_of(mAttachments.begin(), mAttachments.end(),
   [&](auto &pAttachment) {
      return pAttachment && pAttachment->HandleXMLAttribute(attr, valueView);
   });
}
