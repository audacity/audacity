/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file ChannelAttachments.h
  @brief Adapts TrackAttachment interface with extra channel index argument

  Dominic Mazzoni

  Paul Licameli split from Track.h

**********************************************************************/
#ifndef __AUDACITY_CHANNEL_ATTACHMENTS__
#define __AUDACITY_CHANNEL_ATTACHMENTS__

#include "Track.h"

//! Like the TrackAttachment interface, but member functions take an extra
//! argument choosing the channel
class TRACK_API ChannelAttachment
{
public:
    virtual ~ChannelAttachment();

    //! Copy state, for undo/redo purposes
    /*!
     @param iChannel position of the attachment's channel in the group
     The default does nothing
    */
    virtual void CopyTo(Track& track, size_t iChannel) const;

    //! Object may be shared among tracks but hold a special back-pointer to one of them; reassign it
    /*!
     @param iChannel position of the attachment's channel in the group
     The default does nothing
     */
    virtual void Reparent(const std::shared_ptr<Track>& parent, size_t iChannel);

    //! Serialize persistent attributes
    /*! default does nothing */
    virtual void WriteXMLAttributes(XMLWriter& writer, size_t iChannel) const;

    //! Deserialize an attribute, returning true if recognized
    /*! default recognizes no attributes, and returns false */
    virtual bool HandleXMLAttribute(
        const std::string_view& attr, const XMLAttributeValueView& valueView, size_t iChannel);
};

//! Holds multiple objects as a single attachment to Track
class TRACK_API ChannelAttachmentsBase : public TrackAttachment
{
public:
    using Factory
        =std::function<std::shared_ptr<ChannelAttachment>(Track&, size_t)>;

    ChannelAttachmentsBase(Track& track, Factory factory);
    ~ChannelAttachmentsBase() override;

    // No copying
    ChannelAttachmentsBase(const ChannelAttachmentsBase&) = delete;
    ChannelAttachmentsBase& operator=(const ChannelAttachmentsBase&) = delete;

    // No move construction
    ChannelAttachmentsBase(ChannelAttachmentsBase&) = delete;

    // Move assignment is allowed
    /*!
     @pre `typeid(*this) == typeid(other)`
     @pre same factories (can't express that in code)
     */
    ChannelAttachmentsBase& operator=(ChannelAttachmentsBase&& other);

    size_t Size() const { return mAttachments.size(); }

    // Override all the TrackAttachment virtuals and pass through to each
    void CopyTo(Track& track) const override;
    void Reparent(const std::shared_ptr<Track>& parent) override;
    void WriteXMLAttributes(XMLWriter& writer) const override;
    bool HandleXMLAttribute(
        const std::string_view& attr, const XMLAttributeValueView& valueView)
    override;

    //! Append the other's attachments to this, assuming concrete subclasses are
    //! the same
    /*!
     Calls Reparent, so it needs a track
     @pre `typeid(*this) == typeid(other)`
     @pre `Size() <= 1`
     @pre `other.Size() <= 1`
     */
    void MakeStereo(const std::shared_ptr<Track>& parent, ChannelAttachmentsBase&& other);

    /*!
     Calls Reparent, so it needs a track
     @pre `Size() <= 2`
     */
    void SwapChannels(const std::shared_ptr<Track>& parent);

    //! Erase attachment at a given index, if it existed, moving later-indexed
    //! attachments to earlier indices
    /*!
     Calls Reparent, so it needs a track
     */
    void Erase(const std::shared_ptr<Track>& parent, size_t index);

protected:
    /*!
     @pre `iChannel < track.NChannels()`
     */
    static ChannelAttachment& Get(
        const AttachedTrackObjects::RegisteredFactory& key, Track& track, size_t iChannel);
    /*!
     @pre `!pTrack || iChannel < pTrack->NChannels()`
     */
    static ChannelAttachment* Find(
        const AttachedTrackObjects::RegisteredFactory& key, Track* pTrack, size_t iChannel);

private:
    const Factory mFactory;
    std::vector<std::shared_ptr<ChannelAttachment> > mAttachments;
};

//! Holds multiple objects of the parameter type as a single attachment to Track
template<typename Attachment>
class ChannelAttachments : public ChannelAttachmentsBase
{
    static_assert(std::is_base_of_v<ChannelAttachment, Attachment>);
public:
    ~ChannelAttachments() override = default;

    /*!
     @pre `iChannel < track.NChannels()`
     */
    static Attachment& Get(
        const AttachedTrackObjects::RegisteredFactory& key,
        Track& track, size_t iChannel)
    {
        return static_cast<Attachment&>(
            ChannelAttachmentsBase::Get(key, track, iChannel));
    }

    /*!
     @pre `!pTrack || iChannel < pTrack->NChannels()`
     */
    static Attachment* Find(
        const AttachedTrackObjects::RegisteredFactory& key,
        Track* pTrack, size_t iChannel)
    {
        return static_cast<Attachment*>(
            ChannelAttachmentsBase::Find(key, pTrack, iChannel));
    }

    //! Type-erasing constructor
    /*!
     @tparam F returns a shared pointer to Attachment (or some subtype of it)

     @pre `f` never returns null

     `f` may assume the precondition that the given channel index is less than
     the given track's number of channels
     */
    template<typename F,
             typename sfinae = std::enable_if_t<std::is_convertible_v<
                                                    std::invoke_result_t<F, Track&, size_t>, std::shared_ptr<Attachment>
                                                    > >
             >
    explicit ChannelAttachments(Track& track, F&& f)
        : ChannelAttachmentsBase{track, std::forward<F>(f)}
    {}
};

#endif
