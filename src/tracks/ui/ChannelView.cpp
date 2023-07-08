/**********************************************************************

Audacity: A Digital Audio Editor

ChannelView.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "ChannelView.h"
#include "Track.h"

#include "ClientData.h"
#include "Project.h"
#include "XMLTagHandler.h"
#include "XMLWriter.h"

#include <sstream>

ChannelView::ChannelView(const std::shared_ptr<Track> &pTrack, size_t iChannel)
   : CommonTrackCell{ pTrack, iChannel }
   , vrulerSize{ 36, 0 }
{
   DoSetHeight( GetDefaultTrackHeight::Call( *pTrack ) );
}

ChannelView::~ChannelView()
{
}

int ChannelView::GetChannelGroupHeight(const Track *pTrack)
{
   const auto GetTrackHeight = [](const Track *pTrack) -> int {
      return pTrack ? GetFromChannelGroup(*pTrack).GetHeight() : 0;
   };
   return pTrack ? TrackList::Channels(pTrack).sum(GetTrackHeight) : 0;
}

int ChannelView::GetCumulativeHeight(const Channel *pChannel)
{
   if (!pChannel)
      return 0;
   auto &view = ChannelView::Get(*pChannel);
   return view.GetCumulativeHeightBefore() + view.GetHeight();
}

int ChannelView::GetCumulativeHeight(const Track *pTrack)
{
   if (!pTrack)
      return 0;
   return GetCumulativeHeight((*pTrack->Channels().rbegin()).get());
}

int ChannelView::GetTotalHeight(const TrackList &list)
{
   return GetCumulativeHeight(*list.rbegin());
}

void ChannelView::CopyTo(Track &track) const
{
   auto &other = GetFromChannelGroup(track);

   other.mMinimized = mMinimized;
   other.vrulerSize = vrulerSize;

   // Let mY remain 0 -- TrackPositioner corrects it later
   other.mY = 0;
   other.mHeight = mHeight;
}

using ChannelViewAttachments = ChannelAttachments<ChannelView>;

static const AttachedTrackObjects::RegisteredFactory keyC{
   [](Track &track){
      return std::make_shared<ChannelViewAttachments>(track,
         [](Track &track, size_t iChannel) {
            assert(iChannel < track.NChannels());
            return DoGetView::Call(track, iChannel);
         }
      );
   }
};

ChannelView &ChannelView::GetFromChannelGroup(
   ChannelGroup &group, size_t iChannel)
{
   auto &track = static_cast<Track&>(group);
   return ChannelViewAttachments::Get(keyC, track, iChannel);
}

const ChannelView &ChannelView::GetFromChannelGroup(
   const ChannelGroup &group, size_t iChannel)
{
   return GetFromChannelGroup(const_cast<ChannelGroup &>(group), iChannel);
}

ChannelView *ChannelView::FindFromChannelGroup(
   ChannelGroup *pGroup, size_t iChannel)
{
   return ChannelViewAttachments::Find(
      keyC, static_cast<Track*>(pGroup), iChannel);
}

void ChannelView::SetMinimized(bool isMinimized)
{
   // Do special changes appropriate to subclass
   DoSetMinimized(isMinimized);

   // Update positions and heights starting from the first track in the group
   auto leader = *TrackList::Channels( FindTrack().get() ).begin();
   if ( leader )
      leader->AdjustPositions();
}

namespace {
// Append a channel number to a base attribute name unless it is 0
std::string AttributeName(const ChannelView &view, std::string name) {
   const auto index = view.GetChannelIndex();
   if (index == 0)
      return move(name);
   std::stringstream stream{ name };
   stream << index;
   return stream.str();
}
std::string HeightAttributeName(const ChannelView &view) {
   return AttributeName(view, "height");
}
std::string MinimizedAttributeName(const ChannelView &view) {
   return AttributeName(view, "minimized");
}
}

void ChannelView::WriteXMLAttributes(XMLWriter &xmlFile) const
{
   xmlFile.WriteAttr(HeightAttributeName(*this), GetExpandedHeight());
   xmlFile.WriteAttr(MinimizedAttributeName(*this), GetMinimized());
}

bool ChannelView::HandleXMLAttribute(
   const std::string_view& attr, const XMLAttributeValueView& valueView)
{
   long nValue;

   if (attr == HeightAttributeName(*this) && valueView.TryGet(nValue)) {
      // Bug 2803: Extreme values for track height (caused by integer overflow)
      // will stall Audacity as it tries to create an enormous vertical ruler.
      // So clamp to reasonable values.
      nValue = std::max( 40l, std::min( nValue, 1000l ));
      SetExpandedHeight(nValue);
      return true;
   }
   else if (attr == MinimizedAttributeName(*this) && valueView.TryGet(nValue)) {
      SetMinimized(nValue != 0);
      return true;
   }
   else
      return false;
}

auto ChannelView::GetSubViews(const wxRect &rect) -> Refinement
{
   return { { rect.GetTop(), shared_from_this() } };
}

bool ChannelView::IsSpectral() const
{
   return false;
}

void ChannelView::DoSetMinimized(bool isMinimized)
{
   mMinimized = isMinimized;
}

std::shared_ptr<ChannelVRulerControls> ChannelView::GetVRulerControls()
{
   if (!mpVRulerControls)
      // create on demand
      mpVRulerControls = DoGetVRulerControls();
   return mpVRulerControls;
}

std::shared_ptr<const ChannelVRulerControls>
ChannelView::GetVRulerControls() const
{
   return const_cast<ChannelView*>(this)->GetVRulerControls();
}

void ChannelView::DoSetY(int y)
{
   mY = y;
}

int ChannelView::GetHeight() const
{
   if ( GetMinimized() )
      return GetMinimizedHeight();

   return mHeight;
}

void ChannelView::SetExpandedHeight(int h)
{
   DoSetHeight(h);
   FindTrack()->AdjustPositions();
}

void ChannelView::DoSetHeight(int h)
{
   mHeight = h;
}

std::shared_ptr<CommonTrackCell> ChannelView::GetAffordanceControls()
{
   return {};
}

ChannelView &ChannelView::Get(Channel &channel)
{
   return GetFromChannelGroup(channel.GetChannelGroup(),
      channel.GetChannelIndex());
}

const ChannelView &ChannelView::Get(const Channel &channel)
{
   return Get(const_cast<Channel&>(channel));
}

ChannelView *ChannelView::Find(Channel *pChannel)
{
   if (!pChannel)
      return nullptr;
   return FindFromChannelGroup(
      &pChannel->GetChannelGroup(), pChannel->GetChannelIndex());
}

const ChannelView *ChannelView::Find(const Channel *pChannel)
{
   return Find(const_cast<Channel*>(pChannel));
}

namespace {

/*!
 Attached to each project, it receives track list events and maintains the
 cache of cumulative track view heights for use by TrackPanel.
 */
struct TrackPositioner final : ClientData::Base
{
   AudacityProject &mProject;

   explicit TrackPositioner( AudacityProject &project )
      : mProject{ project }
   {
      mSubscription = TrackList::Get( project )
         .Subscribe(*this, &TrackPositioner::OnUpdate);
   }
   TrackPositioner( const TrackPositioner & ) = delete;
   TrackPositioner &operator=( const TrackPositioner & ) = delete;

   void OnUpdate(const TrackListEvent & e)
   {
      switch (e.mType) {
      case TrackListEvent::ADDITION:
      case TrackListEvent::DELETION:
      case TrackListEvent::PERMUTED:
      case TrackListEvent::RESIZING:
         break;
      default:
         return;
      }
      auto iter =
         TrackList::Get(mProject).Find(e.mpTrack.lock().get());
      if (!*iter)
         return;

      auto prev = iter;
      auto yy = ChannelView::GetCumulativeHeight(*--prev);

      while (auto pTrack = *iter) {
         for (auto pChannel : (*iter)->Channels()) {
            auto &view = ChannelView::Get(*pChannel);
            view.SetCumulativeHeightBefore(yy);
            yy += view.GetHeight();
         }
         ++iter;
      }
   }

   Observer::Subscription mSubscription;
};

static const AudacityProject::AttachedObjects::RegisteredFactory key{
  []( AudacityProject &project ){
     return std::make_shared< TrackPositioner >( project );
   }
};

}

DEFINE_ATTACHED_VIRTUAL(DoGetView) {
   return nullptr;
}

DEFINE_ATTACHED_VIRTUAL(GetDefaultTrackHeight) {
   return nullptr;
}
