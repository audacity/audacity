/**********************************************************************

Audacity: A Digital Audio Editor

UIHandle.cpp

Paul Licameli

**********************************************************************/
#include "UIHandle.h"
#include "Channel.h"
#include "RefreshCode.h"
#include "Track.h"

UIHandle::~UIHandle()
{
}

void UIHandle::Enter(bool, AudacityProject*)
{
}

bool UIHandle::HasRotation() const
{
    return false;
}

bool UIHandle::Rotate(bool)
{
    return false;
}

bool UIHandle::HasEscape(AudacityProject*) const
{
    return false;
}

bool UIHandle::Escape(AudacityProject*)
{
    return false;
}

bool UIHandle::HandlesRightClick()
{
    return false;
}

bool UIHandle::StopsOnKeystroke()
{
    return false;
}

void UIHandle::OnProjectChange(AudacityProject*)
{
}

bool UIHandle::IsDragging() const
{
    return false;
}

std::shared_ptr<const Track>
UIHandle::TrackFromChannel(const std::shared_ptr<const Channel>& pChannel)
{
    return pChannel
           ? static_cast<const Track&>(pChannel->GetChannelGroup())
           .shared_from_this()
           : nullptr;
}
