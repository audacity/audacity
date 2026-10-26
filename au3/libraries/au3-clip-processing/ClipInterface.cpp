#include "ClipInterface.h"

ClipTimes::~ClipTimes() = default;

void ClipInterfaceAttachment::Reparent(ClipInterface&)
{
}

void ClipInterfaceAttachment::WriteXMLAttributes(XMLWriter&) const
{
}

bool ClipInterfaceAttachment::HandleXMLAttribute(
    const std::string_view&, const XMLAttributeValueView&)
{
    return false;
}

void ClipInterfaceAttachment::OnSplit(ClipSide)
{
}

ClipInterface::~ClipInterface() = default;

void ClipInterface::OnSplit(ClipSide splitSide)
{
    ForEach([splitSide](ClipInterfaceAttachment& attachment) {
        attachment.OnSplit(splitSide);
    });
}
