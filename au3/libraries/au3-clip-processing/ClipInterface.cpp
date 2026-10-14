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

ClipInterface::~ClipInterface() = default;
