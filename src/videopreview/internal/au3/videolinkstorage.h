/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "au3-registries/ClientData.h"
#include "au3-xml/XMLTagHandler.h"
#include "global/async/notification.h"

#include "../../videopreviewtypes.h"

class AudacityProject;
class XMLWriter;

namespace au::videopreview {

class VideoLinkStorage final : public ClientData::Base, public XMLTagHandler
{
public:
    static VideoLinkStorage& Get(AudacityProject& project);
    static const VideoLinkStorage& Get(const AudacityProject& project);

    const VideoLinks& links() const;
    void setLinks(VideoLinks links);
    void clear();

    muse::async::Notification linkChanged() const;

    void WriteXML(XMLWriter& xmlFile) const;

    bool HandleXMLTag(const std::string_view& tag, const AttributesList& attrs) override;
    XMLTagHandler* HandleXMLChild(const std::string_view& tag) override;

private:
    VideoLinks m_links;
    int m_currentReadLink = -1;
    muse::async::Notification m_linkChanged;
};

void modifyVideoLinkUndoState(AudacityProject& project);
}
