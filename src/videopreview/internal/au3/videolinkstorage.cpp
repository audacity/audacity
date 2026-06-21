/*
* Audacity: A Digital Audio Editor
*/
#include "videolinkstorage.h"

#include <typeindex>
#include <utility>

#include <wx/string.h>

#include "au3-project/Project.h"
#include "au3-project-file-io/ProjectFileIO.h"
#include "au3-project-history/UndoManager.h"
#include "au3-xml/XMLAttributeValueView.h"
#include "au3-xml/XMLWriter.h"

using namespace au::videopreview;

namespace {
constexpr std::string_view VIDEO_PREVIEW_TAG = "video_preview";
constexpr std::string_view SEGMENT_TAG = "segment";

const XMLAttributeValueView* attr(const AttributesList& attrs, std::string_view name)
{
    for (const auto& [attrName, value] : attrs) {
        if (attrName == name) {
            return &value;
        }
    }

    return nullptr;
}

std::string attrString(const AttributesList& attrs, std::string_view name, const std::string& fallback = {})
{
    if (const XMLAttributeValueView* value = attr(attrs, name)) {
        return value->ToString();
    }

    return fallback;
}

template<typename T>
T attrValue(const AttributesList& attrs, std::string_view name, T fallback = {})
{
    if (const XMLAttributeValueView* value = attr(attrs, name)) {
        return value->Get(fallback);
    }

    return fallback;
}

static const AudacityProject::AttachedObjects::RegisteredFactory storageKey {
    [](AudacityProject&) {
        return std::make_shared<VideoLinkStorage>();
    }
};

static ProjectFileIORegistry::ObjectWriterEntry writerEntry {
    [](const AudacityProject& project, XMLWriter& xmlFile) {
        VideoLinkStorage::Get(project).WriteXML(xmlFile);
    }
};

static ProjectFileIORegistry::ObjectReaderEntry readerEntry {
    std::string(VIDEO_PREVIEW_TAG),
    [](AudacityProject& project) -> XMLTagHandler* {
        return &VideoLinkStorage::Get(project);
    }
};

struct VideoLinkRestorer final : UndoStateExtension
{
    explicit VideoLinkRestorer(AudacityProject& project)
        : link(VideoLinkStorage::Get(project).link())
    {
    }

    void RestoreUndoRedoState(AudacityProject& project) override
    {
        VideoLinkStorage::Get(project).setLink(link);
    }

    VideoLink link;
};

static UndoRedoExtensionRegistry::Entry<VideoLinkRestorer> undoEntry {
    [](AudacityProject& project) -> std::shared_ptr<UndoStateExtension> {
        return std::make_shared<VideoLinkRestorer>(project);
    }
};
}

VideoLinkStorage& VideoLinkStorage::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get<VideoLinkStorage>(storageKey);
}

const VideoLinkStorage& VideoLinkStorage::Get(const AudacityProject& project)
{
    return Get(const_cast<AudacityProject&>(project));
}

const VideoLink& VideoLinkStorage::link() const
{
    return m_link;
}

void VideoLinkStorage::setLink(VideoLink link)
{
    m_link = std::move(link);
    m_linkChanged.notify();
}

void VideoLinkStorage::clear()
{
    setLink({});
}

muse::async::Notification VideoLinkStorage::linkChanged() const
{
    return m_linkChanged;
}

void VideoLinkStorage::WriteXML(XMLWriter& xmlFile) const
{
    if (!m_link.isValid()) {
        return;
    }

    xmlFile.StartTag(wxT("video_preview"));
    xmlFile.WriteAttr(wxT("source"), wxString::FromUTF8(m_link.sourcePath.toStdString().c_str()));
    xmlFile.WriteAttr(wxT("track_title"), wxString::FromUTF8(m_link.trackTitle.toStdString().c_str()));
    xmlFile.WriteAttr(wxT("stream_index"), m_link.streamIndex);
    xmlFile.WriteAttr(wxT("stream_id"), m_link.streamId);

    for (const VideoSegment& segment : m_link.segments) {
        if (!segment.isValid()) {
            continue;
        }

        xmlFile.StartTag(wxT("segment"));
        xmlFile.WriteAttr(wxT("track_id"), static_cast<long long>(segment.clipKey.trackId));
        xmlFile.WriteAttr(wxT("clip_id"), static_cast<long long>(segment.clipKey.itemId));
        xmlFile.WriteAttr(wxT("title"), wxString::FromUTF8(segment.title.toStdString().c_str()));
        xmlFile.WriteAttr(wxT("group_id"), static_cast<long long>(segment.groupId));
        xmlFile.WriteAttr(wxT("color_index"), segment.colorIndex);
        xmlFile.WriteAttr(wxT("project_start"), segment.projectStart, 10);
        xmlFile.WriteAttr(wxT("project_end"), segment.projectEnd, 10);
        xmlFile.WriteAttr(wxT("source_start"), segment.sourceStart, 10);
        xmlFile.WriteAttr(wxT("source_end"), segment.sourceEnd, 10);
        xmlFile.EndTag(wxT("segment"));
    }

    xmlFile.EndTag(wxT("video_preview"));
}

bool VideoLinkStorage::HandleXMLTag(const std::string_view& tag, const AttributesList& attrs)
{
    if (tag == VIDEO_PREVIEW_TAG) {
        m_link = {};
        m_link.sourcePath = muse::io::path_t(attrString(attrs, "source"));
        m_link.trackTitle = muse::String::fromStdString(attrString(attrs, "track_title", "Video"));
        m_link.streamIndex = attrValue<int>(attrs, "stream_index", -1);
        m_link.streamId = attrValue<int>(attrs, "stream_id", -1);
        return true;
    }

    if (tag == SEGMENT_TAG) {
        VideoSegment segment;
        segment.clipKey.trackId = attrValue<long long>(attrs, "track_id", trackedit::INVALID_TRACK);
        segment.clipKey.itemId = attrValue<long long>(attrs, "clip_id", trackedit::INVALID_TRACK_ITEM);
        segment.title = muse::String::fromStdString(attrString(attrs, "title"));
        segment.groupId = attrValue<int>(attrs, "group_id", -1);
        segment.colorIndex = attrValue<int>(attrs, "color_index", segment.colorIndex);
        segment.projectStart = attrValue<double>(attrs, "project_start");
        segment.projectEnd = attrValue<double>(attrs, "project_end");
        segment.sourceStart = attrValue<double>(attrs, "source_start");
        segment.sourceEnd = attrValue<double>(attrs, "source_end");

        if (segment.isValid()) {
            m_link.segments.push_back(std::move(segment));
        }

        return true;
    }

    return false;
}

XMLTagHandler* VideoLinkStorage::HandleXMLChild(const std::string_view& tag)
{
    if (tag == SEGMENT_TAG) {
        return this;
    }

    return nullptr;
}

void au::videopreview::modifyVideoLinkUndoState(AudacityProject& project)
{
    UndoManager::Get(project).ModifyState(typeid(VideoLinkRestorer));
}
