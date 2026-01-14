/*
 * Audacity: A Digital Audio Editor
 */

#include "au3selectionsaver.h"

#include "iselectioncontroller.h"

#include "framework/global/log.h"
#include "au3wrap/internal/domaccessor.h"

#include "au3-project/Project.h"
#include "au3-label-track/LabelTrack.h"
#include "au3-wave-track/WaveTrack.h"

namespace au::trackedit {
namespace {
constexpr auto selectionTag = "selection";
constexpr auto selectedItemTag = "selectedItem";
constexpr auto selectedTrackTag = "selectedTrack";

constexpr auto dataSelectedStartTimeAttr = "dataSelectedStartTime";
constexpr auto dataSelectedEndTimeAttr = "dataSelectedEndTime";
constexpr auto focusedTrackIndexAttr = "focusedTrackIndex";
constexpr auto trackIndexAttr = "trackIndex";
constexpr auto itemIndexAttr = "itemIndex";

const ::Track* trackByIndex(const ::AudacityProject& project, int i)
{
    const auto& tracks = ::TrackList::Get(project);
    if (i < static_cast<int>(tracks.Size())) {
        auto it = tracks.begin();
        std::advance(it, i);
        return *it;
    } else {
        return nullptr;
    }
}

int indexByTrack(const ::AudacityProject& project, const ::Track& track)
{
    const auto& tracks = ::TrackList::Get(project);
    const ::TrackIter<const ::Track> it = tracks.Find(&track);
    return std::distance(tracks.begin(), it);
}

int indexByTrackId(const ::AudacityProject& project, au::trackedit::TrackId trackId)
{
    const ::Track* track = au3::DomAccessor::findTrack(project, ::TrackId { trackId });
    if (!track) {
        return -1;
    }
    return indexByTrack(project, *track);
}

static ProjectFileIORegistry::ObjectWriterEntry selectedItemsWriterEntry {
    [](const AudacityProject& project, XMLWriter& xmlFile) {
        Au3SelectionSaver::Get(project).serialize(xmlFile);
    }
};

static ProjectFileIORegistry::ObjectReaderEntry selectedItemsReaderEntry {
    selectionTag,
    Au3SelectionSaver::GetPtr
};
} // namespace {

static const ::AudacityProject::AttachedObjects::RegisteredFactory key{
    [](AudacityProject& project)
    {
        return std::make_shared<Au3SelectionSaver>(project);
    } };

Au3SelectionSaver& Au3SelectionSaver::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get<Au3SelectionSaver>(key);
}

Au3SelectionSaver& Au3SelectionSaver::Get(const AudacityProject& project)
{
    return Get(const_cast<AudacityProject&>(project));
}

Au3SelectionSaver* Au3SelectionSaver::GetPtr(::AudacityProject& project)
{
    return &Get(project);
}

Au3SelectionSaver::Au3SelectionSaver(const ::AudacityProject& project)
    : m_project{project} {}

void Au3SelectionSaver::serialize(XMLWriter& writer)
{
    IF_ASSERT_FAILED(m_controller) {
        return;
    }
    const auto& ctrl = *m_controller;

    writer.StartTag(selectionTag);
    writer.WriteAttr(dataSelectedStartTimeAttr, ctrl.dataSelectedStartTime());
    writer.WriteAttr(dataSelectedEndTimeAttr, ctrl.dataSelectedEndTime());
    writer.WriteAttr(focusedTrackIndexAttr, indexByTrackId(m_project, ctrl.focusedTrack()));
    // TODO frequency selection

    auto items = ctrl.selectedClips();
    const auto labels = ctrl.selectedLabels();
    items.insert(items.end(), labels.begin(), labels.end());

    for (const auto& item : items) {
        const ::Track* track = au3::DomAccessor::findTrack(m_project, ::TrackId { item.trackId });
        IF_ASSERT_FAILED(track) {
            continue;
        }
        const auto trackIndex = indexByTrack(m_project, *track);

        auto itemIndex = -1;
        if (const auto* waveTrack = dynamic_cast<const ::WaveTrack*>(track)) {
            itemIndex = au3::DomAccessor::findClipIndexById(waveTrack, item.itemId);
        } else if (const auto* labelTrack = dynamic_cast<const ::LabelTrack*>(track)) {
            itemIndex = labelTrack->GetLabelIndex(item.itemId);
        } else {
            assert(false);
            continue;
        }

        writer.StartTag(selectedItemTag);
        {
            writer.WriteAttr(trackIndexAttr, trackIndex);
            writer.WriteAttr(itemIndexAttr, itemIndex);
        }
        writer.EndTag(selectedItemTag);
    }

    for (const au::trackedit::TrackId trackId : ctrl.selectedTracks()) {
        writer.StartTag(selectedTrackTag);
        writer.WriteAttr(trackIndexAttr, indexByTrackId(m_project, trackId));
        writer.EndTag(selectedTrackTag);
    }
    writer.EndTag(selectedItemTag);
}

::XMLTagHandler* Au3SelectionSaver::HandleXMLChild(const std::string_view& tag)
{
    if (tag == selectionTag || tag == selectedItemTag || tag == selectedTrackTag) {
        return this;
    }
    return nullptr;
}

bool Au3SelectionSaver::HandleXMLTag(const std::string_view& tag, const ::AttributesList& attrs)
{
    if (tag == selectionTag) {
        deserializeSelection(attrs);
        return true;
    } else if (tag == selectedItemTag) {
        deserializeSelectedItem(attrs);
        return true;
    } else if (tag == selectedTrackTag) {
        deserializeSelectedTrack(attrs);
        return true;
    } else {
        return false;
    }
}

void Au3SelectionSaver::deserializeSelection(const ::AttributesList& attrs)
{
    for (auto pair : attrs) {
        const auto attr = pair.first;
        const auto value = pair.second;
        if (attr == dataSelectedStartTimeAttr) {
            m_dataSelectedStartTime = value.Get<double>();
        } else if (attr == dataSelectedEndTimeAttr) {
            m_dataSelectedEndTime = value.Get<double>();
        } else if (attr == focusedTrackIndexAttr) {
            m_focusedTrackIndex = value.Get<int>();
        } else {
            LOGW() << "Unknown selection attribute: " << attr;
        }
    }
}

void Au3SelectionSaver::deserializeSelectedItem(const ::AttributesList& attrs)
{
    int trackIndex = -1;
    int itemIndex = -1;
    for (auto pair : attrs) {
        const auto attr = pair.first;
        const auto value = pair.second;
        if (attr == trackIndexAttr) {
            trackIndex = value.Get<int>();
        } else if (attr == itemIndexAttr) {
            itemIndex = value.Get<int>();
        } else {
            LOGW() << "Unknown selected item attribute: " << attr;
        }
    }

    if (trackIndex != -1 && itemIndex != -1) {
        m_selectedItemsCoordinates.emplace_back(trackIndex, itemIndex);
    }
}

void Au3SelectionSaver::deserializeSelectedTrack(const ::AttributesList& attrs)
{
    if (!attrs.empty()) {
        const auto& [attr, value] = attrs[0];
        if (attr == trackIndexAttr) {
            m_selectedTrackIndices.push_back(value.Get<int>());
        } else {
            LOGW() << "Unknown selected track attribute: " << attr;
        }
    }
}

ClipKeyList Au3SelectionSaver::deserializedClipCoordinatesToKeys() const
{
    ClipKeyList keys;
    for (const auto& [trackIndex, itemIndex] : m_selectedItemsCoordinates) {
        const auto track = trackByIndex(m_project, trackIndex);
        if (!track) {
            continue;
        }

        const auto* waveTrack = dynamic_cast<const ::WaveTrack*>(track);
        if (!waveTrack) {
            continue;
        }

        const auto interval = waveTrack->GetSortedClipByIndex(itemIndex);
        if (!interval) {
            continue;
        }

        keys.emplace_back(track->GetId(), interval->GetId());
    }
    return keys;
}

LabelKeyList Au3SelectionSaver::deserializedLabelCoordinatesToKeys() const
{
    LabelKeyList keys;
    for (const auto& [trackIndex, itemIndex] : m_selectedItemsCoordinates) {
        const auto track = trackByIndex(m_project, trackIndex);
        if (!track) {
            continue;
        }

        const auto* labelTrack = dynamic_cast<const ::LabelTrack*>(track);
        if (!labelTrack) {
            continue;
        }

        const ::LabelStruct* label = labelTrack->GetLabel(itemIndex);
        if (!label) {
            continue;
        }

        keys.emplace_back(track->GetId(), label->GetId());
    }

    return keys;
}

TrackIdList Au3SelectionSaver::deserializedTrackIndicesToIds() const
{
    TrackIdList ids;
    for (const int trackIndex : m_selectedTrackIndices) {
        if (const auto track = trackByIndex(m_project, trackIndex)) {
            ids.emplace_back(track->GetId().raw());
        }
    }
    return ids;
}

void Au3SelectionSaver::onOpen(ISelectionController* controller)
{
    m_controller = controller;

    m_controller->setSelectedClips(deserializedClipCoordinatesToKeys());
    m_controller->setSelectedLabels(deserializedLabelCoordinatesToKeys());
    m_controller->setSelectedTracks(deserializedTrackIndicesToIds());

    m_controller->setDataSelectedStartTime(m_dataSelectedStartTime, true);
    m_controller->setDataSelectedEndTime(m_dataSelectedEndTime, true);

    if (const auto track = trackByIndex(m_project, m_focusedTrackIndex)) {
        m_controller->setFocusedTrack(track->GetId());
    }
}
}
