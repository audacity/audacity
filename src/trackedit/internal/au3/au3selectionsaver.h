/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "trackedittypes.h"

#include "au3-xml/XMLTagHandler.h"
#include "au3-registries/ClientData.h"

class AudacityProject;

namespace au::trackedit {
class ISelectionController;

class Au3SelectionSaver : public ::ClientData::Base, public ::XMLTagHandler
{
public:
    static Au3SelectionSaver& Get(::AudacityProject&);
    static Au3SelectionSaver& Get(const ::AudacityProject&);
    static Au3SelectionSaver* GetPtr(::AudacityProject&);

    Au3SelectionSaver(const ::AudacityProject&);

    void onOpen(ISelectionController*);
    void serialize(XMLWriter&);

private:
    ::XMLTagHandler* HandleXMLChild(const std::string_view& tag) override;
    bool HandleXMLTag(const std::string_view& tag, const ::AttributesList& attrs) override;

    void deserializeSelection(const ::AttributesList&);
    void deserializeSelectedItem(const ::AttributesList&);
    void deserializeSelectedTrack(const ::AttributesList&);

    ClipKeyList deserializedClipCoordinatesToKeys() const;
    LabelKeyList deserializedLabelCoordinatesToKeys() const;
    TrackIdList deserializedTrackIndicesToIds() const;

    const ::AudacityProject& m_project;

    ISelectionController* m_controller = nullptr;

    std::vector<std::pair<int, int> > m_selectedItemsCoordinates;
    std::vector<int> m_selectedTrackIndices;
    au::trackedit::secs_t m_dataSelectedStartTime { 0. };
    au::trackedit::secs_t m_dataSelectedEndTime { 0. };
    int m_focusedTrackIndex { -1 };
};
}
