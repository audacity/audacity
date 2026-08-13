/*
* Audacity: A Digital Audio Editor
*/

#include "au3labelsimporter.h"

#include "au3-track/Track.h"
#include "au3-label-track/LabelTrack.h"

#include "au3wrap/au3types.h"
#include "au3wrap/internal/wxtypes_convert.h"
#include "au3wrap/internal/domaccessor.h"
#include "au3wrap/internal/domconverter.h"

#include "labelsutils.h"

using namespace au::au3;
using namespace au::importexport;

muse::RetVal<au::trackedit::TrackId> Au3LabelsImporter::importData(const muse::io::path_t& filePath, trackedit::TrackId dstTrackId)
{
    Au3Project* project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    IF_ASSERT_FAILED(project) {
        return muse::make_ret(muse::Ret::Code::InternalError);
    }

    wxTextFile textFile(wxFromPath(filePath));

    if (!textFile.Open()) {
        return muse::make_ret(muse::Ret::Code::InternalError);
    }

    LabelFormat format = au3labelFormatFromSuffix(filePath);

    auto& tracks = Au3TrackList::Get(*project);

    // The label track holding the imported labels is named after the imported file
    const wxString trackName = wxFromPath(muse::io::filename(filePath, false /* includingExtension */));

    // Import into the given label track (e.g. a drag-preview track), otherwise create a new one
    Au3LabelTrack* labelTrack = nullptr;
    if (dstTrackId != -1) {
        labelTrack = dynamic_cast<Au3LabelTrack*>(DomAccessor::findTrack(*project, Au3TrackId(dstTrackId)));
    }

    const bool createdTrack = labelTrack == nullptr;
    if (createdTrack) {
        labelTrack = ::LabelTrack::Create(tracks, trackName);
    } else if (labelTrack->GetName() != trackName) {
        labelTrack->SetName(trackName);
    }

    const size_t labelCountBeforeImport = labelTrack->GetLabels().size();

    // Import labels into the track
    labelTrack->Import(textFile, format);

    textFile.Close();

    const auto prj = globalContext()->currentTrackeditProject();
    if (prj) {
        if (createdTrack) {
            prj->notifyAboutTrackAdded(DomConverter::labelTrack(labelTrack));
        } else {
            prj->notifyAboutTrackChanged(DomConverter::labelTrack(labelTrack));
        }

        // Notify about each imported label
        const auto& labels = labelTrack->GetLabels();
        for (size_t i = labelCountBeforeImport; i < labels.size(); ++i) {
            prj->notifyAboutLabelAdded(DomConverter::label(labelTrack, &labels[i]));
        }
    }

    return muse::RetVal<trackedit::TrackId>::make_ok(labelTrack->GetId());
}

std::vector<std::string> Au3LabelsImporter::supportedExtensions() const
{
    return {
        fileSuffixFromType(FileType::TEXT),
        fileSuffixFromType(FileType::SUBRIP),
        fileSuffixFromType(FileType::WEBVTT),
    };
}
