/*
* Audacity: A Digital Audio Editor
*/

#include "au3labelsexporter.h"

#include <FileNames.h>

#include "libraries/lib-track/Track.h"
#include "libraries/lib-label-track/LabelTrack.h"

#include "au3wrap/au3types.h"

using namespace au::au3;
using namespace au::importexport;

static LabelFormat labelFormatFromSuffix(const muse::io::path_t& filePath)
{
    std::string suffix = muse::io::suffix(filePath);
    if (suffix == "srt") {
        return LabelFormat::SUBRIP;
    } else if (suffix == "vtt") {
        return LabelFormat::WEBVTT;
    }

    return LabelFormat::TEXT;
}

muse::Ret Au3LabelsExporter::exportData(const muse::io::path_t& filePath, const trackedit::TrackIdList& includedLabelTracksIds)
{
    Au3Project* project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    IF_ASSERT_FAILED(project) {
        return muse::make_ret(muse::Ret::Code::InternalError);
    }

    std::vector<const Au3LabelTrack*> labelTracks;

    const Au3TrackList& tracks = Au3TrackList::Get(*project);
    for (auto labelTrack : tracks.Any<const LabelTrack>()) {
        if (muse::contains(includedLabelTracksIds, labelTrack->GetId().raw())) {
            labelTracks.emplace_back(labelTrack);
        }
    }

    IF_ASSERT_FAILED(!labelTracks.empty()) {
        return muse::make_ret(muse::Ret::Code::InternalError);
    }

    wxTextFile textFile(wxString(filePath.toStdString()));

    bool open = false;
    if (textFile.Exists()) {
        open = textFile.Open();
    } else {
        open = textFile.Create();
    }

    if (!open) {
        return muse::make_ret(muse::Ret::Code::InternalError);
    }

    textFile.Clear();

    for (auto labelTrack: labelTracks) {
        labelTrack->Export(textFile, labelFormatFromSuffix(filePath));
    }

    textFile.Write();
    textFile.Close();

    return muse::make_ret(muse::Ret::Code::Ok);
}
