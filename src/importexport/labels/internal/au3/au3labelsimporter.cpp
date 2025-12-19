/*
* Audacity: A Digital Audio Editor
*/

#include "au3labelsimporter.h"

#include "au3-track/Track.h"
#include "au3-label-track/LabelTrack.h"

#include "au3wrap/au3types.h"
#include "au3wrap/internal/domconverter.h"

#include "labelsutils.h"

using namespace au::au3;
using namespace au::importexport;

muse::Ret Au3LabelsImporter::importData(const muse::io::path_t& filePath)
{
    Au3Project* project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    IF_ASSERT_FAILED(project) {
        return muse::make_ret(muse::Ret::Code::InternalError);
    }

    wxTextFile textFile(wxString(filePath.toStdString()));

    if (!textFile.Open()) {
        return muse::make_ret(muse::Ret::Code::InternalError);
    }

    LabelFormat format = au3labelFormatFromSuffix(filePath);

    auto& tracks = Au3TrackList::Get(*project);

    // Create a new label track for imported labels
    Au3LabelTrack* labelTrack = ::LabelTrack::Create(tracks);

    // Import labels into the track
    labelTrack->Import(textFile, format);

    textFile.Close();

    // Notify project about the new track
    const auto prj = globalContext()->currentTrackeditProject();
    if (prj) {
        prj->notifyAboutTrackAdded(DomConverter::labelTrack(labelTrack));

        // Notify about each imported label
        const auto& labels = labelTrack->GetLabels();
        for (size_t i = 0; i < labels.size(); ++i) {
            prj->notifyAboutLabelAdded(DomConverter::label(labelTrack, &labels[i]));
        }
    }

    return muse::make_ret(muse::Ret::Code::Ok);
}
