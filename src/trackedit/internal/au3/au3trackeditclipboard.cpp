/*
* Audacity: A Digital Audio Editor
*/

#include <QGuiApplication>
#include <QMimeData>
#include <QClipboard>

#include "global/io/fileinfo.h"

#include "au3trackeditclipboard.h"
#include "au3trackdata.h"

#include "containers.h"

#include "au3-track/Track.h"
#include "au3-wave-track/WaveClip.h"
#include "au3-wave-track/WaveTrack.h"
#include "au3-label-track/LabelTrack.h"

namespace {
static std::vector<muse::io::path_t> extractLocalFilePaths(const QMimeData* md)
{
    std::vector<muse::io::path_t> out;
    if (!md) {
        return out;
    }

    if (md->hasUrls()) {
        const auto urls = md->urls();
        out.reserve(static_cast<size_t>(urls.size()));
        for (const QUrl& url : urls) {
            if (url.isLocalFile() && muse::io::FileInfo::exists(url.toLocalFile())) {
                out.emplace_back(muse::io::path_t(url.toLocalFile()));
            }
        }
    }

    return out;
}
}

using namespace au::trackedit;

std::vector<ITrackDataPtr> Au3TrackeditClipboard::trackDataCopy() const
{
    std::vector<ITrackDataPtr> sourceData = clipboardData()->trackData();

    std::vector<Au3TrackDataPtr> deepCopiedTracksData;
    deepCopiedTracksData.reserve(sourceData.size());
    for (const auto& data : sourceData) {
        const auto& au3Data = std::static_pointer_cast<Au3TrackData>(data);
        deepCopiedTracksData.push_back(std::make_shared<Au3TrackData>(au3Data->track()->Duplicate()));
    }

    std::vector<Au3TrackDataPtr> sourceAu3Data;
    sourceAu3Data.reserve(sourceData.size());
    for (const auto& data : sourceData) {
        sourceAu3Data.push_back(std::static_pointer_cast<Au3TrackData>(data));
    }

    auto copiedGroupIds = getGroupIDs(sourceAu3Data);
    auto newGroupIds = createNewGroupIDs(copiedGroupIds);
    updateTracksDataWithIDs(deepCopiedTracksData, copiedGroupIds, newGroupIds);

    return { deepCopiedTracksData.begin(), deepCopiedTracksData.end() };
}

void Au3TrackeditClipboard::addTrackData(ITrackDataPtr trackData)
{
    clipboardData()->addTrackData(std::move(trackData));
}

void Au3TrackeditClipboard::clearTrackData()
{
    clipboardData()->clearTrackData();
}

bool Au3TrackeditClipboard::trackDataEmpty() const
{
    return clipboardData()->trackDataEmpty();
}

size_t Au3TrackeditClipboard::trackDataSize() const
{
    return clipboardData()->trackDataSize();
}

void Au3TrackeditClipboard::setMultiSelectionCopy(bool newValue)
{
    clipboardData()->setMultiSelectionCopy(newValue);
}

bool Au3TrackeditClipboard::isMultiSelectionCopy() const
{
    return clipboardData()->isMultiSelectionCopy();
}

void Au3TrackeditClipboard::setRangeSelectionCopy(bool newValue)
{
    clipboardData()->setRangeSelectionCopy(newValue);
}

bool Au3TrackeditClipboard::isRangeSelectionCopy() const
{
    return clipboardData()->isRangeSelectionCopy();
}

std::vector<muse::io::path_t> Au3TrackeditClipboard::systemClipboardFilePaths() const
{
    const QClipboard* cb = QGuiApplication::clipboard();
    if (!cb) {
        return {};
    }

    const QMimeData* md = cb->mimeData();
    return extractLocalFilePaths(md);
}

void Au3TrackeditClipboard::clearSystemClipboard()
{
    auto* md = new QMimeData();
    md->setText(QString());
    QGuiApplication::clipboard()->setMimeData(md);
}

std::set<int64_t> Au3TrackeditClipboard::getGroupIDs(const std::vector<Au3TrackDataPtr>& tracksData)
{
    std::set<int64_t> groupIds;

    for (const Au3TrackDataPtr& data : tracksData) {
        if (auto waveTrack = dynamic_cast<au3::Au3WaveTrack*>(data->track().get())) {
            for (const auto& clip : waveTrack->Intervals()) {
                if (clip->GetGroupId() != -1) {
                    groupIds.emplace(clip->GetGroupId());
                }
            }
        } else if (auto labelTrack = dynamic_cast<au3::Au3LabelTrack*>(data->track().get())) {
            for (const au3::Au3Label& label : labelTrack->GetLabels()) {
                if (label.GetGroupId() != -1) {
                    groupIds.emplace(label.GetGroupId());
                }
            }
        }
    }

    return groupIds;
}

std::vector<int64_t> Au3TrackeditClipboard::createNewGroupIDs(const std::set<int64_t>& groupIDs) const
{
    std::vector<int64_t> newGroupIds;

    auto prj = globalContext()->currentTrackeditProject();

    int64_t newId = 0;
    for (size_t i = 0; i < groupIDs.size(); i++) {
        newGroupIds.push_back(prj->createNewGroupID(newId));
        newId = newGroupIds.back() + 1; // + 1 or it would return the same repeatedly.
    }

    return newGroupIds;
}

void Au3TrackeditClipboard::updateTracksDataWithIDs(const std::vector<Au3TrackDataPtr>& tracksData,
                                                    const std::set<int64_t>& groupIDs,
                                                    const std::vector<int64_t>& newGroupIDs)
{
    DO_ASSERT(groupIDs.size() == newGroupIDs.size());

    const auto remapped = [&groupIDs, &newGroupIDs](int64_t currentID) {
        const auto it = groupIDs.find(currentID);
        DO_ASSERT(it != groupIDs.end());
        return newGroupIDs[std::distance(groupIDs.begin(), it)];
    };

    for (const Au3TrackDataPtr& data : tracksData) {
        if (auto waveTrack = dynamic_cast<au3::Au3WaveTrack*>(data->track().get())) {
            for (const auto& clip : waveTrack->Intervals()) {
                if (clip->GetGroupId() != -1) {
                    clip->SetGroupId(remapped(clip->GetGroupId()));
                }
            }
        } else if (auto labelTrack = dynamic_cast<au3::Au3LabelTrack*>(data->track().get())) {
            const LabelArray& labels = labelTrack->GetLabels();
            for (size_t i = 0; i < labels.size(); ++i) {
                if (labels[i].GetGroupId() != -1) {
                    au3::Au3Label label = labels[i];
                    label.SetGroupId(remapped(label.GetGroupId()));
                    labelTrack->SetLabel(i, label);
                }
            }
        }
    }
}
