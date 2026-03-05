/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "trackedit/iclipboarddata.h"

namespace au::trackedit {
class ClipboardData : public IClipboardData
{
public:
    std::vector<ITrackDataPtr> trackData() const override;
    void addTrackData(ITrackDataPtr data) override;
    void clearTrackData() override;
    bool trackDataEmpty() const override;
    size_t trackDataSize() const override;

    void setMultiSelectionCopy(bool val) override;
    bool isMultiSelectionCopy() const override;

private:
    std::vector<ITrackDataPtr> m_tracksData;
    bool m_isMultiSelectionCopy = false;
};
}
