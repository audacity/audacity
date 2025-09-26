/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "record/irecordmetercontroller.h"

namespace au::record {
class RecordMeterController : public IRecordMeterController
{
public:
    [[nodiscard]] bool isRecordMeterVisible() const override;
    void setRecordMeterVisible(bool visible) override;
    [[nodiscard]] muse::async::Notification isRecordMeterVisibleChanged() const override;

private:
    muse::async::Notification m_isRecordMeterVisibleChanged;
    bool m_recordMeterVisible = false;
};
}
