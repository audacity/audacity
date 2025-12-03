/*
* Audacity: A Digital Audio Editor
*/
#include "labelstableviewverticalheader.h"

using namespace au::projectscene;

LabelsTableViewVerticalHeader::LabelsTableViewVerticalHeader(QObject* parent)
    : muse::uicomponents::TableViewHeader(parent)
{
}

LabelKey LabelsTableViewVerticalHeader::labelKey() const
{
    return m_labelKey;
}

void LabelsTableViewVerticalHeader::setLabelKey(const LabelKey& labelKey)
{
    m_labelKey = labelKey;
}
