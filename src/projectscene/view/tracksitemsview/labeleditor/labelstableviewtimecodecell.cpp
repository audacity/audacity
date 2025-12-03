/*
* Audacity: A Digital Audio Editor
*/
#include "labelstableviewtimecodecell.h"

#include "playback/iaudiooutput.h"

using namespace au::projectscene;

LabelsTableViewTimecodeCell::LabelsTableViewTimecodeCell(QObject* parent)
    : muse::uicomponents::TableViewCell(parent)
{
}

LabelsTableViewTimecodeCell::LabelsTableViewTimecodeCell(const TableViewCell* other)
{
    setValue(other->value());
}

double LabelsTableViewTimecodeCell::sampleRate() const
{
    return playback()->audioOutput()->sampleRate();
}

double LabelsTableViewTimecodeCell::tempo() const
{
    auto project = globalContext()->currentTrackeditProject();
    if (!project) {
        return 0.0;
    }

    return project->timeSignature().tempo;
}

int LabelsTableViewTimecodeCell::upperTimeSignature() const
{
    auto project = globalContext()->currentTrackeditProject();
    if (!project) {
        return 0;
    }

    return project->timeSignature().upper;
}

int LabelsTableViewTimecodeCell::lowerTimeSignature() const
{
    auto project = globalContext()->currentTrackeditProject();
    if (!project) {
        return 0.0;
    }

    return project->timeSignature().lower;
}
