/*
* Audacity: A Digital Audio Editor
*/
#include "labelstableviewmodel.h"

#include "framework/global/translation.h"

#include "uicomponents/components/timecodemodel.h"
#include "uicomponents/components/frequencymodel.h"

#include "labelstableviewverticalheader.h"
#include "labelstableviewtrackcell.h"
#include "labelstableviewtimecodecell.h"

static const muse::actions::ActionCode SELECT_LABEL_TRACK_CODE("select-label-track");
static const muse::actions::ActionCode NEW_LABEL_TRACK_CODE("new-label-track");

static const int TRACK_COLUMN = 0;
static const int LABEL_COLUMN = 1;
static const int START_TIME_COLUMN = 2;
static const int END_TIME_COLUMN = 3;
static const int LOW_FREQUENCY_COLUMN = 4;
static const int HIGH_FREQUENCY_COLUMN = 5;

using namespace au::projectscene;
using namespace muse;
using namespace muse::ui;
using namespace muse::uicomponents;

LabelsTableViewModel::LabelsTableViewModel(QObject* parent)
    : AbstractTableViewModel(parent)
{
}

void LabelsTableViewModel::load()
{
    setHorizontalHeaders(makeHorizontalHeaders());
    setVerticalHeaders(makeVerticalHeaders());

    setTable(makeTable());
}

QVector<TableViewHeader*> LabelsTableViewModel::makeHorizontalHeaders()
{
    QVector<TableViewHeader*> hHeaders;

    hHeaders << makeHorizontalHeader(qtrc("projectscene", "Track"),
                                     static_cast<TableViewCellType::Type>(LabelsTableViewCellType::Type::Track),
                                     TableViewCellEditMode::Mode::DoubleClick, 152);
    hHeaders << makeHorizontalHeader(qtrc("projectscene", "Label text"), TableViewCellType::Type::String,
                                     TableViewCellEditMode::Mode::DoubleClick, 296);

    static auto timecodeModelStub = au::uicomponents::TimecodeModel();
    MenuItemList timecodeFormats = timecodeModelStub.availableFormats();

    TableViewHeader* startTimeHeader = makeHorizontalHeader(qtrc("projectscene", "Start time"),
                                                            static_cast<TableViewCellType::Type>(LabelsTableViewCellType::Type::Timecode),
                                                            TableViewCellEditMode::Mode::StartInEdit);
    startTimeHeader->setAvailableFormats(timecodeFormats);
    startTimeHeader->setCurrentFormatId(QString::number(timecodeModelStub.currentFormat()));
    hHeaders << startTimeHeader;

    TableViewHeader* endTimeHeader = makeHorizontalHeader(qtrc("projectscene", "End time"),
                                                          static_cast<TableViewCellType::Type>(LabelsTableViewCellType::Type::Timecode),
                                                          TableViewCellEditMode::Mode::StartInEdit);
    endTimeHeader->setAvailableFormats(timecodeFormats);
    endTimeHeader->setCurrentFormatId(QString::number(timecodeModelStub.currentFormat()));
    hHeaders << endTimeHeader;

    static auto lowFrequencyModelStub = au::uicomponents::FrequencyModel();
    MenuItemList lowFrequencyFormats = lowFrequencyModelStub.availableFormats();

    TableViewHeader* lowFrequencyHeader = makeHorizontalHeader(qtrc("projectscene",
                                                                    "Low frequency"),
                                                               static_cast<TableViewCellType::Type>(LabelsTableViewCellType::Type::Frequency),
                                                               TableViewCellEditMode::Mode::StartInEdit);
    lowFrequencyHeader->setAvailableFormats(lowFrequencyFormats);
    lowFrequencyHeader->setCurrentFormatId(QString::number(lowFrequencyModelStub.currentFormat()));
    hHeaders << lowFrequencyHeader;

    static auto highFrequencyModelStub = au::uicomponents::FrequencyModel();
    MenuItemList highFrequencyFormats = highFrequencyModelStub.availableFormats();

    TableViewHeader* highFrequencyHeader = makeHorizontalHeader(qtrc("projectscene", "High frequency"),
                                                                static_cast<TableViewCellType::Type>(LabelsTableViewCellType::Type::
                                                                                                     Frequency),
                                                                TableViewCellEditMode::Mode::StartInEdit);
    highFrequencyHeader->setAvailableFormats(highFrequencyFormats);
    highFrequencyHeader->setCurrentFormatId(QString::number(highFrequencyModelStub.currentFormat()));
    hHeaders << highFrequencyHeader;

    return hHeaders;
}

QVector<TableViewHeader*> LabelsTableViewModel::makeVerticalHeaders()
{
    const trackedit::ITrackeditProjectPtr project = globalContext()->currentTrackeditProject();
    if (!project) {
        return {};
    }

    QVector<TableViewHeader*> vHeaders;

    for (const trackedit::Track& track : allLabelTracks()) {
        for (const trackedit::Label& label : project->labelList(track.id)) {
            LabelsTableViewVerticalHeader* vHeader = new LabelsTableViewVerticalHeader(this);
            vHeader->setLabelKey(label.key);
            vHeaders << vHeader;
        }
    }

    return vHeaders;
}

QVector<QVector<TableViewCell*> > LabelsTableViewModel::makeTable()
{
    const trackedit::ITrackeditProjectPtr project = globalContext()->currentTrackeditProject();
    if (!project) {
        return {};
    }

    QVector<QVector<TableViewCell*> > table;

    for (const trackedit::Track& track : allLabelTracks()) {
        for (const trackedit::Label& label : project->labelList(track.id)) {
            QVector<TableViewCell*> row;

            row.append(makeTrackCell(track.id, track.title));
            row.append(makeCell(Val(label.title.toStdString())));

            row.append(makeTimecodeCell(Val(label.startTime)));
            row.append(makeTimecodeCell(Val(label.endTime)));

            row.append(makeTimecodeCell(Val(label.lowFrequency)));
            row.append(makeTimecodeCell(Val(label.highFrequency)));

            table.append(row);
        }
    }

    return table;
}

void LabelsTableViewModel::handleTrackMenuItem(int row, int column, const QString& itemId)
{
    TableViewCell* cell = findCell(row, column);
    if (!cell) {
        return;
    }

    LabelsTableViewTrackCell* trackCell = dynamic_cast<LabelsTableViewTrackCell*>(cell);
    if (!trackCell) {
        return;
    }

    if (itemId == NEW_LABEL_TRACK_CODE) {
        // todo: add new label track
        return;
    }

    for (const muse::uicomponents::MenuItem* item : trackCell->availableTracks()) {
        if (item->id() == itemId) {
            trackCell->setCurrentTrackId(item->args().arg<trackedit::TrackId>());
            trackCell->setValue(Val(item->translatedTitle()));
            break;
        }
    }
}

void LabelsTableViewModel::doCellValueChanged(int row, int column)
{
    switch (column) {
    case TRACK_COLUMN: moveLabel(row, column);
        break;
    case LABEL_COLUMN: renameLabel(row, column);
        break;
    case START_TIME_COLUMN: changeLabelStartTime(row, column);
        break;
    case END_TIME_COLUMN: changeLabelEndTime(row, column);
        break;
    case LOW_FREQUENCY_COLUMN: changeLabelLowFrequency(row, column);
        break;
    case HIGH_FREQUENCY_COLUMN: changeLabelHighFrequency(row, column);
        break;
    default: break;
    }
}

TableViewCell* LabelsTableViewModel::makeTrackCell(const trackedit::TrackId& trackId, const QString& trackTitle)
{
    LabelsTableViewTrackCell* result = new LabelsTableViewTrackCell(makeCell(Val(trackTitle)));
    result->setCurrentTrackId(trackId);
    result->setAvailableTracks(makeAvailableTracksList(trackId));
    return result;
}

TableViewCell* LabelsTableViewModel::makeTimecodeCell(const muse::Val& value)
{
    return new LabelsTableViewTimecodeCell(makeCell(value));
}

std::vector<au::trackedit::Track> LabelsTableViewModel::allLabelTracks() const
{
    const trackedit::ITrackeditProjectPtr project = globalContext()->currentTrackeditProject();
    if (!project) {
        return {};
    }

    std::vector<trackedit::Track> result;

    for (const trackedit::Track& track : project->trackList()) {
        if (track.type != trackedit::TrackType::Label) {
            continue;
        }

        result.emplace_back(track);
    }

    return result;
}

MenuItemList LabelsTableViewModel::makeAvailableTracksList(const trackedit::TrackId& currentTrackId)
{
    std::vector<trackedit::Track> labelTracks = allLabelTracks();
    if (labelTracks.empty()) {
        return {};
    }

    MenuItemList result;

    for (const trackedit::Track& track : labelTracks) {
        MenuItem* item = new MenuItem();

        ui::UiAction action;
        action.code = SELECT_LABEL_TRACK_CODE;
        action.title = TranslatableString::untranslatable(track.title);
        item->setAction(action);

        ui::UiActionState state;
        state.enabled = true;
        state.checked = track.id == currentTrackId;
        item->setState(state);

        item->setArgs(actions::ActionData::make_arg1(track.id));
        item->setId(QString::fromStdString(action.code) + action.title.qTranslatedWithoutMnemonic());

        result << item;
    }

    result << makeSeparator();

    MenuItem* item = new MenuItem();

    ui::UiAction action;
    action.code = NEW_LABEL_TRACK_CODE;
    action.title = TranslatableString("projectscene", "New Label track");
    item->setAction(action);

    ui::UiActionState state;
    state.enabled = true;
    item->setState(state);

    item->setId(QString::fromStdString(action.code));

    result << item;

    return result;
}

MenuItem* LabelsTableViewModel::makeSeparator()
{
    MenuItem* item = new MenuItem(this);

    ui::UiAction action;
    action.title = {};
    item->setAction(action);

    return item;
}

void LabelsTableViewModel::moveLabel(int row, int column)
{
    LabelsTableViewVerticalHeader* verticalHeader = dynamic_cast<LabelsTableViewVerticalHeader*>(findVerticalHeader(row));
    if (!verticalHeader) {
        return;
    }

    const LabelsTableViewTrackCell* cell = dynamic_cast<LabelsTableViewTrackCell*>(findCell(row, column));
    if (!cell) {
        return;
    }

    trackedit::LabelKey labelKey = verticalHeader->labelKey().key;

    muse::RetVal<trackedit::LabelKeyList> retVal = trackeditInteraction()->moveLabels({ labelKey }, cell->currentTrackId(),
                                                                                      true /* completed */);
    if (!retVal.ret) {
        return;
    }

    labelKey.trackId = retVal.val.front().trackId;
    labelKey.itemId = retVal.val.front().itemId;

    verticalHeader->setLabelKey(labelKey);
}

void LabelsTableViewModel::renameLabel(int row, int column)
{
    LabelsTableViewVerticalHeader* verticalHeader = dynamic_cast<LabelsTableViewVerticalHeader*>(findVerticalHeader(row));
    if (!verticalHeader) {
        return;
    }

    const TableViewCell* cell = findCell(row, column);
    if (!cell) {
        return;
    }

    trackedit::LabelKey labelKey = verticalHeader->labelKey().key;

    trackeditInteraction()->changeLabelTitle(labelKey, String::fromStdString(cell->value().toString()));
}

void LabelsTableViewModel::changeLabelStartTime(int row, int column)
{
    LabelsTableViewVerticalHeader* verticalHeader = dynamic_cast<LabelsTableViewVerticalHeader*>(findVerticalHeader(row));
    if (!verticalHeader) {
        return;
    }

    TableViewCell* cell = findCell(row, column);
    if (!cell) {
        return;
    }

    trackedit::LabelKey labelKey = verticalHeader->labelKey().key;

    bool ok = trackeditInteraction()->stretchLabelLeft(labelKey, cell->value().toDouble(), true /* completed */);
    if (ok) {
        //! NOTE: When stretching, the beginning and end may switch places, so let's update them
        const trackedit::ITrackeditProjectPtr project = globalContext()->currentTrackeditProject();

        const trackedit::Label actualLabel = project->label(labelKey);

        cell->setValue(Val(actualLabel.startTime));

        TableViewCell* endTimeCell = findCell(row, END_TIME_COLUMN);
        endTimeCell->setValue(Val(actualLabel.endTime));
    }
}

void LabelsTableViewModel::changeLabelEndTime(int row, int column)
{
    LabelsTableViewVerticalHeader* verticalHeader = dynamic_cast<LabelsTableViewVerticalHeader*>(findVerticalHeader(row));
    if (!verticalHeader) {
        return;
    }

    TableViewCell* cell = findCell(row, column);
    if (!cell) {
        return;
    }

    trackedit::LabelKey labelKey = verticalHeader->labelKey().key;

    bool ok = trackeditInteraction()->stretchLabelRight(labelKey, cell->value().toDouble(), true /* completed */);
    if (ok) {
        //! NOTE: When stretching, the beginning and end may switch places, so let's update them
        const trackedit::ITrackeditProjectPtr project = globalContext()->currentTrackeditProject();

        const trackedit::Label actualLabel = project->label(labelKey);

        TableViewCell* startTimeCell = findCell(row, START_TIME_COLUMN);
        startTimeCell->setValue(Val(actualLabel.startTime));

        cell->setValue(Val(actualLabel.endTime));
    }
}

void LabelsTableViewModel::changeLabelLowFrequency(int row, int column)
{
    LabelsTableViewVerticalHeader* verticalHeader = dynamic_cast<LabelsTableViewVerticalHeader*>(findVerticalHeader(row));
    if (!verticalHeader) {
        return;
    }

    const TableViewCell* cell = findCell(row, column);
    if (!cell) {
        return;
    }

    trackedit::LabelKey labelKey = verticalHeader->labelKey().key;

    trackeditInteraction()->changeLabelLowFrequency(labelKey, cell->value().toDouble());
}

void LabelsTableViewModel::changeLabelHighFrequency(int row, int column)
{
    LabelsTableViewVerticalHeader* verticalHeader = dynamic_cast<LabelsTableViewVerticalHeader*>(findVerticalHeader(row));
    if (!verticalHeader) {
        return;
    }

    const TableViewCell* cell = findCell(row, column);
    if (!cell) {
        return;
    }

    trackedit::LabelKey labelKey = verticalHeader->labelKey().key;

    trackeditInteraction()->changeLabelHighFrequency(labelKey, cell->value().toDouble());
}
