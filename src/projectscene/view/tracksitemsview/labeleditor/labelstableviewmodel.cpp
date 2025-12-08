/*
* Audacity: A Digital Audio Editor
*/
#include "labelstableviewmodel.h"

#include "framework/global/translation.h"
#include "framework/global/log.h"

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
            trackCell->setValue_property(item->translatedTitle());
            break;
        }
    }
}

void LabelsTableViewModel::addNewLabel()
{
    ItemMultiSelectionModel* selectionModel = this->selectionModel();
    if (!selectionModel) {
        return;
    }

    QModelIndexList selectedIndexes = selectionModel->selectedIndexes();
    int row = !selectedIndexes.empty() ? selectedIndexes.first().row() : 0;

    std::vector<trackedit::Track> labelTracks = allLabelTracks();
    if (labelTracks.empty()) {
        // todo: create new
        return;
    }

    trackedit::TrackId trackId = labelTracks.front().id;

    RetVal<trackedit::LabelKey> retVal = trackeditInteraction()->addLabel(trackId);
    if (!retVal.ret) {
        LOGE() << retVal.ret.toString();
        return;
    }

    const trackedit::ITrackeditProjectPtr project = globalContext()->currentTrackeditProject();
    trackedit::Label createdLabel = project->label(retVal.val);

    LabelsTableViewVerticalHeader* vHeader = new LabelsTableViewVerticalHeader(this);
    vHeader->setLabelKey(retVal.val);
    insertVerticalHeader(row, vHeader);

    QVector<TableViewCell*> cells;

    QString trackTitle = labelTracks.front().title.toQString();
    cells.append(makeTrackCell(trackId, trackTitle));

    cells.append(makeCell(Val(createdLabel.title.toStdString())));

    cells.append(makeTimecodeCell(Val(createdLabel.startTime)));
    cells.append(makeTimecodeCell(Val(createdLabel.endTime)));

    cells.append(makeTimecodeCell(Val(createdLabel.lowFrequency)));
    cells.append(makeTimecodeCell(Val(createdLabel.highFrequency)));

    insertRow(row, cells);

    QTimer::singleShot(100, [=](){
        TableViewCell* labelCell = findCell(row, LABEL_COLUMN);
        labelCell->requestEdit();
    });
}

void LabelsTableViewModel::removeSelectedLabels()
{
    ItemMultiSelectionModel* selectionModel = this->selectionModel();
    if (!selectionModel) {
        return;
    }

    QModelIndexList selectedIndexes = selectionModel->selectedIndexes();
    if (selectedIndexes.isEmpty()) {
        return;
    }

    std::vector<trackedit::LabelKey> labelKeysToRemove;
    std::set<int> rowsToRemove;

    for (const QModelIndex& index : selectedIndexes) {
        int row = index.row();

        LabelsTableViewVerticalHeader* vHeader = dynamic_cast<LabelsTableViewVerticalHeader*>(findVerticalHeader(row));
        if (!vHeader) {
            continue;
        }

        trackedit::LabelKey labelKey = vHeader->labelKey().key;
        labelKeysToRemove.push_back(labelKey);
        rowsToRemove.insert(row);
    }

    if (labelKeysToRemove.empty()) {
        return;
    }

    bool ok = trackeditInteraction()->removeLabels(labelKeysToRemove, true /* completed */);
    if (!ok) {
        return;
    }

    for (int row : rowsToRemove) {
        removeVerticalHeader(row);
        removeRow(row);
    }
}

void LabelsTableViewModel::exportLabels()
{
    muse::io::path_t exportPath = selectFileForExport();
    Ret ret = labelExporter()->exportData(exportPath);
    if (!ret) {
        LOGE() << ret.toString();
    } else {
        interactive()->revealInFileBrowser(exportPath);
    }
}

void LabelsTableViewModel::importLabels()
{
    muse::io::path_t importPath = selectFileForImport();
    Ret ret = labelsImporter()->importData(importPath);
    if (!ret) {
        LOGE() << ret.toString();
    } else {
        // reload
        load();
    }
}

bool LabelsTableViewModel::doCellValueChangeRequested(int row, int column, const Val& value)
{
    switch (column) {
    case TRACK_COLUMN: return moveLabel(row, column, value);
    case LABEL_COLUMN: return renameLabel(row, column, value);
    case START_TIME_COLUMN: return changeLabelStartTime(row, column, value);
    case END_TIME_COLUMN: return changeLabelEndTime(row, column, value);
    case LOW_FREQUENCY_COLUMN: return changeLabelLowFrequency(row, column, value);
    case HIGH_FREQUENCY_COLUMN: return changeLabelHighFrequency(row, column, value);
    default: break;
    }

    return false;
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

bool LabelsTableViewModel::moveLabel(int row, int column, const Val& value)
{
    Q_UNUSED(value);

    LabelsTableViewVerticalHeader* verticalHeader = dynamic_cast<LabelsTableViewVerticalHeader*>(findVerticalHeader(row));
    if (!verticalHeader) {
        return false;
    }

    const LabelsTableViewTrackCell* cell = dynamic_cast<LabelsTableViewTrackCell*>(findCell(row, column));
    if (!cell) {
        return false;
    }

    trackedit::LabelKey labelKey = verticalHeader->labelKey().key;

    muse::RetVal<trackedit::LabelKeyList> retVal = trackeditInteraction()->moveLabels({ labelKey }, cell->currentTrackId(),
                                                                                      true /* completed */);
    if (!retVal.ret) {
        return false;
    }

    labelKey.trackId = retVal.val.front().trackId;
    labelKey.itemId = retVal.val.front().itemId;

    verticalHeader->setLabelKey(labelKey);

    return true;
}

bool LabelsTableViewModel::renameLabel(int row, int column, const Val& value)
{
    Q_UNUSED(column);

    LabelsTableViewVerticalHeader* verticalHeader = dynamic_cast<LabelsTableViewVerticalHeader*>(findVerticalHeader(row));
    if (!verticalHeader) {
        return false;
    }

    trackedit::LabelKey labelKey = verticalHeader->labelKey().key;

    return trackeditInteraction()->changeLabelTitle(labelKey, String::fromStdString(value.toString()));
}

bool LabelsTableViewModel::changeLabelStartTime(int row, int column, const Val& value)
{
    LabelsTableViewVerticalHeader* verticalHeader = dynamic_cast<LabelsTableViewVerticalHeader*>(findVerticalHeader(row));
    if (!verticalHeader) {
        return false;
    }

    TableViewCell* cell = findCell(row, column);
    if (!cell) {
        return false;
    }

    trackedit::LabelKey labelKey = verticalHeader->labelKey().key;

    bool ok = trackeditInteraction()->stretchLabelLeft(labelKey, value.toDouble(), true /* completed */);
    if (ok) {
        //! NOTE: When stretching, the beginning and end may switch places, so let's update them
        const trackedit::ITrackeditProjectPtr project = globalContext()->currentTrackeditProject();

        const trackedit::Label actualLabel = project->label(labelKey);

        cell->setValue(Val(actualLabel.startTime));

        TableViewCell* endTimeCell = findCell(row, END_TIME_COLUMN);
        endTimeCell->setValue(Val(actualLabel.endTime));
    }

    return ok;
}

bool LabelsTableViewModel::changeLabelEndTime(int row, int column, const Val& value)
{
    LabelsTableViewVerticalHeader* verticalHeader = dynamic_cast<LabelsTableViewVerticalHeader*>(findVerticalHeader(row));
    if (!verticalHeader) {
        return false;
    }

    TableViewCell* cell = findCell(row, column);
    if (!cell) {
        return false;
    }

    trackedit::LabelKey labelKey = verticalHeader->labelKey().key;

    bool ok = trackeditInteraction()->stretchLabelRight(labelKey, value.toDouble(), true /* completed */);
    if (ok) {
        //! NOTE: When stretching, the beginning and end may switch places, so let's update them
        const trackedit::ITrackeditProjectPtr project = globalContext()->currentTrackeditProject();

        const trackedit::Label actualLabel = project->label(labelKey);

        TableViewCell* startTimeCell = findCell(row, START_TIME_COLUMN);
        startTimeCell->setValue(Val(actualLabel.startTime));

        cell->setValue(Val(actualLabel.endTime));
    }

    return ok;
}

bool LabelsTableViewModel::changeLabelLowFrequency(int row, int column, const Val& value)
{
    Q_UNUSED(column);

    LabelsTableViewVerticalHeader* verticalHeader = dynamic_cast<LabelsTableViewVerticalHeader*>(findVerticalHeader(row));
    if (!verticalHeader) {
        return false;
    }

    trackedit::LabelKey labelKey = verticalHeader->labelKey().key;

    return trackeditInteraction()->changeLabelLowFrequency(labelKey, value.toDouble());
}

bool LabelsTableViewModel::changeLabelHighFrequency(int row, int column, const Val& value)
{
    Q_UNUSED(column);

    LabelsTableViewVerticalHeader* verticalHeader = dynamic_cast<LabelsTableViewVerticalHeader*>(findVerticalHeader(row));
    if (!verticalHeader) {
        return false;
    }

    trackedit::LabelKey labelKey = verticalHeader->labelKey().key;

    return trackeditInteraction()->changeLabelHighFrequency(labelKey, value.toDouble());
}

io::path_t LabelsTableViewModel::selectFileForExport()
{
    std::vector<std::string> filter = labelExporter()->fileFilter();
    io::path_t defaultDir = exportConfiguration()->labelsDirectoryPath();

    io::path_t filePath = interactive()->selectSavingFileSync(muse::trc("global", "Open"), defaultDir, filter);

    if (!filePath.empty()) {
        exportConfiguration()->setLabelsDirectoryPath(io::dirpath(filePath));
    }

    return filePath;
}

io::path_t LabelsTableViewModel::selectFileForImport()
{
    std::vector<std::string> filter = labelsImporter()->fileFilter();
    io::path_t defaultDir = importConfiguration()->labelsDirectoryPath();

    io::path_t filePath = interactive()->selectOpeningFileSync(muse::trc("global", "Open"), defaultDir, filter);

    if (!filePath.empty()) {
        importConfiguration()->setLabelsDirectoryPath(io::dirpath(filePath));
    }

    return filePath;
}
