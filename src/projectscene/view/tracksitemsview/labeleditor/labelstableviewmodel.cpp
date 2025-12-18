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
static const std::string START_TIME_COLUMN_NAME = "startTime";

static const int END_TIME_COLUMN = 3;
static const std::string END_TIME_COLUMN_NAME = "endTime";

static const int LOW_FREQUENCY_COLUMN = 4;
static const std::string LOW_FREQUENCY_COLUMN_NAME = "lowFrequency";

static const int HIGH_FREQUENCY_COLUMN = 5;
static const std::string HIGH_FREQUENCY_COLUMN_NAME = "highFrequency";

using namespace au::projectscene;
using namespace muse;
using namespace muse::ui;
using namespace muse::uicomponents;

static std::vector<std::string> importExportFilter(const std::vector<au::importexport::FileFilter>& fileFilters)
{
    std::vector<std::string> result;
    for (const au::importexport::FileFilter& fileFilter : fileFilters) {
        result.push_back(fileFilter.title);
    }
    return result;
}

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

    static auto startTimeModelStub = au::uicomponents::TimecodeModel();
    MenuItemList startTimeFormats = startTimeModelStub.availableFormats();

    TableViewHeader* startTimeHeader = makeHorizontalHeader(qtrc("projectscene", "Start time"),
                                                            static_cast<TableViewCellType::Type>(LabelsTableViewCellType::Type::Timecode),
                                                            TableViewCellEditMode::Mode::StartInEdit);
    startTimeHeader->setAvailableFormats(startTimeFormats);

    startTimeHeader->setCurrentFormatId(labelEditorColumnFormat(START_TIME_COLUMN_NAME, startTimeModelStub.currentFormat()));
    connectToColumnFormatChange(startTimeHeader, START_TIME_COLUMN_NAME);
    hHeaders << startTimeHeader;

    static auto endTimeModelStub = au::uicomponents::TimecodeModel();
    MenuItemList endTimeFormats = endTimeModelStub.availableFormats();

    TableViewHeader* endTimeHeader = makeHorizontalHeader(qtrc("projectscene", "End time"),
                                                          static_cast<TableViewCellType::Type>(LabelsTableViewCellType::Type::Timecode),
                                                          TableViewCellEditMode::Mode::StartInEdit);
    endTimeHeader->setAvailableFormats(endTimeFormats);
    endTimeHeader->setCurrentFormatId(labelEditorColumnFormat(END_TIME_COLUMN_NAME, endTimeModelStub.currentFormat()));
    connectToColumnFormatChange(endTimeHeader, END_TIME_COLUMN_NAME);
    hHeaders << endTimeHeader;

    static auto lowFrequencyModelStub = au::uicomponents::FrequencyModel();
    MenuItemList lowFrequencyFormats = lowFrequencyModelStub.availableFormats();

    TableViewHeader* lowFrequencyHeader = makeHorizontalHeader(qtrc("projectscene",
                                                                    "Low frequency"),
                                                               static_cast<TableViewCellType::Type>(LabelsTableViewCellType::Type::Frequency),
                                                               TableViewCellEditMode::Mode::StartInEdit);
    lowFrequencyHeader->setAvailableFormats(lowFrequencyFormats);
    lowFrequencyHeader->setCurrentFormatId(labelEditorColumnFormat(LOW_FREQUENCY_COLUMN_NAME, lowFrequencyModelStub.currentFormat()));
    connectToColumnFormatChange(lowFrequencyHeader, LOW_FREQUENCY_COLUMN_NAME);
    hHeaders << lowFrequencyHeader;

    static auto highFrequencyModelStub = au::uicomponents::FrequencyModel();
    MenuItemList highFrequencyFormats = highFrequencyModelStub.availableFormats();

    TableViewHeader* highFrequencyHeader = makeHorizontalHeader(qtrc("projectscene", "High frequency"),
                                                                static_cast<TableViewCellType::Type>(LabelsTableViewCellType::Type::
                                                                                                     Frequency),
                                                                TableViewCellEditMode::Mode::StartInEdit);
    highFrequencyHeader->setAvailableFormats(highFrequencyFormats);
    highFrequencyHeader->setCurrentFormatId(labelEditorColumnFormat(HIGH_FREQUENCY_COLUMN_NAME, highFrequencyModelStub.currentFormat()));
    connectToColumnFormatChange(highFrequencyHeader, HIGH_FREQUENCY_COLUMN_NAME);
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

QString LabelsTableViewModel::labelEditorColumnFormat(const std::string& columnName, int defaultValue) const
{
    int result = configuration()->labelEditorColumnFormat(columnName);
    return QString::number(result == -1 ? defaultValue : result);
}

void LabelsTableViewModel::connectToColumnFormatChange(muse::uicomponents::TableViewHeader* columnHeader, const std::string& columnName)
{
    connect(columnHeader, &TableViewHeader::currentFormatIdChanged, [=](){
        configuration()->setLabelEditorColumnFormat(columnName, columnHeader->currentFormatId().toInt());
    });
}

void LabelsTableViewModel::handleTrackMenuItem(int row, int column, const QString& itemId)
{
    if (!isRowValid(row) || !isColumnValid(column)) {
        return;
    }

    QString labelTrackItemId = itemId;

    if (labelTrackItemId == NEW_LABEL_TRACK_CODE) {
        labelTrackItemId = createNewLabelTrack(row);
    }

    if (labelTrackItemId.isEmpty()) {
        return;
    }

    LabelsTableViewTrackCell* trackCell = dynamic_cast<LabelsTableViewTrackCell*>(findCell(row, column));

    for (const muse::uicomponents::MenuItem* item : trackCell->availableTracks()) {
        if (item->id() == labelTrackItemId) {
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
        createNewLabelTrack(-1);
        addNewLabel();
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
    std::vector<int> rowsToRemove;

    for (const QModelIndex& index : selectedIndexes) {
        int row = index.row();

        LabelsTableViewVerticalHeader* vHeader = dynamic_cast<LabelsTableViewVerticalHeader*>(findVerticalHeader(row));
        if (!vHeader) {
            continue;
        }

        trackedit::LabelKey labelKey = vHeader->labelKey().key;
        if (muse::contains(labelKeysToRemove, labelKey)) {
            continue;
        }

        labelKeysToRemove.push_back(labelKey);
        rowsToRemove.push_back(row);
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
    case TRACK_COLUMN: return moveLabel(row, value);
    case LABEL_COLUMN: return renameLabel(row, value);
    case START_TIME_COLUMN: return changeLabelStartTime(row, value);
    case END_TIME_COLUMN: return changeLabelEndTime(row, value);
    case LOW_FREQUENCY_COLUMN: return changeLabelLowFrequency(row, value);
    case HIGH_FREQUENCY_COLUMN: return changeLabelHighFrequency(row, value);
    default: break;
    }

    return false;
}

TableViewCell* LabelsTableViewModel::makeTrackCell(const trackedit::TrackId& trackId, const QString& trackTitle)
{
    LabelsTableViewTrackCell* result = new LabelsTableViewTrackCell(makeCell(Val(trackTitle)));

    //! NOTE: The order is important.
    //! Setting the current track ID updates the selected item in the list of available tracks.
    result->setAvailableTracks(makeAvailableTracksList());
    result->setCurrentTrackId(trackId);

    //! NOTE: Update the track title in the cell.
    //! Keep the title synchronized with the track from the list of available tracks.
    for (const MenuItem* item : result->availableTracks()) {
        if (item->args().arg<trackedit::TrackId>() == trackId) {
            result->setValue_property(item->translatedTitle());
            break;
        }
    }

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

MenuItemList LabelsTableViewModel::makeAvailableTracksList()
{
    std::vector<trackedit::Track> labelTracks = allLabelTracks();
    if (labelTracks.empty()) {
        return {};
    }

    MenuItemList result;

    int index = 0;
    for (const trackedit::Track& track : labelTracks) {
        MenuItem* item = new MenuItem();

        ui::UiAction action;
        action.code = SELECT_LABEL_TRACK_CODE;
        action.title = TranslatableString::untranslatable(String::number(index) + " - " + track.title);
        action.checkable = muse::ui::Checkable::Yes;
        item->setAction(action);

        ui::UiActionState state;
        state.enabled = true;
        item->setState(state);

        item->setArgs(actions::ActionData::make_arg1(track.id));
        item->setId(QString::fromStdString(action.code) + action.title.qTranslatedWithoutMnemonic());

        result << item;
        index++;
    }

    result << makeSeparator();

    MenuItem* item = new MenuItem();

    ui::UiAction action;
    action.code = NEW_LABEL_TRACK_CODE;
    action.title = TranslatableString("projectscene", "New label track");
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

bool LabelsTableViewModel::moveLabel(int row, const Val& value)
{
    Q_UNUSED(value);

    LabelsTableViewVerticalHeader* verticalHeader = dynamic_cast<LabelsTableViewVerticalHeader*>(findVerticalHeader(row));
    if (!verticalHeader) {
        return false;
    }

    const LabelsTableViewTrackCell* cell = dynamic_cast<LabelsTableViewTrackCell*>(findCell(row, TRACK_COLUMN));
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

bool LabelsTableViewModel::renameLabel(int row, const Val& value)
{
    LabelsTableViewVerticalHeader* verticalHeader = dynamic_cast<LabelsTableViewVerticalHeader*>(findVerticalHeader(row));
    if (!verticalHeader) {
        return false;
    }

    trackedit::LabelKey labelKey = verticalHeader->labelKey().key;

    return trackeditInteraction()->changeLabelTitle(labelKey, String::fromStdString(value.toString()));
}

bool LabelsTableViewModel::changeLabelStartTime(int row, const Val& value)
{
    LabelsTableViewVerticalHeader* verticalHeader = dynamic_cast<LabelsTableViewVerticalHeader*>(findVerticalHeader(row));
    if (!verticalHeader) {
        return false;
    }

    TableViewCell* cell = findCell(row, START_TIME_COLUMN);
    if (!cell) {
        return false;
    }

    trackedit::LabelKey labelKey = verticalHeader->labelKey().key;

    TableViewCell* endTimeCell = findCell(row, END_TIME_COLUMN);

    //! NOTE: correct the right side so that the left side is not higher than the right
    if (muse::RealIsEqualOrLess(endTimeCell->value().toDouble(), value.toDouble())) {
        //! NOTE: without push to history
        labelsInteraction()->stretchLabelRight(labelKey, value.toDouble(), true /* completed */);
        endTimeCell->setValue(value);
    }

    return trackeditInteraction()->stretchLabelLeft(labelKey, value.toDouble(), true /* completed */);
}

bool LabelsTableViewModel::changeLabelEndTime(int row, const Val& value)
{
    LabelsTableViewVerticalHeader* verticalHeader = dynamic_cast<LabelsTableViewVerticalHeader*>(findVerticalHeader(row));
    if (!verticalHeader) {
        return false;
    }

    TableViewCell* cell = findCell(row, END_TIME_COLUMN);
    if (!cell) {
        return false;
    }

    trackedit::LabelKey labelKey = verticalHeader->labelKey().key;

    TableViewCell* startTimeCell = findCell(row, START_TIME_COLUMN);

    //! NOTE: correct the left side so that the right side is not lower than the left
    if (muse::RealIsEqualOrMore(startTimeCell->value().toDouble(), value.toDouble())) {
        //! NOTE: without push to history
        labelsInteraction()->stretchLabelLeft(labelKey, value.toDouble(), true /* completed */);
        startTimeCell->setValue(value);
    }

    return trackeditInteraction()->stretchLabelRight(labelKey, value.toDouble(), true /* completed */);
}

bool LabelsTableViewModel::changeLabelLowFrequency(int row, const Val& value)
{
    LabelsTableViewVerticalHeader* verticalHeader = dynamic_cast<LabelsTableViewVerticalHeader*>(findVerticalHeader(row));
    if (!verticalHeader) {
        return false;
    }

    trackedit::LabelKey labelKey = verticalHeader->labelKey().key;

    return trackeditInteraction()->changeLabelLowFrequency(labelKey, value.toDouble());
}

bool LabelsTableViewModel::changeLabelHighFrequency(int row, const Val& value)
{
    LabelsTableViewVerticalHeader* verticalHeader = dynamic_cast<LabelsTableViewVerticalHeader*>(findVerticalHeader(row));
    if (!verticalHeader) {
        return false;
    }

    trackedit::LabelKey labelKey = verticalHeader->labelKey().key;

    return trackeditInteraction()->changeLabelHighFrequency(labelKey, value.toDouble());
}

QString LabelsTableViewModel::createNewLabelTrack(int currentRow)
{
    RetVal<muse::Val> rv = interactive()->openSync("audacity://projectscene/addnewlabeltrack");
    if (!rv.ret) {
        return QString();
    }

    trackedit::TrackId newTrackId = rv.val.toInt();

    QString newLabelTrackItemId;

    for (int i = 0; i < rowCount(); i++) {
        LabelsTableViewTrackCell* trackCell = dynamic_cast<LabelsTableViewTrackCell*>(findCell(i, TRACK_COLUMN));
        if (!trackCell) {
            continue;
        }

        MenuItemList availableTracks = makeAvailableTracksList();
        trackCell->setAvailableTracks(availableTracks);

        if (currentRow == i) {
            for (const MenuItem* item : availableTracks) {
                if (item->args().arg<trackedit::TrackId>() == newTrackId) {
                    newLabelTrackItemId = item->id();
                    break;
                }
            }
        }
    }

    return newLabelTrackItemId;
}

io::path_t LabelsTableViewModel::selectFileForExport()
{
    std::vector<std::string> filter = importExportFilter(labelsImportExportConfiguration()->fileFilter());
    io::path_t defaultDir = labelsImportExportConfiguration()->labelsDirectoryPath();

    io::path_t filePath = interactive()->selectSavingFileSync(muse::trc("global", "Save"), defaultDir, filter);

    if (!filePath.empty()) {
        labelsImportExportConfiguration()->setLabelsDirectoryPath(io::dirpath(filePath));
    }

    return filePath;
}

io::path_t LabelsTableViewModel::selectFileForImport()
{
    std::vector<std::string> filter = importExportFilter(labelsImportExportConfiguration()->fileFilter());
    io::path_t defaultDir = labelsImportExportConfiguration()->labelsDirectoryPath();

    io::path_t filePath = interactive()->selectOpeningFileSync(muse::trc("global", "Open"), defaultDir, filter);

    if (!filePath.empty()) {
        labelsImportExportConfiguration()->setLabelsDirectoryPath(io::dirpath(filePath));
    }

    return filePath;
}
