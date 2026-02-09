/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <qqmlintegration.h>

#include "framework/global/modularity/ioc.h"
#include "framework/interactive/iinteractive.h"

#include "context/iglobalcontext.h"
#include "trackedit/itrackeditinteraction.h"
#include "trackedit/ilabelsinteraction.h"
#include "importexport/labels/ilabelsexporter.h"
#include "importexport/labels/ilabelsimporter.h"
#include "importexport/labels/ilabelsconfiguration.h"
#include "iprojectsceneconfiguration.h"

#include "uicomponents/qml/Muse/UiComponents/abstracttableviewmodel.h"

#include "trackedit/trackedittypes.h"

namespace au::projectscene {
namespace LabelsTableViewCellType {
Q_NAMESPACE;
QML_ELEMENT;

enum class Type {
    Track = static_cast<int>(muse::uicomponents::TableViewCellType::Type::UserType) + 1,
    Timecode,
    Frequency
};

Q_ENUM_NS(Type)
}

class LabelsTableViewModel : public muse::uicomponents::AbstractTableViewModel, public muse::Injectable
{
    Q_OBJECT
    QML_ELEMENT;

    muse::GlobalInject<importexport::ILabelsConfiguration> labelsImportExportConfiguration;
    muse::GlobalInject<IProjectSceneConfiguration> configuration;

    muse::Inject<muse::IInteractive> interactive = { this };
    muse::Inject<context::IGlobalContext> globalContext = { this };
    muse::Inject<trackedit::ITrackeditInteraction> trackeditInteraction = { this };
    muse::Inject<trackedit::ILabelsInteraction> labelsInteraction = { this };
    muse::Inject<importexport::ILabelsExporter> labelExporter = { this };
    muse::Inject<importexport::ILabelsImporter> labelsImporter = { this };

public:
    explicit LabelsTableViewModel(QObject* parent = nullptr);

    Q_INVOKABLE void load();

    Q_INVOKABLE void handleTrackMenuItem(int row, int column, const QString& itemId);

    Q_INVOKABLE void addNewLabel();
    Q_INVOKABLE void removeSelectedLabels();

    Q_INVOKABLE void exportLabels();
    Q_INVOKABLE void importLabels();

private:
    bool doCellValueChangeRequested(int row, int column, const muse::Val& value) override;

    QVector<muse::uicomponents::TableViewHeader*> makeHorizontalHeaders();
    QVector<muse::uicomponents::TableViewHeader*> makeVerticalHeaders();
    QVector<QVector<muse::uicomponents::TableViewCell*> > makeTable();

    QString labelEditorColumnFormat(const std::string& columnName, int defaultValue) const;
    void connectToColumnFormatChange(muse::uicomponents::TableViewHeader* columnHeader, const std::string& columnName);

    std::vector<trackedit::Track> allLabelTracks() const;

    muse::uicomponents::TableViewCell* makeTrackCell(const trackedit::TrackId& trackId, const QString& trackTitle);
    muse::uicomponents::TableViewCell* makeTimecodeCell(const muse::Val& value);

    muse::uicomponents::MenuItemList makeAvailableTracksList();
    muse::uicomponents::MenuItem* makeSeparator();

    bool moveLabel(int row, const muse::Val& value);
    bool renameLabel(int row, const muse::Val& value);
    bool changeLabelStartTime(int row, const muse::Val& value);
    bool changeLabelEndTime(int row, const muse::Val& value);
    bool changeLabelLowFrequency(int row, const muse::Val& value);
    bool changeLabelHighFrequency(int row, const muse::Val& value);

    muse::RetVal<QString> createNewLabelTrack(int currentRow);

    muse::io::path_t selectFileForExport();
    muse::io::path_t selectFileForImport();
};
}
