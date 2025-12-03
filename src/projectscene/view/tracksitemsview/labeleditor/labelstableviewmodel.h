/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <qqmlintegration.h>

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "trackedit/itrackeditinteraction.h"

#include "uicomponents/qml/Muse/UiComponents/abstracttableviewmodel.h"

#include "trackedit/trackedittypes.h"

namespace au::projectscene {
namespace LabelsTableViewCellType {
Q_NAMESPACE;
QML_ELEMENT;

enum class Type {
    Track = static_cast<int>(muse::uicomponents::TableViewCellType::Type::UserType) + 1,
    Timecode
};

Q_ENUM_NS(Type)
}

class LabelsTableViewModel : public muse::uicomponents::AbstractTableViewModel, public muse::Injectable
{
    Q_OBJECT
    QML_ELEMENT;

    muse::Inject<context::IGlobalContext> globalContext = { this };
    muse::Inject<trackedit::ITrackeditInteraction> trackeditInteraction = { this };

public:
    explicit LabelsTableViewModel(QObject* parent = nullptr);

    Q_INVOKABLE void load();

    Q_INVOKABLE void handleTrackMenuItem(int row, int column, const QString& itemId);

private:
    void doCellValueChanged(int row, int column) override;

    QVector<muse::uicomponents::TableViewHeader*> makeHorizontalHeaders();
    QVector<muse::uicomponents::TableViewHeader*> makeVerticalHeaders();
    QVector<QVector<muse::uicomponents::TableViewCell*>> makeTable();

    std::vector<trackedit::Track> allLabelTracks() const;

    muse::uicomponents::TableViewCell* makeTrackCell(const trackedit::TrackId& trackId, const QString& trackTitle);
    muse::uicomponents::TableViewCell* makeTimecodeCell(const muse::Val& value);

    muse::uicomponents::MenuItemList makeAvailableTracksList(const trackedit::TrackId& currentTrackId);
    muse::uicomponents::MenuItem* makeSeparator();

    void moveLabel(int row, int column);
    void renameLabel(int row, int column);
    void changeLabelStartTime(int row, int column);
    void changeLabelEndTime(int row, int column);
    void changeLabelLowFrequency(int row, int column);
    void changeLabelHighFrequency(int row, int column);
};
}
