/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "uicomponents/qml/Muse/UiComponents/abstracttableviewmodel.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "internal/exportconfiguration.h"

namespace au::importexport {
namespace ChannelMappingTableViewCellType {
Q_NAMESPACE;
QML_ELEMENT;

enum class Type {
    Mapping = static_cast<int>(muse::uicomponents::TableViewCellType::Type::UserType) + 1,
};

Q_ENUM_NS(Type);
}

class ChannelMappingTableViewModel : public muse::uicomponents::AbstractTableViewModel, public muse::Injectable
{
    Q_OBJECT
    QML_ELEMENT;

    muse::Inject<context::IGlobalContext> globalContext = { this };
    muse::GlobalInject<importexport::ExportConfiguration> exportConfiguration;

public:
    explicit ChannelMappingTableViewModel(QObject* parent = nullptr);

    Q_INVOKABLE void load(bool reload = false);
    Q_INVOKABLE void handleEdit(int row, int column);
    Q_INVOKABLE void setChannelCount(int count);
    Q_INVOKABLE void commitChanges();

private:
    QVector<muse::uicomponents::TableViewHeader*> makeHorizontalHeaders();
    QVector<muse::uicomponents::TableViewHeader*> makeVerticalHeaders();
    QVector<QVector<muse::uicomponents::TableViewCell*> > makeTable();

    bool doCellValueChangeRequested(int row, int column, const muse::Val& value) override;
    void loadMatrixFromConfiguration();

    std::vector<trackedit::Track> m_tracks;
    std::vector<std::vector<bool> > m_matrix;
    int m_channelCount = 1;
};
}
