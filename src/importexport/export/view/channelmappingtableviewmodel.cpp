/*
* Audacity: A Digital Audio Editor
*/
#include "channelmappingtableviewmodel.h"

#include "framework/global/translation.h"
#include "framework/global/log.h"

#include "importexport/export/exportutils.h"

using namespace muse;
using namespace muse::uicomponents;

using namespace au::importexport;

ChannelMappingTableViewModel::ChannelMappingTableViewModel(QObject* parent)
    : AbstractTableViewModel(parent), muse::Injectable(muse::modularity::globalCtx())
{
}

void ChannelMappingTableViewModel::load(bool reload)
{
    setHorizontalHeaders(makeHorizontalHeaders());
    setVerticalHeaders(makeVerticalHeaders());
    if (!reload) {
        loadMatrixFromConfiguration();
    }
    setTable(makeTable());
}

void ChannelMappingTableViewModel::handleEdit(int row, int column)
{
    if (!isRowValid(row) || column < 0 || column >= m_channelCount) {
        return;
    }

    if (row >= m_matrix.size() || column >= m_matrix[row].size()) {
        return;
    }

    TableViewCell* cell = findCell(row, column);
    if (!cell) {
        return;
    }

    const bool current = cell->value().toBool();
    const bool next = !current;

    m_matrix[row][column] = next;

    cell->setValue(muse::Val(next));
}

void ChannelMappingTableViewModel::setChannelCount(int count)
{
    count = std::max(1, count);
    if (count == m_channelCount) {
        return;
    }
    m_channelCount = count;
}

void ChannelMappingTableViewModel::commitChanges()
{
    exportConfiguration()->setExportCustomChannelMapping(utils::matrixToVal(m_matrix));
}

QVector<TableViewHeader*> ChannelMappingTableViewModel::makeHorizontalHeaders()
{
    QVector<TableViewHeader*> h;
    for (int channel = 0; channel < m_channelCount; ++channel) {
        h << makeHorizontalHeader(
            QString::number(channel + 1),
            static_cast<TableViewCellType::Type>(ChannelMappingTableViewCellType::Type::Mapping),
            TableViewCellEditMode::Mode::StartInEdit,
            40);
    }

    return h;
}

QVector<TableViewHeader*> ChannelMappingTableViewModel::makeVerticalHeaders()
{
    rebuildChannelRows();

    QVector<TableViewHeader*> v;

    for (const auto& row : m_rows) {
        auto* vh = new muse::uicomponents::TableViewHeader(this);
        vh->setTitle(row.title);
        v << vh;
    }

    return v;
}

QVector<QVector<TableViewCell*> > ChannelMappingTableViewModel::makeTable()
{
    using muse::Val;
    using muse::uicomponents::TableViewCell;

    const int rows = m_rows.size();
    const int cols = std::max(1, m_channelCount);

    std::vector<std::vector<bool> > newMatrix(rows, std::vector<bool>(cols, false));

    const int oldRows = m_matrix.size();
    const int rowLimit = std::min(rows, oldRows);

    for (int rIdx = 0; rIdx < rowLimit; ++rIdx) {
        const int oldCols = m_matrix[rIdx].size();
        const int colLimit = std::min(cols, oldCols);

        for (int cIdx = 0; cIdx < colLimit; ++cIdx) {
            newMatrix[rIdx][cIdx] = m_matrix[rIdx][cIdx];
        }
    }

    m_matrix = std::move(newMatrix);

    QVector<QVector<TableViewCell*> > table;
    table.reserve(rows);

    for (int rIdx = 0; rIdx < rows; ++rIdx) {
        QVector<TableViewCell*> row;
        row.reserve(cols);

        for (int cIdx = 0; cIdx < cols; ++cIdx) {
            row.append(makeCell(Val(m_matrix[rIdx][cIdx])));
        }

        table.append(row);
    }

    return table;
}

bool ChannelMappingTableViewModel::doCellValueChangeRequested(int row, int column, const Val& value)
{
    Q_UNUSED(row);
    Q_UNUSED(column);
    Q_UNUSED(value);
    return true;
}

void ChannelMappingTableViewModel::loadMatrixFromConfiguration()
{
    const int rows = m_rows.size();
    const int cols = std::max(1, m_channelCount);

    const muse::Val val = exportConfiguration()->exportCustomChannelMapping();

    const auto saved = utils::valToMatrix(val);

    m_matrix.assign(rows, std::vector<bool>(cols, false));

    const int rowLimit = std::min(rows, static_cast<int>(saved.size()));
    for (int rIdx = 0; rIdx < rowLimit; ++rIdx) {
        const int colLimit = std::min(cols, static_cast<int>(saved[static_cast<size_t>(rIdx)].size()));
        for (int cIdx = 0; cIdx < colLimit; ++cIdx) {
            m_matrix[rIdx][cIdx] = saved[rIdx][cIdx];
        }
    }
}

void ChannelMappingTableViewModel::rebuildChannelRows()
{
    const auto project = globalContext()->currentTrackeditProject();
    m_rows.clear();

    if (!project) {
        return;
    }

    for (const auto& track : project->trackList()) {
        if (track.type == trackedit::TrackType::Label) {
            continue;
        }

        const QString baseName = track.title.toQString();
        const bool isStereo = (track.type == trackedit::TrackType::Stereo);

        if (!isStereo) {
            ChannelRow m;
            m.track = track;
            m.title = baseName;
            m_rows.push_back(std::move(m));
        } else {
            ChannelRow l;
            l.track = track;
            l.title = baseName + " L";
            m_rows.push_back(std::move(l));

            ChannelRow r;
            r.track = track;
            r.title = baseName + " R";
            m_rows.push_back(std::move(r));
        }
    }
}
