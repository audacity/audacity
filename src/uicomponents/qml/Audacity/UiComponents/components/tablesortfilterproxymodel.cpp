/*
 * Audacity: A Digital Audio Editor
 */
#include "tablesortfilterproxymodel.h"

#include "framework/global/types/val.h"
#include "framework/uicomponents/qml/Muse/UiComponents/itemmultiselectionmodel.h"
#include "framework/uicomponents/qml/Muse/UiComponents/internal/tableviewcell.h"
#include "framework/uicomponents/view/modelutils.h"

namespace au::uicomponents {
TableSortFilterProxyModel::TableSortFilterProxyModel(QObject* parent)
    : QSortFilterProxyModel(parent),
    m_selectionModel(new muse::uicomponents::ItemMultiSelectionModel(this))
{
    m_selectionModel->setModel(this);
    setDynamicSortFilter(true);
    muse::uicomponents::ModelUtils::connectRowCountChangedSignal(this, &TableSortFilterProxyModel::rowCountChanged);
}

QItemSelectionModel* TableSortFilterProxyModel::selectionModel() const
{
    return m_selectionModel;
}

void TableSortFilterProxyModel::setSourceModel(QAbstractItemModel* sourceModel)
{
    QSortFilterProxyModel::setSourceModel(sourceModel);
    if (sourceModel) {
        const int cols = sourceModel->columnCount();
        m_sortPipeline.erase(
            std::remove_if(m_sortPipeline.begin(), m_sortPipeline.end(),
                           [cols](const SortKey& k) { return k.column < 0 || k.column >= cols; }),
            m_sortPipeline.end());
    }
    reapplySort();
}

void TableSortFilterProxyModel::toggleColumnSort(int column)
{
    auto it = std::find_if(m_sortPipeline.begin(), m_sortPipeline.end(),
                           [column](const SortKey& k) { return k.column == column; });

    if (it == m_sortPipeline.end()) {
        m_sortPipeline.push_back({ column, true });
        if (static_cast<int>(m_sortPipeline.size()) > m_maxSortKeys) {
            m_sortPipeline.erase(m_sortPipeline.begin());
        }
    } else {
        const bool ascending = !it->ascending;
        m_sortPipeline.erase(it);
        m_sortPipeline.push_back({ column, ascending });
    }

    reapplySort();
}

void TableSortFilterProxyModel::clearSort()
{
    if (m_sortPipeline.empty()) {
        return;
    }
    m_sortPipeline.clear();
    reapplySort();
}

void TableSortFilterProxyModel::invalidateFilters()
{
    beginFilterChange();
#if QT_VERSION >= QT_VERSION_CHECK(6, 10, 0)
    endFilterChange(QSortFilterProxyModel::Direction::Rows);
#else
    invalidateFilter();
#endif
}

int TableSortFilterProxyModel::mapRowToSource(int proxyRow) const
{
    const QModelIndex idx = mapToSource(index(proxyRow, 0));
    return idx.isValid() ? idx.row() : -1;
}

void TableSortFilterProxyModel::reapplySort()
{
    if (!sourceModel()) {
        return;
    }
    if (m_sortPipeline.empty()) {
        sort(-1);
        return;
    }
    invalidate();
    sort(0, Qt::AscendingOrder);
}

bool TableSortFilterProxyModel::filterAcceptsRow(int sourceRow, const QModelIndex& sourceParent) const
{
    Q_UNUSED(sourceParent);
    return acceptsRow(sourceRow);
}

bool TableSortFilterProxyModel::lessThan(const QModelIndex& left, const QModelIndex& right) const
{
    for (auto it = m_sortPipeline.rbegin(); it != m_sortPipeline.rend(); ++it) {
        const int cmp = compareCells(it->column, left.row(), right.row());
        if (cmp == 0) {
            continue;
        }
        return it->ascending ? (cmp < 0) : (cmp > 0);
    }
    return false;
}

bool TableSortFilterProxyModel::acceptsRow(int sourceRow) const
{
    Q_UNUSED(sourceRow);
    return true;
}

int TableSortFilterProxyModel::compareCells(int column, int leftSourceRow, int rightSourceRow) const
{
    QAbstractItemModel* src = sourceModel();
    if (!src) {
        return 0;
    }
    const QModelIndex leftIdx = src->index(leftSourceRow, column);
    const QModelIndex rightIdx = src->index(rightSourceRow, column);
    const QVariant leftData = src->data(leftIdx);
    const QVariant rightData = src->data(rightIdx);

    muse::Val leftVal;
    muse::Val rightVal;
    if (leftData.canConvert<muse::uicomponents::TableViewCell*>()
        && rightData.canConvert<muse::uicomponents::TableViewCell*>()) {
        auto* l = leftData.value<muse::uicomponents::TableViewCell*>();
        auto* r = rightData.value<muse::uicomponents::TableViewCell*>();
        if (l && r) {
            leftVal = l->value();
            rightVal = r->value();
        }
    } else {
        leftVal = muse::Val::fromQVariant(leftData);
        rightVal = muse::Val::fromQVariant(rightData);
    }

    if (leftVal < rightVal) {
        return -1;
    }
    if (rightVal < leftVal) {
        return 1;
    }
    return 0;
}
}
