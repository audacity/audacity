/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QSortFilterProxyModel>
#include <QItemSelectionModel>

#include <vector>

namespace au::uicomponents {
// Table-aware sort/filter proxy. Unlike the list-oriented SortFilterProxyModel
// in the muse framework, this class sorts by column and supports a stable
// multi-key sort pipeline. Subclasses override acceptsRow() and compareCells()
// to plug in domain-specific filter predicates and per-column comparators.
class TableSortFilterProxyModel : public QSortFilterProxyModel
{
    Q_OBJECT

    Q_PROPERTY(QItemSelectionModel * selectionModel READ selectionModel CONSTANT)
    Q_PROPERTY(int rowCount READ rowCount NOTIFY rowCountChanged)

public:
    explicit TableSortFilterProxyModel(QObject* parent = nullptr);

    QItemSelectionModel* selectionModel() const;

    void setSourceModel(QAbstractItemModel* sourceModel) override;

    Q_INVOKABLE void toggleColumnSort(int column);
    Q_INVOKABLE void clearSort();
    Q_INVOKABLE void invalidateFilters();
    Q_INVOKABLE int mapRowToSource(int proxyRow) const;

signals:
    void rowCountChanged();

protected:
    bool filterAcceptsRow(int sourceRow, const QModelIndex& sourceParent) const override;
    bool lessThan(const QModelIndex& left, const QModelIndex& right) const override;

    // Return true to include sourceRow in the filtered view. Default: accept all.
    virtual bool acceptsRow(int sourceRow) const;

    // Per-column comparison, returning <0 / 0 / >0. The default extracts
    // TableViewCell values from AbstractTableViewModel-style sources and
    // compares them via muse::Val; override for domain-specific ordering.
    virtual int compareCells(int column, int leftSourceRow, int rightSourceRow) const;

    void setMaxSortKeys(int maxKeys) { m_maxSortKeys = maxKeys; }

private:
    void reapplySort();

    struct SortKey {
        int column = -1;
        bool ascending = true;
    };
    std::vector<SortKey> m_sortPipeline;

    QItemSelectionModel* m_selectionModel = nullptr;
    int m_maxSortKeys = 3;
};
}
