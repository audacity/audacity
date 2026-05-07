/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "uicomponents/components/tablesortfilterproxymodel.h"

namespace au::effects {
class PluginManagerTableViewModel;

class PluginManagerSortFilterProxy : public au::uicomponents::TableSortFilterProxyModel
{
    Q_OBJECT

public:
    explicit PluginManagerSortFilterProxy(PluginManagerTableViewModel* view);

protected:
    bool acceptsRow(int sourceRow) const override;
    int compareCells(int column, int leftSourceRow, int rightSourceRow) const override;

private:
    PluginManagerTableViewModel* m_view = nullptr;
};
}
