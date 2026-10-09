/*
 * Audacity: A Digital Audio Editor
 */
#include "autorecoverymodel.h"

#include "framework/global/async/async.h"

#include "framework/global/translation.h"
#include "framework/uicomponents/qml/Muse/UiComponents/internal/tableviewcell.h"
#include "framework/uicomponents/qml/Muse/UiComponents/internal/tableviewheader.h"

#include "project/types/projecttypes.h"

using namespace au::appshell;
using namespace muse::uicomponents;

namespace {
constexpr int selectedColumnWidth = 80;
constexpr int nameColumnWidth = 526;
}

AutoRecoveryModel::AutoRecoveryModel(QObject* parent)
    : AbstractTableViewModel(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void AutoRecoveryModel::componentComplete()
{
    //: Column header of the automatic crash recovery dialog. Here "Select" is a verb.
    const QString selectTitle = muse::qtrc("appshell/autorecovery", "Select");
    //: Column header of the automatic crash recovery dialog. Here "Name" is a noun.
    const QString nameTitle = muse::qtrc("appshell/autorecovery", "Name");

    setHorizontalHeaders({
        makeHorizontalHeader(selectTitle, static_cast<TableViewCellType::Type>(AutoRecoveryTableViewCellType::Type::Selected),
                             TableViewCellEditMode::Mode::StartInEdit, selectedColumnWidth),
        makeHorizontalHeader(nameTitle, TableViewCellType::Type::String, TableViewCellEditMode::Mode::DoubleClick, nameColumnWidth),
    });

    m_items.clear();
    for (const muse::io::path_t& path : sessionsManager()->projectsForRestore()) {
        m_items.push_back({ path, true });
    }

    rebuildTable();
}

void AutoRecoveryModel::rebuildTable()
{
    // Not shown, but StyledTableView expects one per row
    QVector<TableViewHeader*> verticalHeaders;
    QVector<QVector<TableViewCell*> > table;
    for (const Item& item : m_items) {
        verticalHeaders << new TableViewHeader(this);

        QVector<TableViewCell*> row;
        row.append(makeCell(muse::Val(item.selected)));
        row.append(makeCell(muse::Val(muse::io::completeBasename(item.path).toQString())));
        table.append(row);
    }
    setVerticalHeaders(verticalHeaders);
    setTable(table);

    emit selectionChanged();
}

bool AutoRecoveryModel::hasSelection() const
{
    return std::any_of(m_items.begin(), m_items.end(), [](const Item& item) { return item.selected; });
}

muse::io::paths_t AutoRecoveryModel::selectedProjects() const
{
    muse::io::paths_t projects;
    for (const Item& item : m_items) {
        if (item.selected) {
            projects.push_back(item.path);
        }
    }
    return projects;
}

void AutoRecoveryModel::recoverSelectedAsync()
{
    muse::async::Async::call(nullptr, [sessionsManager = sessionsManager(), projects = selectedProjects()]() {
        sessionsManager->restore(projects);
    });
}

void AutoRecoveryModel::setSelected(int row, bool selected)
{
    m_items[row].selected = selected;

    TableViewCell* const cell = findCell(row, s_selectedColumn);
    IF_ASSERT_FAILED(cell) {
        return;
    }
    cell->setValue(muse::Val(selected));

    const QModelIndex idx = index(row, s_selectedColumn);
    emit dataChanged(idx, idx);
}

void AutoRecoveryModel::toggleSelected(int row)
{
    if (row < 0 || row >= static_cast<int>(m_items.size())) {
        return;
    }

    setSelected(row, !m_items[row].selected);
    emit selectionChanged();
}

void AutoRecoveryModel::invertSelection()
{
    for (int row = 0; row < static_cast<int>(m_items.size()); ++row) {
        setSelected(row, !m_items[row].selected);
    }
    emit selectionChanged();
}

void AutoRecoveryModel::discardSelected()
{
    muse::io::paths_t toDiscard;
    bool hasUnsavedProjects = false;
    for (const Item& item : m_items) {
        if (item.selected) {
            toDiscard.push_back(item.path);
            hasUnsavedProjects = hasUnsavedProjects || project::isAudacityUnsavedFile(item.path);
        }
    }

    if (toDiscard.empty()) {
        return;
    }

    const auto doDiscard = [this, toDiscard]() {
        sessionsManager()->discard(toDiscard);

        m_items.erase(std::remove_if(m_items.begin(), m_items.end(), [](const Item& item) {
            return item.selected;
        }), m_items.end());
        rebuildTable();

        if (m_items.empty()) {
            emit allProjectsDiscarded();
        }
    };

    // Discarding a project that was saved before only reverts it to its last save, hence no need to ask.
    if (!hasUnsavedProjects) {
        doDiscard();
        return;
    }

    auto promise = interactive()->question(
        muse::trc("appshell/autorecovery", "Are you sure you want to discard the selected projects?"),
        muse::trc("appshell/autorecovery", "Choosing \"Yes\" permanently deletes the selected projects immediately."),
        { muse::IInteractive::Button::No, muse::IInteractive::Button::Yes },
        muse::IInteractive::Button::No, {},
        muse::trc("appshell/autorecovery", "Automatic crash recovery"));

    promise.onResolve(this, [doDiscard](const muse::IInteractive::Result& res) {
        if (res.isButton(muse::IInteractive::Button::Yes)) {
            doDiscard();
        }
    });
}
