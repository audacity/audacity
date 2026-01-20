#include "workspacelayoutpagemodel.h"

#include "global/translation.h"

using namespace au::appshell;
using namespace muse;

WorkspaceLayoutPageModel::WorkspaceLayoutPageModel(QObject* parent)
    : QObject(parent), muse::Injectable(muse::iocCtxForQmlObject(this))
{
#ifdef MUSE_MODULE_WORKSPACE
    // Listen to workspace changes to update selection
    m_workspaceManager()->currentWorkspaceChanged().onNotify(this, [this]() {
        updateWorkspaces();
    });
#endif
}

void WorkspaceLayoutPageModel::load()
{
    // Initialize with workspace options
    m_workspaces.clear();

#ifdef MUSE_MODULE_WORKSPACE
    // Get current workspace from workspace manager
    std::string currentWorkspaceName;
    if (m_workspaceManager() && m_workspaceManager()->currentWorkspace()) {
        currentWorkspaceName = m_workspaceManager()->currentWorkspace()->name();
    }
#endif

    WorkspaceInfo modern;
    modern.m_code = "Modern";
    modern.m_title = qtrc("appshell/gettingstarted", "Modern");
    modern.m_description = qtrc("appshell/gettingstarted", "A clearer interface. Ideal for new users");
#ifdef MUSE_MODULE_WORKSPACE
    modern.m_selected = (currentWorkspaceName == "Modern");
#else
    modern.selected = true; // Default fallback
#endif
    m_workspaces.append(modern);

    WorkspaceInfo classic;
    classic.m_code = "Classic";
    classic.m_title = qtrc("appshell/gettingstarted", "Classic");
    classic.m_description = qtrc("appshell/gettingstarted", "Closely matches the layout of Audacity 3");
#ifdef MUSE_MODULE_WORKSPACE
    classic.m_selected = (currentWorkspaceName == "Classic");
#else
    classic.selected = false; // Default fallback
#endif
    m_workspaces.append(classic);

    updateWorkspaces();
}

void WorkspaceLayoutPageModel::selectWorkspace(const QString& workspaceCode)
{
    if (currentWorkspaceCode() == workspaceCode) {
        return;
    }

#ifdef MUSE_MODULE_WORKSPACE
    // Use the same mechanism as WorkspacesToolBar - call changeCurrentWorkspace directly
    if (m_workspaceManager()) {
        m_workspaceManager()->changeCurrentWorkspace(workspaceCode.toStdString());
    }
    // Note: updateWorkspaces() will be called automatically via currentWorkspaceChanged signal
#else
    // Fallback: Update local selection state only
    for (WorkspaceInfo& workspace : m_workspaces) {
        workspace.selected = (workspace.code == workspaceCode);
    }
    emit workspacesChanged();
#endif
}

QVariantList WorkspaceLayoutPageModel::workspaces() const
{
    QVariantList result;
    for (const WorkspaceInfo& workspace : m_workspaces) {
        result << workspace.toMap();
    }
    return result;
}

QString WorkspaceLayoutPageModel::currentWorkspaceCode() const
{
    for (const WorkspaceInfo& workspace : m_workspaces) {
        if (workspace.m_selected) {
            return workspace.m_code;
        }
    }
    return "modern"; // Default fallback
}

QString WorkspaceLayoutPageModel::pageTitle()
{
    return qtrc("appshell/gettingstarted", "What UI layout (workspace) do you want?");
}

void WorkspaceLayoutPageModel::updateWorkspaces()
{
#ifdef MUSE_MODULE_WORKSPACE
    // Get current workspace from workspace manager
    std::string currentWorkspaceName;
    if (m_workspaceManager() && m_workspaceManager()->currentWorkspace()) {
        currentWorkspaceName = m_workspaceManager()->currentWorkspace()->name();
    }
#endif

    for (WorkspaceInfo& workspace : m_workspaces) {
#ifdef MUSE_MODULE_WORKSPACE
        // Update selection based on current workspace
        workspace.m_selected = (workspace.m_code.toStdString() == currentWorkspaceName);
#endif
    }

    emit workspacesChanged();
}

QString WorkspaceLayoutPageModel::navigationAccessibleName()
{
    return qtrc("appshell/gettingstarted", "Workspace layout options");
}

QString WorkspaceLayoutPageModel::navigationAccessibleDescription()
{
    return qtrc("appshell/gettingstarted", "Choose your preferred workspace layout for the Audacity interface");
}

QString WorkspaceLayoutPageModel::pageAccessibleDescription()
{
    return qtrc("appshell/gettingstarted", "Select a workspace layout that suits your workflow. You can change this later.");
}

QString WorkspaceLayoutPageModel::currentlySelectedText()
{
    return qtrc("appshell/gettingstarted", "Currently selected");
}

QString WorkspaceLayoutPageModel::clickToSelectText()
{
    return qtrc("appshell/gettingstarted", "Click to select this workspace");
}

QString WorkspaceLayoutPageModel::availableWorkspaceText()
{
    return qtrc("appshell/gettingstarted", "Available workspace");
}

QString WorkspaceLayoutPageModel::additionalInfoText()
{
    return qtrc("appshell/gettingstarted", "You can change between these layouts at any time using our new 'workspaces' feature.");
}

QString WorkspaceLayoutPageModel::additionalInfoAccessibleName()
{
    return qtrc("appshell/gettingstarted", "Additional information");
}

QString WorkspaceLayoutPageModel::previewAccessibleName()
{
    return qtrc("appshell/gettingstarted", "Workspace layout preview");
}

QString WorkspaceLayoutPageModel::previewAccessibleDescription()
{
    return qtrc("appshell/gettingstarted", "Preview of the selected workspace layout showing the arrangement of interface elements");
}

QString WorkspaceLayoutPageModel::formatNavigationDescription(const QString& description, bool selected) const
{
    const QString statusText = selected ? currentlySelectedText() : clickToSelectText();
    //: %1 is the workspace description, %2 is the selection status (e.g. "Currently selected" or "Click to select this workspace")
    return qtrc("appshell/gettingstarted", "%1. %2").arg(description).arg(statusText);
}

QString WorkspaceLayoutPageModel::formatAccessibleDescription(const QString& description, bool selected) const
{
    const QString statusText = selected ? currentlySelectedText() : availableWorkspaceText();
    //: %1 is the workspace description, %2 is the availability status (e.g. "Currently selected" or "Available workspace")
    return qtrc("appshell/gettingstarted", "%1. %2").arg(description).arg(statusText);
}
