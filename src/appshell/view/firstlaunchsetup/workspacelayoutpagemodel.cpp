#include "workspacelayoutpagemodel.h"

#include "global/translation.h"

using namespace au::appshell;
using namespace muse;

WorkspaceLayoutPageModel::WorkspaceLayoutPageModel(QObject* parent)
    : QObject(parent)
{
    // Listen to theme changes to update image paths
    m_uiConfiguration()->currentThemeChanged().onNotify(this, [this]() {
        updateWorkspaces();
    });

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
    modern.m_imagePath = "resources/UILayout_LightMode.png"; // Will be updated based on theme
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
    classic.m_imagePath = "resources/UILayout_LightMode.png"; // Will be updated based on theme
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

QString WorkspaceLayoutPageModel::currentImagePath() const
{
    for (const WorkspaceInfo& workspace : m_workspaces) {
        if (workspace.m_selected) {
            return workspace.m_imagePath;
        }
    }
    return ""; // Fallback
}

QString WorkspaceLayoutPageModel::pageTitle()
{
    return qtrc("appshell/gettingstarted", "What UI layout (workspace) do you want?");
}

void WorkspaceLayoutPageModel::updateWorkspaces()
{
    // Update image paths based on current theme
    const bool isDarkTheme = m_uiConfiguration()->isDarkMode();
    const QString imagePath = isDarkTheme ? "resources/UILayout_DarkMode.png" : "resources/UILayout_LightMode.png";

#ifdef MUSE_MODULE_WORKSPACE
    // Get current workspace from workspace manager
    std::string currentWorkspaceName;
    if (m_workspaceManager() && m_workspaceManager()->currentWorkspace()) {
        currentWorkspaceName = m_workspaceManager()->currentWorkspace()->name();
    }
#endif

    for (WorkspaceInfo& workspace : m_workspaces) {
        workspace.m_imagePath = imagePath;
#ifdef MUSE_MODULE_WORKSPACE
        // Update selection based on current workspace
        workspace.m_selected = (workspace.m_code.toStdString() == currentWorkspaceName);
#endif
    }

    emit workspacesChanged();
}
