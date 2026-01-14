#ifndef AU_APPSHELL_WORKSPACELAYOUTPAGEMODEL_H
#define AU_APPSHELL_WORKSPACELAYOUTPAGEMODEL_H

#include <QObject>
#include <QtQml/qqmlregistration.h>

#include "async/asyncable.h"
#include "modularity/ioc.h"
#include "ui/iuiconfiguration.h"
#include "muse_framework_config.h"

#ifdef MUSE_MODULE_WORKSPACE
#include "workspace/iworkspacemanager.h"
#endif

namespace au::appshell {
struct WorkspaceInfo {
    QString m_code;
    QString m_title;
    QString m_description;
    bool m_selected = false;

    [[nodiscard]] QVariantMap toMap() const
    {
        return {
            { "code", m_code },
            { "title", m_title },
            { "description", m_description },
            { "selected", m_selected }
        };
    }
};

class WorkspaceLayoutPageModel : public QObject, public muse::async::Asyncable, public muse::Injectable
{
    Q_OBJECT
    QML_ELEMENT

    Q_PROPERTY(QVariantList workspaces READ workspaces NOTIFY workspacesChanged)
    Q_PROPERTY(QString currentWorkspaceCode READ currentWorkspaceCode NOTIFY workspacesChanged)
    Q_PROPERTY(QString pageTitle READ pageTitle CONSTANT)
    Q_PROPERTY(QString navigationAccessibleName READ navigationAccessibleName CONSTANT)
    Q_PROPERTY(QString navigationAccessibleDescription READ navigationAccessibleDescription CONSTANT)
    Q_PROPERTY(QString pageAccessibleDescription READ pageAccessibleDescription CONSTANT)
    Q_PROPERTY(QString currentlySelectedText READ currentlySelectedText CONSTANT)
    Q_PROPERTY(QString clickToSelectText READ clickToSelectText CONSTANT)
    Q_PROPERTY(QString availableWorkspaceText READ availableWorkspaceText CONSTANT)
    Q_PROPERTY(QString additionalInfoText READ additionalInfoText CONSTANT)
    Q_PROPERTY(QString additionalInfoAccessibleName READ additionalInfoAccessibleName CONSTANT)
    Q_PROPERTY(QString previewAccessibleName READ previewAccessibleName CONSTANT)
    Q_PROPERTY(QString previewAccessibleDescription READ previewAccessibleDescription CONSTANT)

    muse::GlobalInject<muse::ui::IUiConfiguration> m_uiConfiguration;

#ifdef MUSE_MODULE_WORKSPACE
    muse::Inject<muse::workspace::IWorkspaceManager> m_workspaceManager { this };
#endif

public:
    explicit WorkspaceLayoutPageModel(QObject* parent = nullptr);

    Q_INVOKABLE void load();
    Q_INVOKABLE void selectWorkspace(const QString& workspaceCode);
    Q_INVOKABLE QString formatNavigationDescription(const QString& description, bool selected) const;
    Q_INVOKABLE QString formatAccessibleDescription(const QString& description, bool selected) const;

    QVariantList workspaces() const;
    QString currentWorkspaceCode() const;
    static QString pageTitle();
    static QString navigationAccessibleName();
    static QString navigationAccessibleDescription();
    static QString pageAccessibleDescription();
    static QString currentlySelectedText();
    static QString clickToSelectText();
    static QString availableWorkspaceText();
    static QString additionalInfoText();
    static QString additionalInfoAccessibleName();
    static QString previewAccessibleName();
    static QString previewAccessibleDescription();

signals:
    void workspacesChanged();

private:
    void updateWorkspaces();

    QList<WorkspaceInfo> m_workspaces;
};
}

#endif // AU_APPSHELL_WORKSPACELAYOUTPAGEMODEL_H
