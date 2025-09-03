#ifndef AU_APPSHELL_CLIPVISUALIZATIONPAGEMODEL_H
#define AU_APPSHELL_CLIPVISUALIZATIONPAGEMODEL_H

#include <QObject>
#include <QVariantList>

#include "async/asyncable.h"
#include "modularity/ioc.h"
#include "projectscene/iprojectsceneconfiguration.h"

namespace au::appshell {
struct ClipStyleInfo {
    int m_style;
    QString m_title;
    QString m_description;
    QString m_imagePath;
    bool m_selected = false;

    [[nodiscard]] QVariantMap toMap() const
    {
        return {
            { "style", m_style },
            { "title", m_title },
            { "description", m_description },
            { "imagePath", m_imagePath },
            { "selected", m_selected }
        };
    }
};

class ClipVisualizationPageModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    muse::Inject<projectscene::IProjectSceneConfiguration> m_projectSceneConfiguration;

    Q_PROPERTY(QVariantList clipStyles READ clipStyles NOTIFY clipStylesChanged)
    Q_PROPERTY(int currentClipStyle READ currentClipStyle NOTIFY currentClipStyleChanged)
    Q_PROPERTY(QString currentImagePath READ currentImagePath NOTIFY currentClipStyleChanged)
    Q_PROPERTY(QString pageTitle READ pageTitle CONSTANT)
    Q_PROPERTY(QString navigationAccessibleName READ navigationAccessibleName CONSTANT)
    Q_PROPERTY(QString navigationAccessibleDescription READ navigationAccessibleDescription CONSTANT)
    Q_PROPERTY(QString pageAccessibleDescription READ pageAccessibleDescription CONSTANT)
    Q_PROPERTY(QString previewAccessibleName READ previewAccessibleName CONSTANT)
    Q_PROPERTY(QString previewAccessibleDescription READ previewAccessibleDescription CONSTANT)
    Q_PROPERTY(QString currentlySelectedText READ currentlySelectedText CONSTANT)
    Q_PROPERTY(QString clickToSelectText READ clickToSelectText CONSTANT)
    Q_PROPERTY(QString availableOptionText READ availableOptionText CONSTANT)

public:
    explicit ClipVisualizationPageModel(QObject* parent = nullptr);

    Q_INVOKABLE void load();
    Q_INVOKABLE void selectClipStyle(int style);
    Q_INVOKABLE QString formatNavigationDescription(const QString& description, bool selected) const;
    Q_INVOKABLE QString formatAccessibleDescription(const QString& description, bool selected) const;

    QVariantList clipStyles() const;
    int currentClipStyle() const;
    QString currentImagePath() const;
    static QString pageTitle();
    static QString navigationAccessibleName();
    static QString navigationAccessibleDescription();
    static QString pageAccessibleDescription();
    static QString previewAccessibleName();
    static QString previewAccessibleDescription();
    static QString currentlySelectedText();
    static QString clickToSelectText();
    static QString availableOptionText();

signals:
    void clipStylesChanged();
    void currentClipStyleChanged();

private:
    void updateClipStyles();

    QList<ClipStyleInfo> m_clipStyles;
};
}

#endif // AU_APPSHELL_CLIPVISUALIZATIONPAGEMODEL_H
