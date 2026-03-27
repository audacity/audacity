/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "interactive/iplatforminteractive.h"

namespace au::projectscene {
class GetEffectsModel : public QObject, public muse::async::Asyncable, public muse::Contextable
{
    Q_OBJECT

    muse::GlobalInject<muse::IPlatformInteractive> platformInteractive;

    Q_PROPERTY(QVariantList effectsGroups READ effectsGroups NOTIFY effectsGroupsChanged FINAL)
    Q_PROPERTY(QVariantList categories READ categories NOTIFY categoriesChanged FINAL)
    Q_PROPERTY(int selectedCategoryIndex READ selectedCategoryIndex WRITE setSelectedCategoryIndex NOTIFY selectedCategoryIndexChanged FINAL)
    Q_PROPERTY(bool isLoading READ isLoading NOTIFY isLoadingChanged FINAL)
    Q_PROPERTY(bool hasError READ hasError NOTIFY hasErrorChanged FINAL)

public:
    explicit GetEffectsModel(QObject* parent = nullptr);

    Q_INVOKABLE void load();
    Q_INVOKABLE void openEffectUrl(const QString& effectCode) const;
    Q_INVOKABLE void openBecomeAPartnerUrl() const;

    QVariantList effectsGroups() const;
    QVariantList categories() const;
    int selectedCategoryIndex() const;
    void setSelectedCategoryIndex(int index);
    bool isLoading() const;
    bool hasError() const;
    void openUrl(const std::string& url) const;

signals:
    void effectsGroupsChanged();
    void categoriesChanged();
    void selectedCategoryIndexChanged();
    void isLoadingChanged();
    void hasErrorChanged();

private:
    void setIsLoading(bool loading);
    void setHasError(bool error);

    struct EffectData {
        QString iconUrl;
        QString code;
        QString title;
        QString subtitle;
        QString category;
    };

    struct EffectsGroupData {
        QString title;
        QList<EffectData> effects;
    };

    QList<EffectsGroupData> m_allGroups;
    QStringList m_categories;
    int m_selectedCategoryIndex = 0;
    bool m_isLoading = false;
    bool m_hasError = false;
};
}
