/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "ui/iuiactionsregister.h"

#include "uicomponents/view/toolbaritem.h"

namespace au::projectscene {
class SnapToolBarItem : public muse::uicomponents::ToolBarItem
{
    Q_OBJECT

    Q_PROPERTY(bool isSnapEnabled READ isSnapEnabled WRITE setIsSnapEnabled NOTIFY isSnapEnabledChanged FINAL)
    Q_PROPERTY(QString currentValue READ currentValue NOTIFY currentValueChanged FINAL)

    Q_PROPERTY(QVariantList availableSnapTypes READ availableSnapTypes NOTIFY availableSnapTypesChanged)

    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<muse::ui::IUiActionsRegister> uiActionsRegister;

public:
    explicit SnapToolBarItem(const muse::ui::UiAction& action, muse::uicomponents::ToolBarItemType::Type type, QObject* parent = nullptr);

    Q_INVOKABLE void handleMenuItem(const QString& itemId);

    bool isSnapEnabled() const;
    void setIsSnapEnabled(bool newIsSnapEnabled);

    QString currentValue() const;
    QVariantList availableSnapTypes() const;

signals:
    void isSnapEnabledChanged();
    void currentValueChanged();

    void availableSnapTypesChanged();

private:
    IProjectViewStatePtr viewState() const;

    void onProjectChanged();

    muse::uicomponents::MenuItem* makeMenu(const muse::TranslatableString& title, const muse::uicomponents::MenuItemList& items,
                                           const QString& menuId);
    muse::uicomponents::MenuItem* makeMenuItem(const QString& id, const muse::TranslatableString& title);
    muse::uicomponents::MenuItem* makeSeparator();

    muse::uicomponents::MenuItem* makeSecondsAndSamplesMenu();
    muse::uicomponents::MenuItem* makeVideoFramesMenu();
    muse::uicomponents::MenuItem* makeCDFramesMenu();

    muse::uicomponents::MenuItem* findCheckedItem(const muse::uicomponents::MenuItemList& items) const;

    void updateCheckedState();
    void doUpdateCheckedState(muse::uicomponents::MenuItemList& items);

    void updateEnableTripletsCheckedState();

    bool m_isSnapEnabled = false;
    bool m_isTripletsEnabled = false;
    muse::uicomponents::MenuItemList m_availableSnapTypes;
};
}
