/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_PROJECTSCENE_PLAYBACKTOOLBARMODEL_H
#define AU_PROJECTSCENE_PLAYBACKTOOLBARMODEL_H

#include <QHash>

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "ui/iuiactionsregister.h"
#include "ui/iuiconfiguration.h"

#include "uicomponents/view/abstractmenumodel.h"

namespace au::projectscene {
class PlaybackToolBarModel : public mu::uicomponents::AbstractMenuModel
{
    Q_OBJECT

    mu::Inject<mu::context::IGlobalContext> context;
    mu::Inject<mu::ui::IUiActionsRegister> actionsRegister;
    mu::Inject<mu::ui::IUiConfiguration> uiConfiguration;

public:
    explicit PlaybackToolBarModel(QObject* parent = nullptr);

    Q_INVOKABLE void load() override;

    QVariant data(const QModelIndex& index, int role) const override;
    QHash<int, QByteArray> roleNames() const override;

private:
    enum InputRoles {
        IsMenuSecondaryRole = AbstractMenuModel::Roles::UserRole + 1,
        OrderRole,
        SectionRole
    };

    void onProjectChanged();

    void updateState();

    bool isMenuSecondary(const mu::actions::ActionCode& actionCode) const;

    mu::uicomponents::MenuItem* makeActionItem(const mu::ui::UiAction& action, const QString& section,
                                               const mu::uicomponents::MenuItemList& subitems = {});
};
}

#endif // AU_PROJECTSCENE_PLAYBACKTOOLBARMODEL_H
