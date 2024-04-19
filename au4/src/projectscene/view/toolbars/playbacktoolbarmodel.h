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
class PlaybackToolBarModel : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT

    muse::Inject<au::context::IGlobalContext> context;
    muse::Inject<muse::ui::IUiActionsRegister> actionsRegister;
    muse::Inject<muse::ui::IUiConfiguration> uiConfiguration;

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

    bool isMenuSecondary(const muse::actions::ActionCode& actionCode) const;

    muse::uicomponents::MenuItem* makeActionItem(const muse::ui::UiAction& action, const QString& section,
                                                 const muse::uicomponents::MenuItemList& subitems = {});
};
}

#endif // AU_PROJECTSCENE_PLAYBACKTOOLBARMODEL_H
