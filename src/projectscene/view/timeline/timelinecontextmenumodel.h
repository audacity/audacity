/**********************************************************************

  Audacity: A Digital Audio Editor

**********************************************************************/

#pragma once

#include "context/iglobalcontext.h"
#include "modularity/ioc.h"

#include "uicomponents/view/abstractmenumodel.h"

namespace au::projectscene {
class TimelineContextMenuModel : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT

    muse::Inject<context::IGlobalContext> globalContext;

public:
    Q_INVOKABLE void load() override;

private:
    muse::uicomponents::MenuItemList makeRulerItems();
};
}  // namespace au::projectscene
