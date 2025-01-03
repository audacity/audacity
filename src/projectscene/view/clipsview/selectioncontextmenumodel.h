/**********************************************************************

  Audacity: A Digital Audio Editor

**********************************************************************/

#pragma once

#include "context/iglobalcontext.h"
#include "modularity/ioc.h"

#include "uicomponents/view/abstractmenumodel.h"

namespace au::projectscene {
class SelectionContextMenuModel : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT

public:
    Q_INVOKABLE void load() override;

private:
    muse::uicomponents::MenuItemList makeItems();
};
}  // namespace au::projectscene
