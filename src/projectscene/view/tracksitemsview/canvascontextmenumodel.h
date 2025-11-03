/**********************************************************************

  Audacity: A Digital Audio Editor

**********************************************************************/

#pragma once

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "uicomponents/view/abstractmenumodel.h"

namespace au::projectscene {
class CanvasContextMenuModel : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT

public:
    Q_INVOKABLE void load() override;

private:

    muse::uicomponents::MenuItemList makeItems();
};
}
