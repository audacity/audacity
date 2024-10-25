/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "uicomponents/view/abstracttoolbarmodel.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

namespace au::projectscene {
class UndoRedoToolBarModel : public muse::uicomponents::AbstractToolBarModel
{
    Q_OBJECT

    muse::Inject<au::context::IGlobalContext> context;

public:

    Q_INVOKABLE void load() override;
};
}
