/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "uicomponents/view/abstracttoolbarmodel.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

namespace au::projectscene {
class ProjectToolBarModel : public muse::uicomponents::AbstractToolBarModel
{
    Q_OBJECT

    Q_PROPERTY(bool isCompactMode READ isCompactMode WRITE setIsCompactMode CONSTANT FINAL)

    muse::Inject<au::context::IGlobalContext> context;

public:
    Q_INVOKABLE void load() override;

    bool isCompactMode() const;
    void setIsCompactMode(bool isCompactMode);

private:
    bool m_isCompactMode = false;
};
}
