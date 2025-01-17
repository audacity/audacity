/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "context/iglobalcontext.h"
#include "uicomponents/view/abstractmenumodel.h"
#include "types/projectscenetypes.h"

namespace au::projectscene {
class ClipContextMenuModel : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT

    muse::Inject<context::IGlobalContext> globalContext;

    Q_PROPERTY(ClipKey clipKey READ clipKey WRITE setClipKey NOTIFY clipKeyChanged FINAL)

public:
    ClipContextMenuModel() = default;

    Q_INVOKABLE void load() override;

    ClipKey clipKey() const;
    void setClipKey(const ClipKey& newClipKey);

signals:
    void clipKeyChanged();

private:
    void onActionsStateChanges(const muse::actions::ActionCodeList& codes) override;
    void updateStretchEnabledState(muse::uicomponents::MenuItem& item);

    ClipKey m_clipKey;
};
}
