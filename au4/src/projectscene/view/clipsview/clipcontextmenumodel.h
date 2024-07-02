/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "uicomponents/view/abstractmenumodel.h"
#include "types/projectscenetypes.h"

namespace au::projectscene {
class ClipContextMenuModel : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT
    Q_PROPERTY(ClipKey clipKey READ clipKey WRITE setClipKey NOTIFY clipKeyChanged FINAL)

public:
    ClipContextMenuModel() = default;

    Q_INVOKABLE void loadItems();
    ClipKey clipKey() const;
    void setClipKey(const ClipKey& newClipKey);
signals:
    void clipKeyChanged();
private:
    ClipKey m_clipKey;
};
}
