/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "context/iglobalcontext.h"
#include "uicomponents/view/abstractmenumodel.h"

#include "iprojectsceneconfiguration.h"
#include "types/projectscenetypes.h"

namespace au::projectscene {
class ClipContextMenuModel : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT

    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<projectscene::IProjectSceneConfiguration> projectSceneConfiguration;

    Q_PROPERTY(ClipKey clipKey READ clipKey WRITE setClipKey NOTIFY clipKeyChanged FINAL)

public:
    ClipContextMenuModel() = default;

    Q_INVOKABLE void load() override;
    Q_INVOKABLE void handleMenuItem(const QString& itemId) override;

    ClipKey clipKey() const;
    void setClipKey(const ClipKey& newClipKey);

signals:
    void clipKeyChanged();
    void clipTitleEditRequested();

private:
    void onActionsStateChanges(const muse::actions::ActionCodeList& codes) override;
    void updateStretchEnabledState(muse::uicomponents::MenuItem& item);

    muse::uicomponents::MenuItemList makeClipColourItems();
    muse::uicomponents::MenuItemList makeWaveColourItems();
    void updateColorCheckedState();
    void updateWaveColorCheckedState();
    void updateColorMenu();
    void updateWaveColorMenu();

    ClipKey m_clipKey;
    muse::actions::ActionCodeList m_colorChangeActionCodeList;
    muse::actions::ActionCodeList m_wave_colorChangeActionCodeList;
};
}
