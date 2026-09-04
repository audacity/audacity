/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_VIDEO_VIDEOPANELMENUMODEL_H
#define AU_VIDEO_VIDEOPANELMENUMODEL_H

#include "uicomponents/qml/Muse/UiComponents/abstractmenumodel.h"

#include "global/async/asyncable.h"
#include "framework/interactive/iinteractive.h"
#include "modularity/ioc.h"

#include "../ivideoservice.h"

namespace au::video {
//! The "..." menu in the video panel's title bar.
//!
//! Detach lives here rather than on a button: it is a rare and destructive
//! action, and the panel is narrow enough that a button competing with the
//! picture for width is not worth it.
//!
//! The items are built by hand rather than through the action registry
//! because they are only ever reachable from this one menu, so a global
//! action code and a shortcut would be scope nobody asked for. DockPanelView
//! forwards ids it does not recognise here, which is what makes that work.
class VideoPanelMenuModel : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT

public:
    explicit VideoPanelMenuModel(QObject* parent = nullptr);

    void load() override;
    void handleMenuItem(const QString& itemId) override;

private:
    muse::ContextInject<IVideoService> videoService { this };
    muse::ContextInject<muse::IInteractive> interactive { this };
};
}

#endif // AU_VIDEO_VIDEOPANELMENUMODEL_H
