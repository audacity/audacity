/*
* Audacity: A Digital Audio Editor
*/
#include "videopanelmenumodel.h"

#include "translation.h"

#include "../videoattachpolicy.h"

using namespace au::video;
using namespace muse::uicomponents;

static const QString ATTACH_ITEM_ID("video-attach");
static const QString DETACH_ITEM_ID("video-detach");
static const QString SET_OFFSET_ITEM_ID("video-set-offset");

VideoPanelMenuModel::VideoPanelMenuModel(QObject* parent)
    : AbstractMenuModel(parent)
{
}

void VideoPanelMenuModel::load()
{
    if (!m_subscribed && videoService() != nullptr) {
        // Rebuild whenever a video comes or goes, so the enabled state is not
        // frozen at whatever it was when the panel was first created.
        videoService()->attachedChanged().onNotify(this, [this]() {
            load();
        });
        m_subscribed = true;
    }

    if (!m_subscribed && videoService() != nullptr) {
        // Rebuild whenever a video comes or goes, so the enabled state is not
        // frozen at whatever it was when the panel was first created.
        videoService()->attachedChanged().onNotify(this, [this]() {
            load();
        });
        m_subscribed = true;
    }

    // Rebuilt on every open, so "Set offset…" carries the current value and
    // both items disable themselves when there is nothing attached.
    const bool hasVideo = videoService() != nullptr && videoService()->isAttached();

    MenuItemList items;

    // Always available: with nothing attached this is the only way in from
    // the menu, and with something attached it replaces it, which is a normal
    // thing to want and is one click to undo with Detach.
    MenuItem* attachItem = new MenuItem(this);
    attachItem->setId(ATTACH_ITEM_ID);
    attachItem->setTitle(muse::TranslatableString("video", "Attach video…"));
    attachItem->setEnabled(true);
    items << attachItem;

    MenuItem* offsetItem = new MenuItem(this);
    offsetItem->setId(SET_OFFSET_ITEM_ID);
    offsetItem->setTitle(muse::TranslatableString("video", "Set video offset…"));
    offsetItem->setEnabled(hasVideo);
    items << offsetItem;

    MenuItem* detachItem = new MenuItem(this);
    detachItem->setId(DETACH_ITEM_ID);
    detachItem->setTitle(muse::TranslatableString("video", "Detach video"));
    detachItem->setEnabled(hasVideo);
    items << detachItem;

    setItems(items);
}

void VideoPanelMenuModel::handleMenuItem(const QString& itemId)
{
    if (videoService() == nullptr) {
        return;
    }

    if (itemId == ATTACH_ITEM_ID) {
        attach();
        return;
    }

    if (itemId == DETACH_ITEM_ID) {
        videoService()->detach();
        return;
    }

    if (itemId == SET_OFFSET_ITEM_ID) {
        if (interactive() != nullptr) {
            interactive()->open(muse::Uri("audacity://video/offset"));
        }
        return;
    }

    AbstractMenuModel::handleMenuItem(itemId);
}

void VideoPanelMenuModel::attach()
{
    if (interactive() == nullptr) {
        return;
    }

    const std::vector<std::string> filter {
        muse::trc("video", "Video files") + " (" + videoFileFilter() + ")"
    };

    const muse::io::path_t path = interactive()->selectOpeningFileSync(
        muse::trc("video", "Attach video"), muse::io::path_t(), filter);

    if (path.empty()) {
        return;
    }

    videoService()->attach(path.toStdString());
}
