/*
* Audacity: A Digital Audio Editor
*/
#include "videopanelmenumodel.h"

#include "translation.h"

using namespace au::video;
using namespace muse::uicomponents;

static const QString DETACH_ITEM_ID("video-detach");
static const QString SET_OFFSET_ITEM_ID("video-set-offset");

VideoPanelMenuModel::VideoPanelMenuModel(QObject* parent)
    : AbstractMenuModel(parent)
{
}

void VideoPanelMenuModel::load()
{
    // Rebuilt on every open, so "Set offset…" carries the current value and
    // both items disable themselves when there is nothing attached.
    const bool hasVideo = videoService() != nullptr && videoService()->isAttached();

    MenuItemList items;

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
