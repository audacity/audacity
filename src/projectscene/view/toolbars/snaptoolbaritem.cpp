/*
* Audacity: A Digital Audio Editor
*/
#include "snaptoolbaritem.h"

#include <QVariantMap>

using namespace au::projectscene;
using namespace au::project;
using namespace muse::ui;
using namespace muse::uicomponents;
using namespace muse::actions;

static const QString ENABLE_TRIPLETS_ID = "enable-triplets";

static QString snapTypeToId(SnapType type)
{
    return QString::number(static_cast<int>(type));
}

static SnapType idToSnapType(const QString& id)
{
    return static_cast<SnapType>(id.toInt());
}

static bool isEnableTripletsAvailable(const SnapType& type)
{
    static const std::vector<SnapType> types = {
        SnapType::Half,
        SnapType::Quarter,
        SnapType::Eighth,
        SnapType::Sixteenth,
        SnapType::ThirtySecond,
        SnapType::SixtyFourth,
        SnapType::HundredTwentyEighth
    };

    return muse::contains(types, type);
}

SnapToolBarItem::SnapToolBarItem(const muse::ui::UiAction& action, muse::uicomponents::ToolBarItemType::Type type,
                                 QObject* parent)
    : muse::uicomponents::ToolBarItem(action, type, parent)
{
    globalContext()->currentProjectChanged().onNotify(this, [this](){
        onProjectChanged();
    });

    onProjectChanged();

    m_availableSnapTypes = {
        makeMenuItem(snapTypeToId(SnapType::Bar), muse::TranslatableString("projectscene", "Bar")),
        makeMenuItem(snapTypeToId(SnapType::Half), muse::TranslatableString("projectscene", "1/2")),
        makeMenuItem(snapTypeToId(SnapType::Quarter), muse::TranslatableString("projectscene", "1/4")),
        makeMenuItem(snapTypeToId(SnapType::Eighth), muse::TranslatableString("projectscene", "1/8")),
        makeMenuItem(snapTypeToId(SnapType::Sixteenth), muse::TranslatableString("projectscene", "1/16")),
        makeMenuItem(snapTypeToId(SnapType::ThirtySecond), muse::TranslatableString("projectscene", "1/32")),
        makeMenuItem(snapTypeToId(SnapType::SixtyFourth), muse::TranslatableString("projectscene", "1/64")),
        makeMenuItem(snapTypeToId(SnapType::HundredTwentyEighth), muse::TranslatableString("projectscene", "1/128")),
        makeSeparator(),
        makeMenuItem(ENABLE_TRIPLETS_ID, muse::TranslatableString("projectscene", "Enable triplets")),
        makeSeparator(),
        makeSecondsAndSamplesMenu(),
        makeVideoFramesMenu(),
        makeCDFramesMenu()
    };

    updateCheckedState();
}

void SnapToolBarItem::handleMenuItem(const QString& itemId)
{
    auto projectViewState = viewState();
    if (!projectViewState) {
        return;
    }

    if (itemId == ENABLE_TRIPLETS_ID) {
        projectViewState->setIsSnapTripletsEnabled(!m_isTripletsEnabled);
    } else {
        auto snapType = idToSnapType(itemId);
        projectViewState->setSnapType(snapType);
    }
}

bool SnapToolBarItem::isSnapEnabled() const
{
    return m_isSnapEnabled;
}

void SnapToolBarItem::setIsSnapEnabled(bool newIsSnapEnabled)
{
    if (m_isSnapEnabled == newIsSnapEnabled) {
        return;
    }

    auto projectViewState = viewState();
    if (!projectViewState) {
        return;
    }

    projectViewState->setIsSnapEnabled(newIsSnapEnabled);
}

IProjectViewStatePtr SnapToolBarItem::viewState() const
{
    IAudacityProjectPtr prj = globalContext()->currentProject();
    return prj ? prj->viewState() : nullptr;
}

void SnapToolBarItem::onProjectChanged()
{
    auto projectViewState = viewState();
    if (!projectViewState) {
        return;
    }

    projectViewState->snap().ch.onReceive(this, [this](const Snap& snap){
        if (m_isSnapEnabled != snap.enabled) {
            m_isSnapEnabled = snap.enabled;
            emit isSnapEnabledChanged();
        }

        if (m_isTripletsEnabled) {
            m_isTripletsEnabled = isEnableTripletsAvailable(snap.type);
        }

        if (m_isTripletsEnabled != snap.isSnapTriplets) {
            m_isTripletsEnabled = snap.isSnapTriplets;
        }

        updateCheckedState();

        emit currentValueChanged();
    });

    m_isSnapEnabled = projectViewState->isSnapEnabled();
    emit isSnapEnabledChanged();

    updateCheckedState();
    emit currentValueChanged();
}

QString SnapToolBarItem::currentValue() const
{
    auto projectViewState = viewState();
    if (!projectViewState) {
        return QString();
    }

    MenuItem* item = findCheckedItem(m_availableSnapTypes);

    return item ? item->action().title.qTranslatedWithoutMnemonic() : QString();
}

QVariantList SnapToolBarItem::availableSnapTypes() const
{
    auto projectViewState = viewState();
    if (!projectViewState) {
        return {};
    }

    return menuItemListToVariantList(m_availableSnapTypes);
}

MenuItem* SnapToolBarItem::makeMenu(const muse::TranslatableString& title, const MenuItemList& items,
                                    const QString& menuId)
{
    MenuItem* item = new MenuItem(this);
    item->setId(menuId);
    item->setSubitems(items);

    UiAction action;
    action.title = title;
    item->setAction(action);

    UiActionState state;
    state.enabled = true;
    item->setState(state);

    return item;
}

MenuItem* SnapToolBarItem::makeMenuItem(const QString& id, const muse::TranslatableString& title)
{
    MenuItem* item = new MenuItem(this);
    item->setId(id);

    UiAction action;
    action.checkable = Checkable::Yes;
    item->setAction(action);

    UiActionState state;
    state.enabled = true;
    item->setState(state);

    item->setTitle(title);

    return item;
}

MenuItem* SnapToolBarItem::makeSeparator()
{
    MenuItem* item = new MenuItem(this);

    UiAction action;
    action.title = {};
    item->setAction(action);

    return item;
}

MenuItem* SnapToolBarItem::makeSecondsAndSamplesMenu()
{
    MenuItemList items = {
        makeMenuItem(snapTypeToId(SnapType::Seconds), muse::TranslatableString("projectscene", "Seconds")),
        makeMenuItem(snapTypeToId(SnapType::Deciseconds), muse::TranslatableString("projectscene", "Deciseconds")),
        makeMenuItem(snapTypeToId(SnapType::Centiseconds), muse::TranslatableString("projectscene", "Centiseconds")),
        makeMenuItem(snapTypeToId(SnapType::Milliseconds), muse::TranslatableString("projectscene", "Milliseconds")),
        makeMenuItem(snapTypeToId(SnapType::Samples), muse::TranslatableString("projectscene", "Samples"))
    };

    return makeMenu(muse::TranslatableString("projectscene", "Seconds & samples"), items, "seconds-and-samples");
}

MenuItem* SnapToolBarItem::makeVideoFramesMenu()
{
    MenuItemList items = {
        makeMenuItem(snapTypeToId(SnapType::FilmFrames), muse::TranslatableString("projectscene", "Video Frames (24 fps)")),
        makeMenuItem(snapTypeToId(SnapType::NTSCFrames), muse::TranslatableString("projectscene", "NTSC Frames (29.97 fps)")),
        makeMenuItem(snapTypeToId(SnapType::NTSCFramesDrop), muse::TranslatableString("projectscene", "NTSC Frames (30 fps)")),
        makeMenuItem(snapTypeToId(SnapType::PALFrames), muse::TranslatableString("projectscene", "PAL Frames (25 fps)"))
    };

    return makeMenu(muse::TranslatableString("projectscene", "Video frames"), items, "video-frames");
}

MenuItem* SnapToolBarItem::makeCDFramesMenu()
{
    MenuItemList items = {
        makeMenuItem(snapTypeToId(SnapType::CDDAFrames), muse::TranslatableString("projectscene", "CDDA Frames (75 fps)"))
    };

    return makeMenu(muse::TranslatableString("projectscene", "CD frames"), items, "cd-frames");
}

MenuItem* SnapToolBarItem::findCheckedItem(const muse::uicomponents::MenuItemList& items) const
{
    for (MenuItem* menuItem : items) {
        if (!menuItem) {
            continue;
        }

        if (menuItem->id() == ENABLE_TRIPLETS_ID) {
            continue;
        }

        if (menuItem->state().checked) {
            return menuItem;
        }

        auto subitems = menuItem->subitems();
        if (!subitems.empty()) {
            MenuItem* subitem = findCheckedItem(subitems);
            if (subitem && subitem->state().checked) {
                return subitem;
            }
        }
    }

    return nullptr;
}

void SnapToolBarItem::updateCheckedState()
{
    if (!viewState()) {
        return;
    }

    doUpdateCheckedState(m_availableSnapTypes);
    emit availableSnapTypesChanged();
    emit currentValueChanged();
}

void SnapToolBarItem::doUpdateCheckedState(muse::uicomponents::MenuItemList& items)
{
    for (MenuItem* menuItem : items) {
        if (!menuItem) {
            continue;
        }

        auto subitems = menuItem->subitems();
        if (!subitems.empty()) {
            doUpdateCheckedState(subitems);
            continue;
        }

        QString itemId = menuItem->id();
        if (itemId.isEmpty()) {
            continue;
        }

        bool checked = false;
        if (itemId == ENABLE_TRIPLETS_ID) {
            checked = m_isTripletsEnabled;
        } else {
            SnapType snapType = idToSnapType(itemId);
            checked = snapType == viewState()->snapType();
        }

        UiActionState state;
        state.enabled = true;
        state.checked = checked;
        menuItem->setState(state);
    }
}
