/*
 * Audacity: A Digital Audio Editor
 */
#include "trackspectrogramcontextmenumodel.h"

#include "actions/actiontypes.h"
#include "framework/global/translation.h"
#include "framework/uicomponents/qml/Muse/UiComponents/menuitem.h"
#include "types/translatablestring.h"

namespace au::spectrogram {
TrackSpectrogramContextMenuModel::TrackSpectrogramContextMenuModel(QObject* parent)
    : AbstractMenuModel(parent) {}

void TrackSpectrogramContextMenuModel::init()
{
    frequencySelectionController()->frequencySelectionChanged().onReceive(this, [this](auto) { load(); });
    load();
}

void TrackSpectrogramContextMenuModel::setTrackId(int trackId)
{
    if (m_trackId != trackId) {
        m_trackId = trackId;
        emit trackIdChanged();
    }
}

muse::uicomponents::MenuItem* TrackSpectrogramContextMenuModel::makeSpectralEffectItem(SpectralEffectId id)
{
    const std::optional<SpectralEffect> effect = spectralEffectsRegister()->spectralEffect(id);
    if (!effect) {
        return nullptr;
    }
    muse::uicomponents::MenuItem* const item = makeMenuItem(effect->action);
    IF_ASSERT_FAILED(item) {
        return nullptr;
    }
    item->setTitle(muse::TranslatableString::untranslatable(effect->title));
    return item;
}

void TrackSpectrogramContextMenuModel::load()
{
    AbstractMenuModel::load();

    using namespace muse;
    using namespace muse::actions;

    muse::uicomponents::MenuItemList items;
    if (auto item = makeSpectralEffectItem(SpectralEffectId::DeleteSelection)) {
        items.push_back(item);
    }
    if (auto item = makeSpectralEffectItem(SpectralEffectId::DeleteCenterFrequency)) {
        items.push_back(item);
    }

    if (!items.empty()) {
        // If the spectral effects above were found, then the following two should be found, too.
        items.push_back(makeSeparator());
    }

    if (auto item = makeSpectralEffectItem(SpectralEffectId::AmplifySelection)) {
        items.push_back(item);
    }
    if (auto item = makeSpectralEffectItem(SpectralEffectId::AmplifyCenterFrequency)) {
        items.push_back(item);
    }

    if (!items.empty()) {
        items.push_back(makeSeparator());
    }

    uicomponents::MenuItem* const settingsItem = makeMenuItem("action://trackedit/track-spectrogram-settings");
    IF_ASSERT_FAILED(settingsItem) {
        return;
    }
    items.push_back(settingsItem);

    setItems(std::move(items));
}

void TrackSpectrogramContextMenuModel::handleMenuItem(const QString& itemId)
{
    AbstractMenuModel::handleMenuItem(itemId);
}

void TrackSpectrogramContextMenuModel::onActionsStateChanges(const muse::actions::ActionCodeList& codes)
{
    AbstractMenuModel::onActionsStateChanges(codes);
}
}
