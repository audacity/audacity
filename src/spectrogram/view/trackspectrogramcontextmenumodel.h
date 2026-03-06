/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ifrequencyselectioncontroller.h"

#include "framework/global/modularity/ioc.h"
#include "framework/uicomponents/qml/Muse/UiComponents/abstractmenumodel.h"
#include "ispectraleffectsregister.h"

namespace au::spectrogram {
class TrackSpectrogramContextMenuModel : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT

    Q_PROPERTY(int trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged)

    muse::Inject<IFrequencySelectionController> frequencySelectionController { this };
    muse::Inject<ISpectralEffectsRegister> spectralEffectsRegister { this };

public:
    TrackSpectrogramContextMenuModel(QObject* parent = nullptr);

    // Couldn't find out why implementing QQmlParserStatus broke compilation. Have to use `init` instead.
    Q_INVOKABLE void init();

    void load() override;
    Q_INVOKABLE void handleMenuItem(const QString& itemId) override;

    int trackId() const { return m_trackId; }
    void setTrackId(int trackId);

signals:
    void trackIdChanged();

private:
    void onActionsStateChanges(const muse::actions::ActionCodeList& codes) override;
    muse::uicomponents::MenuItem* makeSpectralEffectItem(SpectralEffectId id, const char* title);
    int m_trackId = -1;
};
}
