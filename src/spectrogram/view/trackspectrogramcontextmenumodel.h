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
    Q_PROPERTY(QString trackTitle READ trackTitle WRITE setTrackTitle NOTIFY trackTitleChanged)

    muse::GlobalInject<ISpectralEffectsRegister> spectralEffectsRegister;

    muse::ContextInject<IFrequencySelectionController> frequencySelectionController { this };

public:
    TrackSpectrogramContextMenuModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    void load() override;
    Q_INVOKABLE void handleMenuItem(const QString& itemId) override;

    int trackId() const { return m_trackId; }
    void setTrackId(int trackId);

    QString trackTitle() const { return m_trackTitle; }
    void setTrackTitle(const QString& trackTitle);

signals:
    void trackIdChanged();
    void trackTitleChanged();

private:
    void onActionsStateChanges(const muse::actions::ActionCodeList& codes) override;
    muse::uicomponents::MenuItem* makeSpectralEffectItem(SpectralEffectId id);
    int m_trackId = -1;
    muse::String m_trackTitle;
};
}
