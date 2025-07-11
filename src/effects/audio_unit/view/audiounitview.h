/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QQuickItem>

#include "global/modularity/ioc.h"
#include "effects/effects_base/ieffectinstancesregister.h"

class AUControl;

namespace au::effects {
class AudioUnitView : public QQuickItem
{
    Q_OBJECT
    Q_PROPERTY(int instanceId READ instanceId WRITE setInstanceId NOTIFY instanceIdChanged FINAL)
    Q_PROPERTY(int sidePadding READ sidePadding WRITE setSidePadding NOTIFY sidePaddingChanged FINAL)
    Q_PROPERTY(int topPadding READ topPadding WRITE setTopPadding NOTIFY topPaddingChanged FINAL)
    Q_PROPERTY(int bottomPadding READ bottomPadding WRITE setBottomPadding NOTIFY bottomPaddingChanged FINAL)
    Q_PROPERTY(int minimumWidth READ minimumWidth WRITE setMinimumWidth NOTIFY minimumWidthChanged FINAL)

    muse::Inject<IEffectInstancesRegister> instancesRegister;

public:
    AudioUnitView(QQuickItem* parent = nullptr);
    ~AudioUnitView() override;

    int instanceId() const;
    void setInstanceId(int newInstanceId);

    Q_INVOKABLE void init();
    Q_INVOKABLE void deinit();

    int sidePadding() const;
    void setSidePadding(int newSidePadding);

    int topPadding() const;
    void setTopPadding(int newTopPadding);

    int bottomPadding() const;
    void setBottomPadding(int newBottomPadding);

    int minimumWidth() const;
    void setMinimumWidth(int newMinimumWidth);

signals:
    void instanceIdChanged();
    void titleChanged();

    void sidePaddingChanged();
    void topPaddingChanged();
    void bottomPaddingChanged();
    void minimumWidthChanged();

private:
    void embedNativeView();

    void updateViewGeometry();

    int m_instanceId = -1;

    std::unique_ptr<AUControl> m_auControl;

    int m_sidePadding = 0;
    int m_topPadding = 0;
    int m_bottomPadding = 0;
    int m_minimumWidth = 0;
};
}
