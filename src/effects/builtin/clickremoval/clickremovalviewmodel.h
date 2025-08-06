/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../common/abstracteffectmodel.h"

#include "effects/effects_base/ieffectsprovider.h"

namespace au::effects {
class ClickRemovalEffect;
class ClickRemovalViewModel : public AbstractEffectModel
{
    Q_OBJECT

    Q_PROPERTY(int threshold READ threshold WRITE setThreshold NOTIFY thresholdChanged FINAL)
    Q_PROPERTY(int thresholdMin READ thresholdMin CONSTANT FINAL)
    Q_PROPERTY(int thresholdMax READ thresholdMax CONSTANT FINAL)
    Q_PROPERTY(int thresholdStep READ thresholdStep CONSTANT FINAL)

    Q_PROPERTY(int width READ width WRITE setWidth NOTIFY widthChanged FINAL)
    Q_PROPERTY(int widthMin READ widthMin CONSTANT FINAL)
    Q_PROPERTY(int widthMax READ widthMax CONSTANT FINAL)
    Q_PROPERTY(int widthStep READ widthStep CONSTANT FINAL)

    muse::Inject<IEffectsProvider> effectsProvider;

public:
    ClickRemovalViewModel() = default;

    int threshold() const;
    void setThreshold(int newThreshold);
    int thresholdMin() const;
    int thresholdMax() const;
    int thresholdStep() const;

    int width() const;
    void setWidth(int newWidth);
    int widthMin() const;
    int widthMax() const;
    int widthStep() const;

signals:
    void thresholdChanged();
    void widthChanged();

private:
    void doReload() override;

    ClickRemovalEffect* effect() const;
};
}
