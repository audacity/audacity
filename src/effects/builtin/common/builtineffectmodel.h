/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "au3-components/EffectInterface.h"

#include "modularity/ioc.h"
#include "effects/effects_base/ieffectsprovider.h"
#include "effects/effects_base/irealtimeeffectservice.h"
#include "effects/effects_base/view/abstracteffectviewmodel.h"
#include "trackedit/iprojecthistory.h"

namespace au::effects {
class BuiltinEffectModel : public AbstractEffectViewModel
{
    Q_OBJECT
    Q_PROPERTY(QString effectId READ effectId NOTIFY effectIdChanged FINAL)
    Q_PROPERTY(bool usesPresets READ usesPresets CONSTANT FINAL)

public:
    muse::Inject<IRealtimeEffectService> realtimeEffectService{ this };
    muse::Inject<trackedit::IProjectHistory> projectHistory{ this };
    muse::Inject<IEffectsProvider> effectsProvider{ this };

public:
    BuiltinEffectModel(QObject* parent, int instanceId);

    QString effectId() const;

    Q_INVOKABLE void commitSettings();

    virtual bool usesPresets() const { return true; }

signals:
    void effectIdChanged();

protected:

    virtual void doReload() = 0;
    virtual void doUpdateSettings() {}

    std::shared_ptr<effects::EffectInstance> instance() const;
    const EffectSettings& settings() const;
    void modifySettings(const std::function<void(EffectSettings& settings)>&);

    template<typename T>
    const T& settings() const
    {
        const T* st = settings().cast<T>();
        IF_ASSERT_FAILED(st) {
            static T null;
            return null;
        }
        return *st;
    }

    template<typename EffectType>
    const EffectType& effect() const
    {
        const EffectId effectId = this->effectId();
        const Effect* e = effectsProvider()->effect(effectId);
        return *dynamic_cast<const EffectType*>(e);
    }

    template<typename EffectType>
    EffectType& effect()
    {
        return const_cast<EffectType&>(static_cast<const BuiltinEffectModel*>(this)->effect<EffectType>());
    }

private:
    void doInit() override;
    void doStartPreview() override;
    void doStopPreview() override;

    EffectSettingsAccessPtr settingsAccess() const;
};
}
