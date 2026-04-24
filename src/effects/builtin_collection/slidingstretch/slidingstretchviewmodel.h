/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "effects/builtin/view/builtineffectmodel.h"

namespace au::effects {
class SlidingStretchViewModel : public BuiltinEffectModel
{
    Q_OBJECT

public:
    SlidingStretchViewModel(QObject* parent, int instanceId);

    Q_INVOKABLE double pctToSemitones(double pct) const;
    Q_INVOKABLE double semitonesToPct(double semitones) const;

private:
    void doReload() override;
};

class SlidingStretchViewModelFactory : public EffectViewModelFactory<SlidingStretchViewModel>
{
};
}
