/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "trackedit/itrackeditinteraction.h"
#include "trackedit/iselectioncontroller.h"

namespace au::projectscene {
class PitchAndSpeedChangeModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<trackedit::ITrackeditInteraction> trackeditInteraction;
    muse::Inject<trackedit::ISelectionController> selectionController;

    Q_PROPERTY(int pitch READ pitch WRITE setPitch NOTIFY pitchChanged FINAL)
    Q_PROPERTY(int speedPercentage READ speedPercentage WRITE setSpeedPercentage NOTIFY speedPercentageChanged FINAL)
    Q_PROPERTY(bool optimizeForVoice READ optimizeForVoice WRITE setOptimizeForVoice NOTIFY optimizeForVoiceChanged FINAL)

public:
    PitchAndSpeedChangeModel(QObject* parent = nullptr);

    Q_INVOKABLE void load(const QString& trackIdStr, const QString& clipIdStr);

    int pitch() const;
    void setPitch(int pitch);

    int speedPercentage() const;
    void setSpeedPercentage(int speedPercentage);

    bool optimizeForVoice() const;
    void setOptimizeForVoice(bool optimize);

signals:
    void pitchChanged();
    void speedPercentageChanged();
    void optimizeForVoiceChanged();

    void closeDialogRequested();

private:
    trackedit::ITrackeditProjectPtr trackeditProject() const;

    void setClip(const trackedit::Clip& clip);

    trackedit::Clip m_clip;
};
}
