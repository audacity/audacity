/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "playback/iplayback.h"

#include "uicomponents/qml/Muse/UiComponents/internal/tableviewcell.h" // todo: public?

namespace au::projectscene {
class LabelsTableViewTimecodeCell : public muse::uicomponents::TableViewCell
{
    Q_OBJECT
    QML_ELEMENT;

    Q_PROPERTY(double sampleRate READ sampleRate NOTIFY sampleRateChanged FINAL)
    Q_PROPERTY(double tempo READ tempo NOTIFY timeSignatureChanged FINAL)
    Q_PROPERTY(int upperTimeSignature READ upperTimeSignature NOTIFY timeSignatureChanged FINAL)
    Q_PROPERTY(int lowerTimeSignature READ lowerTimeSignature NOTIFY timeSignatureChanged FINAL)

    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<playback::IPlayback> playback;

public:
    explicit LabelsTableViewTimecodeCell(QObject* parent = nullptr);
    LabelsTableViewTimecodeCell(const TableViewCell* other);

    double sampleRate() const;
    double tempo() const;
    int upperTimeSignature() const;
    int lowerTimeSignature() const;

signals:
    void sampleRateChanged();
    void timeSignatureChanged();
};
}
