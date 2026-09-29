/*
 * Audacity: A Digital Audio Editor
 */

#pragma once

#include <QWindow>
#include <QResizeEvent>
#include <AudioUnit/AudioComponent.h>
#include <AudioUnit/AudioUnit.h>

class AUControl : public QWindow
{
    Q_OBJECT

public:
    AUControl(QWindow* parent = nullptr);
    ~AUControl();

    bool create(AudioComponent comp, AudioUnit unit, bool custom);
    void close();
    void cocoaViewResized();

signals:
    void sizeChanged();

protected:
    void resizeEvent(QResizeEvent* event) override;

private:
    void createCocoa();
    void createGeneric();
    void setupView();

    void* mAUView;
    void* mView;
    AudioComponent mComponent;
    AudioUnit mUnit;
    QSize mFixedSize;
};
