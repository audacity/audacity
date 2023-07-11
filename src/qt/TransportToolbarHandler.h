#pragma once

#include <QtCore/QBindable>
#include <QtCore/QObject>
#include <QtQml/qqmlregistration.h>
#include "uicomponents/ToolbarHandler.h"

class TransportToolbarHandler : public ToolbarHandler
{
   Q_OBJECT
   QML_ELEMENT

   Q_PROPERTY(bool isPlaying READ Playing WRITE SetPlaying)
   Q_PROPERTY(bool playVisible READ PlayVisible WRITE SetPlayVisible BINDABLE BindablePlayVisible)
   Q_PROPERTY(bool stopVisible READ StopVisible WRITE SetStopVisible BINDABLE BindableStopVisible)
   Q_PROPERTY(bool recordVisible READ RecordVisible WRITE SetRecordVisible BINDABLE BindableRecordVisible)
   Q_PROPERTY(bool rewindVisible READ RewindVisible WRITE SetRewindVisible BINDABLE BindableRewindVisible)
   Q_PROPERTY(bool fastForwardVisible READ FastForwardVisible WRITE SetFastForwardVisible BINDABLE BindableFastForwardVisible)
   Q_PROPERTY(bool loopVisible READ LoopVisible WRITE SetLoopVisible BINDABLE BindableLoopVisible)

signals:
   void playVisibleChanged(bool isVisible);
   void stopVisibleChanged(bool isVisible);
   void recordVisibleChanged(bool isVisible);
   void rewindVisibleChanged(bool isVisible);
   void fastForwardVisibleChanged(bool isVisible);
   void loopVisibleChanged(bool isVisible);
   void updateStatusBar(QString status);
   void playStateChanged(bool isPlaying);
   void playbackStopped();

public:
   explicit TransportToolbarHandler(QObject *parent = nullptr);
   virtual ~TransportToolbarHandler() = default;

   Q_INVOKABLE void RegisterToolbarConfiguration() override;
   void MonitorForConfigurationChanges() override;

   bool Playing() const;
   void SetPlaying(bool state);

   bool PlayVisible() const;
   void SetPlayVisible(bool isVisible);
   QBindable<bool> BindablePlayVisible();

   bool StopVisible() const;
   void SetStopVisible(bool isVisible);
   QBindable<bool> BindableStopVisible();

   bool RecordVisible() const;
   void SetRecordVisible(bool isVisible);
   QBindable<bool> BindableRecordVisible();

   bool RewindVisible() const;
   void SetRewindVisible(bool isVisible);
   QBindable<bool> BindableRewindVisible();

   bool FastForwardVisible() const;
   void SetFastForwardVisible(bool isVisible);
   QBindable<bool> BindableFastForwardVisible();

   bool LoopVisible() const;
   void SetLoopVisible(bool isVisible);
   QBindable<bool> BindableLoopVisible();

public slots:
   void Play();
   void Stop();
   void Rewind();
   void FastForward();
   void Record();
   void Loop();

private:
   bool m_isPlaying{ false };

   Q_OBJECT_BINDABLE_PROPERTY_WITH_ARGS(TransportToolbarHandler,
                                        bool, m_playVisible, true,
                                        &TransportToolbarHandler::playVisibleChanged);

   Q_OBJECT_BINDABLE_PROPERTY_WITH_ARGS(TransportToolbarHandler,
                                        bool, m_stopVisible, true,
                                        &TransportToolbarHandler::stopVisibleChanged);

   Q_OBJECT_BINDABLE_PROPERTY_WITH_ARGS(TransportToolbarHandler,
                                        bool, m_recordVisible, true,
                                        &TransportToolbarHandler::recordVisibleChanged);

   Q_OBJECT_BINDABLE_PROPERTY_WITH_ARGS(TransportToolbarHandler,
                                        bool, m_rewindVisible, true,
                                        &TransportToolbarHandler::rewindVisibleChanged);

   Q_OBJECT_BINDABLE_PROPERTY_WITH_ARGS(TransportToolbarHandler,
                                        bool, m_fastForwardVisible, true,
                                        &TransportToolbarHandler::fastForwardVisibleChanged);

   Q_OBJECT_BINDABLE_PROPERTY_WITH_ARGS(TransportToolbarHandler,
                                        bool, m_loopVisible, true,
                                        &TransportToolbarHandler::loopVisibleChanged);
};
