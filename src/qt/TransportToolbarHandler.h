#ifndef TRANSPORT_TOOLBAR_HANDLER_H
#define TRANSPORT_TOOLBAR_HANDLER_H

#include <QtCore/QObject>
#include <QtQml/qqmlregistration.h>

class TransportToolbarHandler : public QObject
{
   Q_OBJECT
   QML_ELEMENT

   Q_PROPERTY(bool isPlaying READ Playing WRITE SetPlaying)

signals:
   void updateStatusBar(QString status);
   void playStateChanged(bool isPlaying);
   void playbackStopped();

public:
   explicit TransportToolbarHandler(QObject *parent = nullptr);
   virtual ~TransportToolbarHandler() = default;

   bool Playing() const;
   void SetPlaying(bool state);

public slots:
   void Play();
   void Stop();
   void Rewind();
   void FastForward();
   void Record();
   void Loop();

private:
   bool m_isPlaying{ false };
};

#endif
