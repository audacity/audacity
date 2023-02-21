#ifndef TRANSPORT_TOOLBAR_HANDLER_H
#define TRANSPORT_TOOLBAR_HANDLER_H

#include <QtCore/QObject>
#include <QtQml/qqmlregistration.h>

class TransportToolbarHandler : public QObject
{
   Q_OBJECT
   QML_ELEMENT

   Q_PROPERTY(bool isPlaying READ playing WRITE setPlaying)

signals:
   void updateStatusBar(QString status);
   void playStateChanged(bool isPlaying);

public:
   explicit TransportToolbarHandler(QObject *parent = nullptr);
   virtual ~TransportToolbarHandler() = default;

   bool playing() const;
   void setPlaying(bool state);

public slots:
   void play();
   void stop();
   void rewind();
   void fastForward();
   void record();
   void loop();

private:
   bool m_isPlaying{ false };
};

#endif
