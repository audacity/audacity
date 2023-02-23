#ifndef VOLUME_CONTROL_HANDLER_H
#define VOLUME_CONTROL_HANDLER_H

#include <QtCore/QObject>
#include <QtQml/qqmlregistration.h>

class VolumeControlHandler : public QObject
{
   Q_OBJECT
   QML_ELEMENT

   Q_PROPERTY(qreal volume READ Volume WRITE SetVolume NOTIFY volumeChanged)
   Q_PROPERTY(qreal leftValue READ LeftValue WRITE SetLeftValue NOTIFY leftValueChanged)
   Q_PROPERTY(qreal rightValue READ RightValue WRITE SetRightValue NOTIFY rightValueChanged)

signals:
   void resetIndicators();
   void volumeChanged(qreal volume);
   void leftValueChanged(qreal value);
   void rightValueChanged(qreal value);

public:
   explicit VolumeControlHandler(QObject *parent = nullptr);
   virtual ~VolumeControlHandler() = default;

   qreal Volume() const;
   void SetVolume(qreal volume);

   qreal LeftValue() const;
   void SetLeftValue(qreal value);
   qreal RightValue() const;
   void SetRightValue(qreal value);

public slots:
   void Reset();
   void ChangeVolume(qreal volume);

private:
   qreal m_volume{ 0.5 };
   qreal m_leftValue{ 0.0 };
   qreal m_rightValue{ 0.0 };
};

#endif
