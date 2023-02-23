#ifndef METER_PANEL_HANDLER_H
#define METER_PANEL_HANDLER_H

#include <QtCore/QObject>
#include <QtQml/qqmlregistration.h>

class MeterPanelHandler : public QObject
{
   Q_OBJECT
   QML_ELEMENT

   Q_PROPERTY(qreal value READ Value WRITE SetValue NOTIFY valueChanged)

signals:
   void resetIndicators();
   void valueChanged(qreal value);

public:
   explicit MeterPanelHandler(QObject *parent = nullptr);
   virtual ~MeterPanelHandler() = default;

   qreal Value() const;
   void SetValue(qreal value);

public slots:
   void Reset();

private:
   qreal m_value{ 0.0 };
};

#endif
