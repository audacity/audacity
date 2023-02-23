#include <QDebug>
#include "MeterPanelHandler.h"

MeterPanelHandler::MeterPanelHandler(QObject* parent)
   : QObject(parent)
{
}

qreal MeterPanelHandler::Value() const
{
   return m_value;
}

void MeterPanelHandler::SetValue(qreal value)
{
   if (value < 0.0) {
      qDebug() << "MeterPanelHandler value cannot be less than 0";
      return;
   }

   if (value > 1.0) {
      qDebug() << "MeterPanelHandler value cannot be greater than 1";
      return;
   }

   m_value = value;
   emit valueChanged(value);
}

void MeterPanelHandler::Reset()
{
   m_value = 0.0;
   emit resetIndicators();
}
