#include <QtGlobal>
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
   Q_ASSERT(value >= 0.0 && value <= 1.0);

   m_value = value;
   emit valueChanged(value);
}

void MeterPanelHandler::Reset()
{
   m_value = 0.0;
   emit resetIndicators();
}
