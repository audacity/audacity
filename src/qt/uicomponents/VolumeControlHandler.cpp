#include <QtGlobal>
#include "VolumeControlHandler.h"

VolumeControlHandler::VolumeControlHandler(QObject* parent)
   : QObject(parent)
{
}

qreal VolumeControlHandler::Volume() const
{
   return m_volume;
}

void VolumeControlHandler::SetVolume(qreal volume)
{
   Q_ASSERT(volume >= 0.0 && volume <= 1.0);

   if (m_volume != volume) {
      m_volume = volume;
      emit volumeChanged(volume);
   }
}

qreal VolumeControlHandler::LeftValue() const
{
   return m_leftValue;
}

void VolumeControlHandler::SetLeftValue(qreal value)
{
   Q_ASSERT(value >= 0.0 && value <= 1.0);

   m_leftValue = value;
   emit leftValueChanged(value);
}

qreal VolumeControlHandler::RightValue() const
{
   return m_rightValue;
}

void VolumeControlHandler::SetRightValue(qreal value)
{
   Q_ASSERT(value >= 0.0 && value <= 1.0);

   m_rightValue = value;
   emit rightValueChanged(value);
}

void VolumeControlHandler::Reset()
{
   m_leftValue = 0.0;
   m_rightValue = 0.0;

   emit resetIndicators();
}

void VolumeControlHandler::ChangeVolume(qreal volume)
{
   m_volume = volume;
}
