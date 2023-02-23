#include <QDebug>
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
   if (volume < 0.0) {
      qDebug() << "volume cannot be less than 0";
      return;
   }

   if (volume > 1.0) {
      qDebug() << "volume cannot be greater than 1";
      return;
   }

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
   if (value < 0.0) {
      qDebug() << "VolumeControlHandler left value cannot be less than 0";
      return;
   }

   if (value > 1.0) {
      qDebug() << "VolumeControlHandler left value cannot be greater than 1";
      return;
   }

   m_leftValue = value;
   emit leftValueChanged(value);
}

qreal VolumeControlHandler::RightValue() const
{
   return m_rightValue;
}

void VolumeControlHandler::SetRightValue(qreal value)
{
   if (value < 0.0) {
      qDebug() << "VolumeControlHandler right value cannot be less than 0";
      return;
   }

   if (value > 1.0) {
      qDebug() << "VolumeControlHandler right value cannot be greater than 1";
      return;
   }

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
   //qDebug() << "Changing volume level" << volume;
   m_volume = volume;
}
