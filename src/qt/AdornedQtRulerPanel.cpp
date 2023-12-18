#include <cassert>
#include <QtCore/QDebug>
#include <QtCore/QPair>
#include <QtCore/QPoint>
#include <QtCore/QString>
#include <QtCore/QVector>
#include <QtGui/QColor>
#include <QtGui/QPainter>
#include <QtGui/QStaticText>
#include <QtGui/QWheelEvent>
#include <QtQml/QQmlContext>
#include "AdornedQtRulerPanel.h"
#include "ProjectQMLEnvironment.h"
#include "ViewInfo.h"

AdornedQtRulerPanel::AdornedQtRulerPanel(QQuickItem *parent)
   : QQuickPaintedItem(parent)
{
}

void AdornedQtRulerPanel::paint(QPainter *painter)
{
   assert(painter != nullptr);
   if (painter == nullptr)
      return;

   const auto w = width();
   const auto h = height();

   const auto littleTick = h / 8;
   const auto bigTick = h / 4;
   const auto fullTick = h;

   QVector<QLineF> ticks;
   QVector<QPair<int, QString>> values;

   int value = 0;
   int x = m_offset;

   while (x < w)
   {
      const int count = ticks.count();

      const int tickLength = [&]() {
         if (count % 10 == 0)
            return fullTick;
         else if (count % 2 == 0)
            return bigTick;
         else
            return littleTick;
      }();

      if (ticks.count() % 2 == 0)
      {
         values.append(
            qMakePair(
               x + (values.count() % 5 == 0 ? 3 : 0),
               QString("%1:%2").arg(value / 60).arg(value % 60, 2, 10, QChar('0'))
            )
         );
         value++;
      }

      ticks.append(QLineF(x, h - 2, x, h - 1 - tickLength));

      x += m_interval;
   }

   painter->save();

   QPen pen = painter->pen();
   pen.setWidth(1);

   if (m_textColor.isValid())
   {
      m_textFont.setPixelSize(12);

      for (qsizetype i = 0; i < values.count(); i++)
      {
         m_textFont.setBold(i % 5 == 0);
         pen.setColor(m_textColor);

         painter->setFont(m_textFont);
         painter->setPen(pen);
         painter->drawText(QRectF(values[i].first, 0, w, h / 2), Qt::AlignLeft | Qt::AlignVCenter, values[i].second);
      }
   }

   if (m_tickColor.isValid())
   {
      for (auto i = 0; i < ticks.count(); ++i)
      {
         m_tickColor.setAlphaF(i % 10 == 0 ? 0.6 : 1.0);
         pen.setColor(m_tickColor);

         painter->setPen(pen);
         painter->drawLine(ticks[i]);
      }
   }

   painter->restore();
}

int AdornedQtRulerPanel::Offset() const
{
   return m_offset;
}

void AdornedQtRulerPanel::SetOffset(int offset)
{
   if (m_offset != offset)
   {
      m_offset = offset;
      update();
      emit offsetChanged();
   }
}

QFont AdornedQtRulerPanel::TextFont() const
{
   return m_textFont;
}

void AdornedQtRulerPanel::SetTextFont(const QFont& font)
{
   if (m_textFont != font)
   {
      m_textFont = font;
      update();
      emit textFontChanged();
   }
}

QColor AdornedQtRulerPanel::TextColor() const
{
   return m_textColor;
}

void AdornedQtRulerPanel::SetTextColor(const QColor& color)
{
   if (m_textColor != color)
   {
      m_textColor = color;
      update();
      emit textColorChanged();
   }
}

QColor AdornedQtRulerPanel::TickColor() const
{
   return m_tickColor;
}

void AdornedQtRulerPanel::SetTickColor(const QColor& color)
{
   if (m_tickColor != color)
   {
      m_tickColor = color;
      update();
      emit tickColorChanged();
   }
}

void AdornedQtRulerPanel::ZoomIn()
{
   m_interval += 4;
   update();
}

void AdornedQtRulerPanel::ZoomOut()
{
   m_interval -= 4;
   if (m_interval < 1)
      m_interval = 4;

   update();
}

void AdornedQtRulerPanel::UpdateTheme()
{
   update();
}

void AdornedQtRulerPanel::wheelEvent(QWheelEvent *event)
{
   assert(event != nullptr);

   if (event->angleDelta().y() > 0)
   {
      m_interval += 4;
   }
   else
   {
      m_interval -= 4;
      if (m_interval < 1)
         m_interval = 4;
   }

   update();
}

void AdornedQtRulerPanel::componentComplete()
{
   QQuickItem::componentComplete();

   auto engine = qmlEngine(this);
   auto project = audacity::ProjectQMLEnvironment::GetProject(*engine);

   m_viewInfo = &ViewInfo::Get(*project);
}
