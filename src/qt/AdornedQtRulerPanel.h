#pragma once

#include <QtQuick/QQuickPaintedItem>
#include <QtQml/qqmlregistration.h>

class QPainter;
class QWheelEvent;

class AdornedQtRulerPanel : public QQuickPaintedItem
{
   Q_OBJECT
   QML_NAMED_ELEMENT(AdornedRulerPanel)

   Q_PROPERTY(int offset READ Offset WRITE SetOffset NOTIFY offsetChanged FINAL)

signals:
   void offsetChanged();

public:
   explicit AdornedQtRulerPanel(QQuickItem *parent = nullptr);
   virtual ~AdornedQtRulerPanel() = default;

   void paint(QPainter *painter) override;

   int Offset() const;
   void SetOffset(int offset);

   Q_INVOKABLE void ZoomIn();
   Q_INVOKABLE void ZoomOut();
   Q_INVOKABLE void UpdateTheme();

protected:
   void wheelEvent(QWheelEvent *event) override;

   int m_offset{ 0 };
   int m_interval{ 40 };
};
