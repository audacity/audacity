#pragma once

#include <QtGui/QColor>
#include <QtGui/QFont>
#include <QtQuick/QQuickPaintedItem>
#include <QtQml/qqmlregistration.h>

class QPainter;
class QWheelEvent;
class ViewInfo;

class AdornedQtRulerPanel : public QQuickPaintedItem
{
   Q_OBJECT
   QML_NAMED_ELEMENT(AdornedRulerPanel)

   Q_PROPERTY(int offset READ Offset WRITE SetOffset NOTIFY offsetChanged FINAL)
   Q_PROPERTY(QFont textFont READ TextFont WRITE SetTextFont NOTIFY textFontChanged FINAL)
   Q_PROPERTY(QColor textColor READ TextColor WRITE SetTextColor NOTIFY textColorChanged FINAL)
   Q_PROPERTY(QColor tickColor READ TickColor WRITE SetTickColor NOTIFY tickColorChanged FINAL)

signals:
   void offsetChanged();
   void textFontChanged();
   void textColorChanged();
   void tickColorChanged();

public:
   explicit AdornedQtRulerPanel(QQuickItem *parent = nullptr);
   virtual ~AdornedQtRulerPanel() = default;

   void paint(QPainter *painter) override;

   int Offset() const;
   void SetOffset(int offset);

   QFont TextFont() const;
   void SetTextFont(const QFont& font);

   QColor TextColor() const;
   void SetTextColor(const QColor& color);

   QColor TickColor() const;
   void SetTickColor(const QColor& color);

   Q_INVOKABLE void ZoomIn();
   Q_INVOKABLE void ZoomOut();
   Q_INVOKABLE void UpdateTheme();

protected:
   void wheelEvent(QWheelEvent *event) override;
   void componentComplete() override;

   int m_offset{ 0 };
   int m_interval{ 40 };
   ViewInfo* m_viewInfo{};
   QFont m_textFont{};
   QColor m_textColor{};
   QColor m_tickColor{};
};
