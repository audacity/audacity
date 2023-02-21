#ifndef APPLICATION_CONFIGURATION_H
#define APPLICATION_CONFIGURATION_H

#include <QtCore/QObject>
#include <QtGui/QColor>
#include <QtGui/QFont>
#include <QtQml/qqml.h>

class ApplicationConfiguration : public QObject
{
   Q_OBJECT
   QML_ELEMENT

   Q_PROPERTY(QFont iconFont READ iconFont NOTIFY themeChanged FINAL)
   Q_PROPERTY(QFont bodyFont READ bodyFont NOTIFY themeChanged FINAL)

   Q_PROPERTY(QColor backgroundColor1 READ backgroundColor1 NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor backgroundColor2 READ backgroundColor2 NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor backgroundColor3 READ backgroundColor3 NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor fontColor1 READ fontColor1 NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor fontColor2 READ fontColor2 NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor buttonColor READ buttonColor NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor accentColor READ accentColor NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor textFieldColor READ textFieldColor NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor timecodeColor READ timecodeColor NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor playColor READ playColor NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor recordColor READ recordColor NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor strokeColor1 READ strokeColor1 NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor waveformRMSColor READ waveformRMSColor NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor waveformHighlightColor READ waveformHighlightColor NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor waveformPeakColor READ waveformPeakColor NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor clipRegionColor READ clipRegionColor NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor clipStrokeColor READ clipStrokeColor NOTIFY themeChanged FINAL)

   Q_PROPERTY(qreal itemOpacityDisabled READ itemOpacityDisabled NOTIFY themeChanged FINAL)
   Q_PROPERTY(qreal buttonOpacityNormal READ buttonOpacityNormal NOTIFY themeChanged FINAL)
   Q_PROPERTY(qreal buttonOpacityHit READ buttonOpacityHit NOTIFY themeChanged FINAL)
   Q_PROPERTY(qreal buttonOpacityHover READ buttonOpacityHover NOTIFY themeChanged FINAL)

   Q_PROPERTY(int borderWidth READ borderWidth NOTIFY themeChanged FINAL)
   Q_PROPERTY(int defaultButtonSize READ defaultButtonSize NOTIFY themeChanged FINAL)

signals:
   void themeChanged();

public:
   ApplicationConfiguration();

   QFont iconFont() const;
   QFont bodyFont() const;

   QColor backgroundColor1() const;
   QColor backgroundColor2() const;
   QColor backgroundColor3() const;
   QColor fontColor1() const;
   QColor fontColor2() const;
   QColor buttonColor() const;
   QColor accentColor() const;
   QColor textFieldColor() const;
   QColor timecodeColor() const;
   QColor playColor() const;
   QColor recordColor() const;
   QColor strokeColor1() const;
   QColor waveformRMSColor() const;
   QColor waveformHighlightColor() const;
   QColor waveformPeakColor() const;
   QColor clipRegionColor() const;
   QColor clipStrokeColor() const;

   qreal itemOpacityDisabled() const;
   qreal buttonOpacityNormal() const;
   qreal buttonOpacityHit() const;
   qreal buttonOpacityHover() const;

   int borderWidth() const;
   int defaultButtonSize() const;

private:
   QFont m_iconFont;
   QFont m_bodyFont;
   QColor m_backgroundColor1{ "#F5F5F6" };
   QColor m_backgroundColor2{ "#E6E9ED" };
   QColor m_backgroundColor3{ "#333640" };
   QColor m_fontColor1{ "#111132" };
   QColor m_fontColor2{ "#FFFFFF" };
   QColor m_buttonColor{ "#CFD5DD" };
   QColor m_accentColor{ "#9F9FFF" };
   QColor m_textFieldColor{ "#FFFFFF" };
   QColor m_timecodeColor{ "#1E2026" };
   QColor m_playColor{ "#0F7745" };
   QColor m_recordColor{ "#C54444" };
   QColor m_strokeColor1{ "#CED1D4" };
   QColor m_waveformRMSColor{ "#9090F5" };
   QColor m_waveformHighlightColor{ "#ECF1FD" };
   QColor m_waveformPeakColor{ "#3232C8" };
   QColor m_clipRegionColor{ "#C8C8D0" };
   QColor m_clipStrokeColor{ "#252527" };
   qreal m_itemOpacityDisabled{ 0.3 };
   qreal m_buttonOpacityNormal{ 0.7 };
   qreal m_buttonOpacityHit{ 1.0 };
   qreal m_buttonOpacityHover{ 0.5 };
   int m_borderWidth{ 0 };
   int m_defaultButtonSize{ 32 };
};

#endif
