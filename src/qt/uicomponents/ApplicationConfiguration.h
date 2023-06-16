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

   Q_PROPERTY(QString currentTheme READ CurrentTheme WRITE SetCurrentTheme NOTIFY themeChanged FINAL)

   Q_PROPERTY(QFont iconFont READ IconFont NOTIFY themeChanged FINAL)
   Q_PROPERTY(QFont bodyFont READ BodyFont NOTIFY themeChanged FINAL)
   Q_PROPERTY(QFont timecodeFont READ TimecodeFont NOTIFY themeChanged FINAL)

   Q_PROPERTY(QColor backgroundColor1 READ BackgroundColor1 NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor backgroundColor2 READ BackgroundColor2 NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor backgroundColor3 READ BackgroundColor3 NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor fontColor1 READ FontColor1 NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor fontColor2 READ FontColor2 NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor buttonColor READ ButtonColor NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor accentColor READ AccentColor NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor textFieldColor READ TextFieldColor NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor timecodeColor READ TimecodeColor NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor playColor READ PlayColor NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor recordColor READ RecordColor NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor strokeColor READ StrokeColor NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor waveformRMSColor READ WaveformRMSColor NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor waveformHighlightColor READ WaveformHighlightColor NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor waveformPeakColor READ WaveformPeakColor NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor clipStrokeColor READ ClipStrokeColor NOTIFY themeChanged FINAL)
   Q_PROPERTY(QColor clipHeaderColor READ ClipHeaderColor NOTIFY themeChanged FINAL)

   Q_PROPERTY(qreal itemOpacityDisabled READ ItemOpacityDisabled NOTIFY themeChanged FINAL)
   Q_PROPERTY(qreal buttonOpacityNormal READ ButtonOpacityNormal NOTIFY themeChanged FINAL)
   Q_PROPERTY(qreal buttonOpacityHit READ ButtonOpacityHit NOTIFY themeChanged FINAL)
   Q_PROPERTY(qreal buttonOpacityHover READ ButtonOpacityHover NOTIFY themeChanged FINAL)

   Q_PROPERTY(int borderWidth READ BorderWidth NOTIFY themeChanged FINAL)
   Q_PROPERTY(int defaultButtonSize READ DefaultButtonSize NOTIFY themeChanged FINAL)

signals:
   void themeChanged();

public:
   ApplicationConfiguration();

   QString CurrentTheme() const;
   void SetCurrentTheme(const QString& newTheme);

   QFont IconFont() const;
   QFont BodyFont() const;
   QFont TimecodeFont() const;

   QColor BackgroundColor1() const;
   QColor BackgroundColor2() const;
   QColor BackgroundColor3() const;
   QColor FontColor1() const;
   QColor FontColor2() const;
   QColor ButtonColor() const;
   QColor AccentColor() const;
   QColor TextFieldColor() const;
   QColor TimecodeColor() const;
   QColor PlayColor() const;
   QColor RecordColor() const;
   QColor StrokeColor() const;
   QColor WaveformRMSColor() const;
   QColor WaveformHighlightColor() const;
   QColor WaveformPeakColor() const;
   QColor ClipStrokeColor() const;
   QColor ClipHeaderColor() const;

   qreal ItemOpacityDisabled() const;
   qreal ButtonOpacityNormal() const;
   qreal ButtonOpacityHit() const;
   qreal ButtonOpacityHover() const;

   int BorderWidth() const;
   int DefaultButtonSize() const;

private:
   QString m_currentTheme;
   QFont m_iconFont;
   QFont m_bodyFont;
   QFont m_timecodeFont;
   QColor m_backgroundColor1;
   QColor m_backgroundColor2;
   QColor m_backgroundColor3;
   QColor m_fontColor1;
   QColor m_fontColor2;
   QColor m_buttonColor;
   QColor m_accentColor;
   QColor m_textFieldColor;
   QColor m_timecodeColor;
   QColor m_playColor;
   QColor m_recordColor;
   QColor m_strokeColor;
   QColor m_waveformRMSColor;
   QColor m_waveformHighlightColor;
   QColor m_waveformPeakColor;
   QColor m_clipStrokeColor;
   QColor m_clipHeaderColor;
   qreal m_itemOpacityDisabled{ 0.3 };
   qreal m_buttonOpacityNormal{ 0.7 };
   qreal m_buttonOpacityHit{ 1.0 };
   qreal m_buttonOpacityHover{ 0.5 };
   int m_borderWidth{ 0 };
   int m_defaultButtonSize{ 32 };
};

#endif
