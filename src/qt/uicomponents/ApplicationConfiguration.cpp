#include <QFontDatabase>
#include <QGuiApplication>
#include "ApplicationConfiguration.h"

namespace {
   QMap<QString, QFont> applicationFonts;

   void LoadApplicationFont(const QString& filename)
   {
      if (applicationFonts.contains(filename))
         return;

      auto id = QFontDatabase::addApplicationFont(filename);
      if (id == -1)
         return;

      auto family = QFontDatabase::applicationFontFamilies(id).at(0);
      applicationFonts.insert(filename, QFont(family));
   }

   QFont GetApplicationFont(const QString& filename)
   {
      LoadApplicationFont(filename);
      return applicationFonts[filename];
   }
}

ApplicationConfiguration::ApplicationConfiguration()
{
   m_iconFont = GetApplicationFont(":/fonts/MusescoreIcon.ttf");

   m_bodyFont = QGuiApplication::font();
   m_bodyFont.setPixelSize(16);
}

QFont ApplicationConfiguration::IconFont() const
{
   return m_iconFont;
}

QFont ApplicationConfiguration::BodyFont() const
{
   return m_bodyFont;
}

QColor ApplicationConfiguration::BackgroundColor1() const
{
   return m_backgroundColor1;
}

QColor ApplicationConfiguration::BackgroundColor2() const
{
   return m_backgroundColor2;
}

QColor ApplicationConfiguration::BackgroundColor3() const
{
   return m_backgroundColor3;
}

QColor ApplicationConfiguration::FontColor1() const
{
   return m_fontColor1;
}

QColor ApplicationConfiguration::FontColor2() const
{
   return m_fontColor2;
}

QColor ApplicationConfiguration::ButtonColor() const
{
   return m_buttonColor;
}

QColor ApplicationConfiguration::AccentColor() const
{
   return m_accentColor;
}

QColor ApplicationConfiguration::TextFieldColor() const
{
   return m_textFieldColor;
}

QColor ApplicationConfiguration::TimecodeColor() const
{
   return m_timecodeColor;
}

QColor ApplicationConfiguration::PlayColor() const
{
   return m_playColor;
}

QColor ApplicationConfiguration::RecordColor() const
{
   return m_recordColor;
}

QColor ApplicationConfiguration::StrokeColor1() const
{
   return m_strokeColor1;
}

QColor ApplicationConfiguration::WaveformRMSColor() const
{
   return m_waveformRMSColor;
}

QColor ApplicationConfiguration::WaveformHighlightColor() const
{
   return m_waveformHighlightColor;
}

QColor ApplicationConfiguration::WaveformPeakColor() const
{
   return m_waveformPeakColor;
}

QColor ApplicationConfiguration::ClipRegionColor() const
{
   return m_clipRegionColor;
}

QColor ApplicationConfiguration::ClipStrokeColor() const
{
   return m_clipRegionColor;
}

qreal ApplicationConfiguration::ItemOpacityDisabled() const
{
   return m_itemOpacityDisabled;
}

qreal ApplicationConfiguration::ButtonOpacityNormal() const
{
   return m_buttonOpacityNormal;
}

qreal ApplicationConfiguration::ButtonOpacityHit() const
{
   return m_buttonOpacityHit;
}

qreal ApplicationConfiguration::ButtonOpacityHover() const
{
   return m_buttonOpacityHover;
}

int ApplicationConfiguration::BorderWidth() const
{
   return m_borderWidth;
}

int ApplicationConfiguration::DefaultButtonSize() const
{
   return m_defaultButtonSize;
}
