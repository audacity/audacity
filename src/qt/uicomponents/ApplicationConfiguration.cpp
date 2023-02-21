#include <QFontDatabase>
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
   m_bodyFont = GetApplicationFont(":/fonts/Inter-Regular.ttf");
}

QFont ApplicationConfiguration::iconFont() const
{
   return m_iconFont;
}

QFont ApplicationConfiguration::bodyFont() const
{
   return m_bodyFont;
}

QColor ApplicationConfiguration::backgroundColor1() const
{
   return m_backgroundColor1;
}

QColor ApplicationConfiguration::backgroundColor2() const
{
   return m_backgroundColor2;
}

QColor ApplicationConfiguration::backgroundColor3() const
{
   return m_backgroundColor3;
}

QColor ApplicationConfiguration::fontColor1() const
{
   return m_fontColor1;
}

QColor ApplicationConfiguration::fontColor2() const
{
   return m_fontColor2;
}

QColor ApplicationConfiguration::buttonColor() const
{
   return m_buttonColor;
}

QColor ApplicationConfiguration::accentColor() const
{
   return m_accentColor;
}

QColor ApplicationConfiguration::textFieldColor() const
{
   return m_textFieldColor;
}

QColor ApplicationConfiguration::timecodeColor() const
{
   return m_timecodeColor;
}

QColor ApplicationConfiguration::playColor() const
{
   return m_playColor;
}

QColor ApplicationConfiguration::recordColor() const
{
   return m_recordColor;
}

QColor ApplicationConfiguration::strokeColor1() const
{
   return m_strokeColor1;
}

QColor ApplicationConfiguration::waveformRMSColor() const
{
   return m_waveformRMSColor;
}

QColor ApplicationConfiguration::waveformHighlightColor() const
{
   return m_waveformHighlightColor;
}

QColor ApplicationConfiguration::waveformPeakColor() const
{
   return m_waveformPeakColor;
}

QColor ApplicationConfiguration::clipRegionColor() const
{
   return m_clipRegionColor;
}

QColor ApplicationConfiguration::clipStrokeColor() const
{
   return m_clipRegionColor;
}

qreal ApplicationConfiguration::itemOpacityDisabled() const
{
   return m_itemOpacityDisabled;
}

qreal ApplicationConfiguration::buttonOpacityNormal() const
{
   return m_buttonOpacityNormal;
}

qreal ApplicationConfiguration::buttonOpacityHit() const
{
   return m_buttonOpacityHit;
}

qreal ApplicationConfiguration::buttonOpacityHover() const
{
   return m_buttonOpacityHover;
}

int ApplicationConfiguration::borderWidth() const
{
   return m_borderWidth;
}

int ApplicationConfiguration::defaultButtonSize() const
{
   return m_defaultButtonSize;
}
