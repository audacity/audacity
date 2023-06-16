#include <algorithm>
#include <QFontDatabase>
#include <QGuiApplication>
#include <QMap>
#include <QString>
#include <QVariant>
#include "ApplicationConfiguration.h"

namespace
{
   enum ThemeStyleKey
   {
      BACKGROUND_COLOR_1,
      BACKGROUND_COLOR_2,
      BACKGROUND_COLOR_3,
      FONT_COLOR_1,
      FONT_COLOR_2,
      BUTTON_COLOR,
      ACCENT_COLOR,
      TEXT_FIELD_COLOR,
      TIMECODE_COLOR,
      PLAY_COLOR,
      RECORD_COLOR,
      STROKE_COLOR,
      WAVEFORM_RMS_COLOR,
      WAVEFORM_HIGHLIGHT_COLOR,
      WAVEFORM_PEAK_COLOR,
      CLIP_STROKE_COLOR,
      CLIP_HEADER_COLOR,
   };

   QString light("light");
   QString dark("dark");

   struct ThemeInfo
   {
      QString name;
      QMap<ThemeStyleKey, QVariant> values;

      QColor ToColor(const ThemeStyleKey& key) const
      {
         return values[key].value<QColor>();
      }
   };

   const QMap<ThemeStyleKey, QVariant> LIGHT_THEME_VALUES
   {
      { BACKGROUND_COLOR_1, "#F5F5F6" },
      { BACKGROUND_COLOR_2, "#E6E9ED" },
      { BACKGROUND_COLOR_3, "#333640" },
      { FONT_COLOR_1, "#111132" },
      { FONT_COLOR_2, "#FFFFFF" },
      { BUTTON_COLOR, "#CFD5DD" },
      { ACCENT_COLOR, "#9F9FFF" },
      { TEXT_FIELD_COLOR, "#FFFFFF" },
      { TIMECODE_COLOR, "#1E2026" },
      { PLAY_COLOR, "#0F7745" },
      { RECORD_COLOR, "#C54444" },
      { STROKE_COLOR, "#CED1D4" },
      { WAVEFORM_RMS_COLOR, "#9090F5" },
      { WAVEFORM_HIGHLIGHT_COLOR, "#ECF1FD" },
      { WAVEFORM_PEAK_COLOR, "#3232C8" },
      { CLIP_STROKE_COLOR, "#252527" },
      { CLIP_HEADER_COLOR, "#F3F3F3" }
   };

   const QMap<ThemeStyleKey, QVariant> DARK_THEME_VALUES
   {
      { BACKGROUND_COLOR_1, "#1F2225" },
      { BACKGROUND_COLOR_2, "#1A1D20" },
      { BACKGROUND_COLOR_3, "#131619" },
      { FONT_COLOR_1, "#EBF6FF" },
      { FONT_COLOR_2, "#D6EDFF" },
      { BUTTON_COLOR, "#2E3438" },
      { ACCENT_COLOR, "#008BF5" },
      { TEXT_FIELD_COLOR, "#1A2025" },
      { TIMECODE_COLOR, "#131619" },
      { PLAY_COLOR, "#0F7745" },
      { RECORD_COLOR, "#C54444" },
      { STROKE_COLOR, "#2E3439" },
      { WAVEFORM_RMS_COLOR, "#9090F5" },
      { WAVEFORM_HIGHLIGHT_COLOR, "#ECF1FD" },
      { WAVEFORM_PEAK_COLOR, "#3232C8" },
      { CLIP_STROKE_COLOR, "#42484B" },
      { CLIP_HEADER_COLOR, "#4F5559" }
   };

   using ThemeList = std::vector<ThemeInfo>;

   ThemeList themes
   {
      ThemeInfo { light, LIGHT_THEME_VALUES },
      ThemeInfo { dark, DARK_THEME_VALUES }
   };
}

ApplicationConfiguration::ApplicationConfiguration()
{
   QFontDatabase::addApplicationFont(":/fonts/MusescoreIcon.ttf");
   QFontDatabase::addApplicationFont(":/fonts/Lato-Bold.ttf");
   QFontDatabase::addApplicationFont(":/fonts/Lato-BoldItalic.ttf");
   QFontDatabase::addApplicationFont(":/fonts/Lato-Italic.ttf");
   QFontDatabase::addApplicationFont(":/fonts/Lato-Regular.ttf");

   SetCurrentTheme(light);

   m_timecodeFont = QFontDatabase::font("Lato", "Bold", 14);
   m_iconFont = QFontDatabase::font("MusescoreIcon", "", 12);
   m_bodyFont = QGuiApplication::font();
   m_bodyFont.setPixelSize(16);
}

QString ApplicationConfiguration::CurrentTheme() const
{
   return m_currentTheme;
}

void ApplicationConfiguration::SetCurrentTheme(const QString& newTheme)
{
   if (m_currentTheme == newTheme)
      return;

   auto it = std::find_if(themes.cbegin(), themes.cend(),
      [&newTheme](const ThemeInfo& theme)
      {
         return theme.name == newTheme;
      });

   if (it == themes.cend())
      return;

   const auto& theme = *it;

   m_backgroundColor1 = theme.ToColor(BACKGROUND_COLOR_1);
   m_backgroundColor2 = theme.ToColor(BACKGROUND_COLOR_2);
   m_backgroundColor3 = theme.ToColor(BACKGROUND_COLOR_3);
   m_fontColor1 = theme.ToColor(FONT_COLOR_1);
   m_fontColor2 = theme.ToColor(FONT_COLOR_2);
   m_buttonColor = theme.ToColor(BUTTON_COLOR);
   m_accentColor = theme.ToColor(ACCENT_COLOR);
   m_textFieldColor = theme.ToColor(TEXT_FIELD_COLOR);
   m_timecodeColor = theme.ToColor(TIMECODE_COLOR);
   m_playColor = theme.ToColor(PLAY_COLOR);
   m_recordColor = theme.ToColor(RECORD_COLOR);
   m_strokeColor = theme.ToColor(STROKE_COLOR);
   m_waveformRMSColor = theme.ToColor(WAVEFORM_RMS_COLOR);
   m_waveformHighlightColor = theme.ToColor(WAVEFORM_HIGHLIGHT_COLOR);
   m_waveformPeakColor = theme.ToColor(WAVEFORM_PEAK_COLOR);
   m_clipStrokeColor = theme.ToColor(CLIP_STROKE_COLOR);
   m_clipHeaderColor = theme.ToColor(CLIP_HEADER_COLOR);

   m_currentTheme = newTheme;

   emit themeChanged();
}
QFont ApplicationConfiguration::IconFont() const
{
   return m_iconFont;
}

QFont ApplicationConfiguration::BodyFont() const
{
   return m_bodyFont;
}

QFont ApplicationConfiguration::TimecodeFont() const
{
   return m_timecodeFont;
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

QColor ApplicationConfiguration::StrokeColor() const
{
   return m_strokeColor;
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

QColor ApplicationConfiguration::ClipStrokeColor() const
{
   return m_clipStrokeColor;
}

QColor ApplicationConfiguration::ClipHeaderColor() const
{
   return m_clipHeaderColor;
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
