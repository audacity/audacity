#include "Theme.h"

#include <QFontDatabase>
#include <QGuiApplication>

audacity::Theme::Theme(QObject* parent)
   : QObject(parent)
{
   mTimecodeFont = QFontDatabase::font("Lato", "Bold", 14);
   mIconFont = QFontDatabase::font("MusescoreIcon", "", 12);
   mBodyFont = QGuiApplication::font();
   mBodyFont.setPixelSize(16);
}

audacity::Theme::~Theme() = default;

QString audacity::Theme::name() const
{
   return mName;
}

void audacity::Theme::setName(const QString& value)
{
   mName = value;
}

QFont audacity::Theme::iconFont() const
{
   return mIconFont;
}

QFont audacity::Theme::bodyFont() const
{
   return mBodyFont;
}

QFont audacity::Theme::timecodeFont() const
{
   return mTimecodeFont;
}

QColor audacity::Theme::backgroundColor1() const
{
   return mBackgroundColor1;
}

QColor audacity::Theme::backgroundColor2() const
{
   return mBackgroundColor2;
}

QColor audacity::Theme::backgroundColor3() const
{
   return mBackgroundColor3;
}

QColor audacity::Theme::backgroundColor4() const
{
   return mBackgroundColor4;
}

QColor audacity::Theme::fontColor1() const
{
   return mFontColor1;
}

QColor audacity::Theme::fontColor2() const
{
   return mFontColor2;
}

QColor audacity::Theme::buttonColor() const
{
   return mButtonColor;
}

QColor audacity::Theme::brandColor() const
{
   return mBrandColor;
}

QColor audacity::Theme::textFieldColor() const
{
   return mTextFieldColor;
}

QColor audacity::Theme::successColor() const
{
   return mSuccessColor;
}

QColor audacity::Theme::dangerColor() const
{
   return mDangerColor;
}

QColor audacity::Theme::strokeColor1() const
{
   return mStrokeColor1;
}

QColor audacity::Theme::strokeColor2() const
{
   return mStrokeColor2;
}

QColor audacity::Theme::strokeColor3() const
{
   return mStrokeColor3;
}

QColor audacity::Theme::waveformRMSColor() const
{
   return mWaveformRMSColor;
}

QColor audacity::Theme::waveformHighlightColor() const
{
   return mWaveformHighlightColor;
}

QColor audacity::Theme::waveformPeakColor() const
{
   return mWaveformPeakColor;
}

QColor audacity::Theme::clipStrokeColor() const
{
   return mClipStrokeColor;
}

QColor audacity::Theme::clipHeaderColor() const
{
   return mClipHeaderColor;
}

QColor audacity::Theme::textHighlightColor() const
{
   return mTextHighlightColor;
}

QColor audacity::Theme::invalidInputColor() const
{
   return mInvalidInputColor;
}
