#include "UiTheme.h"

#include <QFontDatabase>
#include <QGuiApplication>
#include <QQmlComponent>
#include <QQmlEngine>
#include <QQmlProperty>
#include <QtCore/QDirIterator>

int UiTheme::TypeID = -1;

UiTheme::UiTheme(QObject *parent)
   : QObject(parent)
{
   mTimecodeFont = QFontDatabase::font("Lato", "Bold", 14);
   mIconFont = QFontDatabase::font("MusescoreIcon", "", 12);
   mBodyFont = QGuiApplication::font();
   mBodyFont.setPixelSize(16);
}

void UiTheme::applyTheme(QQmlEngine& engine, const QString& name)
{
   const auto path = QString("qrc:/audacity/imports/Audacity/UiThemes/qml/%2").arg(name);
   QQmlComponent component(&engine, path);
   if(component.status() != QQmlComponent::Status::Ready)
   {
      qWarning() << "Cannot create theme component" << component.errorString();
      return;
   }

   QScopedPointer theme(component.create());

   auto meta = theme->metaObject();
   for(int i = 0; i < meta->propertyCount(); ++i)
   {
      auto metaProperty = meta->property(i);
      auto value = QQmlProperty::read(theme.get(), metaProperty.name());
      if(!value.isValid())
         continue;
      if(!QQmlProperty::write(this, metaProperty.name(), value))
         qWarning() << "Unknown theme property: " << metaProperty.name();
   }
}

void UiTheme::Register()
{
   TypeID = qmlRegisterSingletonType<UiTheme>(
      "Audacity.UiThemes", 1, 0,
      "UiTheme",
      [](QQmlEngine* engine, QJSEngine*)
      {
         return new UiTheme;
      });
}

UiTheme* UiTheme::Get(QQmlEngine& engine)
{
   if(TypeID != -1)
      return engine.singletonInstance<UiTheme*>(TypeID);
   return nullptr;
}
QStringList UiTheme::themes()
{
   QStringList themes;
   QStringList excludeFiles{ "Theme.qml", "UiTheme.qml" };

   QDirIterator it(
      QString(":%1/Audacity/UiThemes")
         .arg(uithemes_QML_IMPORT_PREFIX),
      { "*.qml" },
      QDir::Files,
      QDirIterator::Subdirectories);
   while (it.hasNext())
   {
      auto path = it.next();
      auto fileName = QFileInfo(path).fileName();

      if (!excludeFiles.contains(fileName))
         themes << fileName;
   }

   return themes;
}

void UiTheme::applyTheme(const QString& name)
{
   applyTheme(*qmlEngine(this), name);
}

QFont UiTheme::iconFont() const
{
   return mIconFont;
}

QFont UiTheme::bodyFont() const
{
   return mBodyFont;
}

QFont UiTheme::timecodeFont() const
{
   return mTimecodeFont;
}

QColor UiTheme::backgroundColor1() const
{
   return mBackgroundColor1;
}

QColor UiTheme::backgroundColor2() const
{
   return mBackgroundColor2;
}

QColor UiTheme::backgroundColor3() const
{
   return mBackgroundColor3;
}

QColor UiTheme::backgroundColor4() const
{
   return mBackgroundColor4;
}

QColor UiTheme::fontColor1() const
{
   return mFontColor1;
}

QColor UiTheme::fontColor2() const
{
   return mFontColor2;
}

QColor UiTheme::buttonColor() const
{
   return mButtonColor;
}

QColor UiTheme::brandColor() const
{
   return mBrandColor;
}

QColor UiTheme::textFieldColor() const
{
   return mTextFieldColor;
}

QColor UiTheme::successColor() const
{
   return mSuccessColor;
}

QColor UiTheme::dangerColor() const
{
   return mDangerColor;
}

QColor UiTheme::strokeColor1() const
{
   return mStrokeColor1;
}

QColor UiTheme::strokeColor2() const
{
   return mStrokeColor2;
}

QColor UiTheme::strokeColor3() const
{
   return mStrokeColor3;
}

QColor UiTheme::waveformRMSColor() const
{
   return mWaveformRMSColor;
}

QColor UiTheme::waveformHighlightColor() const
{
   return mWaveformHighlightColor;
}

QColor UiTheme::waveformPeakColor() const
{
   return mWaveformPeakColor;
}

QColor UiTheme::clipStrokeColor() const
{
   return mClipStrokeColor;
}

QColor UiTheme::clipHeaderColor() const
{
   return mClipHeaderColor;
}

QColor UiTheme::textHighlightColor() const
{
   return mTextHighlightColor;
}

QColor UiTheme::invalidInputColor() const
{
   return mInvalidInputColor;
}
