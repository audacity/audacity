/*  SPDX-License-Identifier: GPL-2.0-or-later */

#include "QtQuickUiServices.h"

#include <mutex>
#include <stdexcept>

#include "CodeConversions.h"
#include "QtQuickWindowPlacement.h"
#include "QMLEngineFactory.h"

#include <QWindow>
#include <QApplication>
#include <QAbstractEventDispatcher>
#include <QDesktopServices>
#include <QTimer>
#include <QQmlComponent>
#include <QQmlProperty>
#include <QDialogButtonBox>
#include <qthread.h>

using namespace BasicUI;

namespace
{
   int GetStandardButtons(const MessageBoxOptions& options)
   {
      int buttons = QDialogButtonBox::NoButton;
      switch(options.buttonStyle)
      {
      case Button::Default:
      case Button::Ok:
         buttons = QDialogButtonBox::Ok;
         break;
      case Button::YesNo:
         buttons = QDialogButtonBox::Yes | QDialogButtonBox::No;
      }
      if(options.cancelButton)
         buttons = buttons | QDialogButtonBox::Cancel;
      return buttons;
   }

   MessageBoxResult ToMessageBoxResult(int button)
   {
      switch(button)
      {
      case QDialogButtonBox::Yes:
         return MessageBoxResult::Yes;
      case QDialogButtonBox::No:
         return MessageBoxResult::No;
      case QDialogButtonBox::Ok:
         return MessageBoxResult::Ok;
      case QDialogButtonBox::Cancel:
         return MessageBoxResult::Cancel;
      default:
         return MessageBoxResult::None;
      }
   }

   QStringList ToQStringList(const TranslatableStrings& strings)
   {
      QStringList result;
      std::transform(
         strings.begin(),
         strings.end(),
         std::back_inserter(result),
         [](const auto& str) { return audacity::ToQString(str.Translation()); }
      );
      return result;
   }

   std::unique_ptr<QObject> LoadWindow(QQmlEngine& engine, const QString& dialogName, const QVariantMap& properties = {}, QWindow* parent = nullptr)
   {
      QQmlComponent component(&engine,
         QString("qrc%1/Audacity/QtInitUi/%2.qml")
            .arg(QT_INIT_QML_IMPORT_PATH)
            .arg(dialogName)
      );
      if(component.status() != QQmlComponent::Status::Ready)
      {
         qDebug() << component.errorString();
         throw std::runtime_error("failed to load component");
      }

      auto dialog = std::unique_ptr<QObject>(component.createWithInitialProperties(properties));

      if(parent != nullptr)
      {
         auto window = qobject_cast<QWindow*>(dialog.get());
         QQmlProperty::write(window, "modality", QVariant::fromValue(Qt::WindowModal));
         window->setParent(parent);
         QObject::connect(window, &QWindow::widthChanged, [window](int width)
         {
            if(auto parent = window->parent())
               window->setX((parent->width() - width) / 2);
         });
         QObject::connect(window, &QWindow::heightChanged, [window](int height)
         {
            if(auto parent = window->parent())
               window->setY((parent->height() - height) / 2);
         });
         return dialog;
      }
      return std::unique_ptr<QObject>(component.createWithInitialProperties(properties));
   }

   class QtQuickProgressDialog final : public ProgressDialog
   {
      std::unique_ptr<QObject> mDialog{};
      bool mOpened{false};
      int mResult{0};
   public:
      QtQuickProgressDialog(QQmlEngine& engine, unsigned flags)
      {
         mDialog = LoadWindow(engine, "ProgressDialog",
            {
               { "stopVisible", static_cast<bool>(flags & ProgressShowStop) },
               { "confirmStopOrCancel", static_cast<bool>(flags & ProgressConfirmStopOrCancel) }
            });
         Reinit();
      }

      ProgressResult Poll(unsigned long long numerator, unsigned long long denominator,
         const TranslatableString& message) override
      {
         if(!mOpened)
         {
            QMetaObject::invokeMethod(mDialog.get(), "show");
            mOpened = true;
         }
         if(mResult == 0)
         {
            const auto value = denominator > 0
               ? std::clamp(static_cast<double>(numerator) / static_cast<double>(denominator),
                     0.0,
                     1.0)
               : 0.0;
            QQmlProperty::write(mDialog.get(), "value", value);
            if(!message.empty())
               SetMessage(message);

            mResult = QQmlProperty::read(mDialog.get(), "result").toInt();
         }
         switch(mResult)
         {
         case 0: return ProgressResult::Success;
         case QDialogButtonBox::StandardButton::Abort:
            return ProgressResult::Stopped;
         case QDialogButtonBox::StandardButton::Cancel:
            return ProgressResult::Cancelled;
         default: return ProgressResult::Failed;
         }
      }

      void SetMessage(const TranslatableString& message) override
      {
         QQmlProperty::write(mDialog.get(), "text", audacity::ToQString(message.Translation()));
      }

      void SetDialogTitle(const TranslatableString& title) override
      {
         QQmlProperty::write(mDialog.get(), "title", audacity::ToQString(title.Translation()));
      }

      void Reinit() override
      {
         QQmlProperty::write(mDialog.get(), "from", 0.0);
         QQmlProperty::write(mDialog.get(), "to", 1.0);
         QQmlProperty::write(mDialog.get(), "value", 0.0);
      }
   };

   class QtQuickGenericProgressDialog : public GenericProgressDialog
   {
      std::unique_ptr<QObject> mDialog;
      bool mOpened{false};
   public:
      QtQuickGenericProgressDialog(QQmlEngine& engine,
         const QString& title,
         const QString& text,
         QWindow* parent = nullptr)
      {
         mDialog = LoadWindow(engine, "GenericProgressDialog", {}, parent);
         QQmlProperty::write(mDialog.get(), "title", title);
         QQmlProperty::write(mDialog.get(), "text", text);
      }

      ProgressResult Pulse() override
      {
         if(!mOpened)
         {
            QMetaObject::invokeMethod(mDialog.get(), "show");
            mOpened = true;
         }
         Yield();
         return ProgressResult::Success;
      }
   };
}

QQmlEngine& QtQuickUiServices::GetOrCreateEngine(const QObject* object)
{
   if(object != nullptr)
   {
      if(auto engine = qmlEngine(object))
         return *engine;
   }

   if(!mFallbackQMLEngine)
      mFallbackQMLEngine = audacity::QMLEngineFactory::Call();
   return *mFallbackQMLEngine;
}

QtQuickUiServices::QtQuickUiServices() = default;

QtQuickUiServices& QtQuickUiServices::Get()
{
   static std::once_flag init;
   static QtQuickUiServices services;
   std::call_once(init, [&]
   {
      BasicUI::Install(&services);
   });
   return services;
}

QtQuickUiServices::~QtQuickUiServices() = default;

void QtQuickUiServices::DoCallAfter(const Action& action)
{
   if(auto app = QApplication::instance())
   {
      auto timer = new QTimer;
      timer->setInterval(0);
      timer->setSingleShot(true);
      timer->start();
      QObject::connect(timer, &QTimer::timeout, action);
      timer->moveToThread(app->thread());
   }
   else
      qWarning("Application instance does not exist, action will not be executed!");
}

void QtQuickUiServices::DoYield()
{
   if(auto eventDispatcher = QAbstractEventDispatcher::instance())
      eventDispatcher->processEvents(QEventLoop::AllEvents);
}

void QtQuickUiServices::DoShowErrorDialog(const WindowPlacement& placement,
                       const TranslatableString& dlogTitle,
                       const TranslatableString& message,
                       const ManualPageID& helpPage,
                       const ErrorDialogOptions& options)
{
#ifdef HAS_SENTRY_REPORTING
   if(options.type == ErrorDialogType::ModalErrorReport)
         throw std::runtime_error();
#endif
   DoMessageBox(message,
      MessageBoxOptions{}
         .Caption(dlogTitle)
         .Parent(&const_cast<WindowPlacement&>(placement))
         .ButtonStyle(Button::Ok)
   );
}

MessageBoxResult
QtQuickUiServices::DoMessageBox(const TranslatableString& message,
                                BasicUI::MessageBoxOptions options)
{
   const auto parent = options.parent == nullptr
      ? nullptr
      : QtQuickWindowPlacement::Get(*options.parent);
   auto& engine = GetOrCreateEngine(parent);

   auto dialog = LoadWindow(engine, "MessageBox",
      {
         { "buttons", GetStandardButtons(options) },
         { "title", audacity::ToQString(options.caption.Translation()) },
         { "text", audacity::ToQString(message.Translation()) }
      }, parent);

   QMetaObject::invokeMethod(dialog.get(), "show");

   while(true)
   {
      if(!QQmlProperty::read(dialog.get(), "visible").toBool())
         return ToMessageBoxResult(QQmlProperty::read(dialog.get(), "result").toInt());
      DoYield();
   }
}

std::unique_ptr<ProgressDialog>
QtQuickUiServices::DoMakeProgress(const TranslatableString& title,
               const TranslatableString& message,
               unsigned flags,
               const TranslatableString& remainingLabelText)
{
   auto dialog = std::make_unique<QtQuickProgressDialog>(GetOrCreateEngine(nullptr), flags);
   dialog->SetDialogTitle(title);
   dialog->SetMessage(message);
   return dialog;
}

std::unique_ptr<GenericProgressDialog>
QtQuickUiServices::DoMakeGenericProgress(const WindowPlacement& placement,
                      const TranslatableString& title,
                      const TranslatableString& message)
{
   auto parent = QtQuickWindowPlacement::Get(placement);
   return std::make_unique<QtQuickGenericProgressDialog>(
      GetOrCreateEngine(parent),
      audacity::ToQString(title.Translation()),
      audacity::ToQString(message.Translation()),
      parent);
}

int QtQuickUiServices::DoMultiDialog(const TranslatableString& message,
                  const TranslatableString& title,
                  const TranslatableStrings& buttons,
                  const ManualPageID& helpPage,
                  const TranslatableString& boxMsg,
                  bool log)
{
   auto dialog = LoadWindow(
      GetOrCreateEngine(nullptr),
      audacity::ToQString(title.Translation()),
      {
         { "text" , audacity::ToQString(message.Translation()) },
         { "buttons" , ToQStringList(buttons) },
         { "title" , audacity::ToQString(title.Translation()) },
         { "groupTitle" , audacity::ToQString(boxMsg.Translation()) },
      });
   QMetaObject::invokeMethod(dialog.get(), "show");
   while(true)
   {
      if(!QQmlProperty::read(dialog.get(), "visible").toBool())
         return QQmlProperty::read(dialog.get(), "result").toInt();
      DoYield();
   }
}

bool QtQuickUiServices::DoOpenInDefaultBrowser(const wxString& url)
{
   return QDesktopServices::openUrl(QUrl(audacity::ToQString(url)));
}

std::unique_ptr<WindowPlacement> QtQuickUiServices::DoFindFocus()
{
   return std::make_unique<QtQuickWindowPlacement>(QGuiApplication::focusWindow());
}

void QtQuickUiServices::DoSetFocus(const WindowPlacement& focus)
{
   if(auto qwindow = QtQuickWindowPlacement::Get(focus))
      qwindow->requestActivate();
}

bool QtQuickUiServices::IsUsingRtlLayout() const
{
   return QGuiApplication::isRightToLeft();
}

bool QtQuickUiServices::IsUiThread() const
{
   if(auto app = QApplication::instance())
      return app->thread() == QThread::currentThread();
   return false;
}
