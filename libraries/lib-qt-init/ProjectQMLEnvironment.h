/*  SPDX-License-Identifier: GPL-2.0-or-later */

#pragma once

#include <QQmlEngine>

#include "Project.h"
#include "ClientData.h"

namespace audacity
{

/**
 * \brief Gives an access to a QML environment attached to a project
 */
class QT_INIT_API ProjectQMLEnvironment final
   : public ClientData::Base
{
   class Properties final
      : public ClientData::Site<Properties, QObject>
   {
   public:

      explicit Properties(ProjectQMLEnvironment& env);
      
      ProjectQMLEnvironment& mEnv;
   };

   AudacityProject& mProject;
   std::unique_ptr<QQmlEngine> mEngine;
   Properties mProperties;
public:

   ///\brief Binds object returned by a factory to the new variable
   ///in the root qml context of a project.
   ///Returned QObject will be owned by hosting environment.
   class QT_INIT_API Property final
   {
      friend class ProjectQMLEnvironment;
   public:
      /// \brief Factory adapter type
      using PropertyFactory = std::function<std::unique_ptr<QObject>(QQmlEngine&, AudacityProject&)>;
      /**
       * \param name Name of the new variable in the root context
       * \param factory Creates a variable
       */
      Property(const QString& name, PropertyFactory factory);
   private:
      static Properties::DataPointer CreateProperty(Properties& properties,
                                                    const QString& name,
                                                    const PropertyFactory& factory);

      Properties::RegisteredFactory mFactory;
   };

   //!Complementary adapter function that returns property value
   //!using attachment registration object as a key.
   template< typename ValueType = QObject >
   ValueType &GetProperty( const Property &key )
   {
      return mProperties.Get<ValueType>(key.mFactory);
   }

   explicit ProjectQMLEnvironment(AudacityProject& project);
   ~ProjectQMLEnvironment() override;

   AudacityProject& GetProject() noexcept;
   QQmlEngine& GetEngine() noexcept;

   static ProjectQMLEnvironment& Get(AudacityProject& project);
   static AudacityProject* GetProject(QQmlEngine& engine);
};

}
