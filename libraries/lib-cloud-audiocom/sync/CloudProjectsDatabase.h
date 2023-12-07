/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectsDatabase.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "sqlite/Connection.h"

namespace cloud::audiocom::sync
{
class CloudProjectsDatabase
{
   CloudProjectsDatabase();
   ~CloudProjectsDatabase() = default;

public:
   static CloudProjectsDatabase& Get();

   bool IsOpen() const;

   sqlite::Connection& GetConnection();
   const sqlite::Connection& GetConnection() const;

private:
   sqlite::Connection mConnection;

};
} // namespace cloud::audiocom::sync
