/**********************************************************************

Sneedacity: A Digital Audio Editor

ProjectHistory.h

Paul Licameli split from ProjectManager.h

**********************************************************************/

#ifndef __SNEEDACITY_PROJECT_HISTORY__
#define __SNEEDACITY_PROJECT_HISTORY__

#include "ClientData.h"

class SneedacityProject;
struct UndoState;
enum class UndoPush : unsigned char;

class SNEEDACITY_DLL_API ProjectHistory final
   : public ClientData::Base
{
public:
   static ProjectHistory &Get( SneedacityProject &project );
   static const ProjectHistory &Get( const SneedacityProject &project );

   explicit ProjectHistory( SneedacityProject &project )
      : mProject{ project }
   {}
   ProjectHistory( const ProjectHistory & ) PROHIBITED;
   ProjectHistory &operator=( const ProjectHistory & ) PROHIBITED;
   ~ProjectHistory() override;

   void InitialState();
   void SetStateTo(unsigned int n, bool doAutosave = true);
   bool UndoAvailable() const;
   bool RedoAvailable() const;
   void PushState(
      const TranslatableString &desc,
      const TranslatableString &shortDesc); // use UndoPush::AUTOSAVE
   void PushState(
      const TranslatableString &desc,
      const TranslatableString &shortDesc, UndoPush flags);
   void RollbackState();
   void ModifyState(bool bWantsAutoSave);    // if true, writes auto-save file.
      // Should set only if you really want the state change restored after
      // a crash, as it can take many seconds for large (eg. 10 track-hours)
      // projects
   void PopState(const UndoState &state, bool doAutosave = true);

   bool GetDirty() const { return mDirty; }
   void SetDirty( bool value ) { mDirty = value; }

private:
   SneedacityProject &mProject;

   bool mDirty{ false };
};

#endif
