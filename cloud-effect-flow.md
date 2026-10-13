# "Apply in other checkout" flow

A destructive effect is applied by a second Audacity instance (the "side", later a cloud effect) working on a checkout of the project. Meanwhile the user can keep working in the main instance.

```mermaid
sequenceDiagram
    actor User
    participant Main as Main instance
    participant AC as audio.com
    participant Side as Side instance

    User->>Main: Effect dialog, "Apply in other checkout", Apply
    Note over Main: (1) Effect + settings become an action
    Note over Main: (2) Lock the selection
    Main->>AC: (3) Save (snapshot S1)
    Main->>Side: (4) Start with S1, the action and a sign-in token
    loop (5) Watch for a new head
        Main->>AC: Poll
    end

    Side->>AC: (6) Open S1 as a checkout
    Note over Side: (6) Lock everything outside the selection
    Note over Side: (7) Apply the effect
    User->>Main: Edit locked audio
    Main->>User: (2) "This audio is locked" warning
    User->>Main: Cancel
    Note over Main: (2) Edit rolled back
    Side->>AC: (8) Save
    opt (9) Main saved meanwhile (conflict)
        Side->>AC: Rebase onto the head and save
    end
    Note over Side: (10) Quit

    Main->>AC: Poll: new head
    Main->>User: (11) Sync / Discard / Do nothing?
    User->>Main: Sync
    Main->>AC: (12) Fetch the changes
    Note over Main: (12) Check only locked audio changed,<br/>apply in place, unlock,<br/>history item named after the effect
```

## Steps

| Step | What                                                                                                                    | Depends on | Remarks                                                                                                                                                  |
| ---- | ----------------------------------------------------------------------------------------------------------------------- | ---------- | -------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 1    | Effect dialog option; effect settings serialized as an `action://effects/apply?…` query                                 |            | Option is demo code. In prod, cloud effects have a little icon.                                                                                          |
| 2    | Lock the selection: split edge blocks, lock inner blocks, `modifyState` + `markUnsaved`, locked audio visual indication | F1         | An undo past the locking throws a "Cancel job?" warning. Improvement ideas welcome.                                                                      |
| 3    | Save to audio.com before handing over (also for new, never-saved projects)                                              |            | UX: careful with this requirement. Would work on local server without an account.                                                                        |
| 4    | Start the side process: `--checkout`, own sync database, action, sign-in token handed over safely (?)                   |            | (?): written to a file, readable by users only, and deleted after read by side job. Need to figure out how it will be when side runs on audiocom server. |
| 5    | Poll audio.com for a new project head                                                                                   |            |
| 6    | Open a snapshot as a checkout; lock its complement                                                                      | 2          |
| 7    | Dispatch the action once the project is open                                                                            | 1, 6       |
| 8    | Save the checkout to audio.com                                                                                          |            |
| 9    | Rebase the checkout onto the head on version conflict                                                                   | F2         |
| 10   | Quit once saved                                                                                                         | 8, 9       |
| 11   | Ask the user what to do with the new head                                                                               | 5          |
| 12   | Integrate the head in place, keeping unsaved work; name the history item                                                | F2         |

Foundations, needed before the steps above:

- **F1** Persistent clip and track IDs (`uid`), so clips can be matched across copies of the project.
- **F2** Project patches: compute what a checkout changed (replaced blocks, new clips/tracks) and apply it to another version of the project, with unit tests. Depends on F1.
