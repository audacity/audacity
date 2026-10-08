## Cloud and out-of-process effects

We want to implement cloud effects (see `./cloud-effects-questions.md`) but this is a massive undertaking. I would like to explore what intermediary milestone could be reached which paves the way to cloud effects yet if possible already provides value to users.

This intermediary milestone could be out-of-process effects. The flows:

| Step                   | OOP           | Cloud                                                                      | Both                  |
| ---------------------- | ------------- | -------------------------------------------------------------------------- | --------------------- |
| Effect discovery       | ?             | Asks audiocom on startup (and maybe regularly?) about available effects    |                       |
| UI (optional)          |               |                                                                            | Specified in manifest |
| Effect input provision | extraction    | Query sent to server (with project revision), project checkout, extraction |                       |
| Start process          | async command | command (inherently async)                                                 |                       |
| Process finished       |
