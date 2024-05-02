There are several ways to contribute to Audacity:

## Developing

Audacity is mostly coded in C++. You'll need to be at least somewhat comfortable in it to start contributing code.

If you want to develop Audacity:

* Build instructions can be found in the BUILDING.md
* [Coding standards](https://audacity.gitbook.io/dev/getting-started/coding-standards) and other info on the process can be found [in the dev resources](https://audacity.gitbook.io/dev/).
* You will need to [sign the CLA](https://www.audacityteam.org/cla/) to contribute code.
* If you need help, you can ask in the [Audacity dev discord](https://discord.gg/N3XKxzTrq3).

### Developing Plug-ins

Audacity supports a wide variety of plug-in APIs, namely [Nyquist](https://manual.audacityteam.org/man/nyquist.html), LV2, Audio Units (macOS only) and VST2 effects. These plug-ins generally won't be shipped as part of Audacity, but they still may be a tremendous help for Audacity users.

## Testing / Finding bugs

You can download current development builds in the [Actions tab on Github](https://github.com/audacity/audacity/actions). These builds are tied to certain pull requests. As a rule of thumb: If it's not yet merged, report any broken things you find which might be a result of the pull request directly as a comment on the pull request. If the code has been merged, [make a new bug in the issue tracker](https://github.com/audacity/audacity/issues/new/choose).

When reporting bugs, try to find the most general form of it. For example, if you encounter a bug when amplifying a clip 2 hours into a project, try to see if it also happens when you're using a different effect (Normalize, for example), and if it also happens if the clip is near the beginning.

Bugs must be reproducible. If you can't find steps to reproduce a bug, try asking if you went wrong somewhere [in the Audacity forum](https://forum.audacityteam.org/) instead.

## Translating

See [our translators page](https://www.audacityteam.org/community/translators/) for more information on how to translate Audacity.

## Feedback & Feature requests

If you have anything you think Audacity can do better, you can voice it in [the relevant section in the forum](https://forum.audacityteam.org/c/feedback-and-discussion-forum/adding-features-to-audacity/22). If you have a very concrete idea something in the code which should be added or changed, you can also [make an enhancement request in the issue tracker](https://github.com/audacity/audacity/issues/new/choose).

## Supporting Users

The most active Audacity user community can be found [in the forum](https://forum.audacityteam.org/), so if you want to help users with their problems, this would be the first stop.

### Making video tutorials

Video tutorials are very helpful to users, as they tend to be easier to follow than written instructions. When you make video tutorials, make sure to mention which version of Audacity you're using, so that users know what the problem might be should they stumble upon your tutorial after a few years and find that their version of Audacity does very different things to yours.
