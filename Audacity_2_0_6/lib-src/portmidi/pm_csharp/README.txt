This code was offered by Aaron Oxford as is. The pm_managed directory contains the code. If you develop a more complete C# wrapper for PortMidi, please consider contributing your code to the project. -RBD

---- from Aaron Oxford ----

I've attached the managed C++ project which I've inserted into my 2005 version of PortMIDI's VS solution.  I wouldn't think the functions I've implemented would have changed so it all should still work with the latest version of PM. Obviously you won't want to permanently embed this since it means the whole solution can only be built under VS2005, but it's easy for a VS2005 user to insert the project after the solution is converted or even just build it separately.

Making the managed wrapper turned out to be dead easy in the end (it was more of a battle finding the correct build settings & SDK's and learning to configure VS than anything else). Anyone wanting to use something I've not implemented yet simply needs to add more stubs like this

                int Pm_Initialize()
                {
                        ::Pm_Initialize();
                        return 0;
                }

to the code. To call from C# it's just a matter of

                ManagedPortMIDI mpm = new ManagedPortMIDI();
                int err = mpm.Pm_Initialize();

Anyway as the little code example above indicates, the support really is basic and more likely than not to break at the first hint of something unexpected. As I said, I'd be happy to contribute but I don't think there's much to contribute yet. :-)
