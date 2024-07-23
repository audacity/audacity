## Using Visual Studio

To build Audacity 4 on Windows with Visual Studio rather than Qt Creator, from the root of the repository, run the following command:

```bash
cmake -S./au4 -B./build4
```

Open `./build4/audacity.sln`, build `INSTALL`.
(Couldn't find out how to fix this, but the install still misses jpeg8.dll and libpng16(d).dll. To get them you can build the audacity 3 project and find them in your output dir. Copy them to the install target dir `./build4/install/bin`.)
Now you should be able to debug the `audacity` target.
