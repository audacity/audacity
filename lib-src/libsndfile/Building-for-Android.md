# Building for Android

Assuming the Android Ndk is installed at location `/path/to/toolchain`, building
libsndfile for Android (arm-linux-androideabi) should be as simple as:
```
./autogen.sh
export ANDROID_TOOLCHAIN_HOME=/path/to/android/toolchain
./Scripts/android-configure.sh
make
```
The `Scripts/android-configure.sh` contains four of variables; `ANDROID_NDK_VER`,
`ANDROID_GCC_VER`, `ANDROID_API_VER` and `ANDROID_TARGET` that can be overridden
by setting them before the script is run.

Since I (erikd), do almost zero Android development, I am happy accept patches
for this documentation and script to improve its utility for real Android
developers.
