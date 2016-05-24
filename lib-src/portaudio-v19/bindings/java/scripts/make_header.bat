REM Generate the JNI header file from the Java code for JPortAudio
REM by Phil Burk

javah -classpath ../jportaudio/bin -d ../c/src com.portaudio.PortAudio com.portaudio.BlockingStream
