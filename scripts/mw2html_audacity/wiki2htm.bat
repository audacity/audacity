python mw2html.py http://manual.audacityteam.org/man ..\..\help\temp -s
rmdir /S /Q ..\..\help\manual
mkdir ..\..\help\manual
xcopy ..\..\help\temp\manual.audacityteam.org ..\..\help\manual\ /E /C /Y /Q
rmdir /S /Q ..\..\help\temp

