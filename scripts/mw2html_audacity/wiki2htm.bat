python mw2html.py http://alphamanual.audacityteam.org/man ..\..\help\temp -s
rmdir /S /Q ..\..\help\manual
mkdir ..\..\help\manual
xcopy ..\..\help\temp\alphamanual.audacityteam.org ..\..\help\manual\ /E /C /Y /Q
rmdir /S /Q ..\..\help\temp

