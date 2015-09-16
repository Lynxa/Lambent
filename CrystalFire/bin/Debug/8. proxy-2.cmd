@echo off
echo Proxy-2 Window
echo ==============
.\utils\eclipse -b agents/proxy -e proxy(localhost,6000,localhost,7200)
pause