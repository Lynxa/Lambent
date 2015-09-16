@echo off
echo Proxy-1 Window
echo ==============
.\utils\eclipse -b agents/proxy -e proxy(localhost,6000,localhost,7100)
pause