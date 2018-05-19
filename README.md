# Dashcam Player Light

# TODO:
* check db version on start
* win version
* macos version
* options editing
* form attrs and options saving
* temp in a user's home dir
* localization
* tests

## resource compiler

install packages

```bash
sudo apt install mingw-w64 mingw-w64-tools
```

and add to /etc/fpc.cfg

```
# MS Windows .rc resource compiler
#IFDEF CPUAMD64
-FCx86_64-w64-mingw32-windres
#ENDIF
#IFDEF cpui386
-FCi386-w64-mingw32-windres
#ENDIF
```