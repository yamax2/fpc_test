FROM ubuntu:latest

RUN apt update
RUN DEBIAN_FRONTEND="noninteractive" apt -y install tzdata
RUN apt install -y mc fpc lazarus mingw-w64 mingw-w64-tools libsqlite3-dev
RUN echo -e "# MS Windows .rc resource compiler\n#IFDEF CPUAMD64\n-FCx86_64-w64-mingw32-windres\n#ENDIF\n#IFDEF cpui386\n-FCi386-w64-mingw32-windres\n#ENDIF"

#CMD lazbuild dpl.lpr
CMD bash
