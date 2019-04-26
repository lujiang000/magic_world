@echo off
set DIR=D:\magic_world\
set type=local
set COOKIE=cookie_8xwpjmJ6FOBCZrKVhiXLsNe1sPPt0ASJebtatWd52Wcw2PSwVJe070esNpLvN41E
set inet_dist_listen_min=7000
set inet_dist_listen_max=7100
set inp=%1
if "%inp%" == "" goto fun_wait_input

goto fun_run

:fun_wait_input
    set inp=
    echo.
    echo ===================================
    echo make:编译服务端源码
    echo start:启动游戏
    echo debug:启动游戏 可以查看启动时候出错
    echo proto:编译proto
    echo sync:同步proto给前端
    echo ===================================
    set /p inp=请输入指令:
    goto fun_run

:fun_run
    if [%inp%]==[make] goto fun_make
    if [%inp%]==[start] goto fun_start
    if [%inp%]==[debug] goto fun_debug
    if [%inp%]==[proto] goto fun_proto
    if [%inp%]==[sync] goto fun_sync
    goto where_to_go

:where_to_go
	rem 区分是否带有命令行参数
	if [%1]==[] goto fun_wait_input
	goto end

:fun_make
    cd %DIR%
    erl -make ：
    copy /y main.app  .\ebin\main.app
    copy /y emysql.app  .\ebin\emysql.app
    echo 编译完成
    goto where_to_go

:fun_start
    cd %DIR%
    start werl -pa ebin  +P 1024000  -setcookie %COOKIE%  -hidden -kernel inet_dist_listen_min %inet_dist_listen_min% inet_dist_listen_max %inet_dist_listen_max%  -name magic_world@192.168.0.79  -s main start -extra local
    goto where_to_go 

:fun_debug
	cd %DIR%
	erl -pa ebin  +P 1024000  -setcookie %COOKIE% -hidden -kernel inet_dist_listen_min %inet_dist_listen_min% inet_dist_listen_max %inet_dist_listen_max%  -name guess@127.0.0.1 -s main start -extra local
	pause
	goto where_to_go

:fun_proto
	cd %DIR%
	cd .\protobuffs\ebin
	erl -eval "protobuffs_compile:scan_file(\"../all.proto\"),erlang:halt()"
	move all_pb.beam ..\Output
	move all_pb.hrl ..\Output
	copy /y ..\Output\all_pb.beam ..\..\ebin\all_pb.beam
	copy /y ..\Output\all_pb.hrl ..\..\include\all_pb.hrl
	goto where_to_go

:fun_sync
	cd %DIR%
	Set SVN=D:\svn\bin
	copy .\include\error_msg.hrl .\..\..\..\program\magic_world\main\error_msg
	copy .\protobuffs\proto\*.proto .\..\..\..\program\magic_world\main\proto
	cd .\..\..\..\program\guess\main 
	"%SVN%\TortoiseProc.exe"/command:update /path:"D:\program\magic_world\main\proto\*"/closeoned:0
	"%SVN%\TortoiseProc.exe"/command:commit /path:"D:\program\magic_world\main\*"/closeoned:0
	goto where_to_go
:end

