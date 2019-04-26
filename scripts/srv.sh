#!/bin/bash
ROOT=`cd $(dirname $0);pwd`
NETIP=127.0.0.1
nodename=magic_world

inet_dist_listen_min=7000
inet_dist_listen_max=7010

mysqlname=magic_world
mysqlpass=123456
mysqluser=root

COOKIE=cookie_8xwpjmJ6FOBCZrKVhiXLsNe1sPPt0ASJebtatWd52Wcw2PSwVJe070esNpLvN41E

fun_ssh() {
        cd $ROOT/..
        scp -r ebin root@120.78.222.244:/mnt/magic_world/
        echo 上传成功
}

fun_delete() {
        cd $ROOT/..
        rm -rf dets screenlog.0
        mkdir dets
        mysql -uroot -p -e"use game;truncate role;truncate role_chat"
}

fun_create(){
    cd $ROOT/../protobuffs/ebin
    erl -eval 'protobuffs_compile:scan_file("../all.proto"), erlang:halt()'
    echo proto编译完成
    mv all_pb.beam ../Output/
    mv all_pb.hrl ../Output/
    cp -f ../Output/all_pb.beam ../../ebin/all_pb.beam
    cp -f ../Output/all_pb.hrl ../../include/all_pb.hrl
    echo proto复制完成
}

fun_make(){
    cd $ROOT/..
    erl -make
    echo 编译完成
    cp -f ./main.app ebin/main.app
    cp -f ./emysql.app ebin/emysql.app
    echo 复制完成
}

fun_sync_proto() {
    cd $ROOT/..
    cp include/error_msg.hrl ../../program/magic_world/main/error_msg/
    cp protobuffs/proto/*.proto ../../program/magic_world/main/proto/
    cd ../../program/magic_world/main && svn up && svn ci -m ""
}
fun_data() {
    cd $ROOT/..
    python  ./scripts/data.py
    echo 执行完成
}

fun_start(){
    cd $ROOT/..
    cp -f main.app ebin/main.app
    cp -f ./emysql.app ebin/emysql.app
    type=local
    if is_screen_exists $nodename; then
        ERR "节点\e[92m$nodename\e[0;0m已经启动"
        exit 1
    fi
    start_file=./start.sh
	CMD="erl -pa ebin +P 1024000 +K true -setcookie $COOKIE -hidden -kernel inet_dist_listen_min $inet_dist_listen_min inet_dist_listen_max $inet_dist_listen_max -name $nodename@$NETIP -s main start -extra $type"
    cat > ${start_file} <<EOF
#!/bin/bash
ulimit -SHn 102400
${CMD}
EOF
    chmod +x ${start_file}
    screen -dmSL $nodename -s ${start_file}
    screen -r $nodename
}

fun_mysql(){
    cd $ROOT/..
    name=root
    pass=123456
    mysql -u$mysqluser -p$mysqlpass $mysqlname<sql/sql.sql
    ERR "执行完成"
    exit
}

fun_help(){
    echo "-make 编译"
    echo "-start 启动"
    echo "-mysql 执行mysql脚本"
    echo "-sync name 执行同步beam文件脚本"
    echo "-rpc name mod fun arg执行远程服务器调用脚本"
}

fun_host(){
    var=$(cat 'rsync.conf')
    check_ip $1
    ip=`get_info $1 2`
    port=`get_info $1 3`
    echo $ip
    echo $port
}

fun_rpc(){
     echo $#
     if [ $# -ge 4 ];then
     	name=$2
     	mod=$3
     	fun=$4
     	if [ "$5" = "" ];then
          	arg=[]
     	else
         	arg=[$5]
     	fi
     	check_ip $2
     	remot_node_name=`get_info $2 4`
     	erl -hidden -noshell -name rpc_remot -setcookie $COOKIE -eval "io:setopts([{encoding,unicode}]),io:format(\"~w~n\", [rpc:call('$remot_node_name', $mod, $fun, $arg)])." -s c q
     else
       echo "参数不足 节点名，模块名，方法名，参数（没有可以不填）"
     fi



}

fun_sync(){
    if [ "$2" == "all" ];then
    str=`cat rsync.conf | grep -v "^%" | grep -v "^$" `
    read -p "$str
是否要同步代码到以上服务器，请输入yes/no:" name
    if [ "$name" == "yes" ];then
    cat rsync.conf | grep -v "^%" | grep -v "^$" | while read line
    do
    ip=`echo $line | awk '{split($0,a,"|");print a[2]}'`
    remotdir=`echo $line | awk '{split($0,a,"|");print a[3]}'`
    if [ "$3" == "" ];then
    rsync -avzp ../ebin/*.beam $ip::$remotdir/ebin --password-file=/etc/rsyncd.pass
    rsync -avzp ../sql/sql.sql $ip::$remotdir/sql --password-file=/etc/rsyncd.pass
    rsync -avzp rsync.conf $ip::$remotdir/scripts/ --password-file=/etc/rsyncd.pass
    else
    rsync -avzp ../ebin/$3.beam $ip::$remotdir/ebin --password-file=/etc/rsyncd.pass
    fi
    done
    else
    echo 同步取消
    fi
    else
    ip=`get_info $2 2`
    check_ip $2
    ip=`get_info $2 2`
    remotdir=`get_info $2 3`
    if [ "$3" == "" ];then
    rsync -avzp ../ebin/*.beam $ip::$remotdir/ebin --password-file=/etc/rsyncd.pass
    rsync -avzp ../sql/sql.sql $ip::$remotdir/sql --password-file=/etc/rsyncd.pass
    rsync -avzp rsync.conf $ip::$remotdir/scripts/ --password-file=/etc/rsyncd.pass
    else
    for i in $@;
	do
	echo "hahahaha:"$i"wwwwwwww"
	if [ "$i" == "" ];then
	echo 同步完成
        else
    rsync -avzp ../ebin/$i.beam $ip::$remotdir/ebin --password-file=/etc/rsyncd.pass
    fi
    done
    fi
    fi


}


check_ip(){
   if [ "$1" = "" ];then
      echo ----请输入服务器代号请查看rsync.conf----------------
      cat rsync.conf
      echo ----请输入服务器代号请查看rsync.conf---------------
      exit 0
   else
   flag=`get_info $1 2`
   echo flag----$flag
   if [ "$flag" = "" ];then
      echo ----不存在的服务器代号请查看rsync.conf----------------
      cat rsync.conf
      echo ----不存在的服务器代号请查看rsync.conf---------------
      exit 0
  fi
  fi

}
#
#fun_ssh(){
#    if [ "$2" == "all" ];then
#    echo `pwd`
#    str=`cat rsync.conf | grep -v "^%" | grep -v "^$" `
#    read -p "$str
#是否要同步代码到以上服务器，请输入yes/no:" name
#    if [ "$name" == "yes" ];then
#    cat rsync.conf | grep -v "^%" | grep -v "^$" | while read line
#    do
#    ip=`echo $line | awk '{split($0,a,"|");print a[2]}'`
#    name=`echo $line | awk '{split($0,a,"|");print a[1]}'`
#    remotdir=`echo $line | awk '{split($0,a,"|");print a[3]}'`
#    ssh -ttn $ip  "/$remotdir/game/scripts/srv.sh -rpc $name $3 $4 $5"
#    echo $ip 服务器执行脚本 srv.sh -rpc $name $3  $4 $5
#    done
#    else
#    echo 同步取消
#    fi
#    else
#    ip=`get_info $2 2`
#    check_ip $2
#    remotdir=`get_info $2 3`
#    ssh -tt $ip  /$remotdir/game/scripts/srv.sh -rpc $2 $3 $4 $5
#    echo $ip 服务器同步完成
#    fi
#
#}

fun_rpc_start(){
    if [ "$2" == "all" ];then
    echo `pwd`
    str=`cat rsync.conf | grep -v "^%" | grep -v "^$" `
    read -p "$str
是否一键启动服务器，请输入yes/no:" name
    if [ "$name" == "yes" ];then
    cat rsync.conf | grep -v "^%" | grep -v "^$" | while read line
    do
    ip=`echo $line | awk '{split($0,a,"|");print a[2]}'`
    remotdir=`echo $line | awk '{split($0,a,"|");print a[3]}'`
    ssh -ttn $ip  "/$remotdir/game/scripts/srv.sh -start "
    echo $ip 服务器执行脚本 srv.sh -start
    done
    else
    echo 同步取消
    fi
    else
    ip=`get_info $2 2`
    check_ip $2
    remotdir=`get_info $2 3`
    ssh -tt $ip  /$remotdir/game/scripts/srv.sh -start
    echo $ip 服务器同步完成
    fi
}


fun_bs_ssh(){
    if [ "$2" == "all" ];then
    echo `pwd`
    str=`cat rsync.conf | grep -v "^%" | grep -v "^$" `
    read -p "$str
是否要同步代码到以上服务器，请输入yes/no:" name
    if [ "$name" == "yes" ];then
    cat rsync.conf | grep -v "^%" | grep -v "^$" | while read line
    do
    ip=`echo $line | awk '{split($0,a,"|");print a[2]}'`
    name=`echo $line | awk '{split($0,a,"|");print a[1]}'`
    remotdir=`echo $line | awk '{split($0,a,"|");print a[3]}'`
    ssh -ttn $ip  "/$remotdir/bs/scripts/stop.sh"
    sleep 2s
    ssh -ttn $ip  "/$remotdir/bs/scripts/game.sh"
    echo $ip 服务器执行脚本 srv.sh -rpc $name $3  $4 $5
    done
    else
    echo 同步取消
    fi
    else
    ip=`get_info $2 2`
    check_ip $2
    remotdir=`get_info $2 3`
    ssh -ttn $ip  "/$remotdir/bs/scripts/stop.sh"
    sleep 2s
    ssh -ttn $ip  "/$remotdir/bs/scripts/game.sh"
    echo $ip 服务器同步完成 "/$remotdir/bs/scripts/game.sh"
    fi

}
get_info(){
    tem=$(grep -i $1 'rsync.conf')
##    tem=`sed '/^'$1'=/!d;s/.*=//' test.txt`
    echo | awk '{split("'$tem'", array, "|");print array['$2']}'


}

## 注意此函数不是绝对可靠，有可能会找出名称相似的screen session
is_screen_exists(){
    [[ $(screen -ls | grep "$nodename" | wc -l ) -gt 0 ]] ; return $?
}

# 输出一条普通信息
INFO(){
    echo -e "\e[92m=>\e[0;0m ${1}"
}

# 输出一条错误信息
ERR(){
    >&2 echo -e "\e[91m>>\e[0;0m ${1}"
}

case $1 in
    '-create') fun_create;;
    '-start') fun_start $2;;
    '-make') fun_make;;
    '-mysql') fun_mysql;;
    '-host') fun_host $2;;
    '-sync') fun_sync $@;;
    '-rpc') fun_rpc $@;;
    '-ssh') fun_ssh;;
    '-delete') fun_delete;;
    '-bs_ssh') fun_bs_ssh $@;;
    '-rpc_start') fun_rpc_start $@;;
    '-sync_proto') fun_sync_proto;;
    '-data') fun_data;;
    *) fun_help;;
esac

