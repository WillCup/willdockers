#!/bin/bash
## 检查当前进程中是否还有metamap的gunicorn进程活着
function check_gunicorn() {
    lines=`ps -ef | grep metamap_settings | grep -c  gunicorn`
    if [[ $lines > 1 ]]; then
        echo "${lines} gunicorns still running..."
        ps -ef |grep metamap_settings
        return ${lines}
    else
        echo "All gunicorns has been killed"
        return 0
    fi
}

#################################
###  2. 停止所有gunicorn进程   ####
#################################
pid=`ps -ef | grep gunicorn | grep metamap_settings | awk '{if($3 == '1') print $2}'`
if [[ $pid > 0 ]]; then
    echo "Got gunicorn master pid : ${pid}"
    kill $pid
    check_gunicorn
    status=$?
    sleep 10s
    until [ $status -eq 0 ]
    do
        check_gunicorn
        status=$?
        sleep 5s
    done
else
    echo "Cannot find master pid for gunicorn"
fi


######################################
###  3. 使用virtualenv启动gunicorn ####
######################################

gunicorn metamap_django.wsgi:application -c metamap_settings.py
echo "##########   log  ############"
sleep 3s
tail -20 /tmp/metamap_gunicorn_error.log
