e:
cd E:\p10
//redis-cli.exe 
redis-cli.exe script load "local lv=redis.call('get',KEYS[1]..KEYS[2]) if lv==false then return redis.call('get',KEYS[1]) else return lv end"
redis-cli.exe evalsha 50255469c8cc34ae8f8a4b57da5a49952b6c11d2 2 000030001.chm:00016 :00001
pause