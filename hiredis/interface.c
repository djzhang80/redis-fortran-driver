#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "hiredis.h"
#include <winsock.h>


redisContext *c;
redisReply *reply;

void releasereply()
{
freeReplyObject(reply);
}

void disconnect()
{
	redisFree(c);

}
void connect_to_redis()
{
	redisReply *reply;
	const char *hostname = "127.0.0.1";
	int port = 6379;

	struct timeval timeout = { 1, 5000000 }; // 1.5 seconds


	if (!w32initWinSock()) {
		printf("Winsock init error %d", WSAGetLastError());
		exit(1);
	};

    c = redisConnectWithTimeout(hostname, port, timeout);



	if (c == NULL || c->err) {
		if (c) {
			printf("Connection error: %s\n", c->errstr);
			redisFree(c);
		}
		else {
			printf("Connection error: can't allocate redis context\n");
		}
		exit(1);
	}

}



char* retrieve(char * msg)
{
	char *rs, *rt;
	//printf("%s", msg);

	reply = redisCommand(c,msg);
	rt = reply->str;
	return rt;
}



