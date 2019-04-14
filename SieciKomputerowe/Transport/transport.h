#ifndef _TRANSPORT_H_
#define _TRANSPORT_H_


#include <iostream>
#include <string.h>
#include <vector>
#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <cmath>
#include <arpa/inet.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/select.h>

class Transporter
{
    public:
    Transporter(int p, FILE *f, int s);
    ~Transporter();
    bool Process();

    private:
    bool PrepareSocket();
    bool SendData();
    bool Send();

    bool GetData();
    bool Get(uint8_t *buff);
    void UpdateTime(int del);

    bool IsLast();
    bool RefreshWindow();

    private:
    sockaddr_in sockAddr;
    FILE *file;
    std::vector<std::pair<int, uint8_t>> dataInfo;
    int port;
    int size;
    int sockfd;
    int current;

    int last;
    int count;
    std::vector<uint8_t*>dataVec;

};


#endif // _TRANSPORT_H_