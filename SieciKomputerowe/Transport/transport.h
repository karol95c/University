#ifndef _TRANSPORT_H_
#define _TRANSPORT_H_

#include <chrono>
#include <cmath>
#include <iostream>
#include <vector>
#include <string.h>
#include <unistd.h>

#include <arpa/inet.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/types.h>

class Transporter
{
    public:
    explicit Transporter(int p, FILE *f, int s);
    ~Transporter();
    void Process();

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
    std::vector<uint8_t*>dataVec;
    std::vector<std::pair<int, uint8_t>> dataInfo;
    sockaddr_in sockAddr;
    FILE *file;
    int port;
    int size;
    int sockfd;
    int current;
    int last;
    int count;
};

#endif // _TRANSPORT_H_