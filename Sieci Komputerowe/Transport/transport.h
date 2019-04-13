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

const int MIN_PORT = 0;
const int MAX_PORT = 65535 + 1;

const int DATAGRAM_SIZE = 1000;
const int WINDOW_SIZE = 2500;

const char* DESTINATION = "156.17.4.30";
const int TIME_TO_WAIT = 500000;


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

    bool Get();
    bool GetData(uint8_t *buff);
    void UpdateTime(int del);

    bool IsLast();

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