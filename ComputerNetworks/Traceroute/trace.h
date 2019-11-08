#include <iostream>
#include <arpa/inet.h>
#include <chrono>
#include "transfer.h"



class Traceroute
{
    public:
    Traceroute(int sock, char* address, sockaddr_in* receip);
    ~Traceroute();
    bool process();

    private:
    int trace();

    Transfer* transfer;
    sockaddr_in* receipent;
    int sockfd;
    char* address_ip;
    PacketStatus status;
    void show(int counter, std::chrono::duration<int64_t, std::milli>& duration_time, std::vector<std::string>& ip_addresses_vec, int ttl);
};