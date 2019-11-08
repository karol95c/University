#include <iostream>
#include <netinet/ip_icmp.h>
#include <vector>
#include <chrono>

enum PacketStatus
{
    ERROR = 1 << 0,
    IN_PROCESS = 1 << 1,
    DESTINATION_REACHED = 1 << 2,
    PREPARED = 1 << 3
};

class Transfer
{
    public:
    Transfer(sockaddr_in* rec, int s, char* addr);
    ~Transfer();
    PacketStatus sendPackets(int ttl, int pid, int sockfd);
    void getPacket();
    PacketStatus processPacket(int& counter, std::chrono::high_resolution_clock::time_point& time, std::chrono::duration<int64_t, std::milli>& duration_time,
        int pid, int ttl, std::vector<std::string>& ip_addresses_vec);
    
    
    private:
    void prepareIcmpHeader(icmphdr& header);
    void addToVector(std::vector<std::string>& ip_addresses_vec, char* ip);
    u_int16_t compute_icmp_checksum (const void *buff, int length);
    sockaddr_in* recipient;
    int sockfd;
    char* sender_address;
};