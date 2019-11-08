#include <iostream>
#include <netinet/ip_icmp.h>
#include <stdint.h>
#include <stdio.h>
#include <arpa/inet.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <assert.h>
#include "transfer.h"

Transfer::Transfer(sockaddr_in* rec, int s, char* addr) : recipient(rec), sockfd(s), sender_address(addr)
{
}

Transfer::~Transfer()
{

}

u_int16_t Transfer::compute_icmp_checksum (const void *buff, int length)
{
	u_int32_t sum;
	const u_int16_t* ptr = (u_int16_t*)(buff);
	assert (length % 2 == 0);
	for (sum = 0; length > 0; length -= 2)
		sum += *ptr++;
	sum = (sum >> 16) + (sum & 0xffff);
	return static_cast<u_int16_t>(~(sum + (sum >> 16)));
}

PacketStatus Transfer::sendPackets(int ttl, int pid, int sockfd)
{
    for(int i = 0; i < 3; ++i)
    {
	 	struct icmp packet;
		packet.icmp_type = ICMP_ECHO;
		packet.icmp_id = pid;
		packet.icmp_seq = ttl;
		packet.icmp_code = 0;
		packet.icmp_cksum = 0;
		packet.icmp_cksum = compute_icmp_checksum ((u_int16_t*)&packet, sizeof(packet));
        setsockopt(sockfd, IPPROTO_IP, IP_TTL, &ttl, sizeof(int));

        ssize_t bytes_sent = sendto(sockfd, &packet, sizeof(packet), 0, (struct sockaddr*) recipient, sizeof(*recipient));
		if(bytes_sent < 1)
        {
            std::cerr << strerror(errno) << std::endl;
            return ERROR;
        }
    }

    return IN_PROCESS;
}


PacketStatus Transfer::processPacket(int& counter, std::chrono::high_resolution_clock::time_point& time, std::chrono::duration<int64_t, std::milli>& duration_time,
	int pid, int ttl, std::vector<std::string>& ip_addresses_vec)
{

	struct sockaddr_in 	sender;	
	socklen_t 		    sender_len = sizeof(sender);
	u_int8_t 			buffer[IP_MAXPACKET];

	ssize_t packet_len = recvfrom (sockfd, buffer, IP_MAXPACKET, MSG_DONTWAIT, (sockaddr*)&sender, &sender_len);
	if (packet_len < 0 && errno != EWOULDBLOCK) return ERROR;

	auto receive_time = std::chrono::high_resolution_clock::now();
    char ip_str[20]; // sender IP
    inet_ntop (AF_INET, &(sender.sin_addr), ip_str, sizeof(ip_str));

	ip *ipp = (struct ip *)buffer;
	icmp *icmp = (struct icmp *)((uint8_t *)ipp + (*ipp).ip_hl * 4);
	counter++;

	switch(icmp->icmp_type)
	{
		case ICMP_ECHOREPLY:
			if(ttl == icmp->icmp_seq && pid == icmp->icmp_id)
			{
				addToVector(ip_addresses_vec, ip_str);
				duration_time += std::chrono::duration_cast<std::chrono::milliseconds>(receive_time - time);

			}
			break;
		
		case ICMP_TIME_EXCEEDED:

		
			struct ip *ipp1 = (struct ip *)((uint8_t *)icmp + 8);
			struct icmp *icmpp1 = (struct icmp *)((uint8_t *)ipp1 + (*ipp1).ip_hl * 4);
			if(ttl == icmpp1->icmp_seq && pid == icmpp1->icmp_id)
			{
				addToVector(ip_addresses_vec, ip_str);
				duration_time += std::chrono::duration_cast<std::chrono::milliseconds>(receive_time - time);

			}
			break;

	}

	
	if (static_cast<std::string>(ip_str) == sender_address)
	{
		return DESTINATION_REACHED;
	}

	return IN_PROCESS;
}

void Transfer::addToVector(std::vector<std::string>& ip_addresses_vec, char* ip)
{
	for (auto address : ip_addresses_vec)
	{
		if(address == static_cast<std::string>(ip)) return;
	}

	ip_addresses_vec.push_back(ip);
}