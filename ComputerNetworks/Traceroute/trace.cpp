#include "trace.h"
#include <sys/types.h>
#include <unistd.h>
#include <vector>

Traceroute::Traceroute(int sock, char* address, sockaddr_in* receip)
{
	sockfd = sock;
	address_ip = address;
	receipent = receip;
	transfer = new Transfer(receipent, sockfd, address_ip);
	status = PREPARED;
}


Traceroute::~Traceroute()
{
	delete transfer;
}	

bool Traceroute::process()
{
	if(status == PREPARED)
	{
		status = IN_PROCESS;
		if (trace() != ERROR)
		{
			return true;
		}

	}
	return false;
}





void Traceroute::show(int counter, std::chrono::duration<int64_t, std::milli>& duration_time, std::vector<std::string>& ip_addresses_vec, int ttl)
{
	std::string time;
	std::cout.width(3);
	std::cout << std::right << ttl << ". ";
	if (status == DESTINATION_REACHED)
	{
		counter = 3;
	}

	switch(counter)
	{
		case 0 : 
			std::cout <<"*\n";
			return;
			break;
		case 1 :
			time = " ???";
			break;
		case 2 :
			time = " ???";
			break;
		case 3 :
			duration_time /= ip_addresses_vec.size();
			time = std::to_string(duration_time.count());
			time += "ms";
			break;
	}

	for(auto ip : ip_addresses_vec)
	{
		std::cout.width(17);
		std::cout << std::left << ip;
	}

	std::cout << time << std::endl;
}

int Traceroute::trace()
{

	int i = 1;
	int pid = getpid();

	while(i < 31 && status != DESTINATION_REACHED)
	{
		auto ip_addresses_vec = std::vector<std::string>();
		ip_addresses_vec.reserve(3);
		
		int counter = 0;

		std::chrono::duration<int64_t, std::milli> duration_time = std::chrono::duration<int64_t, std::milli>();
		
		fd_set descriptors;
		FD_ZERO (&descriptors);
		FD_SET (sockfd, &descriptors);
		struct timeval tv;
		tv.tv_sec = 1;
		tv.tv_usec = 0;

		auto start_time = std::chrono::high_resolution_clock::now();
		status = transfer->sendPackets(i, pid, sockfd);
		if (status == ERROR) return EXIT_FAILURE;


		while (counter < 3)
		{
			int ready = select(sockfd + 1, &descriptors, NULL, NULL, &tv);
			if(ready < 0) 			return EXIT_FAILURE;
			else if(ready == 0) 	break;
			else
			{
				status = transfer->processPacket(counter, start_time, duration_time, pid, i, ip_addresses_vec);
			}
		}

		show(counter, duration_time, ip_addresses_vec, i);
		i++;
	}
	return 0;
}
