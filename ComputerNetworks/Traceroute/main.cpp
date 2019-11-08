#include <iostream>
#include <arpa/inet.h>
#include "errno.h"
#include <string.h>
#include <netinet/in.h>
#include "trace.h"


int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        std::cerr << "Wrong number of arguments!" << std::endl;
        return EXIT_FAILURE;
    }

    int sockfd = socket(AF_INET, SOCK_RAW, IPPROTO_ICMP);

    char* pArgument = argv[1];

    if (sockfd < 0)
    {
        std::cerr << strerror(errno) << std::endl;
        return 0;
    }

    sockaddr_in recipient;

    bzero( &recipient, sizeof( recipient ) );
	recipient.sin_family = AF_INET;

	if(!(inet_pton( AF_INET, pArgument, &recipient.sin_addr )))
    {
        std::cerr << "Ivalid network address." << std::endl;
        return EXIT_FAILURE;
    }

    Traceroute *t = new Traceroute(sockfd, pArgument, &recipient);
    t->process();
    delete t;
    
    return EXIT_SUCCESS;
}