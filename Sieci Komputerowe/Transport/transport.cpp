#include "transport.h"



Transporter::Transporter(int p, FILE *f, int s) : port(p), size(s), current(0), last(-1)
{
    file = f;
    count = ceil (size / DATAGRAM_SIZE);

    dataVec.reserve(WINDOW_SIZE);
    for (int i = 0; i < WINDOW_SIZE; ++i)
    {
        dataVec[i] = (u_int8_t *) malloc(sizeof(u_int8_t) * DATAGRAM_SIZE);
	}

    for (int i = 0; i < count; ++i)
    {
        dataInfo.push_back(std::make_pair(TIME_TO_WAIT, 0));
    }
}

Transporter::~Transporter()
{
    for (int i = 0; i < WINDOW_SIZE; ++i)
    {
        free(dataVec[i]);
	}
    close(sockfd);
}

bool Transporter::PrepareSocket()
{
	sockfd = socket(AF_INET, SOCK_DGRAM, 0);
	if (sockfd < 0) return false;

	bzero(&sockAddr, sizeof(sockAddr));
	sockAddr.sin_family = AF_INET;
	sockAddr.sin_port = htons(port);
	sockAddr.sin_addr.s_addr = htonl(INADDR_ANY);

	if (inet_pton(AF_INET, DESTINATION, &sockAddr.sin_addr) != 1) return false;

	return true;
}

bool Transporter::Process()
{
    if (!PrepareSocket()) return false;

    last = 0;
    bool guard = true;
    while(true) // TODO
    {
        if (!SendData()) return false;

        if (!GetData()) return false;
    }
}

bool Transporter::IsLast()
{
    return current + 1 == count;
}

bool Transporter::SendData()
{
    char get[25];
    int start = current * DATAGRAM_SIZE;
    if (IsLast())
    {
        //int bytesLeft = size - current * DATAGRAM_SIZE;
        sprintf(get, "GET %d %d\n", start, (size - current * DATAGRAM_SIZE));
    }
    else
    {
        sprintf(get, "GET %d %d\n", start, DATAGRAM_SIZE);
    }

    int getLenght = strlen(get);
    if (sendto(sockfd, get, getLenght, 0, (struct sockaddr*) &sockAddr, sizeof(sockAddr) == getLenght))
    {
        return true;
    }
    return false;

}

bool Transporter::Send()
{
    int a = last++;
    for (int i = a; i < a + WINDOW_SIZE && i < count; ++i)
    {
        current = i;
        if (!dataInfo[i].second)
        {
            if (dataInfo[i].first < 0)
            {
                for (int j = 0; j < 3; j++)
                {
                    if (!SendData()) return false;

                }
			    dataInfo[i].first= TIME_TO_WAIT;
            }
        }
    }
    return true;
}
void Transporter::UpdateTime(int del)
{
    for (int i = last; i < count; ++i)
    if (dataInfo[i].first > 0)
    {
        dataInfo[i].first -= del;
    }
}


bool Transporter::GetData(uint8_t *buff)
{
    int i, len;

    if (sscanf((char *) buff, "DATA %d %d\n", &i, &len) != 2) return false;

    i /= DATAGRAM_SIZE;

    if (!dataInfo[i].second)
    {
        dataInfo[i].second = 1;
        memcpy(dataVec[i % WINDOW_SIZE], strchr((char *) buff, '\n') + 1, len);
    }
}

bool Transporter::Get()
{
    fd_set descriptors;
	FD_ZERO (&descriptors);
	FD_SET (sockfd, &descriptors);
	struct timeval tv;
	timerclear(&tv);
	tv.tv_sec = TIME_TO_WAIT / 1000000;  
	tv.tv_usec = TIME_TO_WAIT % 1000000;

    struct timeval before;

    gettimeofday(&before, NULL);

    int sel = select(sockfd + 1, &descriptors, NULL, NULL, &tv);

    if (sel == -1) return false;

    gettimeofday(&tv, NULL);

    int delta = (tv.tv_sec - before.tv_sec) + (tv.tv_usec - before.tv_usec) * 1000000;

    UpdateTime(delta);

    if (sel == 0)
    {
        return true;
    }

    for (int i = 0; i < sel; ++i)
    {
        sockaddr_in sender;
        socklen_t length = sizeof(sender);
        uint8_t buffer[IP_MAXPACKET + 1];

        int dataSize = recvfrom(sockfd, buffer, IP_MAXPACKET, 0, (struct sockaddr*)&sender, &length);

        if (dataSize < 0) return false;

        char sendIP[20];

        inet_ntop(AF_INET, &(sender.sin_addr), sendIP, sizeof(sendIP));

        if (!strcmp(DESTINATION, sendIP) && (ntohs(sender.sin_port) == port))
        {
            GetData(buffer);
        }
    }

    
    return RefreshWindow();

}