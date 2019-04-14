#include "transport.h"


const int MIN_PORT = 0;
const int MAX_PORT = 65535 + 1;

const int DATAGRAM_SIZE = 1000;
const int WINDOW_SIZE = 2500;

const char* DESTINATION = "156.17.4.30";
const int TIME_TO_WAIT = 500000;

Transporter::Transporter(int p, FILE *f, int s) : port(p), size(s), current(0), last(-1)
{
    file = f;
    count = ceil (static_cast<double>(size) / static_cast<double>(DATAGRAM_SIZE));

    dataVec.reserve(WINDOW_SIZE);
    for (int i = 0; i < WINDOW_SIZE; i++)
    {
        dataVec[i] = new uint8_t[DATAGRAM_SIZE];
        //dataVec[i] = (u_int8_t *) malloc(sizeof(u_int8_t) * DATAGRAM_SIZE);
	}

    for (int i = 0; i < count; i++)
    {
        dataInfo.push_back(std::make_pair(TIME_TO_WAIT, 0));
    }
}

Transporter::~Transporter()
{
    for (int i = 0; i < WINDOW_SIZE; i++)
    {
        delete[] dataVec[i];
        //free(dataVec[i]);
	}
    close(sockfd);
}

bool Transporter::PrepareSocket()
{
    std::cout << "PrepareSocket()"<< std::endl;
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
    std::cout << "Process()"<< std::endl;
    if (!PrepareSocket()) return false;

    std::cout << "last: " << last << std::endl;
    std::cout << "count: " << count << std::endl;

    while(last != count - 1)
    {
        std::cout << "loop"<< std::endl;
        if (!SendData()) return false;

        if (!GetData()) return false;
    }
    return true;
}

bool Transporter::IsLast()
{
    std::cout << "IsLast()"<< std::endl;
    return current + 1 == count;
}

bool Transporter::Send()
{
    std::cout << "Send()"<< std::endl;
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
    if (sendto(sockfd, get, getLenght, 0, (struct sockaddr*) &sockAddr, sizeof(sockAddr)) == getLenght)
    {
        return true;
    }
    return false;

}

bool Transporter::SendData()
{
    std::cout << "SendData()"<< std::endl;
    int a = last >= 0 ? last + 1 : 0;
    for (int i = a; i < a + WINDOW_SIZE && i < count; i++)
    {
        current = i;
        if (!dataInfo[i].second)
        {
            if (dataInfo[i].first <= 0)
            {
                for (int j = 0; j < 3; j++)
                {
                    if (!Send()) return false;

                }
			    dataInfo[i].first= TIME_TO_WAIT;
            }
        }
    }
    return true;
}
void Transporter::UpdateTime(int del)
{
    std::cout << "UpdateTime()"<< std::endl;
    std::cout << "last: "<< last << std::endl;
    if (last < 0) last = 0;
    for (int i = last; i < count; i++)
    if (dataInfo[i].first > 0)
    {
        dataInfo[i].first -= del;
    }
}


bool Transporter::RefreshWindow()
{
    std::cout << "RefreshWindow()"<< std::endl;
    for (int i = last + 1; i < count; i++)
    {
        if(!dataInfo[i].second) break;

        ++last;
        int l;

        current = i;
        if (IsLast())
        {
            l = size - i * DATAGRAM_SIZE;
        }
        else
        {
            l = DATAGRAM_SIZE;
        }

        if (fwrite(dataVec[i % WINDOW_SIZE], sizeof(uint8_t), l, file) < 1) return false;

    }

    return true;
}

bool Transporter::Get(uint8_t *buff)
{
    std::cout << "Get()"<< std::endl;
    int i, len;

    if (sscanf((char *) buff, "DATA %d %d\n", &i, &len) != 2) return false;

    i /= DATAGRAM_SIZE;

    if (!dataInfo[i].second)
    {
        dataInfo[i].second = 1;
        memcpy(dataVec[i % WINDOW_SIZE], strchr((char *) buff, '\n') + 1, len);
    }
    return true;
}

bool Transporter::GetData()
{
    std::cout << "GetData()"<< std::endl;
    fd_set descriptors;

	struct timeval tv;
	timerclear(&tv);
	//tv.tv_sec = TIME_TO_WAIT / 1000000;
    tv.tv_sec = 1;
	tv.tv_usec = TIME_TO_WAIT % 1000000;
    FD_ZERO (&descriptors);
	FD_SET (sockfd, &descriptors);
    struct timeval before;

    gettimeofday(&before, NULL);

    int sel = select(sockfd + 1, &descriptors, NULL, NULL, &tv);

    if (sel == -1) return false;

    gettimeofday(&tv, NULL);

    int delta = 1000000 * (tv.tv_sec - before.tv_sec) + (tv.tv_usec - before.tv_usec);

    UpdateTime(delta);
    std::cout << "Break" << std::endl;
    if (sel == 0)
    {
        std::cout << "Break" << std::endl;
        return true;
    }
    std::cout << "Break" << std::endl;
    for (int i = 0; i < sel; i++)
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
            Get(buffer);
        }
    }

    
    return RefreshWindow();

}