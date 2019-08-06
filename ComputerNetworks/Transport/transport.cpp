#include "transport.h"

const int DATA_SIZE = 1000;
const int WINDOW_SIZE = 1000;
const int TIMEXP = 500000;

Transporter::Transporter(int p, FILE *f, int s, char* d) : port(p), size(s), current(-1), last(-1), destination(d)
{
    file = f;

    count = ceil (static_cast<double>(size) / static_cast<double>(DATA_SIZE));

    for (int i = 0; i < WINDOW_SIZE; i++)
    {
        dataVec.push_back(new uint8_t[DATA_SIZE]);
	}

    for (int i = 0; i < count; i++)
    {
        dataInfo.push_back(std::move(std::make_pair(TIMEXP, 0)));
    }
}

Transporter::~Transporter()
{
    for (int i = 0; i < WINDOW_SIZE; i++)
    {
        delete[] dataVec[i];
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

	if (inet_pton(AF_INET, destination, &sockAddr.sin_addr) != 1)
    {
        std::cerr << "Ivalid network address!" << std::endl;
        return false;
    }

	return true;
}

void Transporter::Process()
{
    if (!PrepareSocket())   return;

    while(last != count - 1)
    {
        if (!SendData())    return;

        if (!GetData())     return;
    }
}

bool Transporter::IsLast()
{
    return (current + 1) == count;
}

bool Transporter::Send()
{
    int bytesToSend = 0;
    int start = current * DATA_SIZE;

    if (IsLast())
    {
        bytesToSend = size - current * DATA_SIZE;
    }
    else
    {
        bytesToSend = DATA_SIZE;
    }

    std::string message = "GET " + std::to_string(start) + ' ' + std::to_string(bytesToSend) + '\n';
    int messageLength = message.length();

    if (sendto(sockfd, message.c_str(), messageLength, 0, (struct sockaddr*) &sockAddr, sizeof(sockAddr)) == messageLength)
    {
        return true;
    }

    return false;
}

bool Transporter::SendData()
{
    int j;
    if (last == 0) j = 0;
    else j = last + 1;

    for (int i = j; i < j + WINDOW_SIZE && i < count; i++)
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
			    dataInfo[i].first= TIMEXP;
            }
        }
    }
    return true;
}

void Transporter::UpdateTime(int del)
{
    int k;
    if (last < 0) k = 0;
    else k= last;

    for (; k < count; k++)
    if (dataInfo[k].first > 0)
    {
        dataInfo[k].first -= del;
    }
}

bool Transporter::RefreshWindow()
{
    for (int i = last + 1; i < count; i++)
    {
        if(!dataInfo[i].second) break;

        ++last;
        int l;

        current = i;
        if (IsLast())   l = size - i * DATA_SIZE;
        else            l = DATA_SIZE;

        if (fwrite(dataVec[i % WINDOW_SIZE], sizeof(uint8_t), l, file) < 1) return false;
    }

    return true;
}

bool Transporter::Get(uint8_t *buff)
{
    int i, len;

    if (sscanf((char *) buff, "DATA %d %d\n", &i, &len) != 2) return false;

    i /= DATA_SIZE;

    if (!dataInfo[i].second)
    {
        dataInfo[i].second = 1;
        memcpy(dataVec[i % WINDOW_SIZE], strchr((char *) buff, '\n') + 1, len);
    }
    return true;
}

bool Transporter::GetData()
{
    fd_set descriptors;
    std::chrono::duration<int, std::micro> duration_time = std::chrono::duration<int, std::micro>();    
	timeval tv;
	tv.tv_sec = 0;
	tv.tv_usec = 500000;
    FD_ZERO (&descriptors);
	FD_SET (sockfd, &descriptors);

    auto start = std::chrono::high_resolution_clock::now();

    int sel = select(sockfd + 1, &descriptors, NULL, NULL, &tv);

    if (sel == -1)  return false;

    auto end = std::chrono::high_resolution_clock::now();
    
    auto elapsed = std::chrono::duration_cast<std::chrono::microseconds>(end - start);
    UpdateTime(elapsed.count());

    if (sel == 0)   return true;
    
    for (int i = 0; i < sel; i++)
    {
        sockaddr_in sender;
        socklen_t length = sizeof(sender);
        uint8_t buffer[IP_MAXPACKET + 1];

        int dataSize = recvfrom(sockfd, buffer, IP_MAXPACKET, 0, (struct sockaddr*)&sender, &length);

        if (dataSize < 0)   return false;

        char sendIP[20];

        inet_ntop(AF_INET, &(sender.sin_addr), sendIP, sizeof(sendIP));

        if (!strcmp(destination, sendIP) && (ntohs(sender.sin_port) == port))
        {
            Get(buffer);
        }
    }

    return RefreshWindow();
}