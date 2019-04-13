#include <iostream>
#include <arpa/inet.h>
#include "errno.h"
#include <string.h>
#include <netinet/in.h>
#include <vector>
#include <cmath>
#include <memory>
#include "transport.h"

int main(int argc, char *argv[])
{
    if (argc != 4)
    {
        std::cerr << "Wrong number of arguments!" << std::endl;
        return EXIT_FAILURE;
    }
    
    int port = strtol(argv[1], NULL, 10);
    std::cout <<port<<std::endl;
    if (port < MIN_PORT || port > MAX_PORT || errno == ERANGE)
    {
        return EXIT_FAILURE;
    }

    int size = strtol(argv[3], NULL, 10);
    if (size < 0 || errno  == ERANGE)
    {
        return EXIT_FAILURE;
    }

    char* fileName = argv[2];
    FILE *file;
    file = fopen(fileName, "w");
    if(file == NULL) return EXIT_FAILURE;

    std::unique_ptr<Transporter> t = std::make_unique<Transporter>(port, file, size);


    std::pair<int, uint8_t> dataPairs;

    fclose(file);
    return EXIT_SUCCESS;
}