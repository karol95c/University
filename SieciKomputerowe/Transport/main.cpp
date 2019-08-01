#include <memory>

#include "transport.h"

int main(int argc, char *argv[])
{
    if (argc != 5)
    {
        std::cerr << "Wrong number of arguments!" << std::endl;
        return EXIT_FAILURE;
    }
    
    int port = strtol(argv[2], NULL, 10);
    if (port < 0|| port >= 65535 || errno == ERANGE)
    {
        return EXIT_FAILURE;
    }

    int size = strtol(argv[4], NULL, 10);
    if (size < 0 || errno  == ERANGE)
    {
        return EXIT_FAILURE;
    }

    char* fileName = argv[3];
    FILE *file;
    file = fopen(fileName, "w");
    if(file == NULL) return EXIT_FAILURE;

    char* destIP = argv[1];

    std::unique_ptr<Transporter> t = std::make_unique<Transporter>(port, file, size, destIP);

    t->Process();
    
    fclose(file);

    return EXIT_SUCCESS;
}