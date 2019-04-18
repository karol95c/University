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
    if (port < 0|| port >= 65535 || errno == ERANGE)
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

    t->Process();
    
    fclose(file);
    return EXIT_SUCCESS;
}