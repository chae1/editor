#include "socket_client.h"
#include <exception>
#include <stdexcept>
#include <string>
#include <iostream>

using namespace socket_client;

int main(int argc, char* argv[]) {
    try {
        if (argc <= 2) {
            throw std::runtime_error("error : argc <= 2");
        }

        std::string ip_port = argv[1];
	Socket_client client { ip_port };
	
        auto username = argv[2];
        client.write_msg(username);
        client.write_msg("\n");

        client.write_msg("hi from client.\n");
        client.write_msg("hi from client.\n");

        client.read_msg();
	std::cout << client.buf << std::endl;
    }

    catch (const std::exception& e) {
        std::cerr << e.what() << std::endl;
        return -1;
    }

    return 0;
}
