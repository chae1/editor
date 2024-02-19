#pragma once

#include <string>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>

namespace socket_client {
    constexpr size_t bufsize = 256;
    
    class Socket_client {
    public:
	Socket_client() = delete;
	Socket_client(const std::string& ip_port);

	~Socket_client();

	char buf[bufsize];

	void write_msg(const char *write_msg_buf);
	void read_msg();
	
    private:
	int socket_fd;

	char temp_buf[bufsize];
	size_t temp_buf_unread_begin;
	size_t temp_buf_unread_end;
	
	std::string server_ip;
	std::string server_port;

	struct addrinfo hints;
	struct addrinfo *res, *rp;
    };
}
