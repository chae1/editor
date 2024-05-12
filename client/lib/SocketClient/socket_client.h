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
	Socket_client() {}
	Socket_client(const std::string& ip_port);

	~Socket_client();

	char buf[bufsize];

	void init(const std::string& ip_port);
	void send_msg(const char *msg_buf);
	void send_msg(const std::string& msg);
	
	void read_msg();
	bool recv_msg();
		
    private:
	int socket_fd;

	char temp_buf[bufsize];
	size_t temp_buf_unread_begin { 0 };
	size_t temp_buf_unread_end { 0 };
	
	std::string server_ip;
	std::string server_port;

	struct addrinfo hints;
	struct addrinfo *res, *rp;
    };
}
