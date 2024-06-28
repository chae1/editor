#pragma once

#if defined(WINDOWS)
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
// #pragma comment(lib, "ws2_32.lib")
#define _WINNT_WIN32 0x0601
#include <winsock2.h>
#include <ws2tcpip.h>

#elif defined(LINUX)

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

#endif

#include <string>

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
#if defined(WINDOWS)
	SOCKET sock;
#elif defined(LINUX)
	int socket_fd;
#endif
	
	char temp_buf[bufsize];
	size_t temp_buf_unread_begin { 0 };
	size_t temp_buf_unread_end { 0 };
	
	std::string server_ip;
	std::string server_port;

	struct addrinfo hints;
	struct addrinfo *res, *rp;
    };
}
