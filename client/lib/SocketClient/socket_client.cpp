#include "socket_client.h"
#include <cerrno>
#include <regex>
#include <cstring>
#include <iostream>

// #include <sys/types.h>

#if defined(WINDOWS)
#define _WINNT_WIN32 0x0601
#define NOMINMAX
#include <windows.h>
#include <winerror.h>

#elif defined(LINUX)
#include <arpa/inet.h>
#include <unistd.h>
#include <errno.h>

#else
#endif

#include <fcntl.h>

#include <fmt/format.h>

using namespace socket_client;

void Socket_client::init(const std::string& ip_port) {
    // Extract ip and port
    std::regex regex("([0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}):([0-9]{1,5})");
    std::smatch match;
    if (std::regex_match(ip_port, match, regex)) {
        server_ip = match[1].str();
        server_port = match[2].str();
    } else {
        throw std::runtime_error("run program as ./client xxx.xxx.xxx.xxx:20741");
    }

#if defined(WINDOWS)
    WSADATA wsaData;
    int result = WSAStartup(MAKEWORD(2, 2), &wsaData);
    if (result != 0) {
        std::cerr << "WSAStartup failed with error: " << result << std::endl;
        throw std::runtime_error("WSAStartup failed");
    }
#endif
    
    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_family = AF_INET;
    int ret = getaddrinfo(server_ip.c_str(), server_port.c_str(), &hints, &res);
    if (ret != 0) {
	std::cout << "errno = " << errno << "\n";
        throw std::runtime_error("getaddrinfo failed");
    }

    bool connect_succeed = false;
    for (rp = res; rp != NULL; rp = rp->ai_next) {

#if defined(WINDOWS)
	sock = socket(AF_INET, SOCK_STREAM, 0);
	if (sock == INVALID_SOCKET)
            continue;

	if (connect(sock, rp->ai_addr, rp->ai_addrlen) != -1) {
            connect_succeed = true;
            break;
        }

	closesocket(sock);
	
#elif defined(LINUX)
	socket_fd = socket(AF_INET, SOCK_STREAM, 0);
	if (socket_fd == -1)
	    continue;
	
	if (connect(socket_fd, rp->ai_addr, rp->ai_addrlen) != -1) {
	    connect_succeed = true;
	    break; 
	}

	close(socket_fd);
#endif
      
    }
    
    freeaddrinfo(res);

    if (!connect_succeed)
	throw std::runtime_error("connect failed");
}

Socket_client::Socket_client(const std::string& ip_port) {
    init(ip_port);
}

Socket_client::~Socket_client() {
#if defined(WINDOWS)
    closesocket(sock);
#elif defined(LINUX)
    close(socket_fd);
#endif
}

void Socket_client::send_msg(const char *msg) {
#if defined(WINDOWS)
    int ret = send(sock, msg, strlen(msg), 0);
#elif defined(LINUX)
    int ret = send(socket_fd, msg, strlen(msg), 0);
#endif
    if (ret == -1) {
        std::cout << "errno = " << errno << "\n";
        throw std::runtime_error("send failed");
    }
    
#if defined(WINDOWS)
    ret = send(sock, "\n", strlen("\n"), 0);
#elif defined(LINUX)
    ret = send(socket_fd, "\n", strlen("\n"), 0);
#endif

    if (ret == -1) {
        std::cout << "errno = " << errno << "\n";
        throw std::runtime_error("send failed");
    }
    
    // std::cout << "write msg : " << msg << std::endl;
}

void Socket_client::send_msg(const std::string& msg) {
    const char* msg_buf = msg.c_str();
#if defined(WINDOWS)
    int ret = send(sock, msg_buf, strlen(msg_buf), 0);
#elif defined(LINUX)
    int ret = send(socket_fd, msg_buf, strlen(msg_buf), 0);
#endif
    if (ret == -1) {
        std::cout << "errno = " << errno << "\n";
        throw std::runtime_error("send failed");
    }

#if defined(WINDOWS)
    ret = send(sock, "\n", strlen("\n"), 0);
#elif defined(LINUX)
    ret = send(socket_fd, "\n", strlen("\n"), 0);
#endif
    if (ret == -1) {
        std::cout << "errno = " << errno << "\n";
        throw std::runtime_error("send failed");
    }
    
    // std::cout << "write msg : " << msg << std::endl;
}

void Socket_client::read_msg() {
    bool newline_visited = false;
    memset(buf, 0, sizeof(buf));
    size_t buf_end = 0;
    
    while (!newline_visited || buf_end == 0) {
	newline_visited = false;
	// recv from socket if all characters in temp_buf are read already
	if (temp_buf_unread_begin == temp_buf_unread_end) {
	    memset(temp_buf, 0, sizeof(temp_buf));
#if defined(WINDOWS)
	    int ret = recv(sock, temp_buf, sizeof(temp_buf), 0);
#elif defined(LINUX)
	    int ret = recv(socket_fd, temp_buf, sizeof(temp_buf), 0);
#endif
	    
	    // std::cout << "recv msg : " << temp_buf << "\n";
	    if (ret == -1) {
		std::cout << "errno = " << errno << "\n";
		throw std::runtime_error("recv failed");
	    }

	    temp_buf_unread_begin = 0;
	    temp_buf_unread_end = ret;
	}

	// copy from temp_buf characters before newline
	for (size_t i = temp_buf_unread_begin; i < temp_buf_unread_end; i++) {
	    char c = temp_buf[i];
	    if (c == '\n') {
		// copy before \n
		int copy_len = i - temp_buf_unread_begin;

		if (buf_end + copy_len >= bufsize) {
		    std::cout << "1 " << buf_end << " " << copy_len << " " << bufsize << std::endl;
		    throw std::runtime_error("error msg greater than bufsize");
		}
		
		memcpy(buf + buf_end, temp_buf + temp_buf_unread_begin, copy_len);
		buf_end += copy_len;
		buf[buf_end] = '\0';
		temp_buf_unread_begin = i + 1;
		newline_visited = true;
		break;
	    }
	}
	
	if (!newline_visited) {
	    int copy_len = temp_buf_unread_end - temp_buf_unread_begin;
	    // leave space for '\0'
	    if (buf_end + copy_len >= bufsize) {
		std::cout << "2 " << buf_end << " " << copy_len << " " << bufsize << std::endl;

		throw std::runtime_error("error msg greater than bufsize");
	    }
		
	    memcpy(buf + buf_end, temp_buf + temp_buf_unread_begin, copy_len);
	    buf_end += copy_len;
	    temp_buf_unread_begin = temp_buf_unread_end;
	}
    }

    // fmt::print("begin {} end {}\n", temp_buf_unread_begin, temp_buf_unread_end);
    // std::cout << "read msg : " << buf << "\n";
}
