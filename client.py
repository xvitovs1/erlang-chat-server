# telnet program example
import socket, select, string, sys

def prompt() :
    sys.stdout.write('<You> ')
    sys.stdout.flush()

#main function
if __name__ == "__main__":

    host = 'localhost'
    port = 8080

    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.settimeout(100)

    # connect to remote host
    s.connect((host, port))

    print('Connected to remote host. Start sending messages')
    prompt()

    while 1:
        socket_list = [sys.stdin, s]

        # Get the list sockets which are readable
        read_sockets, write_sockets, error_sockets = select.select(socket_list , [], [])

        for sock in read_sockets:
            #incoming message from remote server
            if sock == s:
                data = sock.recv(4096)
                if not data == b'':
                    print('\nDisconnected from chat server')
                    sys.exit()
                else:
                    sys.stdout.write(data.decode('utf-8'))
                    prompt()
            #user entered a message
            else :
                msg = sys.stdin.readline()
                s.send(msg.encode('utf-8'))
                prompt()
