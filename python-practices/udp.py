import socket
import argparse
from datetime import datetime

MAX_BYTES = 65535

def server(port):
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.bind(('127.0.0.1', port))

    print(f"Server:  listening at {sock.getsockname()}")

    while True:
        data, address = sock.recvfrom(MAX_BYTES)
        text = data.decode('ascii')
        print(f"Server: Client at {address}, says => {text}")
        t = f"You have sent data of {len(data)} bytes"
        sock.sendto(t.encode('ascii'), address)


def client(port):
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    text = f"Time is {datetime.now()}"
    sock.sendto( text.encode('ascii'), ('127.0.0.1', port))
    print(f"Client: The OS assigned a port to a client: {sock.getsockname()}")
    data, address = sock.recvfrom(MAX_BYTES)
    print(f"Client: Received the text from server: {data.decode('ascii')} at {address}")


if __name__ == '__main__':
    print('starts')
    ap = argparse.ArgumentParser(description='Send and receive UDP in localhisr') 
    ap.add_argument('role', help='server or client')
    ap.add_argument('-p', type=int, default=4000, help='UDP port ( default 4000)')
    args = ap.parse_args()
    print(f"Arguments: {args}")

    if args.role == 'server':
        server(args.p)
    else:
        client(args.p)    


