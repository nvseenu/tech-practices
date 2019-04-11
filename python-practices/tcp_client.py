from socket import socket, AF_INET, SOCK_STREAM
s = socket(AF_INET, SOCK_STREAM)	
s.connect(('localhost', 20000))
s.send(b'Hello')
res = s.recv(8192)
print("Recieved resp ::", res)