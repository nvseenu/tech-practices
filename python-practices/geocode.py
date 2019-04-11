import json
import http.client
import socket
from urllib.parse import quote_plus


def geocode(address):

    con = http.client.HTTPConnection('maps.google.com')
    addr = quote_plus(address)
    print(f"addr: {addr}")
    con.request('GET', f"/maps/api/geocode/json?address={addr}&sensor=false")
    rawreply = con.getresponse().read()
    print(rawreply)
    reply = json.loads(rawreply.decode('utf-8'))
    print(reply)


def geocode_over_tcp(address):

    req_message = """
    GET /maps/api/geocode/json?address={}&sensor=false HTTP/1.1\r\n\
    Host: maps.google.com:80\r\n\
    User-Agent: geocode.py \r\n\
    Connection: close\r\n\
    \r\n\
    """
    print(req_message)

    msg =  req_message.format(quote_plus(address))
    soc = socket.socket()
    soc.connect(('maps.google.com', 80))
    soc.sendall(msg.encode('ascii'))
    raw_reply = b''

    while True:
        more = soc.recv(4096)
        print(f"more => {more}")
        if not more:
            break

        raw_reply += more

    print(raw_reply.decode('utf-8'))    




if __name__ == '__main__':
    #geocode('207 N. Defiance St, Archbold, OH')    
    geocode_over_tcp('207 N. Defiance St, Archbold, OH')    