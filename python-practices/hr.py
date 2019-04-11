from http.client import HttpConnection

h1 = HttpConnection('localhost', 5000, timeout=2)
req = h1.request('GET', '/')

