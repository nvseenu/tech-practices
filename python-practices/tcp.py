from socketserver import TCPServer, BaseRequestHandler


class EchoHandler(BaseRequestHandler):

	def handle(self):
		print('Got connection from ', self.client_address)
		while True:
			msg = self.request.recv(8192)
			print('msg :', msg)
			if not msg:
				break
			self.request.send(msg)



if __name__	== '__main__':
	serv = TCPServer(('', 20000) , EchoHandler)
	serv.serve_forever()


