
try:
    from cheroot.wsgi import Server as WSGIServer, PathInfoDispatcher
except ImportError:
    from cherrypy.wsgiserver import CherryPyWSGIServer as WSGIServer, WSGIPathInfoDispatcher as PathInfoDispatcher

from hello import app

d = PathInfoDispatcher({'/': app})
server = WSGIServer(('0.0.0.0', 5000), d)

if __name__ == '__main__':
   try:
      server.start()
   except KeyboardInterrupt:
      server.stop()	

