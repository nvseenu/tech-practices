import cherrypy
import datetime
class HelloWorld:

	@cherrypy.expose
	def index(self):
		return "Hello world"

	@cherrypy.expose	
	def time(self):
		return str(datetime.datetime.now())

	@cherrypy.expose
	def sum(self, a=0, b=0):

		return str(int(a)+int(b))	



cherrypy.quickstart(HelloWorld())		
