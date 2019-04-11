from flask import Flask
import os
import time

app = Flask(__name__)
print(__name__)
print(app.instance_path)

try:
    os.makedirs(app.instance_path)
except OSError:
    pass

print('wsgi app : ', app.wsgi_app)

@app.route('/')
def hello():
    time.sleep(5)
    return 'Hello World !!!!!'	

